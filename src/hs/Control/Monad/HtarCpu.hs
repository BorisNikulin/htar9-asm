{-# LANGUAGE GeneralizedNewtypeDeriving
	, DeriveGeneric
	, FlexibleContexts
	, PatternSynonyms
	#-}

module Control.Monad.HtarCpu
	( -- * HTAR9 Cpu Monad
	  HCpu
	, runHCpu
	, runHCpuWith
	, stepHCpuWith
	, defHCpuState
	  -- * Errors
	, HCpuError
	) where

import Data.Asm
import Control.Monad.Cpu

import Prelude hiding (and, min)
import qualified Prelude as Prelude (min)
import Data.Word
import Data.Bits
import qualified Data.Vector as V
import Control.Monad.Except
import Control.Monad.Reader
import Control.DeepSeq
import GHC.Generics (Generic)

-- | Error type for invalid instructions like jumping to a label
-- which the native htar cpu does not understand.
data HCpuError =
	  InvalidJumpToLabelError (CpuState Word8)
	| InvalidStartingCpuState (CpuState Word8)
	deriving (Show, Generic)

instance NFData HCpuError

-- | HTAR9 Cpu monad type.
newtype HCpu s a = HCpu { unHCpu :: ExceptT HCpuError (ReaderT (V.Vector Inst) (Cpu Word8 s)) a }
	deriving (Functor, Applicative, Monad,
		MonadCpu Word8, MonadReader (V.Vector Inst), MonadError HCpuError)

defHCpuState :: CpuState Word8
defHCpuState = CpuState
	{ cpuRegs  = V.replicate 8 0
	, cpuFlags = V.replicate 2 False
	, cpuRam   = V.replicate 256 0
	, cpuPc    = 0
	}

-- | Like 'runHCpu' but allows bootstrapping starting CpuState. NB: HTAR9 cpu's
-- expects 8 registers, a single conditional flag, and 256 ram entries.
runHCpuWith :: CpuState Word8 -> V.Vector Inst -> Either HCpuError (CpuState Word8)
runHCpuWith s v
	| V.length (cpuRegs s) >= 8
		&& V.length (cpuFlags s) >= 2
		&& V.length (cpuRam s) >= 256
		= case runCpu (runReaderT (runExceptT (unHCpu eval)) v) s of
				((Right _), s') -> Right s'
				((Left e) , _) -> Left e
	| otherwise = Left $ InvalidStartingCpuState s

-- | Runs HTAR9 instructions to completion or error.
-- Only native HTAR9 instructions so
-- a jump to a label will cause an error.
runHCpu :: V.Vector Inst -> Either HCpuError (CpuState Word8)
runHCpu = runHCpuWith defHCpuState

stepHCpuWith :: CpuState Word8 -> Inst -> Either HCpuError (CpuState Word8)
stepHCpuWith s i
	| V.length (cpuRegs s) >= 8
		&& V.length (cpuFlags s) >= 2
		&& V.length (cpuRam s) >= 256
		-- stepInst MUST NEVER call ask and evaluate the result
		= case runCpu (runReaderT (runExceptT (unHCpu $ stepInst i)) undefined) s of
				((Right _), s') -> Right s'
				((Left e) , _) -> Left e
	| otherwise = Left $ InvalidStartingCpuState s

mv :: MonadCpu Word8 m => Reg -> m ()
mv (R r) = do
	x <- getReg 0
	setReg (fromIntegral r) x
	setReg 0 0
	modifyPc (+1)
mv _ = error "invalid args"

str :: MonadCpu Word8 m => Reg -> m ()
str (R r) = do
	i <- getReg (fromIntegral r)
	x <- getReg 0
	setRam (fromIntegral i) x
	modifyPc (+1)
str _ = error "invalid args"

ld :: MonadCpu Word8 m => Reg -> m ()
ld (R r) = do
	i <- getReg (fromIntegral r)
	x <- getRam (fromIntegral i)
	setReg 0 x
	modifyPc (+1)
ld _ = error "invalid args"

dist :: MonadCpu Word8 m => Reg -> m ()
dist (R r) = do
	x <- getReg 0
	y <- getReg (fromIntegral r)
	let absDistInt = abs (fromIntegral x - fromIntegral y) :: Int
	setReg 0 (fromIntegral absDistInt)
	modifyPc (+1)
dist _ = error "invalid args"

min :: MonadCpu Word8 m => Reg -> m ()
min (R r) = do
	x <- getReg 0
	y <- getReg (fromIntegral r)
	setReg 0 (Prelude.min x y)
	modifyPc (+1)
min _ = error "invalid args"

-- | Applies the binary opertor on the contents of ra and
-- the contents of the passed in register in that order.
binOp :: MonadCpu Word8 m => (Word8 -> Word8 -> Word8) -> RegImm -> m ()
binOp op v = do
	x <- getReg 0
	y <- case v of
		RegImmR r -> getReg (fromIntegral r)
		RegImmI a -> return a
		_         -> error "invalid args"
	setReg 0 (x `op` y)
	modifyPc (+1)

add :: MonadCpu Word8 m => RegImm -> m ()
add v = do
	x <- case v of
		(RegImmR r) -> getReg (fromIntegral r)
		(RegImmI x) -> return x
		_           -> error "invalid args"
	y <- getReg 0
	let res = x + y
	setReg 0 res
	-- TODO make this better :D
	if res < (max x y) -- if overflow
		then setFlag 0 False
		else setFlag 0 True
	modifyPc (+1)

sub :: MonadCpu Word8 m => RegImm -> m ()
sub y = binOp (-) y >> setConditionIfRegA (/= 0)

and :: MonadCpu Word8 m => RegImm -> m ()
and y = binOp (.&.) y >> setConditionIfRegA (== 0)

lshft :: MonadCpu Word8 m => RegImm -> m ()
lshft a = binOp (\x y -> shiftL x (fromIntegral y)) a
	>> setConditionIfRegA (/= 0)

rshft :: MonadCpu Word8 m => RegImm -> m ()
rshft a = binOp (\x y -> shiftR x (fromIntegral y)) a
	>> setConditionIfRegA (/= 0)

branchIf :: MonadCpu Word8 m => Bool -> Int -> m ()
branchIf b x = do
	if b
		then do
			-- do a max branch distance check here?
			modifyPc $ (+) (fromIntegral x)
		else
			modifyPc (+1)

bcs :: MonadCpu Word8 m => Jump -> m ()
bcs (JOffset x) = do
	flag <- getFlag 0
	branchIf flag x
bcs _ = error "invalid args"

ba :: MonadCpu Word8 m => Jump -> m ()
ba (JOffset x) = branchIf True x
ba _ = error "invalid args"

setConditionIfRegA :: MonadCpu Word8 m => (Word8 -> Bool) -> m ()
setConditionIfRegA p = do
	x <- getReg 0
	if p x
		then setFlag 0 True
		else setFlag 0 False

eval :: (MonadCpu Word8 m, MonadReader (V.Vector Inst) m, MonadError HCpuError m) => m ()
eval = do
	isDone <- getFlag 1
	if isDone
		then return ()
		else do
			insts <- ask :: (MonadReader (V.Vector Inst) m => m (V.Vector Inst))
			pc <- getPc
			let inst = insts V.! (fromIntegral pc)
			stepInst inst >> eval

stepInst :: (MonadCpu Word8 m, MonadReader (V.Vector Inst) m, MonadError HCpuError m) => Inst -> m ()
stepInst Fin   = setFlag 1 True >> modifyPc (+1)
stepInst Reset = setPc 0
stepInst (Mv   r) = mv r
stepInst (Str  r) = str r
stepInst (Ld   r) = ld r
stepInst (Dist r) = dist r
stepInst (Min  r) = min r
stepInst (Add  o) = add o
stepInst (Sub  o) = sub o
stepInst (And  o) = and o
stepInst (Lshft o) = lshft o
stepInst (Rshft o) = rshft o
stepInst (Bcs j@(JOffset _))  = bcs j
stepInst (Bcs   (JLabel _ _)) = getState >>= throwError . InvalidJumpToLabelError
stepInst (Ba  j@(JOffset _))  = ba  j
stepInst (Ba    (JLabel _ _)) = getState >>= throwError . InvalidJumpToLabelError
stepInst _ = error "invalid args"
