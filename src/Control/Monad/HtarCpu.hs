{-# LANGUAGE GeneralizedNewtypeDeriving
	, FlexibleContexts
	, PatternSynonyms
	#-}

module Control.Monad.HtarCpu
	( -- * HTAR9 Cpu Monad
	  HCpu
	, runHCpu
	, runHCpuWith
	  -- * Errors
	, HCpuError
	) where

import Data.Asm
import Control.Monad.Cpu

import Prelude hiding (and)
import Data.Word
import Data.Bits
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

-- | Error type for invalid instructions like jumping to a label
-- which the native htar cpu does not understand.
data HCpuError = InvalidJumpToLabelError (CpuState Word8)
	| InvalidStartingCpuState (CpuState Word8)
	deriving (Show)

-- | HTAR9 Cpu monad type.
newtype HCpu s a = HCpu { unHCpu :: ExceptT HCpuError (ReaderT (V.Vector Inst) (Cpu Word8 s)) a }
	deriving (Functor, Applicative, Monad,
		MonadCpu Word8, MonadReader (V.Vector Inst), MonadError HCpuError)

-- | Like 'runHCpu' but allows bootstrapping starting CpuState. NB: HTAR9 cpu's
-- expects 8 registers, a single conditional flag, and 256 ram entries.
runHCpuWith :: CpuState Word8 -> V.Vector Inst -> Either HCpuError (CpuState Word8)
runHCpuWith s v
	| V.length (cpuRegs s) >= 8
		&& V.length (cpuFlags s) >= 1
		&& V.length (cpuRam s) >= 256
		= case runCpu (runReaderT (runExceptT (unHCpu evalNext)) v) s of
				((Right _), s) -> Right s
				((Left e) , _) -> Left e
	| otherwise = Left $ InvalidStartingCpuState s

-- | Runs HTAR9 instructions to completion or error.
-- Only native HTAR9 instructions so
-- a jump to a label will cause an error.
runHCpu = runHCpuWith defState
	where
		defState :: CpuState Word8
		defState = CpuState
			{ cpuRegs  = V.replicate 8 0
			, cpuFlags = V.singleton False
			, cpuRam   = V.replicate 256 0
			, cpuPc    = 0
			}

mv :: MonadCpu Word8 m => Reg -> m ()
mv (R r) = do
	x <- getReg 0
	setReg (fromIntegral r) x
	setReg 0 0
	modifyPc (+1)

str :: MonadCpu Word8 m => Reg -> m ()
str (R r) = do
	i <- getReg (fromIntegral r)
	x <- getReg 0
	setRam (fromIntegral i) x
	modifyPc (+1)

ld :: MonadCpu Word8 m => Reg -> m ()
ld (R r) = do
	i <- getReg (fromIntegral r)
	x <- getRam (fromIntegral i)
	setReg 0 x
	modifyPc (+1)

-- | Applies the binary opertor on the contents of ra and
-- the contents of the passed in register in that order.
binOp :: MonadCpu Word8 m => (Word8 -> Word8 -> Word8) -> RegImm -> m ()
binOp op v = do
	x <- getReg 0
	y <- case v of
		RegImmR r -> getReg (fromIntegral r)
		RegImmI x -> return x
	setReg 0 (x `op` y)
	modifyPc (+1)

add :: MonadCpu Word8 m => RegImm -> m ()
add v = do
	x <- case v of
		(RegImmR r) -> getReg (fromIntegral r)
		(RegImmI x) -> return x
	y <- getReg 0
	let res = x + y
	setReg 0 res
	if res < (max x y) -- if overflow
		then setFlag 0 False
		else setFlag 0 True
	modifyPc (+1)

sub :: MonadCpu Word8 m => RegImm -> m ()
sub y = binOp (-) y >> setConditionIfRegA (/= 0)

and :: MonadCpu Word8 m => RegImm -> m ()
and y = binOp (.&.) y >> setConditionIfRegA (== 0)

lshft :: MonadCpu Word8 m => RegImm -> m ()
lshft y = binOp (\x y -> unsafeShiftL x (fromIntegral y)) y
	>> setConditionIfRegA (/= 0)

rshft :: MonadCpu Word8 m => RegImm -> m ()
rshft y = binOp (\x y -> unsafeShiftR x (fromIntegral y)) y
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

ba :: MonadCpu Word8 m => Jump -> m ()
ba (JOffset x) = branchIf True x

setConditionIfRegA :: MonadCpu Word8 m => (Word8 -> Bool) -> m ()
setConditionIfRegA p = do
	x <- getReg 0
	if p x
		then setFlag 0 True
		else setFlag 0 False

evalNext :: (MonadCpu Word8 m, MonadReader (V.Vector Inst) m) => m ()
evalNext = do
	insts <- ask
	pc <- getPc
	let inst = insts V.! (fromIntegral pc)
	evalInst inst

-- TODO error checking
evalInst :: (MonadCpu Word8 m, MonadReader (V.Vector Inst) m) => Inst -> m ()
evalInst Fin   = modifyPc (+1)
evalInst Reset = setPc 0
evalInst inst  = evalInst' inst >> evalNext
	where
		evalInst' :: (MonadCpu Word8 m, MonadReader (V.Vector Inst) m) => Inst -> m ()
		evalInst' (Mv  r) = mv r
		evalInst' (Str r) = str r
		evalInst' (Ld  r) = ld r
		evalInst' (Add o) = add o
		evalInst' (Sub o) = sub o
		evalInst' (And o) = and o
		evalInst' (Lshft o) = lshft o
		evalInst' (Rshft o) = rshft o
		evalInst' (Bcs j@(JOffset _)) = bcs j
		evalInst' (Ba  j@(JOffset _)) = ba  j
