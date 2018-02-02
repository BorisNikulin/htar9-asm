{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.HtarCpu where

import Data.Asm
import Control.Monad.Cpu

import Prelude hiding (and)
import Data.Word
import Data.Bits
import qualified Data.Vector as V
import Control.Monad.Except
import Control.Monad.Reader

data HCpuError = InvalidJumpToLabelError (CpuState Word8)
	deriving (Show)

newtype HCpu s a = HCpu { unHCpu :: ExceptT HCpuError (ReaderT (V.Vector Inst) (Cpu Word8 s)) a }
	deriving (Functor, Applicative, Monad,
		MonadCpu Word8, MonadReader (V.Vector Inst), MonadError HCpuError)

runHCpu :: V.Vector Inst -> Either HCpuError (CpuState Word8)
runHCpu v = result
	where
		defState :: CpuState Word8
		defState = CpuState
			{ cpuRegs  = V.replicate 8 0
			, cpuFlags = V.singleton False
			, cpuRam   = V.replicate 20 0
			, cpuPc    = 0
			}
		result = case runCpu (runReaderT (runExceptT (unHCpu action)) insts) defState of
			((Right _), s) -> Right s
			((Left e) , _) -> Left e

insts = V.fromList [Mv $ Reg 2]

{-action :: HCpu s Word8-}
action = do
	setReg 2 3
	setReg 5 20
	setRam 0 66
	mv $ Reg 5
	add $ Register $ Reg 5
	add $ Imm 5
	str $ Reg 2
	mv $ Reg 0
	ld $ Reg 0


mv :: Reg  -> HCpu s ()
mv (Reg r) = do
	if r == 0
		then setReg 0 0
		else do
			x <- getReg (fromIntegral r)
			setReg 0 x
	modifyPc (+1)

str :: Reg -> HCpu s ()
str (Reg r) = do
	i <- getReg (fromIntegral r)
	x <- getReg 0
	setRam (fromIntegral i) x
	modifyPc (+1)

ld :: Reg -> HCpu s ()
ld (Reg r) = do
	i <- getReg (fromIntegral r)
	x <- getRam (fromIntegral i)
	setReg 0 x
	modifyPc (+1)

-- | Apply the binary opertor on the contents of the register
-- or the immediate and the contents of ra in that order.
binOp :: (Word8 -> Word8 -> Word8) -> RegImm -> HCpu s ()
binOp op v = do
	x <- case v of
		(Register (Reg r)) -> getReg (fromIntegral r)
		(Imm x)            -> return x
	y <- getReg 0
	setReg 0 (x `op` y)
	modifyPc (+1)

add = binOp (+)
sub = binOp (-)
and = binOp (.&.)
lshft = binOp $ \x y -> unsafeShiftL y (fromIntegral x)
rshft = binOp $ \x y -> unsafeShiftR y (fromIntegral x)

evalNext :: HCpu s ()
evalNext = do
	insts <- ask
	pc <- getPc
	let inst = insts V.! (fromIntegral pc)
	evalInst inst

evalInst :: Inst -> HCpu s ()
evalInst (Mv r)  = mv r  >> evalNext
evalInst (Str r) = str r >> evalNext
evalInst (Ld r)  = ld r  >> evalNext
evalInst (Fin)   = return ()
evalInst (Add o) = add o >> evalNext
evalInst (Sub o) = sub o >> evalNext
evalInst (And o) = and o >> evalNext
evalInst (Lshft o) = lshft o >> evalNext
evalInst (Rshft o) = rshft o >> evalNext

