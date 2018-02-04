{-# LANGUAGE OverloadedLists
	, FlexibleInstances
	, MultiParamTypeClasses
	, GeneralizedNewtypeDeriving
	#-}

module Test.Control.Monad.HtarCpu
	( tests
	) where

import Data.Asm
import Control.Monad.Cpu
import Control.Monad.HtarCpu

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Data.Word
import Data.Vector ((//))
import qualified Data.Vector as V
import Data.Bits

-- from
-- https://hackage.haskell.org/package/smallcheck-1.1.3.1/docs/src/Test-SmallCheck-Series.html#line-495
-- but Word changed to Word8
instance Monad m => Serial m Word8 where
	series = (fromIntegral :: N Int -> Word8) <$> series
instance Monad m => CoSerial m Word8 where
	coseries rs = (. (fromIntegral :: Word8 -> Int)) <$> coseries rs

-- | 'N' is a wrapper for 'Integral' types that causes only non-negative values
-- to be generated. Generated functions of type @N a -> b@ do not distinguish
-- different negative values of @a@.
newtype N a = N a deriving (Eq, Ord, Real, Enum, Num, Integral)

instance (Integral a, Serial m a) => Serial m (N a) where
	series = generate $ \d -> map (N . fromIntegral) [0..d]
-- end copy

regs = cpuRegs defState
ram  = cpuRam  defState

defState = CpuState [255,1,8,64,100,56,250,0] (V.singleton False) (V.replicate 256 0 // [(0, 77)]) 0

tests = testGroup "Control.Monad.HtarCpu"
	[ testCase "mv" $
		let Right s = runHCpuWith defState [Mv $ Reg 1, Fin]
		in s @?= defState{cpuRegs = regs // [(0, 1)], cpuPc = 2}
	, testCase "mv ra" $
		let Right s = runHCpuWith defState [Mv $ Reg 0, Fin]
		in s @?= defState{cpuRegs = regs // [(0, 0)], cpuPc = 2}
	, testCase "str" $
		let Right s = runHCpuWith defState [Str $ Reg 2, Fin]
		in s @?= defState{cpuRam = ram // [(8, 255)], cpuPc = 2}
	, testCase "ld" $
		let Right s = runHCpuWith defState [Ld $ Reg 7, Fin]
		in s @?= defState{cpuRegs = regs // [(0,77)], cpuPc = 2}
	, testCase "fin" $
		let Right s = runHCpuWith defState [Fin]
		in s @?= defState{cpuPc = 1}
	, testCase "add" $
		let Right s = runHCpuWith defState [Mv $ Reg 4, Add $ Register $ Reg 1, Fin]
		in s @?= defState{cpuRegs = regs // [(0,101)], cpuPc = 3}
	, testProperty "add flagging" $
		\x ->
			let Right s = runHCpuWith defState [Mv $ Reg 6, Add $ Imm x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, 250 + x)]
			, cpuFlags = [x > 255 - 250]
			, cpuPc = 3
			}
	, testCase "sub" $
		let Right s = runHCpuWith defState [Sub $ Register $ Reg 1, Fin]
		in s @?= defState{cpuRegs = regs // [(0,254)], cpuPc = 2}
	, testProperty "sub flagging" $
		\x ->
			let Right s = runHCpuWith defState [Mv $ Reg 2, Sub $ Imm 3, Sub $ Imm x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, 8 - 3 - x)]
			, cpuFlags = [8 - 3 - x == 0]
			, cpuPc = 4
			}
	, testCase "and" $
		let Right s = runHCpuWith defState [Mv $ Reg 4, And $ Register $ Reg 5, Fin]
		in s @?= defState{cpuRegs = regs // [(0, 100 .&. 56)], cpuPc = 3}
	, testProperty "and flagging" $
		\x ->
			let Right s = runHCpuWith defState [Mv $ Reg 1, And $ Imm x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, 1 .&. x)]
			, cpuFlags = [1 .&. x == 0]
			, cpuPc = 3
			}
	, testCase "lshft" $
		let Right s = runHCpuWith defState [Mv $ Reg 1, Lshft $ Imm 2, Fin]
		in s @?= defState{cpuRegs = regs // [(0, shift 1 2)], cpuPc = 3}
	, testProperty "lshft flagging" $
		\x ->
			let Right s = runHCpuWith defState [Mv $ Reg 3, Lshft $ Imm x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, shiftL 64 (fromIntegral x))]
			, cpuFlags = [shiftL (64 :: Word8) (fromIntegral x)  == 0]
			, cpuPc = 3
			}
	, testCase "rshft" $
		let Right s = runHCpuWith defState [Mv $ Reg 5, Rshft $ Imm 3, Fin]
		in s @?= defState{cpuRegs = regs // [(0, shift 56 (-3))], cpuPc = 3}
	, testProperty "rshft flagging" $
		\x ->
			let Right s = runHCpuWith defState [Mv $ Reg 2, Rshft $ Imm x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, shiftR 8 (fromIntegral x))]
			, cpuFlags = [shiftR (8 :: Word8) (fromIntegral x)== 0]
			, cpuPc = 3
			}
	, testCase "bcs true" $
		let Right s = runHCpuWith defState [Sub $ Imm 255, Bcs $ JumpOffset 2, Fin, Mv $ Reg 7, Fin]
		in s @?= defState{cpuRegs = regs // [(0,0)], cpuFlags = [True], cpuPc = 5}
	, testCase "bcs false" $
		let Right s = runHCpuWith defState [Sub $ Imm 0, Bcs $ JumpOffset 2, Fin, Mv $ Reg 7, Fin]
		in s @?= defState{cpuPc = 3}
	, testCase "ba" $
		let Right s = runHCpuWith defState [Ba $ JumpOffset 2, Fin, Mv $ Reg 7, Fin]
		in s @?= defState{cpuRegs = regs // [(0,0)], cpuPc = 4}
	]
