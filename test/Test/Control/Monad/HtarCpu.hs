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
import Text.AsmParser
import Text.AsmTranslator
import qualified Text.AsmBinParser as Bin

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Data.Word
import Data.Vector ((//))
import qualified Data.Vector as V
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO
import Numeric.Natural

-- TODO mayeb also a tasty option for this depth?
instance Monad m => Serial m Word8 where
	series = localDepth (*4) $ (fromIntegral :: Natural -> Word8) <$> series

-- TODO make tasty options for Reg and Imm depth and probably default to max but /shrug
instance Monad m => Serial m Reg where
	series = generate $ \_ -> mkReg <$> [0..7]

instance Monad m => Serial m Imm where
	series = generate $ \_ -> mkImm <$> [0..55]

instance Monad m => Serial m RegImm where
	series = (mkRegImmFromReg <$> series) \/ (mkRegImmFromImm <$> series)

regs = cpuRegs defState
ram  = cpuRam  defState

defState :: CpuState Word8
defState = CpuState [255,1,34,64,100,56,240,0] (V.singleton False) (V.replicate 256 0 // [(0,77), (1, 240)]) 0

tests = testGroup "Control.Monad.HtarCpu"
	[ testCase "mv" $
		let Right s = runHCpuWith defState [Mv $ mkReg 1, Fin]
		in s @?= defState{cpuRegs = regs // [(0,0), (1, 255)], cpuPc = 2}
	, testCase "mv ra" $
		let Right s = runHCpuWith defState [Mv $ mkReg 0, Fin]
		in s @?= defState{cpuRegs = regs // [(0,0)], cpuPc = 2}
	, testCase "str" $
		let Right s = runHCpuWith defState [Str $ mkReg 2, Fin]
		in s @?= defState{cpuRam = ram // [(34,255)], cpuPc = 2}
	, testCase "ld" $
		let Right s = runHCpuWith defState [Ld $ mkReg 7, Fin]
		in s @?= defState{cpuRegs = regs // [(0,77)], cpuPc = 2}
	, testCase "fin" $
		let Right s = runHCpuWith defState [Fin]
		in s @?= defState{cpuPc = 1}
	, testCase "add" $
		let Right s = runHCpuWith defState [Ld $ mkReg 7, Add $ mkRegister 1, Fin]
		in s @?= defState{cpuRegs = regs // [(0,78)], cpuFlags = [True],  cpuPc = 3}
	, testProperty "add flagging" $
		\(I x) ->
			let Right s = runHCpuWith defState [Mv $ mkReg 0, Ld $ mkReg 1, Add $ mkImmediate x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, 240 + x)]
			, cpuFlags = [x <= 255 - 240]
			, cpuPc = 4
			}
	, testCase "sub" $
		let Right s = runHCpuWith defState [Sub $ mkRegister 1, Fin]
		in s @?= defState{cpuRegs = regs // [(0,254)], cpuFlags = [True], cpuPc = 2}
	, testProperty "sub flagging" $
		\(I x) ->
			let Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, Sub $ mkImmediate x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, 34 - x)]
			, cpuFlags = [34 - x /= 0]
			, cpuPc = 4
			}
	, testCase "and" $
		let Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 37, And $ mkRegister 5, Fin]
		in s @?= defState{cpuRegs = regs // [(0, 37 .&. 56)], cpuPc = 4}
	, testProperty "and flagging" $
		\(I x) ->
			let Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, And $ mkImmediate x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, 34 .&. x)]
			, cpuFlags = [34 .&. x == 0]
			, cpuPc = 4
			}
	, testCase "lshft" $
		let Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 1, Lshft $ mkImmediate 2, Fin]
		in s @?= defState{cpuRegs = regs // [(0, shiftL 1 2)], cpuFlags = [True], cpuPc = 4}
	, testProperty "lshft flagging" $
		\(I x) ->
			let Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 49, Lshft $ mkImmediate x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, shiftL 49 (fromIntegral x))]
			, cpuFlags = [shiftL (49 :: Word8) (fromIntegral x)  /= 0]
			, cpuPc = 4
			}
	, testCase "rshft" $
		let Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 55, Rshft $ mkImmediate 3, Fin]
		in s @?= defState{cpuRegs = regs // [(0, shiftR 55 3)], cpuFlags = [True], cpuPc = 4}
	, testProperty "rshft flagging" $
		\(I x) ->
			let Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, Rshft $ mkImmediate x, Fin]
			in s == defState
			{ cpuRegs = regs // [(0, shiftR 34 (fromIntegral x))]
			, cpuFlags = [shiftR (34 :: Word8) (fromIntegral x) /= 0]
			, cpuPc = 4
			}
	, testCase "bcs true" $
		let Right s = runHCpuWith defState [And $ mkImmediate 0, Bcs $ mkJumpOffset 2, Fin, Mv $ mkReg 7, Fin]
		in s @?= defState{cpuRegs = regs // [(0,0)], cpuFlags = [True], cpuPc = 5}
	, testCase "bcs false" $
		let Right s = runHCpuWith defState [And $ mkImmediate 1, Bcs $ mkJumpOffset 2, Fin, Mv $ mkReg 7, Fin]
		in s @?= defState{cpuRegs = regs // [(0,1)], cpuPc = 3}
	, testCase "ba" $
		let Right s = runHCpuWith defState [Ba $ mkJumpOffset 2, Fin, Mv $ mkReg 7, Fin]
		in s @?= defState{cpuRegs = regs // [(0,0), (7,255)], cpuPc = 4}
	, withResource
		initResource
		(hClose . fst)
		emulTest
	]

initResource = do
	h <- openFile "./test/golden/mult.s" ReadMode
	file <- hGetContents h
	let
		Right (instsWithLabels, t) = parseAsm "mult.s" file
		Right bin                  = translateAsms t instsWithLabels
		Right stripedInsts         = translateLabels t instsWithLabels
		Right insts                = Bin.parseAsm "mult" (BL.unpack $ BL.concat bin)
	-- checked in ghci that strippedInsts == insts (not really a surprise)
	-- emulator is probably the one that's broken
	return (h, V.fromList insts)

emulTest getResource =
	testGroup "Program"
	[ testProperty "mult" $ \x y z -> monadic $ do
		(_, insts) <- getResource
		let
			defState = CpuState
				(V.replicate 8 0)
				(V.singleton False)
				(V.replicate 256 0)
				0
			Right s = runHCpuWith defState{cpuRam = cpuRam defState // [(1, x), (2, y), (3, z)]} insts
			toWord = fromIntegral :: (Word8 -> Word)
			resWord = (toWord x) * (toWord y) * (toWord z)
			low8Mask = shiftL 1 8 - 1 -- 255 done fancily
			high8Mask = shiftL low8Mask 8
			resHigh = fromIntegral $ shiftR (resWord .&. high8Mask) 8
			resLow  = fromIntegral $ resWord .&. low8Mask
		return $ V.slice 4 2 (cpuRam s) == [resHigh, resLow]
	]
