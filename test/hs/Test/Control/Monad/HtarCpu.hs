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
import Lens.Micro.Platform
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
defState = CpuState [255,1,34,64,100,56,240,0] (V.replicate 2 False) (V.replicate 256 0 // [(0,77), (1, 240)]) 0

tests = testGroup "Control.Monad.HtarCpu"
	[ testCase "mv" $
		let Right s = runHCpuWith defState [Mv $ mkReg 1, Fin]
		in (s^?!cpuRegsL.ix 0, s^?!cpuRegsL.ix 1) @?= (0,  255)
	, testCase "mv ra" $
		let Right s = runHCpuWith defState [Mv $ mkReg 0, Fin]
		in s^?!cpuRegsL.ix 0 @?= 0
	, testCase "str" $
		let Right s = runHCpuWith defState [Str $ mkReg 2, Fin]
		in s^?!cpuRamL.ix 34 @?= 255
	, testCase "ld" $
		let Right s = runHCpuWith defState [Ld $ mkReg 7, Fin]
		in s^?!cpuRegsL.ix 0 @?= 77
	, testCase "fin" $
		let Right s = runHCpuWith defState [Fin]
		in s^.cpuPcL @?= 1
	, testCase "reset" $
		let Right s = runHCpuWith defState [Bcs $ mkJumpOffset 2, Ba $ mkJumpOffset 2, Fin, And $ mkImmediate 0 ,Reset]
		in s^.cpuPcL @?= 3
	, testProperty "add" $
		\(I x) ->
			let Right s = runHCpuWith defState [Mv $ mkReg 0, Ld $ mkReg 1, Add $ mkImmediate x, Fin]
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (240 + x, x <= 255 - 240)
	, testProperty "sub" $
		\(I x) ->
			let
				Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, Sub $ mkImmediate x, Fin]
				res     = 34 - x
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (res, res /= 0)
	, testProperty "and" $
		\(I x) ->
			let
				Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, And $ mkImmediate x, Fin]
				res     = 34 .&. x
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (res, res == 0)
	, testProperty "lshft" $
		\(I x) ->
			let
				Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 49, Lshft $ mkImmediate x, Fin]
				res     = shiftL 49 (fromIntegral x)
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (res, res /= 0)
	, testProperty "rshft" $
		\(I x) ->
			let
				Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, Rshft $ mkImmediate x, Fin]
				res     = shiftR 34 (fromIntegral x)
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (res, res /= 0)
	, testCase "bcs true" $
		let Right s = runHCpuWith defState [And $ mkImmediate 0, Bcs $ mkJumpOffset 2, Fin, Mv $ mkReg 7, Fin]
		in (s^?!cpuFlagsL.ix 0, s^.cpuPcL) @?= (True, 5)
	, testCase "bcs false" $
		let Right s = runHCpuWith defState [And $ mkImmediate 1, Bcs $ mkJumpOffset 2, Fin, Mv $ mkReg 7, Fin]
		in (s^?!cpuFlagsL.ix 0, s^.cpuPcL) @?= (False, 3)
	, testCase "ba" $
		let Right s = runHCpuWith defState [Ba $ mkJumpOffset 2, Fin, Mv $ mkReg 7, Fin]
		in s^.cpuPcL @?= 4
	, withResource
		(initResource "./test/golden/mult.s")
		(hClose . view _1)
		emulTest
	]

initResource :: FilePath -> IO (Handle, V.Vector Inst)
initResource fp = do
	h <- openFile fp ReadMode
	file <- hGetContents h
	let
		Right (instsWithLabels, t) = parseAsm "mult.s" file
		Right insts                = translateLabels t instsWithLabels
	return (h, V.fromList insts)

emulTest :: IO (Handle, V.Vector Inst) -> TestTree
emulTest getResource =
	testGroup "Program"
	[ testProperty "mult" $ \x y z -> monadic $ do
		(_, insts) <- getResource
		let
			Right s = runHCpuWith defHCpuState{cpuRam = cpuRam defHCpuState // [(1, x), (2, y), (3, z)]} insts
			toWord = fromIntegral :: (Word8 -> Word)
			resWord = (toWord x) * (toWord y) * (toWord z)
			low8Mask = shiftL 1 8 - 1 -- 255 done fancily
			high8Mask = shiftL low8Mask 8
			resHigh = fromIntegral $ shiftR (resWord .&. high8Mask) 8
			resLow  = fromIntegral $ resWord .&. low8Mask
		return $ V.slice 4 2 (cpuRam s) == [resHigh, resLow]
	]
