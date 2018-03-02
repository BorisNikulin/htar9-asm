{-# LANGUAGE OverloadedLists
	, FlexibleInstances
	, MultiParamTypeClasses
	, GeneralizedNewtypeDeriving
	, BinaryLiterals
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
import qualified Test.Tasty.QuickCheck as QC
{-import Test.Tasty.QuickCheck hiding (testProperty)-}
import Data.Word
import Data.List
import Data.Vector ((//))
import qualified Data.Vector as V
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Lens.Micro.Platform
import System.IO
import Numeric.Natural

newtype Pattern = Pattern Word8
	deriving(Show, Eq, Ord, Enum, Num, Real, Integral, Bits, FiniteBits)

instance Bounded Pattern where
	minBound = 0
	maxBound = 15

newtype SixtyFourWord8s = SixtyFourWord8s [Word8]
	deriving(Show, Eq, Ord)

newtype TwentyWord8s = TwentyWord8s [Word8]
	deriving(Show, Eq, Ord)



-- TODO maybe also a tasty option for this depth?
instance Monad m => Serial m Word8 where
	series = localDepth (*4) $ (fromIntegral :: Natural -> Word8) <$> series

-- TODO make tasty options for Reg and Imm depth and probably default to max but /shrug
instance Monad m => Serial m Reg where
	series = generate $ \_ -> mkReg <$> [0..7]

instance Monad m => Serial m Imm where
	series = generate $ \_ -> mkImm <$> [0..55]

instance Monad m => Serial m RegImm where
	series = (mkRegImmFromReg <$> series) \/ (mkRegImmFromImm <$> series)

instance Monad m => Serial m Pattern where
	series = generate $ \_ -> Pattern <$> [0..15]

instance Monad m => Serial m SixtyFourWord8s where
	series = SixtyFourWord8s <$> (sequence $ replicate 64 series)


instance QC.Arbitrary Pattern where
	arbitrary = Pattern <$> QC.choose (0, 15)

instance QC.Arbitrary SixtyFourWord8s where
	arbitrary = SixtyFourWord8s <$> QC.vector 64

instance QC.Arbitrary TwentyWord8s where
	arbitrary = TwentyWord8s <$> QC.vector 20


defState :: CpuState Word8
defState = CpuState [255,1,34,64,100,56,240,0] (V.replicate 2 False) (V.replicate 256 0 // [(0,77), (1, 240)]) 0

defStateWithRa :: Word8 -> CpuState Word8
defStateWithRa w = set (cpuRegsL.ix 0) w defState

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
	, testProperty "dist" $
		\w (R r) ->
			let
				defState' = defStateWithRa w
				Right s = runHCpuWith defState' [Dist $ mkReg r, Fin]
				dist =  abs $
					(fromIntegral w)
					- (defState'^?!cpuRegsL.ix (fromIntegral r).to fromIntegral) :: Int
			in s^?!cpuRegsL.ix 0 == fromIntegral dist
	, testProperty "min" $
		\w (R r) ->
			let Right s = runHCpuWith (defStateWithRa w) [Min $ mkReg r, Fin]
			in s^?!cpuRegsL.ix 0 == min w (defState^?!cpuRegsL.ix (fromIntegral r))
	, testCase "fin" $
		let Right s = runHCpuWith defState [Fin]
		in s^.cpuPcL @?= 1
	, testCase "reset" $
		let Right s = runHCpuWith defState [Bcs $ mkJumpOffset 2, Ba $ mkJumpOffset 2, Fin, And $ mkImmediate 0 ,Reset]
		in s^.cpuPcL @?= 3
	, testProperty "add" $
		\o ->
			let
				(operand, x) = case o of
					RegImmR r -> (mkRegister r, (defStateWithRa 240)^?!cpuRegsL.ix (fromIntegral r))
					RegImmI v -> (mkImmediate v, v)
				Right s = runHCpuWith defState [Mv $ mkReg 0, Ld $ mkReg 1, Add operand, Fin]
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (240 + x, x <= 255 - 240)
	, testProperty "sub" $
		\o ->
			let
				(operand, x) = case o of
					RegImmR r -> (mkRegister r, (defStateWithRa 34)^?!cpuRegsL.ix (fromIntegral r))
					RegImmI v -> (mkImmediate v, v)
				Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, Sub operand, Fin]
				res     = 34 - x
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (res, res /= 0)
	, testProperty "and" $
		\o ->
			let
				(operand, x) = case o of
					RegImmR r -> (mkRegister r, (defStateWithRa 34)^?!cpuRegsL.ix (fromIntegral r))
					RegImmI v -> (mkImmediate v, v)
				Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, And operand, Fin]
				res     = 34 .&. x
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (res, res == 0)
	, testProperty "lshft" $
		\o  ->
			let
				(operand, x) = case o of
					RegImmR r -> (mkRegister r, (defStateWithRa 49)^?!cpuRegsL.ix (fromIntegral r))
					RegImmI v -> (mkImmediate v, v)
				Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 49, Lshft operand, Fin]
				res     = shiftL 49 (fromIntegral x)
			in (s^?!cpuRegsL.ix 0, s^?!cpuFlagsL.ix 0) == (res, res /= 0)
	, testProperty "rshft" $
		\o ->
			let
				(operand, x) = case o of
					RegImmR r -> (mkRegister r, (defStateWithRa 34)^?!cpuRegsL.ix (fromIntegral r))
					RegImmI v -> (mkImmediate v, v)
				Right s = runHCpuWith defState [Mv $ mkReg 0, Add $ mkImmediate 34, Rshft operand, Fin]
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
	, testGroup "Program" $
		[ withAsmResource "./test/golden/mult.s"   multTest
		, withAsmResource "./test/golden/string.s" stringTest
		, withAsmResource "./test/golden/pair.s"   pairTest
		]
	]

initResource :: FilePath -> IO (Handle, V.Vector Inst)
initResource fp = do
	h <- openFile fp ReadMode
	file <- hGetContents h
	let
		Right (instsWithLabels, t) = parseAsm fp file
		Right insts                = translateLabels t instsWithLabels
	return (h, V.fromList insts)

withAsmResource :: FilePath -> (IO (Handle, V.Vector Inst) -> TestTree) -> TestTree
withAsmResource fp test = withResource (initResource fp) (hClose . view _1) test

multTest :: IO (Handle, V.Vector Inst) -> TestTree
multTest getResource =
	testProperty "mult" $ \x y z -> monadic $ do
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

stringTest :: IO (Handle, V.Vector Inst) -> TestTree
stringTest getResource =
	QC.testProperty "string" $ \(Pattern p) (SixtyFourWord8s xs) -> QC.ioProperty $ do
		(_, insts) <- getResource
		let
			Right s = runHCpuWith defHCpuState{
					cpuRam = cpuRam defHCpuState // ([(6, p)] ++ zip [32..] xs)}
					insts
			cpuCount = s^?!cpuRamL.ix 7
			isMatchInWord8 pat w =
				 any (==  pat) . fmap (.&. 0b1111) . take 5 $ iterate (`shiftR` 1) w
			referenceCount = fromIntegral . sum . fmap fromEnum $ fmap (isMatchInWord8 p) xs
		return $ cpuCount == referenceCount

pairTest :: IO (Handle, V.Vector Inst) -> TestTree
pairTest getResource =
	QC.testProperty "pair" $ \(TwentyWord8s xs) -> QC.ioProperty $ do
		(_, insts) <- getResource
		let
			Right s = runHCpuWith defHCpuState{
				cpuRam = cpuRam defHCpuState // zip [128..] xs}
				insts
			cpuMinDist = s^?!cpuRamL.ix 127
			referenceMinDist = minimum . fmap abs $
				fmap (uncurry (-)) [(fromIntegral x, fromIntegral y) | (x:ys) <- tails xs, y <- ys] :: Int
		return $ cpuMinDist == fromIntegral referenceMinDist
