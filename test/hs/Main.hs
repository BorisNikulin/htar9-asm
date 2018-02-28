module Main
	( main
	) where

import Text.AsmParser
import Text.AsmTranslator

import Test.Text.AsmParser
import Test.Text.AsmBinParser
import Test.Text.AsmTranslator
import Test.Control.Monad.Cpu
import Test.Control.Monad.HtarCpu

import Test.Tasty
import Test.Tasty.Golden
import qualified Data.ByteString.Lazy as BL
import System.FilePath (takeBaseName, replaceExtension)

main = do
	gTests <- goldenTests
	let theTests = topLevelTests ++ [gTests]
	defaultMain $ testGroup "Tests" theTests

topLevelTests =
	[ Test.Text.AsmParser.tests
	, Test.Text.AsmBinParser.tests
	, Test.Text.AsmTranslator.tests
	, Test.Control.Monad.Cpu.tests
	, Test.Control.Monad.HtarCpu.tests
	]

goldenTests :: IO TestTree
goldenTests = do
	asmFiles <- findByExtension [".s"] "./test/golden"
	return $  testGroup "Assembler"
		[ goldenVsString
			(takeBaseName asmFile) -- test name
			binFile -- golden file path
			(asmToBin <$> readFile asmFile) -- action whose result is tested
		| asmFile <- asmFiles
		, let binFile = replaceExtension asmFile ""
		]

asmToBin :: FilePath -> BL.ByteString
asmToBin f = BL.concat bin
	where
		Right (insts, t) = parseAsm "mult.s" f
		Right bin        = translateAsms t insts

