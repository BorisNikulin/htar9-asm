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

main = defaultMain topLevelTests

topLevelTests = testGroup "Tests"
	[ Test.Text.AsmParser.tests
	, Test.Text.AsmBinParser.tests
	, Test.Text.AsmTranslator.tests
	, Test.Control.Monad.Cpu.tests
	, Test.Control.Monad.HtarCpu.tests
	, testGroup "Golden"
		[ goldenVsString "mult" "./test/golden/mult" $ do
			file <- readFile "./test/golden/mult.s"
			let
				Right (insts, t) = parseAsm "mult.s" file
				Right bin        = translateAsms t insts
			return $ BL.concat bin
		]
	]
