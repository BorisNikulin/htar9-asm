module Main
	( main
	) where

import Test.Text.AsmParser
import Test.Text.AsmBinParser
import Test.Text.AsmTranslator
import Test.Control.Monad.Cpu
import Test.Control.Monad.HtarCpu

import Test.Tasty

main = defaultMain topLevelTests

topLevelTests = testGroup "Tests"
	[ Test.Text.AsmParser.tests
	, Test.Text.AsmBinParser.tests
	, Test.Text.AsmTranslator.tests
	, Test.Control.Monad.Cpu.tests
	, Test.Control.Monad.HtarCpu.tests
	]
