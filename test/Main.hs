module Main
	( main
	) where

import Test.Text.AsmParser
import Test.Text.AsmBinParser
import Test.Control.Monad.Cpu
import Test.Control.Monad.HtarCpu

import Test.Tasty

main = defaultMain topLevelTests

topLevelTests = testGroup "Tests"
	[ Test.Text.AsmParser.tests
	, Test.Text.AsmBinParser.tests
	, Test.Control.Monad.Cpu.tests
	, Test.Control.Monad.HtarCpu.tests
	]
