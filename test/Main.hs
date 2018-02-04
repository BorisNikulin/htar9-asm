module Main
	( main
	) where

import Test.Control.Monad.Cpu
import Test.Control.Monad.HtarCpu

import Test.Tasty

main = defaultMain topLevelTests

topLevelTests = testGroup "Tests"
	[ Test.Control.Monad.Cpu.tests
	, Test.Control.Monad.HtarCpu.tests
	]
