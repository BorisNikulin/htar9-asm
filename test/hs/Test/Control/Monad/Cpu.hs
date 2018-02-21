module Test.Control.Monad.Cpu
	( tests
	) where

import Control.Monad.Cpu

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector as V

def :: CpuState Int
def = CpuState (V.singleton 20) (V.singleton True) (V.singleton 60) 4

tests = testGroup "Control.Monad.Cpu"
	[ testCase "getReg" $
		runCpu (getReg 0) def @?= (20, def)
	, testCase "setReg" $
		runCpu (setReg 0 0) def @?= ((), def{cpuRegs = V.singleton 0})
	, testCase "getFlag" $
		runCpu (getFlag 0) def @?= (True, def)
	, testCase "setFlag" $
		runCpu (setFlag 0 False) def @?= ((), def{cpuFlags = V.singleton False})
	, testCase "getRam" $
		runCpu (getRam 0) def @?= (60, def)
	, testCase "setRam" $
		runCpu (setRam 0 0) def @?= ((), def{cpuRam = V.singleton 0})
	, testCase "getPc" $
		runCpu getPc def @?= (4, def)
	, testCase "setPc" $
		runCpu (setPc 0) def @?= ((), def{cpuPc = 0})
	, testCase "modifyPc" $
		runCpu (modifyPc (+3)) def @?= ((), def{cpuPc = 7})
	]

