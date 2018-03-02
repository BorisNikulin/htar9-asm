module Bench.Control.Monad.HtarCpu
	( theBench
	) where

import Data.Asm
import Control.Monad.Cpu
import Control.Monad.HtarCpu
import Text.AsmParser
import Text.AsmTranslator

import Criterion.Main
import qualified Data.Vector as V
import Lens.Micro.Platform
import System.IO

theBench = bgroup "Control.Monad.HtarCpu"
	[ multBenches
	]

multBenches = env (asmInstEnv "./test/golden/mult.s") $ \insts ->
	let
		benchMultWith x y z =
			bench (show x ++ " " ++ show y  ++ " " ++ show z) $
			nf (runHCpuWith (multState x y z)) insts
		multState x y z = defHCpuState & ramIx 1 .~ x & ramIx 2 .~ y & ramIx 3 .~ z
		ramIx i = cpuRamL . ix i
	in
		bgroup "mult"
			[ benchMultWith 0   0   0
			, benchMultWith 1   64  128
			, benchMultWith 4   4   4
			, benchMultWith 255 255 255
			]

asmInstEnv :: FilePath -> IO (V.Vector Inst)
asmInstEnv fp = do
	h <- openFile fp ReadMode
	file <- hGetContents h
	let
		Right (instsWithLabels, t) = parseAsm fp file
		Right insts                = translateLabels t instsWithLabels
	return $ V.fromList insts
