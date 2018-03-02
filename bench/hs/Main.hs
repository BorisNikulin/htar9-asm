module Main
	( main
	) where

import Bench.Control.Monad.HtarCpu

import Criterion.Main

main = defaultMain topLevelBenches

topLevelBenches =
	[ Bench.Control.Monad.HtarCpu.theBench
	]
