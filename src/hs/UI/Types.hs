{-# LANGUAGE TemplateHaskell #-}

module UI.Types where

import Data.Asm
import Control.Monad.Cpu
import Control.Monad.HtarCpu

import Data.Word
import Data.Semigroup
import qualified Data.Vector as V
import Lens.Micro.Platform
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.List

data UIName = InstListName | CpuStateName
	deriving (Show, Eq, Ord)

type UIEvent = ()

-- i could probably make AppState paramteried by name and event type
data AppState = AppState
	{ instList :: List UIName (Word, Inst)
	, cpuState :: CpuState Word8
	}

suffixLenses ''AppState

curLineAttr, curInstAttr, curInstAndLineAttr :: AttrName
curLineAttr        = "curLineAttr"
curInstAttr        = "curInstAttr"
curInstAndLineAttr = "curInstAndLineAttr"

initAppState :: [Inst] -> AppState
initAppState instList = AppState (list InstListName (V.zip (V.enumFromN 0 n) insts) 1) defHCpuState
	where
		insts = V.fromList instList
		n     = V.length insts

curLineColor, curInstColor :: Vty.Color
curLineColor = Vty.rgbColor 10 10 10
curInstColor = Vty.red

theAttrMap = attrMap Vty.defAttr
	[ (curLineAttr, Vty.defAttr `Vty.withStyle` Vty.underline)
	, (curInstAttr, fg curInstColor)
	, (curInstAndLineAttr, fg curInstColor `Vty.withStyle` Vty.underline)
	]
