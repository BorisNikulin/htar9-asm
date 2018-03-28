{-# LANGUAGE PatternSynonyms #-}

module UI.Main where

import Data.Asm
import Control.Monad.Cpu
import Control.Monad.HtarCpu
import UI.Types
import qualified UI.Widget.InstList as InstListWidget
import qualified UI.Widget.CpuState as CpuStateWidget

import Data.Word
import Data.Semigroup
import qualified Data.Vector as V
import Lens.Micro.Platform
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

runUI :: [Inst] -> IO AppState
runUI insts = defaultMain brickApp (initAppState insts)

drawUI :: AppState -> [Widget UIName]
drawUI s =
	[border (padLeft (Pad 1) (InstListWidget.render s))
	<+> border (hLimit 20 (hCenter (CpuStateWidget.render s)))]

pattern KeyChar c <- VtyEvent (Vty.EvKey (Vty.KChar c) [])

handleEvent :: AppState -> BrickEvent UIName UIEvent -> EventM UIName (Next AppState)
handleEvent s (KeyChar 'q') = halt s
handleEvent s (KeyChar 's') = continue . snd $ stepAppState True s
handleEvent s (KeyChar 'r') = continue . go $ stepAppState True s
	where
		go (keepRunning, s)
			| keepRunning = go $ stepAppState False s
			| otherwise   = s

handleEvent s (KeyChar 'i') = continue $ s & cpuStateL.cpuFlagsL.ix 1 .~ False
handleEvent s (KeyChar 'b') = continue $ s & instListL.listElementsL.ix (curLine'^.lineNumL.to fromIntegral) .~ curLine'
	where
		curLine  = s^?!instListL.listElementsL.ix (s^?!instListL.listSelectedL._Just)
		curLine' = curLine & isBreakPointL %~ not
handleEvent s (VtyEvent e) = continue =<< handleEventLensed s instListL (handleListEventVi handleListEvent) e
handleEvent s _ = continue s

-- | Returns (canKeepRunningWithResectToBreakPointsAndTheEnd, nextAppState)
-- where the nextAppState state might be the same one as before.
stepAppState
	:: Bool -- ^ Whether to step past breakpoints
	-> AppState
	-> (Bool, AppState)
stepAppState skipBreakPoint s = (keepRunning, s & cpuStateL .~ cpu & instListL %~ listMoveTo (cpu^.cpuPcL.to fromIntegral))
	where
		(keepRunning, cpu) = case getCurrentLine s of
			Right (Line isBreakPoint _ inst)
				| isBreakPoint && not skipBreakPoint -> (not isBreakPoint, s^.cpuStateL)
				| otherwise -> case stepHCpuWith (s^.cpuStateL) inst of
					Right nextCpuState ->
						if (s^?!cpuStateL.cpuFlagsL.ix 1)
							then (False, s^.cpuStateL)
							else (not isBreakPoint || skipBreakPoint, nextCpuState)
					-- error message or /shrug
					Left  e            -> (False, s^.cpuStateL)
			Left _ -> (False, s^.cpuStateL)

-- | Returns current line with Right or in the case of being one past the end,
-- the previous line in Left.
getCurrentLine :: AppState -> Either Line Line
getCurrentLine s = curLine
	where
		linesL  = instListL.listElementsL
		index   = s^.cpuStateL.cpuPcL.to fromIntegral
		length  = s^.instListL.listElementsL.to V.length
		curLine = if index >= length
			then Left  $ s^?!linesL.ix (length - 1)
			else Right $ s^?!linesL.ix index

brickApp :: App AppState UIEvent UIName
brickApp = App
	{ appDraw         = drawUI
	, appChooseCursor = neverShowCursor
	, appHandleEvent  = handleEvent
	, appStartEvent   = return
	, appAttrMap      = const theAttrMap
	}
