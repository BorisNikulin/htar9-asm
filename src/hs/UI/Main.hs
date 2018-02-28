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

handleEvent :: AppState -> BrickEvent UIName UIEvent -> EventM UIName (Next AppState)
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 's') [])) = continue $ stepAppState s
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'i') [])) = continue $ s & cpuStateL.cpuFlagsL.ix 1 .~ False
handleEvent s (VtyEvent e) = continue =<< handleEventLensed s instListL (handleListEventVi handleListEvent) e
handleEvent s _ = continue s

stepAppState :: AppState -> AppState
stepAppState s = s & cpuStateL .~ cpu & instListL %~ listMoveTo (cpu^.cpuPcL.to fromIntegral)
	where
		cpu = case s^?instListL.listElementsL.ix (s^.cpuStateL.cpuPcL.to fromIntegral) of
			Just (_, inst) -> case stepHCpuWith (s^.cpuStateL) inst of
				Right nextCpuState -> if (s^?!cpuStateL.cpuFlagsL.ix 1)
					then s^.cpuStateL
					else nextCpuState
				-- pop and error message or /shrug
				Left  e            -> s^.cpuStateL
			Nothing  -> s^.cpuStateL

brickApp :: App AppState UIEvent UIName
brickApp = App
	{ appDraw         = drawUI
	, appChooseCursor = neverShowCursor
	, appHandleEvent  = handleEvent
	, appStartEvent   = return
	, appAttrMap      = const theAttrMap
	}
