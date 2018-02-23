module UI.Widget.CpuState where

import Data.Asm
import Control.Monad.Cpu
import UI.Types

import Data.Word
import Data.Semigroup
import qualified Data.Vector as V
import Lens.Micro.Platform
import qualified Graphics.Vty as Vty
import Brick

render :: AppState -> Widget UIName
render s =
	padBot (renderRegs (s^.cpuStateL.cpuRegsL))
	<=> padBot (renderFlags (s^.cpuStateL.cpuFlagsL))
	<=> renderPc (s^.cpuStateL.cpuPcL)
		where
			padBot = padBottom $ Pad 1

renderRegs :: V.Vector Word8 -> Widget UIName
renderRegs = renderVector regLabels
	where
		regLabels = V.map (\x -> "r" <> x <> " = ") . V.map show $ V.enumFromN 0 7

renderFlags :: V.Vector Bool -> Widget UIName
renderFlags = renderVector flagLabels
	where
		flagLabels = V.fromList
			[ "condition = "
			, "done      = "]

renderPc :: Word -> Widget UIName
renderPc = str . mappend "pc = " . show

renderVector :: Show a => V.Vector String -> V.Vector a -> Widget UIName
renderVector = renderVectorWith show

renderVectorWith :: (a -> String) -> V.Vector String -> V.Vector a -> Widget UIName
renderVectorWith f labels = V.foldr (<=>) emptyWidget . V.map str . V.zipWith (<>) labels . V.map f

