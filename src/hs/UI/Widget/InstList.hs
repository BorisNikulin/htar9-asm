{-# LANGUAGE MultiWayIf
	,BangPatterns
#-}

module UI.Widget.InstList where

import Data.Asm
import Control.Monad.Cpu
import UI.Types

import Data.Word
import Data.Semigroup
import qualified Data.Vector as V
import Text.Printf
import Lens.Micro.Platform
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.List

render :: AppState -> Widget UIName
render s = renderList
	(renderInst
		(s^.instListL.listElementsL.to V.length.to (numDigits 0))
		(s^.cpuStateL.cpuPcL))
	True
	(s^.instListL)
		where
			numDigits !n !num
				| num > 10 = numDigits (n+1) (num `div` 10)
				| num < 10 = n + 1

renderInst
	:: Int  -- ^ max width to use for padding instruction numbers
	-> Word -- ^ current pc to draw current instruction with special attribute
	-> Bool
	-> (Word, Inst)
	-> Widget UIName
renderInst width pc focused (i, inst) =  applyAttr . str $
	printf ("%" <> show width <> "d") i
	<> " │ "
	<> instPretty inst
		where
			applyAttr = if
				| focused && pc == i -> withAttr curInstAndLineAttr
				| focused            -> withAttr curLineAttr
				| pc == i            -> withAttr curInstAttr
				| otherwise          -> id
