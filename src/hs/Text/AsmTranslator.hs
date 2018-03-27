{-# Language MultiWayIf#-}

module Text.AsmTranslator
	( -- * Translation functions
	  translateAsm
	, translateAsms
	, translateLabel
	, translateLabels
	  -- * Errors
	, LabelError(..)
	, labelErrorPretty
	) where

import Data.Asm
import Text.AsmParser (SymbolTable)

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Semigroup hiding (Min) -- name clash with Min instruction
import Data.Map.Strict ((!?))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

-- | @'LabelError'@ represents a missing label or
-- the relaeive offset is too large.
data LabelError
	= MissingLabelError SourcePos Ident
	| OffsetError SourcePos Ident Int
	deriving (Show)

-- | Pretty format a 'LabelError'.
labelErrorPretty :: LabelError -> String
labelErrorPretty (MissingLabelError sp l) =
	sourcePosPretty sp
	<> "\n"
	<> "can not branch to label "
	<> show l
	<> " as it is missing"
labelErrorPretty (OffsetError sp l x) =
	sourcePosPretty sp
	<> "\n"
	<> "can not branch to label "
	<> show l
	<> " as a relative jump of "
	<> show x
	<> " is outside the maximum range of -31 to 32"

tReg :: Reg -> Builder
tReg (R r) =  byteString (B.replicate padAmmount '0')
	         <> string8 (showIntAtBase 2 intToDigit rComp "")
	where
		rComp = 7 - r
		padAmmount = if
			| rComp <= 1 -> 2
			| rComp <= 3 -> 1
			| otherwise  -> 0
tReg _ = error "invalid args"

tRegImm :: RegImm -> Builder
tRegImm (RegImmR r) = "111" <> tReg (mkReg r)
tRegImm (RegImmI i) =  byteString (B.replicate padAmmount '0')
	                 <> byteString numStr
	where
		numStr = B.pack $ showIntAtBase 2 intToDigit i ""
		padAmmount = 6 - B.length numStr
tRegImm _ = error "invalid args"

tJumpOffset :: Jump -> Builder
tJumpOffset (JOffset x) =  byteString (B.replicate padAmmount '0')
	                       <> byteString numStr
	where
		numStr = if x >= 0
			then B.pack $ showIntAtBase 2 intToDigit x ""
			else B.pack $ showIntAtBase 2 intToDigit (32 + 31 + 1 + x) ""
		padAmmount = 6 - B.length numStr
tJumpOffset _ = error "invalid args"


calcRelativeOffset :: SymbolTable -> Jump -> Word -> Either LabelError Jump
calcRelativeOffset t (JLabel sp l) from = case t !? l of
	Just to ->
		let offset = fromIntegral to - fromIntegral from
		in if offset >= -31 && offset <= 32
			then Right $ mkJumpOffset offset
			else Left $ OffsetError sp l offset
	Nothing -> Left $ MissingLabelError sp l
calcRelativeOffset _ _ _ = error "invalid args"

-- | shorter alias for 'toLazyByteString'
tlb :: Builder -> BL.ByteString
tlb = toLazyByteString

-- | Takes HTAR9 intructions and neccessary supporting data to
-- translate to 9 chars of @\'1\'@s and @\'0\'@s in little endian.
-- The function is intended to be used with 'Data.AsmParser.parseAsm'.
translateAsm
	:: SymbolTable  -- ^ Map from String labels to intruction number.
	-> (Word, Inst) -- ^ A tuple of intruction number and the corresponding instruction.
	-> Either LabelError BL.ByteString
translateAsm t (i, (Bcs j@(JLabel _ _))) = calcRelativeOffset t j i >>= translateAsm t . (,) i . Bcs
translateAsm t (i, (Ba  j@(JLabel _ _))) = calcRelativeOffset t j i >>= translateAsm t . (,) i . Ba
translateAsm _ (_, inst) = Right . tlb $ translateAsm' inst
	where
		translateAsm' (Mv   r)  = "000000" <> tReg r
		translateAsm' (Str  r)  = "000010" <> tReg r
		translateAsm' (Ld   r)  = "000011" <> tReg r
		translateAsm' (Dist r)  = "000100" <> tReg r
		translateAsm' (Min  r)  = "000101" <> tReg r
		translateAsm' Fin       = "000111000"
		translateAsm' Reset     = "000111001"
		translateAsm' (Add  o)  = "001" <> tRegImm o
		translateAsm' (Sub  o)  = "010" <> tRegImm o
		translateAsm' (And  o)  = "011" <> tRegImm o
		translateAsm' (Lshft o) = "100" <> tRegImm o
		translateAsm' (Rshft o) = "101" <> tRegImm o
		translateAsm' (Bcs j)   = "110" <> tJumpOffset j
		translateAsm' (Ba  j)   = "111" <> tJumpOffset j

-- | Same as 'translateAsm' but for lists of 'Inst'.
translateAsms
	:: SymbolTable -- ^ Map from String labels to intruction number.
	-> [Inst]      -- ^ List of instructions in the same order as was parsed into the table.
	-> Either LabelError [BL.ByteString]
-- make Traversable t instead of a list? (t (Word, Int))
translateAsms t asm = sequence $ translateAsm t <$> zip [0..] asm

-- | If the instruction contains a jump to a label it is replaced with the equivalent offset
-- if valid or a 'LabelError' otherwise.
-- All other instructions return as is.
translateLabel
	:: SymbolTable  -- ^ Map from String labels to intruction number.
	-> (Word, Inst) -- ^ A tuple of intruction number and the corresponding instruction.
	-> Either LabelError Inst
translateLabel t (i, (Bcs j@(JLabel _ _))) = calcRelativeOffset t j i >>= Right . Bcs
translateLabel t (i, (Ba  j@(JLabel _ _))) = calcRelativeOffset t j i >>= Right . Ba
translateLabel _ (_, inst)                 = Right inst


-- | Same as 'translateLabel' but for lists of 'Inst'.
translateLabels
	:: SymbolTable -- ^ Map from String labels to intruction number.
	-> [Inst]      -- ^ List of instructions in the same order as was parsed into the table.
	-> Either LabelError [Inst]
translateLabels t asm = sequence $ translateLabel t <$> zip [0..] asm
