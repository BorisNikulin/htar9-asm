{-# Language MultiWayIf#-}

module Assembler.AsmTranslator
	( LabelError(..)
	, translateAsm) where

import Data.Asm
import Text.AsmParser (SymbolTable)

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (replicate)
import Data.Semigroup
import Data.Map.Strict ((!?))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder
import Text.Megaparsec.Pos (SourcePos)

-- | @'OffsetError'@ represents a missing label or
-- the branch to a label is too far away
-- as branches can only branch to a relative offset.
data LabelError
	= MissingLabelError SourcePos Ident
	| OffsetError SourcePos Ident Int
	deriving (Show)

tReg :: Reg -> Builder
tReg (Reg r) =  string8 (reverse $ showIntAtBase 2 intToDigit rComp "")
	         <> byteString (B.replicate padAmmount '0')
	where
		rComp = 7 - r
		padAmmount = if
			| rComp <= 1 -> 2
			| rComp <= 3 -> 1
			| otherwise  -> 0

tRegImm :: RegImm -> Builder
tRegImm (Register r) = tReg r <> "111"
tRegImm (Imm i)      =  byteString numStr
	                 <> byteString (B.replicate padAmmount '0')
	where
		numStr = B.pack . reverse $ showIntAtBase 2 intToDigit i ""
		padAmmount = 6 - B.length numStr

tJumpOffset :: Jump -> Builder
tJumpOffset (JumpOffset x) =  byteString numStr
	                       <> byteString (B.replicate padAmmount '0')
	where
		numStr = if x >= 0
			then B.pack . reverse $ showIntAtBase 2 intToDigit x ""
			else B.pack . reverse $ showIntAtBase 2 intToDigit (32 + 31 + 1 + x) ""
		padAmmount = 6 - B.length numStr


calcRelativeOffset :: SymbolTable -> Jump -> Word -> Either LabelError Jump
calcRelativeOffset t (JumpLabel sp l) from = case t !? l of
	Just to ->
		let offset = fromIntegral to - fromIntegral from
		in if offset >= -31 && offset <= 32
			then Right $ JumpOffset offset
			else Left $ OffsetError sp l offset
	Nothing -> Left $ MissingLabelError sp l

tlb = toLazyByteString

-- | Takes HTAR9 intructions and neccessary supporting data to
-- translate to 9 chars of @\'1\'@s and @\'0\'@s in little endian.
-- The function is intened to be used with 'Data.AsmParser.parseAsm'.
translateAsm
	:: SymbolTable  -- ^ Map from String labels to intruction number
	-> (Word, Inst) -- ^ A tuple of intruction number and the corresponding instruction
	-> Either LabelError BL.ByteString
translateAsm _ (_, (Mv  r))   = Right . tlb $ tReg r <> "000000"
translateAsm _ (_, (Str r))   = Right . tlb $ tReg r <> "010000"
translateAsm _ (_, (Ld  r))   = Right . tlb $ tReg r <> "110000"
translateAsm _ (_, (Fin))     = Right . tlb $ "000111000"
translateAsm _ (_, (Add o))   = Right . tlb $ tRegImm o <> "100"
translateAsm _ (_, (Sub o))   = Right . tlb $ tRegImm o <> "010"
translateAsm _ (_, (And o))   = Right . tlb $ tRegImm o <> "110"
translateAsm _ (_, (Lshft o)) = Right . tlb $ tRegImm o <> "001"
translateAsm _ (_, (Rshft o)) = Right . tlb $ tRegImm o <> "101"
translateAsm t (i, (Bcs j@(JumpLabel _ _))) = calcRelativeOffset t j i >>= translateAsm t . (,) i . Bcs
translateAsm t (i, (Bcs j@(JumpOffset _)))  = Right . tlb $ tJumpOffset j <> "011"
translateAsm t (i, (Ba  j@(JumpLabel _ _))) = calcRelativeOffset t j i >>= translateAsm t . (,) i . Ba
translateAsm t (i, (Ba  j@(JumpOffset _)))  = Right . tlb $ tJumpOffset j <> "111"

