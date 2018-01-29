{-# Language MultiWayIf#-}

module Assembler.AsmTranslator
	( LabelError(..)
	, translateAsm
	, translateAsms
	) where

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
tReg (Reg r) =  byteString (B.replicate padAmmount '0')
	         <> string8 (showIntAtBase 2 intToDigit rComp "")
	where
		rComp = 7 - r
		padAmmount = if
			| rComp <= 1 -> 2
			| rComp <= 3 -> 1
			| otherwise  -> 0

tRegImm :: RegImm -> Builder
tRegImm (Register r) = "111" <> tReg r
tRegImm (Imm i)      =  byteString (B.replicate padAmmount '0')
	                 <> byteString numStr
	where
		numStr = B.pack $ showIntAtBase 2 intToDigit i ""
		padAmmount = 6 - B.length numStr

tJumpOffset :: Jump -> Builder
tJumpOffset (JumpOffset x) =  byteString (B.replicate padAmmount '0')
	                       <> byteString numStr
	where
		numStr = if x >= 0
			then B.pack $ showIntAtBase 2 intToDigit x ""
			else B.pack $ showIntAtBase 2 intToDigit (32 + 31 + 1 + x) ""
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
translateAsm _ (_, (Mv  r))   = Right . tlb $ "000000" <> tReg r
translateAsm _ (_, (Str r))   = Right . tlb $ "000010" <> tReg r
translateAsm _ (_, (Ld  r))   = Right . tlb $ "000011" <> tReg r
translateAsm _ (_, (Fin))     = Right . tlb $ "000111000"
translateAsm _ (_, (Add o))   = Right . tlb $ "001" <> tRegImm o
translateAsm _ (_, (Sub o))   = Right . tlb $ "010" <> tRegImm o
translateAsm _ (_, (And o))   = Right . tlb $ "011" <> tRegImm o
translateAsm _ (_, (Lshft o)) = Right . tlb $ "100" <> tRegImm o
translateAsm _ (_, (Rshft o)) = Right . tlb $ "101" <> tRegImm o
translateAsm t (i, (Bch j@(JumpLabel _ _))) = calcRelativeOffset t j i >>= translateAsm t . (,) i . Bch
translateAsm t (i, (Bch j@(JumpOffset _)))  = Right . tlb $ "110" <> tJumpOffset j
translateAsm t (i, (Ba  j@(JumpLabel _ _))) = calcRelativeOffset t j i >>= translateAsm t . (,) i . Ba
translateAsm t (i, (Ba  j@(JumpOffset _)))  = Right . tlb $ "111" <> tJumpOffset j

-- | Same as 'translateAsm' but for lists of @(Word, Int)@
translateAsms
	:: SymbolTable
	-> [Inst] -- ^ List of instructions in the same order as was parsed in the table
	-> Either LabelError [BL.ByteString]
{--- make Traversable t instead of a list? (t (Word, Int))-}
translateAsms t asm = sequence $ translateAsm t <$> zip [0..] asm
