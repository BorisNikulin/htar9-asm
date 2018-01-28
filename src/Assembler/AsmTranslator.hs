{-# Language MultiWayIf#-}

module Assembler.AsmTranslator where

import Data.Asm
import Text.AsmParser (SymbolTable)

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (replicate)
import Data.Semigroup
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder

tReg :: Reg -> Builder
tReg (Reg r) = byteString (B.replicate padAmmount '0')
	<> string8 (reverse $ showIntAtBase 2 intToDigit rComp "")
	where
		rComp = 7 - r
		padAmmount = if
			| rComp <= 1 -> 2
			| rComp <= 3 -> 1
			| otherwise  -> 0

tRegImm :: RegImm -> Builder
tRegImm (Register r) = tReg r <> "111"
tRegImm (Imm i)            =  byteString (B.replicate padAmmount '0')
	                       <> byteString numStr
	where
		numStr = B.pack . reverse $ showIntAtBase 2 intToDigit i ""
		padAmmount = 6 - B.length numStr


tlb = toLazyByteString

translateAsm :: SymbolTable -> (Word, Inst) -> BL.ByteString
translateAsm _ (_, (Mv  r))   = tlb $ tReg r <> "000000"
translateAsm _ (_, (Str r))   = tlb $ tReg r <> "010000"
translateAsm _ (_, (Ld  r))   = tlb $ tReg r <> "110000"
translateAsm _ (_, (Fin))     = tlb $ "000111000"
translateAsm _ (_, (Add o))   = tlb $ tRegImm o <> "100"
translateAsm _ (_, (Sub o))   = tlb $ tRegImm o <> "010"
translateAsm _ (_, (And o))   = tlb $ tRegImm o <> "110"
translateAsm _ (_, (Lshft o)) = tlb $ tRegImm o <> "001"
translateAsm _ (_, (Rshft o)) = tlb $ tRegImm o <> "101"
