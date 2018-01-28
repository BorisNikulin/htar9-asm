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
	<> string8 (showIntAtBase 2 intToDigit r "")
	where
		padAmmount = if
			| r <= 1    -> 2
			| r <= 4    -> 1
			| otherwise -> 0

tlb = toLazyByteString

translateAsm :: SymbolTable -> (Word, Inst) -> BL.ByteString
translateAsm _ (_, (Mv  r)) = tlb $ "000000" <> tReg r
translateAsm _ (_, (Str r)) = tlb $ "000010" <> tReg r
translateAsm _ (_, (Ld  r)) = tlb $ "000011" <> tReg r
translateAsm _ (_, (Fin))   = tlb $ "000111000"
