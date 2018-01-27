{-# Language MultiWayIf#-}

module Assembler.AsmTranslator where

import Data.Asm
import Text.AsmParser (SymbolTable)

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (replicate)

tReg :: Reg -> String
tReg (Reg r) = replicate padAmmount '0'
	++ showIntAtBase 2 intToDigit r ""
	where
		str = showIntAtBase 2 intToDigit r ""
		padAmmount = if
			| r <= 1    -> 2
			| r <= 4    -> 1
			| otherwise -> 0


translateAsm :: SymbolTable -> Inst -> String
translateAsm _ (Mv  r) = "000000" ++ tReg r
translateAsm _ (Str r) = "000010" ++ tReg r
translateAsm _ (Ld  r) = "000011" ++ tReg r
translateAsm _ (Fin)   = "000111000"
