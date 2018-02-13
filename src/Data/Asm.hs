module Data.Asm
	( Reg(..)
	, RegImm(..)
	, Ident
	, Jump(..)
	, Inst(..)
	) where

import Data.Word
import Data.Int
import Text.Megaparsec.Pos (SourcePos)

-- | Register specified by a number between 0 and 7 with register a being the same as register 0.
data Reg = Reg Word8
	deriving (Show, Eq)

-- | Regiser or immediate used by many instructions.
data RegImm = Imm Word8    -- ^ unsigned immediate
			| Register Reg
	deriving (Show, Eq)

-- | Identifier for labels and jump instructions.
type Ident = String

-- | Specifies to a jump to relative offset or to a label.
data Jump = JumpOffset Int
		  | JumpLabel SourcePos Ident -- ^ 'SourcePos' for error messages
	deriving (Show, Eq)

-- | All HTAR9 intructions.
data Inst = Mv Reg
		  | Str Reg
		  | Ld Reg
		  | Fin
                  | Reset
		  | Add RegImm
		  | Sub RegImm
		  | And RegImm
		  | Lshft RegImm
		  | Rshft RegImm
		  | Bcs Jump
		  | Ba Jump
	deriving (Show, Eq)
