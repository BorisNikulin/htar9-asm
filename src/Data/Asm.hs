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

-- | Register specified by number [0-7] with ra being the same as r0
data Reg = Reg Word
	deriving (Show)

-- | Regiser or immediate used by many instructions
data RegImm = Imm Word8    -- ^ Unsigned immediate
			| Register Reg
	deriving (Show)

-- | Identifier for labels and bcs and bcu instrucitons
type Ident = String

-- | Specifies to jump to relative offset or to a label
data Jump = JumpOffset Int
		  | JumpLabel SourcePos Ident -- ^ SourcePos for error messages
	deriving (Show)

-- | All HTAR9 intructions
data Inst = Mv Reg
		  | Str Reg
		  | Ld Reg
		  | Fin
		  | Add RegImm
		  | Sub RegImm
		  | And RegImm
		  | Lshft RegImm
		  | Rshft RegImm
		  | Bcs Jump
		  | Ba Jump
	deriving (Show)
