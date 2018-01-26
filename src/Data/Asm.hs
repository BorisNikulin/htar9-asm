module Data.Asm where

import Data.Word
import Data.Int

-- | Register specified by number [0-7] with ra being the same as r0
data Reg = Reg Word
	deriving (Show)

data Imm = UImm Word8 -- ^ Unsigned Immediate
		 | SImm Int8  -- ^ Signed Immediate
	deriving (Show)

-- | Identifier for labels and bch instrucitons
type Ident = String

-- | All HATR9 intructions
data Inst = Mv Reg
		  | Add Reg
		  | Sub Reg
		  | And Reg
		  | Shift Imm
		  | BchLabel Ident
		  | BchOffset Imm
		  | Str Reg
		  | Ld Reg
	deriving (Show)

-- | A single HATR9 asm statement
data Statement = Inst Inst
			   | Label Ident
	deriving (Show)
