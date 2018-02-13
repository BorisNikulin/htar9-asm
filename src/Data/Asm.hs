{-# LANGUAGE GeneralizedNewtypeDeriving
	, PatternSynonyms#-}

module Data.Asm
	( Reg
	, mkReg
	, pattern R
	, Imm
	, mkImm
	, pattern I
	, RegImm
	, mkRegister
	, mkImmediate
	, mkRegImmFromReg
	, mkRegImmFromImm
	, pattern RegImmR
	, pattern RegImmI
	, Ident
	, Jump
	, mkJumpOffset
	, mkJumpLabel
	, pattern JOffset
	, pattern JLabel
	, Inst(..)
	) where

import Data.Word
import Data.Int
import Text.Megaparsec.Pos (SourcePos)

-- | Register specified by a number between 0 and 7 with register a being the same as register 0.
newtype Reg = Reg Word8
	deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

instance Bounded Reg where
	minBound = 0
	maxBound = 7

mkReg :: Integral a => a -> Reg
mkReg rIntegral
	| r >= 0 && r <= 7 = Reg r
	| otherwise        = error $ "register " ++ show r ++ " out of bounds"
		where
			r = fromIntegral rIntegral

pattern R r <- Reg r

newtype Imm = Imm Word8
	deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

instance Bounded Imm where
	minBound = 0
	maxBound = 55

mkImm :: Integral a => a -> Imm
mkImm xIntegral
	| x >= 0 && x <= 55 = Imm x
	| otherwise         = error $ "imm " ++ show x ++ " out of bounds"
		where
			x = fromIntegral xIntegral

pattern I x <- Imm x

-- | Regiser or immediate used by many instructions.
data RegImm = Immediate Imm    -- ^ unsigned immediate
			| Register  Reg
	deriving (Show, Eq)

mkImmediate :: Integral a => a -> RegImm
mkImmediate = Immediate . mkImm

mkRegister :: Integral a => a -> RegImm
mkRegister = Register . mkReg

mkRegImmFromReg :: Reg -> RegImm
mkRegImmFromReg = Register

mkRegImmFromImm :: Imm -> RegImm
mkRegImmFromImm = Immediate

pattern RegImmI x <- Immediate (Imm x)
pattern RegImmR r <- Register  (Reg r)

-- | Identifier for labels and jump instructions.
type Ident = String

-- | Specifies to a jump to relative offset or to a label.
data Jump = JumpOffset Int
		  | JumpLabel SourcePos Ident -- ^ 'SourcePos' for error messages
	deriving (Show, Eq)

mkJumpOffset :: Integral a => a -> Jump
mkJumpOffset xIntegral
	| x >= (-32) && x<= 31 = JumpOffset x
	| otherwise            = error $ "jump offset " ++ show x ++ " is out of bounds"
		where
			x = fromIntegral xIntegral

mkJumpLabel :: SourcePos -> Ident -> Jump
mkJumpLabel sp l = JumpLabel sp l

pattern JOffset x <- JumpOffset x
pattern JLabel sp l <- JumpLabel sp l

-- | All HTAR9 intructions.
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
	deriving (Show, Eq)
