{-# LANGUAGE GeneralizedNewtypeDeriving
	, DeriveGeneric
	, PatternSynonyms
	, BangPatterns
#-}

module Data.Asm
	( -- * Data types
	  -- | Data types are exported without constructors
	  -- except for the top level 'Inst'.
	  -- To create them use the smart constructors.
	  -- NB: Creating invalid HTAR9 instructions
	  -- result in errors being thrown with the exception of
	  -- jumps by relative offset going too far.
	  -- For pattern matching, appropriate unidirectional
	  -- pattern synonyms are provided.
	  -- NB: To use the pattern synonyms, -XPatternSynonyms
	  -- is required.
	  Reg
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
	  -- * Pretty Printing
	, regPretty
	, regImmPretty
	, jumpPretty
	, instPretty
	) where

import Data.Word
import Data.String
import Data.Semigroup hiding (Min)
import Text.Megaparsec.Pos (SourcePos)

import Control.DeepSeq
import GHC.Generics (Generic)

-- | Register specified by a number between 0 and 7 with register a being the same as register 0.
newtype Reg = Reg Word8
	deriving (Show, Eq, Ord, Num, Real, Integral, Enum, NFData)

instance Bounded Reg where
	minBound = 0
	maxBound = 7

mkReg :: Integral a => a -> Reg
mkReg rIntegral
	| r >= 0 && r <= 7 = Reg r
	| otherwise        = error $ "register " ++ show r ++ " out of bounds"
		where
			r = fromIntegral rIntegral

pattern R :: Word8 -> Reg
pattern R r <- Reg r

-- | Unsigned Immediate between 0-55 inclusive.
newtype Imm = Imm Word8
	deriving (Show, Eq, Ord, Num, Real, Integral, Enum, NFData)

instance Bounded Imm where
	minBound = 0
	maxBound = 55

mkImm :: Integral a => a -> Imm
mkImm xIntegral
	| x >= 0 && x <= 55 = Imm x
	| otherwise         = error $ "imm " ++ show x ++ " out of bounds"
		where
			x = fromIntegral xIntegral

pattern I :: Word8 -> Imm
pattern I x <- Imm x

-- | Regiser or immediate used by many instructions.
data RegImm = Immediate Imm    -- ^ unsigned immediate
			| Register  Reg
	deriving (Show, Eq, Generic)

instance NFData RegImm

mkImmediate :: Integral a => a -> RegImm
mkImmediate = Immediate . mkImm

mkRegister :: Integral a => a -> RegImm
mkRegister = Register . mkReg

mkRegImmFromReg :: Reg -> RegImm
mkRegImmFromReg = Register

mkRegImmFromImm :: Imm -> RegImm
mkRegImmFromImm = Immediate

pattern RegImmI :: Word8 -> RegImm
pattern RegImmI x <- Immediate (Imm x)

pattern RegImmR :: Word8 -> RegImm
pattern RegImmR r <- Register  (Reg r)

-- | Identifier for labels and jump instructions.
type Ident = String

-- | Specifies to a jump to relative offset or to a label.
data Jump = JumpOffset Int
		  | JumpLabel SourcePos Ident -- ^ 'SourcePos' for error messages.
	deriving (Show, Eq, Generic)

instance NFData Jump

mkJumpOffset :: Integral a => a -> Jump
mkJumpOffset xIntegral
	| x >= (-32) && x<= 31 = JumpOffset x
	| otherwise            = error $ "jump offset " ++ show x ++ " is out of bounds"
		where
			x = fromIntegral xIntegral

-- | 'SourcePos' is from megaparsec and is used for error reporting.
mkJumpLabel :: SourcePos -> Ident -> Jump
mkJumpLabel sp l = JumpLabel sp l

pattern JOffset :: Int -> Jump
pattern JOffset x <- JumpOffset x

pattern JLabel :: SourcePos -> Ident -> Jump
pattern JLabel sp l <- JumpLabel sp l

-- | All HTAR9 intructions.
data Inst = Mv Reg
		  | Str Reg
		  | Ld Reg
		  | Dist Reg
		  | Min Reg
		  | Fin
		  | Reset
		  | Add RegImm
		  | Sub RegImm
		  | And RegImm
		  | Lshft RegImm
		  | Rshft RegImm
		  | Bcs Jump
		  | Ba Jump
	deriving (Show, Eq, Generic)

instance NFData Inst

regPretty :: IsString a => Reg -> a
regPretty (R r) = fromString $ "r" <> show r

regImmPretty :: IsString a => RegImm -> a
regImmPretty (Register r) = regPretty r
regImmPretty (RegImmI  x) = fromString $ show x -- TODO add on a hex version next to decimal litteral

jumpPretty :: IsString a => Jump -> a
jumpPretty (JOffset  x) = fromString $ show x
jumpPretty (JLabel _ l) = fromString $ show l

instPretty :: (IsString a, Semigroup a) => Inst -> a
instPretty (Mv   r)  = "mov   " <> regPretty r
instPretty (Str  r)  = "str   " <> regPretty r
instPretty (Ld   r)  = "ld    " <> regPretty r
instPretty (Dist r)  = "dist  " <> regPretty r
instPretty (Min  r)  = "min   " <> regPretty r
instPretty Fin       = "fin"
instPretty Reset     = "reset"
instPretty (Add o)   = "add   " <> regImmPretty o
instPretty (Sub o)   = "sub   " <> regImmPretty o
instPretty (And o)   = "and   " <> regImmPretty o
instPretty (Lshft o) = "lshft " <> regImmPretty o
instPretty (Rshft o) = "rshft " <> regImmPretty o
instPretty (Bcs j)   = "bcs   " <> jumpPretty j
instPretty (Ba  j)   = "ba    " <> jumpPretty j
