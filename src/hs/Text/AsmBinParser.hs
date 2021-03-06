module Text.AsmBinParser
	( parseAsm
	) where

import Data.Asm

import Numeric (readInt)
import Data.Char (digitToInt)
import Data.Maybe (listToMaybe)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String

readBin :: Integral a => String -> Maybe a
readBin = fmap fst. listToMaybe . readInt 2 (`elem` ("01" :: String)) digitToInt

readRegImm :: String -> Maybe RegImm
readRegImm s = do
	num <- readBin s
	if num >= 0 && num <= 63
		then if num >= (63 - 7)
			then Just $ mkRegister (63 - num)
			else Just $ mkImmediate (num :: Word)
		else Nothing

readReg :: String -> Maybe Reg
readReg s = do
	regImm <- readRegImm ("111" ++ s)
	case regImm of
		RegImmR r -> Just $ mkReg r
		RegImmI _ -> Nothing
		_         -> Nothing

readJump :: String -> Maybe Jump
readJump s = do
	num <- readBin s
	if num >= 0 && num <= 63
		then Just . mkJumpOffset $ if num <= 31
			then (num :: Int)
			else (num - 64)
		else Nothing

pBin :: Parser Inst
pBin = do
	inst <- takeP (Just "instruction") 9

	let
		(high, top6) = splitAt 3 inst
		(mid,  low)  = splitAt 3 top6
		bot6         = mid ++ low

	let maybeInst = case high of
		"001" -> Add   <$> readRegImm bot6
		"010" -> Sub   <$> readRegImm bot6
		"011" -> And   <$> readRegImm bot6
		"100" -> Lshft <$> readRegImm bot6
		"101" -> Rshft <$> readRegImm bot6
		"110" -> Bcs   <$> readJump   bot6
		"111" -> Ba    <$> readJump   bot6
		"000" -> case mid of
			"000" -> Mv   <$> readReg low
			"010" -> Str  <$> readReg low
			"011" -> Ld   <$> readReg low
			"100" -> Dist <$> readReg low
			"101" -> Min  <$> readReg low
			"111" -> case low of
				"000" -> Just Fin
				"001" -> Just Reset
				_     -> Nothing
			_     -> Nothing
		_     -> Nothing

	case maybeInst of
		Just a  -> return a
		Nothing -> fail "invalid HTAR9 operand or instruction"

pBins :: Parser [Inst]
pBins = many pBin <* eof

-- | Parses a stream of ascii @1@'s and @0@'s into the HTAR9 instruction type 'Inst'.
parseAsm
	:: String -- ^ Name of source file.
	-> String -- ^ Input to parse.
	-> Either (ParseError Char Void) [Inst]
parseAsm = runParser pBins
