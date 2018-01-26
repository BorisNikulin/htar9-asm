module Text.AsmParser where

import Data.Asm

import Data.Void
import Data.Foldable (asum)
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser()
sc = L.space space1 lineCmnt blockCmnt
	where
		lineCmnt = L.skipLineComment "//"
		blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol' sc

integer :: (Integral a) => Parser a
integer = lexeme L.decimal

pReg :: Parser Reg
pReg = (symbol "r" <?> "register") *> (pRegNum <|> pRegA)
	where
		pRegNum = do
			r <- integer
			if r >= 0 && r <= 7
				then return $ Reg r
				else fail $ "register must between 0 and 7"
		pRegA = do
			symbol "a"
			return $ Reg 0

pImm :: Parser Imm
pImm = uImm <|> sImm <?> "immediate"
	where
		uImm = do
			imm <- integer
			if imm >= 0 && imm <= 55
				then return $ UImm imm
				else fail $ "Unsigned imm needs to be between 0 and 55"
		sImm = do
			-- TODO how to not parse spaces after a sign (lookAhead digitChar?) (empty seemed to not work)
			imm <- L.signed sc integer
			if imm >= -31 && imm <= 32
				then return $ SImm imm
				--TODO? + becomes a unisgned instead of a positive signed imm? (make parser error?)
				else fail $ "Unsigned imm nedds to be between -31 and +32"

pIdent :: Parser Ident
pIdent = lexeme $ (:) <$> letterChar <*> many alphaNumChar

pInst :: Parser Inst
pInst = asum [pMv, pAdd, pSub, pAnd, pBch, pShift, pStr, pLd] <?> "assembly instruction"
	where
		ts = try . symbol
		pMv  = ts "mv"  >> Mv  <$> pReg
		pAdd = ts "add" >> Add <$> pReg
		pSub = ts "sub" >> Sub <$> pReg
		pAnd = ts "and" >> And <$> pReg
		pBch = ts "bch" >> (pBchOffset <|> pBchLabel)
			where
				pBchLabel = BchLabel <$> pIdent
				pBchOffset = BchOffset <$> pImm
		pShift = ts "shft" >> Shift <$> pImm
		pStr = ts "str" >> Str <$> pReg
		pLd  = ts "ld"  >> Ld  <$> pReg

pStatement :: Parser Statement
pStatement = (Inst <$> pInst) <|> (Data.Asm.Label <$> (pIdent <* symbol ":"))

pStatements :: Parser [Statement]
pStatements = sc >> concat <$> ((some pStatement) `sepEndBy` (some $ symbol ";"))

