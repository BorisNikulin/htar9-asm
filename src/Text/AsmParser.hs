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

pRegImm :: Parser RegImm
pRegImm = (Register <$> pReg) <|> pUImm <?> "register or unsigned immediate"
	where
		pUImm = do
			imm <- integer
			if imm >= 0 && imm <= 55
				then return $ Imm imm
				else fail "unsigned imm needs to be between 0 and 55"

pIdent :: Parser Ident
pIdent = lexeme $ (:) <$> letterChar <*> many alphaNumChar

pJump :: Parser Jump
pJump = pJumpOffset <|> (JumpLabel <$> pIdent) <?> "label or signed immediate"
	where
		pJumpOffset = do
			-- TODO how to not parse spaces after a sign (lookAhead digitChar?) (empty seemed to not work)
			imm <- L.signed sc integer
			if imm >= -31 && imm <= 32
				then return $ JumpOffset imm
				--TODO? + becomes a unisgned instead of a positive signed imm? (make parser error?)
				else fail $ "unsigned imm nedds to be between -31 and +32"

-- label doesnt seem to print
pInst :: Parser Inst
pInst = asum [pMv, pStr, pLd, pFin, pAdd, pSub, pAnd, pLshft, pRshft, pBcs, pBcu] <?> "assembly instruction"
	where
		ts = try . symbol
		pMv  = ts "mv"  >> Mv  <$> pReg
		pStr = ts "str" >> Str <$> pReg
		pLd  = ts "ld"  >> Ld  <$> pReg
		pFin = ts "fin" >> return Fin
		pAdd = ts "add" >> Add <$> pRegImm
		pSub = ts "sub" >> Sub <$> pRegImm
		pAnd = ts "and" >> And <$> pRegImm
		pLshft = ts "lshft" >> Lshft <$> pRegImm
		pRshft = ts "rshft" >> Rshft <$> pRegImm
		pBcs = ts "bcs" >> Bcs <$> pJump
		pBcu = ts "bcu" >> Bcu <$> pJump

pStatement :: Parser Statement
pStatement = StatementInst <$> pInst
	-- figure out a place for label and so that it works
	<|> Data.Asm.Label <$> pIdent <* (symbol ":")

pStatements :: Parser [Statement]
pStatements = sc *> (concat <$> sepEndBy (some pStatement) (some $ symbol ";")) <* eof

