{-# Language TemplateHaskell  #-}

module Text.AsmParser
	( SymbolTable
	, parseAsm
	) where

import Data.Asm

import Data.Void
import Data.Foldable (asum)
import qualified Data.Map.Strict as M
import Lens.Micro.Platform
import Control.Applicative
import Control.Monad.State.Strict
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = StateT SymbolState (Parsec Void String)

data SymbolState = SymbolState
	{ _symbolTable :: M.Map String Word
	, _pc :: !Word -- ^ program counter (not line number)
	} deriving (Show)

makeLenses ''SymbolState

type SymbolTable = M.Map String Word

def :: SymbolState
def = SymbolState
	{ _symbolTable = M.empty
	, _pc = 0
	}

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
pJump = pJumpOffset <|> (JumpLabel <$> getPosition <*> pIdent) <?> "label or signed immediate"
	where
		pJumpOffset = do
			-- TODO how to not parse spaces after a sign (lookAhead digitChar?) (empty seemed to not work)
			imm <- L.signed sc integer
			if imm >= -31 && imm <= 32
				then return $ JumpOffset imm
				else fail $ "unsigned imm nedds to be between -31 and +32"

pInst :: Parser Inst
pInst = do
	inst <- asum [pMv, pStr, pLd, pFin, pAdd, pSub, pAnd, pLshft, pRshft, pBch, pBa] <?> "assembly instruction"
	pc += 1
	return inst
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
		pBch = ts "bch" >> Bch <$> pJump
		pBa  = ts "ba"  >> Ba  <$> pJump

pInstLabel :: Parser Inst
pInstLabel = ((try pLabel) *> pInst)
	<|> pInst
	where
		pLabel = do
			ident <- pIdent
			symbol ":"
			ss <- get
			if ident `M.member` (ss^.symbolTable)
				then fail "duplicate label"
				else symbolTable .= M.insert ident (ss^.pc) (ss^.symbolTable)
			return ident

pAsm :: Parser [Inst]
pAsm = sc *> (concat <$> sepEndBy (some pInstLabel) (some $ symbol ";")) <* eof

-- | Parses input string according to HTAR9 spec but also while being a bit loose
-- without being ambiguous or against spec.
parseAsm
	:: String -- ^ Name of source file
	-> String -- ^ Input to parse
	-> Either (ParseError Char Void) ([Inst], SymbolTable)
parseAsm n i = case runParser (runStateT pAsm def) n i of
	Right (asm, (SymbolState t _)) -> Right (asm, t)
	Left e                         -> Left e
