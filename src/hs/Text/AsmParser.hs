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

-- | Table for mapping labels to their instruction.
type SymbolTable = M.Map Ident Word

data SymbolState = SymbolState
	{ _symbolTable :: SymbolTable
	, _pc :: !Word -- ^ program counter (not line number)
	} deriving (Show)

makeLenses ''SymbolState

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
				then return $ mkReg (r :: Word)
				else fail $ "register must between 0 and 7"
		pRegA = symbol "a" *> return (mkReg (0 :: Word))

pRegImm :: Parser RegImm
pRegImm = (mkRegister <$> pReg) <|> pUImm <?> "register or unsigned immediate"
	where
		pUImm = do
			imm <- integer
			if imm >= 0 && imm <= 55
				then return $ mkImmediate (imm :: Word)
				else fail "unsigned imm needs to be between 0 and 55"

pIdent :: Parser Ident
pIdent = lexeme $ (:) <$> letterChar <*> many alphaNumChar

pJump :: Parser Jump
pJump = pJumpOffset <|> (mkJumpLabel <$> getPosition <*> pIdent) <?> "label or signed immediate"
	where
		pJumpOffset = do
			-- TODO how to not parse spaces after a sign (lookAhead digitChar?) (empty seemed to not work)
			imm <- L.signed sc integer
			if imm >= -31 && imm <= 32
				then return $ mkJumpOffset (imm :: Int)
				else fail $ "unsigned imm nedds to be between -31 and +32"

pInst :: Parser Inst
pInst = do
	inst <- asum [pMv, pStr, pLd, pFin, pReset, pAdd, pSub, pAnd, pLshft, pRshft, pBcs, pBa] <?> "assembly instruction"
	pc += 1
	return inst
	where
		ts = try . symbol
		pMv  = ts "mv"  >> Mv  <$> pReg
		pStr = ts "str" >> Str <$> pReg
		pLd  = ts "ld"  >> Ld  <$> pReg
		pFin = ts "fin" >> return Fin
		pReset = ts "reset" >> return Reset
		pAdd = ts "add" >> Add <$> pRegImm
		pSub = ts "sub" >> Sub <$> pRegImm
		pAnd = ts "and" >> And <$> pRegImm
		pLshft = ts "lshft" >> Lshft <$> pRegImm
		pRshft = ts "rshft" >> Rshft <$> pRegImm
		pBcs = ts "bcs" >> Bcs <$> pJump
		pBa  = ts "ba"  >> Ba  <$> pJump

pInstLabel :: Parser Inst
pInstLabel = ((try pLabel) *> pInst)
	<|> pInst
	where
		pLabel = do
			ident <- pIdent
			_ <- symbol ":"
			ss <- get
			if ident `M.member` (ss^.symbolTable)
				then fail "duplicate label"
				else symbolTable .= M.insert ident (ss^.pc) (ss^.symbolTable)
			return ident

pAsm :: Parser [Inst]
pAsm = sc *> (concat <$> sepEndBy (some pInstLabel) (some $ symbol ";")) <* eof

-- | Parses input string according to HTAR9 spec while being a bit loose
-- but unambiguously or against spec.
parseAsm
	:: String -- ^ Name of source file
	-> String -- ^ Input to parse
	-> Either (ParseError Char Void) ([Inst], SymbolTable)
parseAsm n i = case runParser (runStateT pAsm def) n i of
	Right (asm, (SymbolState t _)) -> Right (asm, t)
	Left e                         -> Left e
