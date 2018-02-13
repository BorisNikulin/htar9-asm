module Test.Text.AsmTranslator
	( tests
	) where

import Data.Asm
import Text.AsmTranslator
import Text.AsmParser (SymbolTable)

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import Text.Megaparsec.Pos

t :: SymbolTable
t = M.fromList [("label", 5)]

-- the instruction
i :: Inst -> (Word, Inst)
i = (,) 0

sp :: SourcePos
sp = SourcePos "" (mkPos 0) (mkPos 0)

tests = testGroup "Text.AsmTranslator"
	[ testCase "mv" $
		let Right s = translateAsm t $ i . Mv $ mkReg 0
		in s @?= "000000111"
	, testCase "str" $
		let Right s = translateAsm t $ i . Str $ mkReg 0
		in s @?= "000010111"
	, testCase "ld" $
		let Right s = translateAsm t $ i . Ld $ mkReg 0
		in s @?= "000011111"
	, testCase "fin" $
		let Right s = translateAsm t $ i $ Fin
		in s @?= "000111000"
	, testCase "add imm" $
		let Right s = translateAsm t $ i . Add $ mkImmediate 0
		in s @?= "001000000"
	, testCase "add reg" $
		let Right s = translateAsm t $ i . Add $ mkRegister 0
		in s @?= "001111111"
	, testCase "sub imm" $
		let Right s = translateAsm t $ i . Sub $ mkImmediate 0
		in s @?= "010000000"
	, testCase "sub reg" $
		let Right s = translateAsm t $ i . Sub $ mkRegister 0
		in s @?= "010111111"
	, testCase "and imm" $
		let Right s = translateAsm t $ i . And $ mkImmediate 0
		in s @?= "011000000"
	, testCase "and reg" $
		let Right s = translateAsm t $ i . And $ mkRegister 0
		in s @?= "011111111"
	, testCase "lsfht imm" $
		let Right s = translateAsm t $ i . Lshft $ mkImmediate 0
		in s @?= "100000000"
	, testCase "lsfht reg" $
		let Right s = translateAsm t $ i . Lshft $ mkRegister 0
		in s @?= "100111111"
	, testCase "rshft imm" $
		let Right s = translateAsm t $ i . Rshft $ mkImmediate 0
		in s @?= "101000000"
	, testCase "rsfht reg" $
		let Right s = translateAsm t $ i . Rshft $ mkRegister 0
		in s @?= "101111111"
	, testCase "bcs offset" $
		let Right s = translateAsm t $ i . Bcs $ mkJumpOffset 0
		in s @?= "110000000"
	, testCase "bcs label" $
		let Right s = translateAsm t $ i . Bcs $ mkJumpLabel sp "label"
		in s @?= "110000101"
	, testCase "ba offset" $
		let Right s = translateAsm t $ i . Ba $ mkJumpOffset 0
		in s @?= "111000000"
	, testCase "ba label" $
		let Right s = translateAsm t $ i . Ba $ mkJumpLabel sp "label"
		in s @?= "111000101"
	, testCase "reg" $
		let Right s = translateAsm t $ i . Mv $ mkReg 3
		in s @?= "000000100"
	, testCase "imm-reg imm" $
		let Right s = translateAsm t $ i . Add $ mkImmediate 21
		in s @?= "001010101"
	, testCase "imm-reg reg" $
		let Right s = translateAsm t $ i . Add $ mkRegister 3
		in s @?= "001111100"
	, testCase "signed imm" $
		let Right s = translateAsm t $ i . Bcs $ mkJumpOffset (-4)
		in s @?= "110111100"
	]
