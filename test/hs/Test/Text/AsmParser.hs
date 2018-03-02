module Test.Text.AsmParser
	( tests
	) where

import Data.Asm
import Text.AsmParser

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import Text.Megaparsec.Pos

tests = testGroup "Text.AsmParser"
	[ testCase "mv" $
		let Right ([inst], _) = parseAsm "" "mv r0"
		in inst @?= (Mv $ mkReg 0)
	, testCase "str" $
		let Right ([inst], _) = parseAsm "" "str r0"
		in inst @?= (Str $ mkReg 0)
	, testCase "ld" $
		let Right ([inst], _) = parseAsm "" "ld r0"
		in inst @?= (Ld $ mkReg 0)
	, testCase "dist" $
		let Right ([inst], _) = parseAsm "" "dist r0"
		in inst @?= (Dist $ mkReg 0)
	, testCase "min" $
		let Right ([inst], _) = parseAsm "" "min r0"
		in inst @?= (Min $ mkReg 0)
	, testCase "fin" $
		let Right ([inst], _) = parseAsm "" "fin"
		in inst @?= Fin
	, testCase "add imm" $
		let Right ([inst], _) = parseAsm "" "add 0"
		in inst @?= (Add $ mkImmediate 0)
	, testCase "add reg" $
		let Right ([inst], _) = parseAsm "" "add r0"
		in inst @?= (Add $ mkRegister 0)
	, testCase "sub imm" $
		let Right ([inst], _) = parseAsm "" "sub 0"
		in inst @?= (Sub $ mkImmediate 0)
	, testCase "sub reg" $
		let Right ([inst], _) = parseAsm "" "sub r0"
		in inst @?= (Sub $ mkRegister 0)
	, testCase "and imm" $
		let Right ([inst], _) = parseAsm "" "and 0"
		in inst @?= (And $ mkImmediate 0)
	, testCase "and reg" $
		let Right ([inst], _) = parseAsm "" "and r0"
		in inst @?= (And $ mkRegister 0)
	, testCase "lshft imm" $
		let Right ([inst], _) = parseAsm "" "lshft 0"
		in inst @?= (Lshft $ mkImmediate 0)
	, testCase "lsfht reg" $
		let Right ([inst], _) = parseAsm "" "lshft r0"
		in inst @?= (Lshft $ mkRegister 0)
	, testCase "rshft imm" $
		let Right ([inst], _) = parseAsm "" "rshft 0"
		in inst @?= (Rshft $ mkImmediate 0)
	, testCase "rshft reg" $
		let Right ([inst], _) = parseAsm "" "rshft r0"
		in inst @?= (Rshft $ mkRegister 0)
	, testCase "bcs offset" $
		let Right ([inst], _) = parseAsm "" "bcs 0"
		in inst @?= (Bcs $ mkJumpOffset 0)
	, testCase "bcs label" $
		let Right ([inst], t) = parseAsm "" "label: bcs label"
		in (inst, t) @?=
			((Bcs $ mkJumpLabel (SourcePos "" (mkPos 1)  (mkPos 12)) "label" ), M.singleton "label" 0)
	, testCase "ba offset" $
		let Right ([inst], _) = parseAsm "" "ba 0"
		in inst @?= (Ba $ mkJumpOffset 0)
	, testCase "ba label" $
		let Right ([inst], t) = parseAsm "" "label: ba label"
		in (inst, t) @?=
			((Ba $ mkJumpLabel (SourcePos "" (mkPos 1)  (mkPos 11)) "label" ), M.singleton "label" 0)
	, testCase "register" $
		let Right ([inst], _) = parseAsm "" "add r3"
		in inst @?= (Add $ mkRegister 3)
	, testCase "signed imm" $
		let Right ([inst], _) = parseAsm "" "ba -6"
		in inst @?= (Ba $ mkJumpOffset (-6))
	]
