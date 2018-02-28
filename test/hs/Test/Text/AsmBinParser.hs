module Test.Text.AsmBinParser
	( tests
	) where

import Data.Asm
import Text.AsmBinParser

import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "Text.AsmBinParser"
	[ testCase "mv" $
		let Right [inst] = parseAsm "" "000000111"
		in inst @?= (Mv $ mkReg 0)
	, testCase "str" $
		let Right [inst] = parseAsm "" "000010111"
		in inst @?= (Str $ mkReg 0)
	, testCase "ld" $
		let Right [inst] = parseAsm "" "000011111"
		in inst @?= (Ld $ mkReg 0)
	, testCase "fin" $
		let Right [inst] = parseAsm "" "000111000"
		in inst @?= Fin
	, testCase "reset" $
		let Right [inst] = parseAsm "" "000111001"
		in inst @?= Reset
	, testCase "add imm" $
		let Right [inst] = parseAsm "" "001000000"
		in inst @?= (Add $ mkImmediate 0)
	, testCase "add reg" $
		let Right [inst] = parseAsm "" "001111111"
		in inst @?= (Add $ mkRegister 0)
	, testCase "sub imm" $
		let Right [inst] = parseAsm "" "010000000"
		in inst @?= (Sub $ mkImmediate 0)
	, testCase "sub reg" $
		let Right [inst] = parseAsm "" "010111111"
		in inst @?= (Sub $ mkRegister 0)
	, testCase "and imm" $
		let Right [inst] = parseAsm "" "011000000"
		in inst @?= (And $ mkImmediate 0)
	, testCase "and reg" $
		let Right [inst] = parseAsm "" "011111111"
		in inst @?= (And $ mkRegister 0)
	, testCase "lshft imm" $
		let Right [inst] = parseAsm "" "100000000"
		in inst @?= (Lshft $ mkImmediate 0)
	, testCase "lsfht reg" $
		let Right [inst] = parseAsm "" "100111111"
		in inst @?= (Lshft $ mkRegister 0)
	, testCase "rshft imm" $
		let Right [inst] = parseAsm "" "101000000"
		in inst @?= (Rshft $ mkImmediate 0)
	, testCase "rshft reg" $
		let Right [inst] = parseAsm "" "101111111"
		in inst @?= (Rshft $ mkRegister 0)
	, testCase "bcs" $
		let Right [inst] = parseAsm "" "110000000"
		in inst @?= (Bcs $ mkJumpOffset 0)
	, testCase "ba" $
		let Right [inst] = parseAsm "" "111000000"
		in inst @?= (Ba $ mkJumpOffset 0)
	, testCase "register" $
		let Right [inst] = parseAsm "" "001111100"
		in inst @?= (Add $ mkRegister 3)
	, testCase "signed imm" $
		let Right [inst] = parseAsm "" "111111010"
		in inst @?= (Ba $ mkJumpOffset (-6))
	]
