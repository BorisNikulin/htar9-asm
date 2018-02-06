module Foreign.CAssembler
	(cAssemble) where

import Data.Void
import Text.AsmTranslator
import Text.AsmParser
import System.IO.Unsafe
import Data.Either
import Text.Megaparsec
import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Lazy.Char8 as BL

data AsmResultStruct = AsmResultStruct Int CString

type AsmResult = Ptr AsmResultStruct

instance Storable AsmResultStruct where
	sizeOf _ = sizeOf (undefined :: Int) + sizeOf (undefined :: CString)
	peek ptr = do
		status  <- peekByteOff ptr 0
		str     <- peekByteOff ptr 8
		return $ AsmResultStruct status str
	poke ptr (AsmResultStruct status str) = do
		pokeByteOff ptr 0 status
		pokeByteOff ptr 8 str

foreign export ccall cAssemble :: CString -> CString -> IO AsmResult

-- | A version of 'Assembler.AsmTranslator.translateAsms' for use in C or C++
cAssemble n i = do
	ptr <- malloc
	poke ptr $ case parseAsm (cMarshallCString n) (cMarshallCString i) of
		Left perr -> AsmResultStruct 1 (cExtractParseError perr)
		Right (a,b) -> case translateAsms b a of
			Left aerr -> AsmResultStruct 1 (cExtractLabelError aerr)
			Right res -> AsmResultStruct 0 (cExtractCodeList res)
	return ptr

cMarshallCString :: CString -> String
cMarshallCString s = unsafePerformIO (peekCString s)

cExtractParseError :: ParseError Char Void -> CString
cExtractParseError e = unsafePerformIO (newCString (parseErrorPretty e))

cExtractLabelError :: LabelError -> CString
cExtractLabelError e = unsafePerformIO (newCString (labelErrorPretty e))

cExtractCodeList :: [BL.ByteString] -> CString
cExtractCodeList l = unsafePerformIO (newCString (BL.unpack (BL.concat l)))
