module Foreign.CAssembler
	(cAssemble) where

import Data.Void
import Assembler.AsmTranslator
import Text.AsmParser
import System.IO.Unsafe
import Foreign.C.String
import Data.Either
import Text.Megaparsec
import qualified Data.ByteString.Lazy.Char8 as BL

foreign export ccall cAssemble :: CString -> CString -> CString

-- | A version of 'Assembler.AsmTranslator.translateAsms' for use in C or C++
cAssemble n i = either cExtractParseError cInternalAssemble (parseAsm (cMarshallCString n) (cMarshallCString i))
	where
		cInternalAssemble (a,b) = either cExtractLabelError cExtractCodeList (translateAsms b a)

cMarshallCString :: CString -> String
cMarshallCString s = unsafePerformIO (peekCString s)

cExtractParseError :: ParseError Char Void -> CString
cExtractParseError e = unsafePerformIO (newCString (parseErrorPretty e))

cExtractLabelError :: LabelError -> CString
cExtractLabelError e = unsafePerformIO (newCString (labelErrorPretty e))

cExtractCodeList :: [BL.ByteString] -> CString
cExtractCodeList l = unsafePerformIO (newCString (BL.unpack (BL.concat l)))
