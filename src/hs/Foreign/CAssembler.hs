module Foreign.CAssembler
	( cAssemble
	) where

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
	alignment _ = max (sizeOf (undefined :: Int )) (sizeOf (undefined :: CString))

foreign export ccall cAssemble :: CString -> CString -> IO AsmResult

-- | A combination of 'parseAsm' and 'translateAsms' for use in C or C++.
cAssemble
	:: CString -- ^ Name of source file.
	-> CString -- ^ Input to parse.
	-> IO (Ptr AsmResultStruct)
cAssemble n i = do
	ptr <- malloc
	poke ptr $ case parseAsm (cMarshallCString n) (cMarshallCString i) of
		Left pe          -> AsmResultStruct 1 (cExtractParseError pe)
		Right (insts, t) -> case translateAsms t insts of
			Left te   -> AsmResultStruct 1 (cExtractLabelError te)
			Right res -> AsmResultStruct 0 (cExtractCodeList res)
	return ptr

cMarshallCString :: CString -> String
cMarshallCString = unsafePerformIO . peekCString

cExtractParseError :: ParseError Char Void -> CString
cExtractParseError = unsafePerformIO . newCString . parseErrorPretty

cExtractLabelError :: LabelError -> CString
cExtractLabelError = unsafePerformIO . newCString . labelErrorPretty

cExtractCodeList :: [BL.ByteString] -> CString
cExtractCodeList = unsafePerformIO . newCString . BL.unpack . BL.concat
