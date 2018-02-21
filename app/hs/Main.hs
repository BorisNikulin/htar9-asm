{-# LANGUAGE RecordWildCards #-}

module Main
	( main
	) where

import Text.ArgParser
import Text.AsmParser
import Text.AsmTranslator

import System.IO
import System.IO.Error
import Data.Semigroup
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Text.Megaparsec.Error (parseErrorPretty)

main :: IO ()
main = do
	opts <- parseArgs
	case opts of
		AssembleOptions{..}  ->
			withFile optInputFile ReadMode
				(assembleFile opts)
			`catchIOError` handleFileError
				where
					handleFileError e
						| isDoesNotExistError e = hPutStrLn stderr $
							templateFileError e "does not exist"
						| isPermissionError e = hPutStrLn stderr $
							templateFileError e "cannot be opened due to permissions"
						| otherwise = ioError e
					templateFileError e reason =
						"error: input file "
						<> (show . fromJust $ ioeGetFileName e)
						<> " "
						<> reason

		InterpretOptions{} -> return ()
	return ()

assembleFile :: Options -> Handle -> IO ()
assembleFile AssembleOptions{..} h = do
	input <- hGetContents h
	case parseAsm optInputFile input of
		Right (insts, table) ->
			case translateAsms table insts of
				-- might want a catchIOError but that should only happen
				-- on a device full error which at that point the user
				-- has more problems than this not running :D
				Right outputList -> BL.writeFile optOutputFile output
					where
						output = if optFormatted
							then BL.intercalate "\n" outputList <> "\n"
							else BL.concat outputList
				Left te -> hPutStrLn stderr (labelErrorPretty te)
		Left pe -> hPutStrLn stderr (parseErrorPretty pe)
assembleFile _ _ = error "invalid args"
