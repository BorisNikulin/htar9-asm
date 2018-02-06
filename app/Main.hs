{-# LANGUAGE RecordWildCards #-}

module Main
	( main
	) where

import Text.ArgParser
import Text.AsmParser
import Text.AsmTranslator

import System.IO
import Data.Semigroup
import qualified Data.ByteString.Lazy as BL
import Text.Megaparsec.Error (parseErrorPretty)

main :: IO ()
main = do
	opts <- parseArgs
	case opts of
		AssembleOptions{..}  ->
			-- try catch here too
			withFile optInputFile ReadMode $
				assembleFile opts
		InterpretOptions{} -> return ()
	return ()

assembleFile :: Options -> Handle -> IO ()
assembleFile AssembleOptions{..} h = do
	input <- hGetContents h
	case parseAsm optInputFile input of
		Right (insts, table) ->
			case translateAsms table insts of
				-- probably need a try catch here
				Right outputList -> BL.writeFile optOutputFile output
					where
						output = if optFormatted
							then BL.intercalate "\n" outputList <> "\n"
							else BL.concat outputList
				Left te -> putStrLn (labelErrorPretty te)
		Left pe -> putStrLn (parseErrorPretty pe)
