{-# LANGUAGE RecordWildCards #-}

module Main
	( main
	) where

import Text.ArgParser
import Text.AsmParser
import Text.AsmTranslator
import Control.Monad.HtarCpu
import UI.Main

import System.IO
import System.IO.Error
import Data.Semigroup
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Text.Megaparsec.Error (parseErrorPretty)

main :: IO ()
main = do
	opts <- parseArgs
	withFile (optInputFile opts) ReadMode
		(runFile opts)
		`catchIOError` fileErrorhandler


fileErrorhandler :: IOError -> IO ()
fileErrorhandler e
	| isDoesNotExistError e = hPutStrLn stderr $
		showFileError e "does not exist"
	| isPermissionError e = hPutStrLn stderr $
		showFileError e "cannot be opened due to permissions"
	| otherwise = ioError e

showFileError :: IOError -> String -> String
showFileError e reason =
	"error: input file "
	<> (show . fromJust $ ioeGetFileName e)
	<> " "
	<> reason

runFile :: Options -> Handle -> IO ()
runFile opts h = do
	input <- hGetContents h
	case parseAsm (optInputFile opts) input of
		Left pe          -> hPutStrLn stderr $ parseErrorPretty pe
		Right (insts, t) ->
			case opts of
				AssembleOptions{optOutputFile = outFilePath, optFormatted = isFormatted} ->
					case translateAsms t insts of
						Left te          -> hPutStrLn stderr $ labelErrorPretty te
						-- might want to catch device full errors but meh
						Right outputList -> BL.writeFile outFilePath output
							where
								output = if isFormatted
									then BL.intercalate "\n" outputList <> "\n"
									else BL.concat outputList
				InterpretOptions{} ->
					case translateLabels t insts of
						Left te             -> hPutStrLn stderr $ labelErrorPretty te
						Right strippedInsts -> runUI strippedInsts >> return ()
				RunOptions{} ->
					case translateLabels t insts of
						Left te             -> hPutStrLn stderr $ labelErrorPretty te
						Right strippedInsts -> case runHCpu (V.fromList strippedInsts) of
							Left re -> hPutStrLn stderr $ show re -- TODO pretty print error
							Right s -> putStrLn $ show s -- TODO pretty print CpuState
