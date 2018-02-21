module Text.ArgParser
	( Options(..)
	, parseArgs
	, argParser
	) where

import Data.Semigroup
import Options.Applicative

data Options =
	AssembleOptions
	{ optFormatted  :: Bool
	, optOutputFile :: FilePath
	, optInputFile  :: FilePath
	}
	| InterpretOptions
	{ optInputFile  :: FilePath
	} deriving (Show)

assembleArgParser :: Parser Options
assembleArgParser =
	AssembleOptions
	<$> switch
		(  long "formatted"
		<> short 'f'
		<> help "<help>")
	<*> strOption
		(  long "output"
		<> short 'o'
		<> metavar "OUTPUT_FILE"
		<> help "<help>")
	<*> strArgument
		(  metavar "INPUT_FILE"
		<> help "<help>")

argParser :: Parser Options
argParser = assembleArgParser

parseArgs :: IO Options
parseArgs = execParser opts
	where
		opts = info (argParser <**> helper)
			(  fullDesc
			<> header "htar9-asm-hs-exe"
			<> progDesc "HTAR9 Assembler and Interpreter")
