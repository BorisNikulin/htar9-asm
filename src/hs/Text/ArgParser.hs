module Text.ArgParser
	( Options(..)
	, OutputOptions(..)
	, parseArgs
	, argParser
	) where

import Data.Semigroup
import Options.Applicative

-- | Top level data type for command line arguments.
data Options =
	Options
	{ optInterpret  :: Bool
	, optRun        :: Bool
	, optOutputOpts :: Maybe OutputOptions
	, optInputFile  :: FilePath
	} deriving (Show)

-- | Options for file output.
data OutputOptions =
	OutputOptions
	{ outOptFormat :: Bool
	, outOptOutput :: FilePath
	} deriving (Show)

-- | Command line argument parsing action as used
-- by optparse-applicative.
-- Provided for modification if needed.
argParser :: Parser Options
argParser =
	Options
	<$> switch
		(  long "interpret"
		<> short 'i'
		<> help "Interpret source in a tui")
	<*> switch
		(  long "run"
		<> short 'r'
		<> help "Run asm to completion and output resulting cpu state")
	<*> optional
		(OutputOptions
		<$> switch
			(  long "format"
			<> short 'f'
			<> help "Format the output with one instruction per line")
		<*> strOption
			(  long "output"
			<> short 'o'
			<> metavar "OUTPUT_FILE")
		)
	<*> strArgument
		(  metavar "INPUT_FILE"
		<> help "Input HTAR9 asm source")

-- | Convienience function for directly parsing command line arguments.
-- Adds on help text and global descriptions to 'argParser'.
parseArgs :: IO Options
parseArgs = execParser opts
	where
		opts = info (argParser <**> helper)
			(  fullDesc
			<> header "htar9-asm-hs-exe"
			<> progDesc "HTAR9 Haskell Front End")
