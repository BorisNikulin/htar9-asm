{-# LANGUAGE TemplateHaskell
	, RankNTypes
	, ScopedTypeVariables
	#-}

module Control.Monad.CpuEmulator where

import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Lens.Micro.Platform
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST

data CpuEnv w s = CpuEnv
	{ _regs  :: MV.MVector s w
	, _flags :: MV.MVector s Bool
	, _ram   :: MV.MVector s w
	}

makeLenses ''CpuEnv

data CpuState w = CpuState
	{ cpuRegs  :: V.Vector w
	, cpuFlags :: V.Vector Bool
	, cpuRam   :: V.Vector w
	} deriving (Show)


newtype Cpu w s a = Cpu { unCpu :: ReaderT (CpuEnv w s) (ST s) a }

freezeCpuEnv :: PrimMonad m => CpuEnv w (PrimState m) ->  m (CpuState w)
freezeCpuEnv (CpuEnv regs flags ram) = CpuState <$> V.freeze regs <*> V.freeze flags <*> V.freeze ram

runCpu :: forall a w. (forall s. Cpu w s a) -> (forall s. CpuEnv w s) -> (a, CpuState w)
runCpu cpu env = runST $ runReaderT readerComp env
	where
		readerComp :: ReaderT (CpuEnv w s) (ST s) (a, CpuState w)
		readerComp =
			unCpu cpu  >>= \a -> do
				frozenEnv <- ReaderT freezeCpuEnv
				return (a, frozenEnv)


