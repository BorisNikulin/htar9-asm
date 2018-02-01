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

unsafeFreezeCpuEnv :: PrimMonad m => CpuEnv w (PrimState m) ->  m (CpuState w)
unsafeFreezeCpuEnv (CpuEnv regs flags ram) =
	CpuState
	<$> V.unsafeFreeze regs
	<*> V.unsafeFreeze flags
	<*> V.unsafeFreeze ram

runCpu :: forall a w. (forall s. Cpu w s a) -> (forall s. CpuEnv w s) -> (a, CpuState w)
runCpu cpu env = runST $ runReaderT readerComp env
	where
		readerComp :: ReaderT (CpuEnv w s) (ST s) (a, CpuState w)
		readerComp = do
			a <- unCpu cpu
			frozenEnv <- ReaderT unsafeFreezeCpuEnv
			return (a, frozenEnv)

instance Functor (Cpu w s) where
	fmap f (Cpu (ReaderT r)) = Cpu . ReaderT $ \env -> fmap f (r env)

instance Applicative (Cpu w s) where
	pure = Cpu . lift . pure
	(Cpu f) <*> (Cpu r) = Cpu (f <*> r)

instance Monad (Cpu w s) where
	(Cpu r) >>= f = Cpu . ReaderT $ \env -> do
		a <- runReaderT r env
		let (Cpu r2) = f a
		runReaderT r2 env

getReg :: CpuEnv w s -> Int -> Cpu w s w
getReg (CpuEnv regs _ _) i = Cpu $ MV.read regs i
