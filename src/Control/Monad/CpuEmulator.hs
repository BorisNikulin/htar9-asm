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

unsafeThawCpuState :: PrimMonad m => CpuState w -> m (CpuEnv w (PrimState m))
unsafeThawCpuState (CpuState regs flags ram) =
	CpuEnv
	<$> V.unsafeThaw regs
	<*> V.unsafeThaw flags
	<*> V.unsafeThaw ram

unsafeFreezeCpuEnv :: PrimMonad m => CpuEnv w (PrimState m) ->  m (CpuState w)
unsafeFreezeCpuEnv (CpuEnv regs flags ram) =
	CpuState
	<$> V.unsafeFreeze regs
	<*> V.unsafeFreeze flags
	<*> V.unsafeFreeze ram

runCpu :: forall a w. (forall s. Cpu w s a) -> CpuState w -> (a, CpuState w)
runCpu cpu state = runST $ do
	cpuEnv <- unsafeThawCpuState state
	runReaderT readerComp cpuEnv
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

getReg :: Int -> Cpu w s w
getReg i = Cpu . ReaderT $ \r -> MV.read (r^.regs) i

setReg :: Int -> w -> Cpu w s ()
setReg i x = Cpu . ReaderT $ \r -> MV.write (r^.regs) i x

getFlag :: Int -> Cpu w s Bool
getFlag i = Cpu . ReaderT $ \r -> MV.read (r^.flags) i

setFlag :: Int -> Bool -> Cpu w s ()
setFlag i b = Cpu . ReaderT $ \r -> MV.write (r^.flags) i b

getRam :: Int -> Cpu w s w
getRam i = Cpu . ReaderT $ \r -> MV.read (r^.ram) i

setRam :: Int -> w -> Cpu w s ()
setRam i x = Cpu . ReaderT $ \r -> MV.write (r^.ram) i x

v = V.fromList [0, 1, 2, 3, 20] :: V.Vector Int
b = V.fromList [False] :: V.Vector Bool
state = CpuState v b v

test = runCpu comp state

comp = do
	r0 <- getReg 0
	r4 <- getReg 4
	{-return $ r0 + r4-}
	v <- getRam 3
	setFlag 0 True
	flag <- getFlag 0
	setReg 0 100
	if flag
		then return r0
		else return r4

