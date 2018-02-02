{-# LANGUAGE TemplateHaskell
	, RankNTypes
	, ScopedTypeVariables
	, DefaultSignatures
	, MultiParamTypeClasses
	, FunctionalDependencies
	, TypeFamilies
	, FlexibleInstances
	, UndecidableInstances
	#-}

module Control.Monad.CpuEmulator where

import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Lens.Micro.Platform
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Cont

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

thawCpuState :: PrimMonad m => CpuState w -> m (CpuEnv w (PrimState m))
thawCpuState (CpuState regs flags ram) =
	CpuEnv
	<$> V.thaw regs
	<*> V.thaw flags
	<*> V.thaw ram

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

runCpu' :: forall a w. (forall s. CpuState w -> ST s (CpuEnv w s)) -> (forall s. Cpu w s a) -> CpuState w -> (a, CpuState w)
runCpu' thaw cpu state = runST $ do
	cpuEnv <- thaw state
	runReaderT readerComp cpuEnv
	where
		readerComp :: ReaderT (CpuEnv w s) (ST s) (a, CpuState w)
		readerComp = do
			a <- unCpu cpu
			frozenEnv <- ReaderT unsafeFreezeCpuEnv
			return (a, frozenEnv)

-- | Runs Cpu by safely thawing provided vectors O(n).
-- The provided vectors can still be used after the function.
runCpu :: (forall s. Cpu w s a) -> CpuState w -> (a, CpuState w)
runCpu = runCpu' thawCpuState

-- | Runs Cpu by unsafely thawing provided vectors O(1).
-- The provided vectors must not be used after this function.
unsafeRunCpu :: (forall s. Cpu w s a) -> CpuState w -> (a, CpuState w)
unsafeRunCpu = runCpu' unsafeThawCpuState

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

-- mtl typeclasses
class Monad m => MonadCpu w m | m -> w  where
	getReg :: Int -> m w
	default getReg :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => Int -> m w
	getReg = lift . getReg

	setReg :: Int -> w -> m ()
	default setReg :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => Int -> w -> m ()
	setReg = (lift .) . setReg

	getFlag :: Int -> m Bool
	default getFlag :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => Int -> m Bool
	getFlag = lift . getFlag

	setFlag :: Int -> Bool -> m ()
	default setFlag :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => Int -> Bool -> m ()
	setFlag = (lift .) . setFlag

	getRam :: Int -> m w
	default getRam :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => Int -> m w
	getRam = lift . getRam

	setRam :: Int -> w -> m ()
	default setRam :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => Int -> w -> m ()
	setRam = (lift .) . setRam

instance MonadCpu w (Cpu w s) where
	getReg i = Cpu . ReaderT $ \r -> MV.read (r^.regs) i
	setReg i x = Cpu . ReaderT $ \r -> MV.write (r^.regs) i x

	getFlag i = Cpu . ReaderT $ \r -> MV.read (r^.flags) i
	setFlag i b = Cpu . ReaderT $ \r -> MV.write (r^.flags) i b

	getRam i = Cpu . ReaderT $ \r -> MV.read (r^.ram) i
	setRam i x = Cpu . ReaderT $ \r -> MV.write (r^.ram) i x


instance MonadCpu w m => MonadCpu w (ReaderT r m)
instance (MonadCpu w m, Monoid l) => MonadCpu w (WriterT l m)
instance MonadCpu w m => MonadCpu w (StateT s m)
instance (MonadCpu w m, Monoid l) => MonadCpu w (RWST r l s m)
instance MonadCpu w m => MonadCpu w (ExceptT e m)
instance MonadCpu w m => MonadCpu w (ContT r m)

v = V.fromList [0, 1, 2, 3, 20] :: V.Vector Int
b = V.fromList [False] :: V.Vector Bool
initState = CpuState v b v

test = runCpu (runReaderT comp 2) initState

comp :: ReaderT Int (Cpu Int s) Int
comp = do
	env <- ask
	setReg 0 env
	r0 <- getReg 0
	return r0
	{-r4 <- getReg 4-}
	{-{-return $ r0 + r4-}-}
	{-v <- getRam 3-}
	{-setFlag 0 True-}
	{-flag <- getFlag 0-}
	{-if flag-}
		{-then return r0-}
		{-else return r4-}

