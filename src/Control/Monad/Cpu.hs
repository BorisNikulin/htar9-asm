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

module Control.Monad.Cpu
	( -- * MonadCpu class
	  MonadCpu(..)
	, modifyPc
	  -- * The Cpu monad
	, Cpu
	, CpuState(..)
	, runCpu
	, unsafeRunCpu
	) where

import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.STRef
import Lens.Micro.Platform
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST.Strict
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Cont

data CpuEnv w s = CpuEnv
	{ _regs  :: MV.MVector s w
	, _flags :: MV.MVector s Bool
	, _ram   :: MV.MVector s w
	, _pc     :: STRef s Word -- or use Word64?
	}

makeLenses ''CpuEnv

-- | The initial and final state of a 'Cpu' computation.
data CpuState w = CpuState
	{ cpuRegs  :: V.Vector w
	, cpuFlags :: V.Vector Bool
	, cpuRam   :: V.Vector w
	, cpuPc    :: Word
	} deriving (Show)

-- | 'Cpu' monad with registers, flags, ram, and a program counter.
-- 'Cpu' is designed with emulating simple instruction sets and thus is more like
-- a single core, non-superscalar cpu. 'Cpu' is parametarized by the contents of
-- the registers and ram. NB: the getXXX, except getPc, functions can be passed
-- indecies out of bounds which will cause out of bounds errors.
newtype Cpu w s a = Cpu { unCpu :: ReaderT (CpuEnv w s) (ST s) a }

thawCpuState :: CpuState w -> (forall s. ST s (CpuEnv w s))
thawCpuState (CpuState regs flags ram pc) =
	CpuEnv
	<$> V.thaw regs
	<*> V.thaw flags
	<*> V.thaw ram
	<*> newSTRef pc

unsafeThawCpuState :: CpuState w -> ST s (CpuEnv w s)
unsafeThawCpuState (CpuState regs flags ram pc) =
	CpuEnv
	<$> V.unsafeThaw regs
	<*> V.unsafeThaw flags
	<*> V.unsafeThaw ram
	<*> newSTRef pc

freezeCpuEnv :: CpuEnv w s -> ST s (CpuState w)
freezeCpuEnv (CpuEnv regs flags ram pc) =
	CpuState
	<$> V.freeze regs
	<*> V.freeze flags
	<*> V.freeze ram
	<*> readSTRef pc

unsafeFreezeCpuEnv :: CpuEnv w s -> ST s (CpuState w)
unsafeFreezeCpuEnv (CpuEnv regs flags ram pc) =
	CpuState
	<$> V.unsafeFreeze regs
	<*> V.unsafeFreeze flags
	<*> V.unsafeFreeze ram
	<*> readSTRef pc

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

-- | Runs 'Cpu' by safely thawing provided vectors O(n).
-- The provided vectors can still be used after the function.
runCpu :: (forall s. Cpu w s a) -> CpuState w -> (a, CpuState w)
runCpu = runCpu' thawCpuState

-- | Runs 'Cpu' by unsafely thawing provided vectors O(1).
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

-- | The mtl style typeclass for 'Cpu' monads.
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

	getPc :: m Word
	default getPc :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => m Word
	getPc = lift getPc

	setPc :: Word -> m ()
	default setPc :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => Word -> m ()
	setPc = lift . setPc

	getState :: m (CpuState w)
	default getState :: (MonadTrans t, MonadCpu w m1, m ~ t m1) => m (CpuState w)
	getState = lift getState

instance MonadCpu w (Cpu w s) where
	getReg i    = Cpu . ReaderT $ \r -> MV.read  (r^.regs) i
	setReg i x  = Cpu . ReaderT $ \r -> MV.write (r^.regs) i x

	getFlag i   = Cpu . ReaderT $ \r -> MV.read  (r^.flags) i
	setFlag i b = Cpu . ReaderT $ \r -> MV.write (r^.flags) i b

	getRam i    = Cpu . ReaderT $ \r -> MV.read  (r^.ram) i
	setRam i x  = Cpu . ReaderT $ \r -> MV.write (r^.ram) i x

	getPc       = Cpu . ReaderT $ \r -> readSTRef  (r^.pc)
	setPc x     = Cpu . ReaderT $ \r -> writeSTRef (r^.pc) x

	getState    = Cpu . ReaderT $ \r -> freezeCpuEnv r

instance MonadCpu w m => MonadCpu w (ReaderT r m)
instance (MonadCpu w m, Monoid l) => MonadCpu w (WriterT l m)
instance MonadCpu w m => MonadCpu w (StateT s m)
instance (MonadCpu w m, Monoid l) => MonadCpu w (RWST r l s m)
instance MonadCpu w m => MonadCpu w (ExceptT e m)
instance MonadCpu w m => MonadCpu w (ContT r m)

-- | Strictly modifies the program counter with the given function.
modifyPc :: MonadCpu w m => (Word -> Word) -> m ()
modifyPc f = do
	pc <- getPc
	let pc' = f pc
	pc' `seq` setPc pc'
