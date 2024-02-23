{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}

module Control.Carrier.FeatureFlag.IO where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Maybe
import Data.HashTable.IO qualified as H
import Data.Typeable
import Control.Effect.FeatureFlag
import Data.Coerce
import Data.Proxy
import Data.Hashable

data SomeHashable where
  SomeHashable :: (Typeable a, Eq a, Hashable a) => a -> SomeHashable

instance Eq SomeHashable where
  SomeHashable a == SomeHashable b = cast a == Just b

instance Hashable SomeHashable where
  hashWithSalt s (SomeHashable a) = hashWithSalt s a

type HashTable k v = H.BasicHashTable k v
type HashSet a = HashTable a ()

data FlagSet = FlagSet
  { globals :: HashSet TypeRep
  , perDatum :: HashTable SomeHashable (HashSet TypeRep)
  }

newtype FeatureFlagC m a = FeatureFlagC (ReaderC (MVar FlagSet) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runFeatureFlag :: Has (Lift IO) sig m => FeatureFlagC m a -> m a
runFeatureFlag (FeatureFlagC act) = do
  ght <- sendM H.new
  pdt <- sendM H.new
  ref <- sendM . newMVar $ FlagSet ght pdt
  runReader ref act

instance (Has (Lift IO) sig m) => Algebra (FeatureFlag :+: sig) (FeatureFlagC m) where
  alg hdl sig ctx = do
    ref <- FeatureFlagC (ask @(MVar FlagSet))
    case sig of
      L (IsGloballyEnabled a) -> (<$ ctx) <$> (sendM . globalCheckIO ref . typeRepFor $ a)
      L (EnableGlobally a) -> ctx <$ (sendM . globalEnableIO ref . typeRepFor $ a)
      L (DisableGlobally a) -> ctx <$ (sendM . globalDisableIO ref . typeRepFor $ a)
      L (IsEnabledFor item a) -> (<$ ctx) <$> (sendM . globalCheckForIO ref (SomeHashable item) . typeRepFor $ a)
      R other -> FeatureFlagC (alg (coerce . hdl) (R other) ctx)

globalCheckIO :: MVar FlagSet -> TypeRep -> IO Bool
globalCheckIO fs rep = withMVar fs $ \(FlagSet globals _) -> do
  found <- H.lookup globals rep
  pure (isJust found)

globalEnableIO :: MVar FlagSet -> TypeRep -> IO ()
globalEnableIO fs rep = withMVar fs $ \(FlagSet globals _) -> H.insert globals rep ()

globalDisableIO :: MVar FlagSet -> TypeRep -> IO ()
globalDisableIO fs rep = withMVar fs $ \(FlagSet globals _) -> H.insert globals rep ()

globalCheckForIO :: MVar FlagSet -> SomeHashable -> TypeRep -> IO Bool
globalCheckForIO fs item rep = withMVar fs $ \(FlagSet _ perDatum) -> do
  registered <- H.lookup perDatum item
  case registered of
    Nothing -> pure False
    Just flags -> isJust <$> H.lookup flags rep


typeRepFor :: forall a . Typeable a => a -> TypeRep
typeRepFor _ = typeRep (Proxy @a)
