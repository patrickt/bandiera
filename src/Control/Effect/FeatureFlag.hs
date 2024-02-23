module Control.Effect.FeatureFlag where

import Data.Kind (Type)
import Data.Typeable
import Control.Algebra
import Data.Hashable

class Typeable f => Feature f where

data FeatureFlag (m :: Type -> Type) a where
  IsGloballyEnabled :: Feature f => f -> FeatureFlag m Bool
  EnableGlobally :: Feature f => f -> FeatureFlag m ()
  DisableGlobally :: Feature f => f -> FeatureFlag m ()
  IsEnabledFor :: (Feature f, Eq a, Typeable a, Hashable a) => a -> f -> FeatureFlag m Bool

isGloballyEnabled :: (Has FeatureFlag sig m, Feature f) => f -> m Bool
isGloballyEnabled = send . IsGloballyEnabled

isGloballyDisabled :: (Has FeatureFlag sig m, Feature f) => f -> m Bool
isGloballyDisabled = fmap not . isGloballyEnabled

enableGlobally :: (Has FeatureFlag sig m, Feature f) => f -> m ()
enableGlobally = send . EnableGlobally

disableGlobally :: (Has FeatureFlag sig m, Feature f) => f -> m ()
disableGlobally = send . DisableGlobally

isEnabledFor :: (Has FeatureFlag sig m, Typeable a, Eq a, Hashable a, Feature f) => a -> f -> m Bool
isEnabledFor a = send . IsEnabledFor a
