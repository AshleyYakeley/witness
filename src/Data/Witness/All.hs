module Data.Witness.All where

import Data.Functor.Identity
import Data.Kind
import Data.Type.Equality
import Data.Witness.Any
import Prelude

newtype AllF (w :: k -> Type) (f :: k -> Type) = MkAllF
    { getAllF :: forall (t :: k). w t -> f t
    }

newtype AllValue (w :: Type -> Type) = MkAllValue
    { getAllValue :: forall t. w t -> t
    }

allFToAllValue :: AllF w Identity -> AllValue w
allFToAllValue (MkAllF wtit) = MkAllValue $ \wt -> runIdentity $ wtit wt

allValueToAllF :: AllValue w -> AllF w Identity
allValueToAllF (MkAllValue wtt) = MkAllF $ \wt -> Identity $ wtt wt

type family UnAllValue (aw :: Type) :: Type -> Type where
    UnAllValue (AllValue w) = w

splitWitnessList :: TestEquality w => [AnyValue w] -> AllF w []
splitWitnessList [] = MkAllF $ \_ -> []
splitWitnessList ((MkAnyValue wt t):rr) =
    MkAllF $ \wt' ->
        case testEquality wt wt' of
            Just Refl -> t : (getAllF (splitWitnessList rr) wt')
            Nothing -> getAllF (splitWitnessList rr) wt'
