module Data.Witness.All where

import Data.Witness.Any
import Import

type AllF :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype AllF w f = MkAllF
    { getAllF :: forall t. w t -> f t
    }

type AllValue :: (Type -> Type) -> Type
newtype AllValue w = MkAllValue
    { getAllValue :: forall t. w t -> t
    }

setAllValue ::
       forall (w :: Type -> Type) (a :: Type). TestEquality w
    => w a
    -> a
    -> AllValue w
    -> AllValue w
setAllValue wa a (MkAllValue wtt) =
    MkAllValue $ \wa' ->
        case testEquality wa wa' of
            Just Refl -> a
            Nothing -> wtt wa'

allFToAllValue :: forall (w :: Type -> Type). AllF w Identity -> AllValue w
allFToAllValue (MkAllF wtit) = MkAllValue $ \wt -> runIdentity $ wtit wt

allValueToAllF :: forall (w :: Type -> Type). AllValue w -> AllF w Identity
allValueToAllF (MkAllValue wtt) = MkAllF $ \wt -> Identity $ wtt wt

type UnAllValue :: Type -> Type -> Type
type family UnAllValue aw where
    UnAllValue (AllValue w) = w

splitWitnessList ::
       forall (w :: Type -> Type). TestEquality w
    => [AnyValue w]
    -> AllF w []
splitWitnessList [] = MkAllF $ \_ -> []
splitWitnessList ((MkAnyValue wt t):rr) =
    MkAllF $ \wt' ->
        case testEquality wt wt' of
            Just Refl -> t : (getAllF (splitWitnessList rr) wt')
            Nothing -> getAllF (splitWitnessList rr) wt'
