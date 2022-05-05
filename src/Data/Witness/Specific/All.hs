module Data.Witness.Specific.All where

import Data.Witness.Specific.Some
import Import

type AllFor :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype AllFor w f = MkAllFor
    { unAllFor :: forall t. w t -> f t
    }

type AllOf :: (Type -> Type) -> Type
newtype AllOf w = MkAllOf
    { unAllOf :: forall t. w t -> t
    }

setAllOf ::
       forall (w :: Type -> Type) (a :: Type). TestEquality w
    => w a
    -> a
    -> AllOf w
    -> AllOf w
setAllOf wa a (MkAllOf wtt) =
    MkAllOf $ \wa' ->
        case testEquality wa wa' of
            Just Refl -> a
            Nothing -> wtt wa'

allForToAllOf :: forall (w :: Type -> Type). AllFor w Identity -> AllOf w
allForToAllOf (MkAllFor wtit) = MkAllOf $ \wt -> runIdentity $ wtit wt

allOfToAllFor :: forall (w :: Type -> Type). AllOf w -> AllFor w Identity
allOfToAllFor (MkAllOf wtt) = MkAllFor $ \wt -> Identity $ wtt wt

type UnAllOf :: Type -> Type -> Type
type family UnAllOf aw where
    UnAllOf (AllOf w) = w

splitWitnessList ::
       forall (w :: Type -> Type). TestEquality w
    => [SomeOf w]
    -> AllFor w []
splitWitnessList [] = MkAllFor $ \_ -> []
splitWitnessList ((MkSomeOf wt t):rr) =
    MkAllFor $ \wt' ->
        case testEquality wt wt' of
            Just Refl -> t : (unAllFor (splitWitnessList rr) wt')
            Nothing -> unAllFor (splitWitnessList rr) wt'
