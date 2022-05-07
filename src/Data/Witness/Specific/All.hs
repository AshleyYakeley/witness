module Data.Witness.Specific.All where

import Data.Witness.General.WitnessConstraint
import Data.Witness.Specific.Some
import Import

type AllFor :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype AllFor f w = MkAllFor
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

allForToAllOf :: forall (w :: Type -> Type). AllFor Identity w -> AllOf w
allForToAllOf (MkAllFor wtit) = MkAllOf $ \wt -> runIdentity $ wtit wt

allOfToAllFor :: forall (w :: Type -> Type). AllOf w -> AllFor Identity w
allOfToAllFor (MkAllOf wtt) = MkAllFor $ \wt -> Identity $ wtt wt

allMapSome :: AllFor f w -> SomeFor g w -> SomeFor g f
allMapSome (MkAllFor f) = mapSome f

type UnAllOf :: Type -> Type -> Type
type family UnAllOf aw where
    UnAllOf (AllOf w) = w

splitSomeOfList ::
       forall (w :: Type -> Type). TestEquality w
    => [SomeOf w]
    -> AllFor [] w
splitSomeOfList [] = MkAllFor $ \_ -> []
splitSomeOfList ((MkSomeOf wt t):rr) =
    MkAllFor $ \wt' ->
        case testEquality wt wt' of
            Just Refl -> t : (unAllFor (splitSomeOfList rr) wt')
            Nothing -> unAllFor (splitSomeOfList rr) wt'

allForWitnessConstraint ::
       forall k (c :: k -> Constraint) (w :: k -> Type). WitnessConstraint c w
    => AllFor (Compose Dict c) w
allForWitnessConstraint = MkAllFor $ \wt -> Compose $ witnessConstraint wt
