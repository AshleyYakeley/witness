module Data.Witness.Specific.Any where

import Data.Witness.General.Constraint
import Import

-- | Any value with a witness to a parameter of its type.
type AnyF :: forall k. (k -> Type) -> (k -> Type) -> Type
data AnyF w f =
    forall a. MkAnyF (w a)
                     (f a)

matchAnyF :: TestEquality w => w a -> AnyF w f -> Maybe (f a)
matchAnyF wit (MkAnyF cwit cfa) = do
    Refl <- testEquality cwit wit
    return cfa

type AnyValue :: (Type -> Type) -> Type
type AnyValue w = AnyF w Identity

pattern MkAnyValue :: w a -> a -> AnyValue w

pattern MkAnyValue wa a = MkAnyF wa (Identity a)

{-# COMPLETE MkAnyValue #-}

instance (TestEquality w, WitnessConstraint Eq w) => Eq (AnyValue w) where
    MkAnyValue wa a == MkAnyValue wb b =
        case testEquality wa wb of
            Nothing -> False
            Just Refl ->
                case witnessConstraint @_ @Eq wa of
                    Dict -> a == b

matchAnyValue ::
       forall (w :: Type -> Type) (a :: Type). TestEquality w
    => w a
    -> AnyValue w
    -> Maybe a
matchAnyValue wit av = do
    Identity a <- matchAnyF wit av
    return a

type AnyW :: forall k. (k -> Type) -> Type
type AnyW w = AnyF w (Const ())

pattern MkAnyW :: w a -> AnyW w

pattern MkAnyW wa = MkAnyF wa (Const ())

{-# COMPLETE MkAnyW #-}

matchAnyW ::
       forall k (w :: k -> Type) (a :: k). TestEquality w
    => w a
    -> AnyW w
    -> Bool
matchAnyW wit aw = isJust $ matchAnyF wit aw

instance forall k (w :: k -> Type). TestEquality w => Eq (AnyW w) where
    (==) (MkAnyW wa) = matchAnyW wa

mapAnyW :: forall k (w1 :: k -> Type) (w2 :: k -> Type). (forall t. w1 t -> w2 t) -> AnyW w1 -> AnyW w2
mapAnyW f (MkAnyW wt) = MkAnyW $ f wt

instance forall k (w :: k -> Type). AllWitnessConstraint Show w => Show (AnyW w) where
    show (MkAnyW wa) = showAllWitness wa
