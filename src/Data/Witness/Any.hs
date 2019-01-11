module Data.Witness.Any where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Data.Maybe
import Data.Type.Equality
import Data.Witness.Constraint
import Prelude

-- | Any value with a witness to a parameter of its type.
data AnyF (w :: k -> Type) (f :: k -> Type) =
    forall (a :: k). MkAnyF (w a)
                            (f a)

matchAnyF :: TestEquality w => w a -> AnyF w f -> Maybe (f a)
matchAnyF wit (MkAnyF cwit cfa) = do
    Refl <- testEquality cwit wit
    return cfa

type AnyValue w = AnyF w Identity

pattern MkAnyValue :: w a -> a -> AnyValue w

pattern MkAnyValue wa a = MkAnyF wa (Identity a)

{-# COMPLETE MkAnyValue #-}

matchAnyValue :: TestEquality w => w a -> AnyValue w -> Maybe a
matchAnyValue wit av = do
    Identity a <- matchAnyF wit av
    return a

type AnyW w = AnyF w (Const ())

pattern MkAnyW :: w a -> AnyW w

pattern MkAnyW wa = MkAnyF wa (Const ())

{-# COMPLETE MkAnyW #-}

matchAnyW :: TestEquality w => w a -> AnyW w -> Bool
matchAnyW wit aw = isJust $ matchAnyF wit aw

instance TestEquality w => Eq (AnyW w) where
    (==) (MkAnyW wa) = matchAnyW wa

mapAnyW :: (forall t. w1 t -> w2 t) -> AnyW w1 -> AnyW w2
mapAnyW f (MkAnyW wt) = MkAnyW $ f wt

instance AllWitnessConstraint Show w => Show (AnyW w) where
    show (MkAnyW wa) = showAllWitness wa
