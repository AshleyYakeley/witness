module Data.Witness.Any where

import Data.Maybe
import Data.Type.Equality

-- | Any value with a witness to it.
data Any (w :: * -> *) =
    forall (a :: *). MkAny (w a)
                           a

matchAny :: (TestEquality w) => w a -> Any w -> Maybe a
matchAny wit (MkAny cwit ca) = do
    Refl <- testEquality cwit wit
    return ca

-- | Any value with a witness to a parameter of its type.
data AnyF (w :: k -> *) (f :: k -> *) =
    forall (a :: k). MkAnyF (w a)
                            (f a)

matchAnyF :: (TestEquality w) => w a -> AnyF w f -> Maybe (f a)
matchAnyF wit (MkAnyF cwit cfa) = do
    Refl <- testEquality cwit wit
    return cfa

-- | Any witness.
data AnyWitness (w :: k -> *) =
    forall (a :: k). MkAnyWitness (w a)

matchAnyWitness :: (TestEquality w) => w a -> AnyWitness w -> Bool
matchAnyWitness wit (MkAnyWitness cwit) = isJust (testEquality cwit wit)

instance (TestEquality w) => Eq (AnyWitness w) where
    (==) (MkAnyWitness wa) = matchAnyWitness wa
