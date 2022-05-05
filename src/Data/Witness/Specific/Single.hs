{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.Specific.Single where

import Data.Witness.General.Constraint
import Data.Witness.General.Finite
import Data.Witness.Specific.All
import Import

type SingleType :: forall k. k -> k -> Type
type SingleType = (:~:)

instance FiniteWitness (SingleType t) where
    assembleWitnessF getsel = fmap (\ft -> MkAllFor $ \Refl -> ft) $ getsel Refl

instance c t => WitnessConstraint c (SingleType t) where
    witnessConstraint Refl = Dict

singleAllOf :: t -> AllOf (SingleType t)
singleAllOf t = MkAllOf $ \Refl -> t

getSingleAllOf :: AllOf (SingleType t) -> t
getSingleAllOf (MkAllOf f) = f Refl
