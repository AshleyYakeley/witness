{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.Single where

import Data.Witness.All
import Data.Witness.Constraint
import Data.Witness.Finite
import Import

type SingleType :: forall k. k -> k -> Type
type SingleType = (:~:)

instance FiniteWitness (SingleType t) where
    assembleWitnessF getsel = fmap (\ft -> MkAllF $ \Refl -> ft) $ getsel Refl

instance c t => WitnessConstraint c (SingleType t) where
    witnessConstraint Refl = Dict

singleAll :: t -> AllValue (SingleType t)
singleAll t = MkAllValue $ \Refl -> t

getSingleAll :: AllValue (SingleType t) -> t
getSingleAll (MkAllValue f) = f Refl
