{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.Single where

import Data.Constraint
import Data.Type.Equality
import Data.Witness.All
import Data.Witness.Constraint
import Data.Witness.Finite
import Prelude

type SingleWitness = (:~:)

instance FiniteWitness (SingleWitness t) where
    assembleWitnessF getsel = fmap (\ft -> MkAllF $ \Refl -> ft) $ getsel Refl

instance c t => WitnessConstraint c (SingleWitness t) where
    witnessConstraint Refl = Dict

singleAll :: t -> AllValue (SingleWitness t)
singleAll t = MkAllValue $ \Refl -> t

getSingleAll :: AllValue (SingleWitness t) -> t
getSingleAll (MkAllValue f) = f Refl
