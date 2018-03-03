{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.Single where

import Data.Constraint
import Data.Type.Equality
import Data.Witness.All
import Data.Witness.Constraint
import Data.Witness.Finite

type SingleWitness = (:~:)

instance FiniteWitness (SingleWitness t) where
    assembleWitnessF getsel = fmap (\ft -> MkAllF $ \Refl -> ft) $ getsel Refl

instance c t => WitnessConstraint c (SingleWitness t) where
    witnessConstraint Refl = Dict

singleAll :: t -> All (SingleWitness t)
singleAll t = MkAll $ \Refl -> t

getSingleAll :: All (SingleWitness t) -> t
getSingleAll (MkAll f) = f Refl
