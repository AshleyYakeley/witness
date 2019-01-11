{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.Single where

import Data.Constraint
import Data.Type.Equality
import Data.Witness.All
import Data.Witness.Constraint
import Data.Witness.Finite
import Prelude

type SingleType = (:~:)

instance FiniteWitness (SingleType t) where
    assembleWitnessF getsel = fmap (\ft -> MkAllF $ \Refl -> ft) $ getsel Refl

instance c t => WitnessConstraint c (SingleType t) where
    witnessConstraint Refl = Dict

singleAll :: t -> AllValue (SingleType t)
singleAll t = MkAllValue $ \Refl -> t

getSingleAll :: AllValue (SingleType t) -> t
getSingleAll (MkAllValue f) = f Refl
