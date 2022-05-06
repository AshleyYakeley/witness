module Data.Witness.Specific.Submap where

import Data.Witness.General.Finite
import Data.Witness.Specific.All
import Data.Witness.Specific.Either
import Data.Witness.Specific.Some
import Import

type SubmapWitness :: forall k. (k -> Type) -> (k -> Type) -> Type
data SubmapWitness f w = MkSubmapWitness
    { subWitnessDomain :: [Some w]
    , subWitnessMap :: forall t. w t -> f t
    }

subWitnessCodomain :: SubmapWitness f w -> [Some f]
subWitnessCodomain sw = fmap (\(MkSome st) -> MkSome $ subWitnessMap sw st) $ subWitnessDomain sw

mapSubmapWitness :: (forall t. f1 t -> f2 t) -> SubmapWitness f1 w -> SubmapWitness f2 w
mapSubmapWitness ff (MkSubmapWitness ai i) = MkSubmapWitness ai $ \s -> ff $ i s

eitherSubmapWitness :: SubmapWitness t w1 -> SubmapWitness t w2 -> SubmapWitness t (EitherType w1 w2)
eitherSubmapWitness (MkSubmapWitness a1 i1) (MkSubmapWitness a2 i2) =
    MkSubmapWitness
        ((fmap (mapSome LeftType) a1) ++ (fmap (mapSome RightType) a2))
        (unAllFor $ eitherAllFor (MkAllFor i1) (MkAllFor i2))

finiteSubmapWitness :: FiniteWitness w => (forall t. w t -> f t) -> SubmapWitness f w
finiteSubmapWitness wf = MkSubmapWitness allWitnesses wf
