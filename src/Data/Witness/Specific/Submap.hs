module Data.Witness.Specific.Submap where

import Data.Witness.General.Finite
import Data.Witness.Specific.All
import Data.Witness.Specific.Either
import Data.Witness.Specific.Some
import Import

type SubmapWitness :: forall k. (k -> Type) -> (k -> Type) -> Type
data SubmapWitness w f = MkSubmapWitness
    { subWitnessDomain :: [Some w]
    , subWitnessMap :: forall t. w t -> f t
    }

subWitnessCodomain :: SubmapWitness w f -> [Some f]
subWitnessCodomain schema = fmap (\(MkSome st) -> MkSome $ subWitnessMap schema st) $ subWitnessDomain schema

mapSubmapWitness :: (forall t. f1 t -> f2 t) -> SubmapWitness w f1 -> SubmapWitness w f2
mapSubmapWitness ff (MkSubmapWitness ai i) = MkSubmapWitness ai $ \s -> ff $ i s

eitherSubmapWitness ::
       SubmapWitness sel1 itemSchema -> SubmapWitness sel2 itemSchema -> SubmapWitness (EitherType sel1 sel2) itemSchema
eitherSubmapWitness (MkSubmapWitness a1 i1) (MkSubmapWitness a2 i2) =
    MkSubmapWitness
        ((fmap (mapSome LeftType) a1) ++ (fmap (mapSome RightType) a2))
        (unAllFor $ eitherAllFor (MkAllFor i1) (MkAllFor i2))

finiteSubmapWitness :: FiniteWitness w => (forall t. w t -> f t) -> SubmapWitness w f
finiteSubmapWitness wf = MkSubmapWitness allWitnesses wf
