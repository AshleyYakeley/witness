module Data.Witness.Submap where

import Data.Kind
import Data.Witness.All
import Data.Witness.Any
import Data.Witness.Either
import Data.Witness.Finite
import Prelude

data SubmapWitness (w :: k -> *) (f :: k -> *) = MkSubmapWitness
    { subWitnessDomain :: [AnyWitness w]
    , subWitnessMap :: forall (t :: k). w t -> f t
    }

subWitnessCodomain :: SubmapWitness w f -> [AnyWitness f]
subWitnessCodomain schema =
    fmap (\(MkAnyWitness st) -> MkAnyWitness $ subWitnessMap schema st) $ subWitnessDomain schema

mapSubmapWitness :: (forall t. f1 t -> f2 t) -> SubmapWitness w f1 -> SubmapWitness w f2
mapSubmapWitness ff (MkSubmapWitness ai i) = MkSubmapWitness ai $ \s -> ff $ i s

eitherSubmapWitness ::
       SubmapWitness sel1 itemSchema
    -> SubmapWitness sel2 itemSchema
    -> SubmapWitness (EitherWitness sel1 sel2) itemSchema
eitherSubmapWitness (MkSubmapWitness a1 i1) (MkSubmapWitness a2 i2) =
    MkSubmapWitness
        ((fmap (mapAnyWitness LeftWitness) a1) ++ (fmap (mapAnyWitness RightWitness) a2))
        (getAllF $ eitherAllF (MkAllF i1) (MkAllF i2))

finiteSubmapWitness :: FiniteWitness w => (forall t. w t -> f t) -> SubmapWitness w f
finiteSubmapWitness wf = MkSubmapWitness allWitnesses wf
