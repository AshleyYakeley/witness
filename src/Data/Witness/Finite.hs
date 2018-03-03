{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.Finite where

import Data.Constraint
import Data.Countable
import Data.Functor.Const
import Data.Functor.Identity
import Data.List
import Data.Searchable
import Data.Type.Equality
import Data.Witness.All
import Data.Witness.Any
import Data.Witness.Constraint

class FiniteWitness (w :: k -> *) where
    assembleWitnessF :: Applicative m => (forall t. w t -> m (f t)) -> m (AllF w f)

instance (TestEquality w, FiniteWitness w) => Countable (AnyWitness w) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance (TestEquality w, FiniteWitness w) => Searchable (AnyWitness w) where
    search = finiteSearch

instance (TestEquality w, FiniteWitness w) => Finite (AnyWitness w) where
    assemble ::
           forall b f. Applicative f
        => (AnyWitness w -> f b)
        -> f (AnyWitness w -> b)
    assemble afb =
        fmap (\(MkAllF wtcb) (MkAnyWitness wt) -> getConst $ wtcb wt) $
        assembleWitnessF $ \wt -> fmap Const $ afb $ MkAnyWitness wt
    allValues = getConst $ assembleWitnessF $ \wt -> Const [MkAnyWitness wt]

allWitnesses :: FiniteWitness w => [AnyWitness w]
allWitnesses = getConst $ assembleWitnessF $ \wt -> Const [MkAnyWitness wt]

instance (FiniteWitness w, AllWitnessConstraint Show w, WitnessConstraint Show w) => Show (All w) where
    show (MkAll wtt) = let
        showItem :: AnyWitness w -> String
        showItem (MkAnyWitness wt) =
            showAllWitness wt ++
            " -> " ++
            case witnessConstraint @_ @Show wt of
                Dict -> show (wtt wt)
        in "{" ++ intercalate "," (fmap showItem allWitnesses) ++ "}"

assembleWitness :: (FiniteWitness w, Applicative m) => (forall t. w t -> m t) -> m (All w)
assembleWitness wtmt = fmap allFToAll $ assembleWitnessF $ \wt -> fmap Identity $ wtmt wt
