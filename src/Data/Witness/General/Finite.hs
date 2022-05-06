{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.General.Finite where

import Data.Witness.General.AllConstraint
import Data.Witness.General.WitnessConstraint
import Data.Witness.Specific.All
import Data.Witness.Specific.Some
import Import

type FiniteWitness :: forall k. (k -> Type) -> Constraint
class FiniteWitness (w :: k -> Type) where
    assembleWitnessFor ::
           forall (m :: Type -> Type) (f :: k -> Type). Applicative m
        => (forall (t :: k). w t -> m (f t))
        -> m (AllFor f w)

instance (TestEquality w, FiniteWitness w) => Countable (Some w) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance (TestEquality w, FiniteWitness w) => Searchable (Some w) where
    search = finiteSearch

instance (TestEquality w, FiniteWitness w) => Finite (Some w) where
    assemble ::
           forall b f. Applicative f
        => (Some w -> f b)
        -> f (Some w -> b)
    assemble afb =
        fmap (\(MkAllFor wtcb) (MkSome wt) -> getConst $ wtcb wt) $
        assembleWitnessFor $ \wt -> fmap Const $ afb $ MkSome wt
    allValues = getConst $ assembleWitnessFor $ \wt -> Const [MkSome wt]

allWitnesses :: FiniteWitness w => [Some w]
allWitnesses = getConst $ assembleWitnessFor $ \wt -> Const [MkSome wt]

instance (FiniteWitness w, AllConstraint Show w, WitnessConstraint Show w) => Show (AllOf w) where
    show (MkAllOf wtt) = let
        showItem :: Some w -> String
        showItem (MkSome wt) =
            allShow wt ++
            " -> " ++
            case witnessConstraint @_ @Show wt of
                Dict -> show (wtt wt)
        in "{" ++ intercalate "," (fmap showItem allWitnesses) ++ "}"

assembleWitnessOf :: (FiniteWitness w, Applicative m) => (forall t. w t -> m t) -> m (AllOf w)
assembleWitnessOf wtmt = fmap allForToAllOf $ assembleWitnessFor $ \wt -> fmap Identity $ wtmt wt
