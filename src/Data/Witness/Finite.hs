{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.Finite where

import Data.Witness.All
import Data.Witness.Any
import Data.Witness.Constraint
import Import

type FiniteWitness :: forall k. (k -> Type) -> Constraint
class FiniteWitness (w :: k -> Type) where
    assembleWitnessF ::
           forall (m :: Type -> Type) (f :: k -> Type). Applicative m
        => (forall (t :: k). w t -> m (f t))
        -> m (AllF w f)

instance (TestEquality w, FiniteWitness w) => Countable (AnyW w) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance (TestEquality w, FiniteWitness w) => Searchable (AnyW w) where
    search = finiteSearch

instance (TestEquality w, FiniteWitness w) => Finite (AnyW w) where
    assemble ::
           forall b f. Applicative f
        => (AnyW w -> f b)
        -> f (AnyW w -> b)
    assemble afb =
        fmap (\(MkAllF wtcb) (MkAnyW wt) -> getConst $ wtcb wt) $ assembleWitnessF $ \wt -> fmap Const $ afb $ MkAnyW wt
    allValues = getConst $ assembleWitnessF $ \wt -> Const [MkAnyW wt]

allWitnesses :: FiniteWitness w => [AnyW w]
allWitnesses = getConst $ assembleWitnessF $ \wt -> Const [MkAnyW wt]

instance (FiniteWitness w, AllWitnessConstraint Show w, WitnessConstraint Show w) => Show (AllValue w) where
    show (MkAllValue wtt) = let
        showItem :: AnyW w -> String
        showItem (MkAnyW wt) =
            showAllWitness wt ++
            " -> " ++
            case witnessConstraint @_ @Show wt of
                Dict -> show (wtt wt)
        in "{" ++ intercalate "," (fmap showItem allWitnesses) ++ "}"

assembleWitness :: (FiniteWitness w, Applicative m) => (forall t. w t -> m t) -> m (AllValue w)
assembleWitness wtmt = fmap allFToAllValue $ assembleWitnessF $ \wt -> fmap Identity $ wtmt wt
