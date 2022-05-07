{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.Specific.Single where

import Data.Witness.Specific.All
import Import

type SingleType :: forall k. k -> k -> Type
type SingleType = (:~:)

singleAllOf :: t -> AllOf (SingleType t)
singleAllOf t = MkAllOf $ \Refl -> t

getSingleAllOf :: AllOf (SingleType t) -> t
getSingleAllOf (MkAllOf f) = f Refl
