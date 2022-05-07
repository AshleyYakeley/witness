module Data.Type.Witness.General.Order where

import Import

data WOrder (a :: k) (b :: k) where
    WLT :: forall k (a :: k) (b :: k). WOrder a b
    WEQ :: forall k (a :: k). WOrder a a
    WGT :: forall k (a :: k) (b :: k). WOrder a b

class TestEquality w => TestOrder (w :: k -> Type) where
    testOrder :: forall (a :: k) (b :: k). w a -> w b -> WOrder a b
