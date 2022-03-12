module Data.IndNat where

import Prelude

-- | Inductive natural numbers.
data IndNat
    = Zero
    | Succ IndNat

addNat :: IndNat -> IndNat -> IndNat
addNat Zero b = b
addNat (Succ a) b = Succ $ addNat a b

-- | subtractFromNat a b = b - a
subtractFromNat :: IndNat -> IndNat -> Maybe IndNat
subtractFromNat Zero b = Just b
subtractFromNat (Succ a) (Succ b) = subtractFromNat a b
subtractFromNat (Succ _) Zero = Nothing

multiplyNat :: IndNat -> IndNat -> IndNat
multiplyNat Zero _ = Zero
multiplyNat (Succ a) b = addNat (multiplyNat a b) b
