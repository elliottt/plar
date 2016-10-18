module Prop.Adder where

import Prelude hiding (sum)

import Formula
import PP
import Prop.Base


halfsum, halfcarry :: Prop -> Prop -> Prop
halfsum x y = x <=> neg y
halfcarry x y = x /\ y

ha x y s c = (s <=> halfsum x y) /\ (c <=> halfcarry x y)


carry, sum :: Prop -> Prop -> Prop -> Prop
carry x y z = (x /\ y) \/ ((x \/ y) /\ z)

sum x y z = halfsum (halfsum x y) z

fa x y z s c = (s <=> sum x y z) /\ (c <=> carry x y z)


ripplecarry x y out c n =
  conj [ fa (x i) (y i) (c i) (out i) (c (i + 1)) | i <- [0 .. n - 1] ]

toVar :: String -> Int -> Prop
toVar pfx i = Atom (pfx ++ "_" ++ show i)
