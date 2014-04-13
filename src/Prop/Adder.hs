module Prop.Adder where

import Formula
import Prop.Base

halfadder x y = x <=> neg y

halfcarry x y = x /\ y
