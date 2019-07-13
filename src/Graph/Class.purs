module Graph.Class where

import Prelude
import Data.Hashable (class Hashable)

class ( Ord a
      , Hashable a
      ) <= Id a


instance idInt :: ( Ord a, Hashable a ) => Id a