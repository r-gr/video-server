module Graph ( Graph
             , GraphID
             , containsVideoUGen
             ) where

import Unit

type GraphID = Int
type Graph   = (GraphID, [Unit])

containsVideoUGen :: [String] -> [Unit] -> Bool
containsVideoUGen glUGenNames units =
  elem True $ map (\unit -> elem (unitName unit) glUGenNames) units
