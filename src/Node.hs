module Node ( NodeID
            , Node(..)
            ) where

import Data.IntMap (IntMap)
import Graph (Graph)

type NodeID = Int
data Node = Node Graph | Group (IntMap Node)
