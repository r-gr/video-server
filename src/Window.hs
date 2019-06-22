module Window ( WindowID
              , Window(..)
              ) where

import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Node

type WindowID = Int
data Window = Window { wNodes  :: IntMap Node
                     , wID     :: WindowID
                     , wShader :: ByteString
                     }
