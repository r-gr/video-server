module Texture ( Texture(..)
               , TextureClass(..)
               , ImageTexture(..)
               , VideoTexture(..)
               , LoadedVideo(..)
               , FrameGrabber
               , updateAssignments
               , updateStartTime
               )
               where


import Codec.Picture
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL as GL

import Unit
import Graph


type FrameGrabber p = IO (Maybe (Image p, Double))
class TextureClass t where
  texObj      :: t -> GL.TextureObject
  texUnit     :: t -> GL.TextureUnit
  assignments :: t -> [(GraphID, UnitID, Int)]
  texID       :: t -> Int
data Texture = Vid VideoTexture | Img ImageTexture | LVd LoadedVideo
data VideoTexture
  = VideoTexture { vTexObj        :: GL.TextureObject
                 , vTexUnit       :: GL.TextureUnit
                 , vAssignments   :: [(GraphID, UnitID, Int)]
                 , vID            :: Int
                 , vLoop          :: Bool
                 , vFilePath      :: FilePath
                 , vNextFrame     :: (FrameGrabber PixelRGB8)
                 , vCleanupFFmpeg :: (IO ())
                 , vStartTime     :: Maybe Double
                 , vCurrentFrame  :: Int
                 , vFps           :: Maybe Double
                 , vRate          :: Double
                 }
data LoadedVideo
  = LoadedVideo { lvTexUnit       :: GL.TextureUnit
                , lvAssignments   :: [(GraphID, UnitID, Int)]
                , lvID            :: Int
                , lvLoop          :: Bool
                , lvFrames        :: V.Vector (GL.TextureObject, Double)
                , lvNumFrames     :: Int
                , lvStartTime     :: Maybe Double
                , lvCurrentFrame  :: Int
                , lvFps           :: Maybe Double
                , lvRate          :: Double
                }
data ImageTexture
  = ImageTexture { iTexObj      :: GL.TextureObject
                 , iTexUnit     :: GL.TextureUnit
                 , iAssignments :: [(GraphID, UnitID, Int)]
                 , iID          :: Int
                 , iImageRGB8   :: (Image PixelRGB8)
                 , iIsBound     :: Bool
                 }

instance Eq Texture where
  Vid _ == Img _ = False
  Img _ == Vid _ = False
  LVd _ == Vid _ = False
  Vid _ == LVd _ = False
  LVd _ == Img _ = False
  Img _ == LVd _ = False
  Vid v1 == Vid v2 = v1 == v2
  Img i1 == Img i2 = i1 == i2
  LVd v1 == LVd v2 = v1 == v2

instance Eq VideoTexture where
  VideoTexture tObj1 tUnit1 _ _ _ _ _ _ _ _ _ _ == VideoTexture tObj2 tUnit2 _ _ _ _ _ _ _ _ _ _ =
    tObj1 == tObj2 && tUnit1 == tUnit2

instance Eq ImageTexture where
  ImageTexture tObj1 tUnit1 _ _ _ _ == ImageTexture tObj2 tUnit2 _ _ _ _ =
    tObj1 == tObj2 && tUnit1 == tUnit2

instance Eq LoadedVideo where
  LoadedVideo tUnit1 _ vID1 _ _ _ _ _ _ _ == LoadedVideo tUnit2 _ vID2 _ _ _ _ _ _ _ =
    tUnit1 == tUnit2 && vID1 == vID2

instance TextureClass VideoTexture where
  texObj t = vTexObj t
  texUnit t = vTexUnit t
  assignments t = vAssignments t
  texID t = vID t

instance TextureClass ImageTexture where
  texObj t = iTexObj t
  texUnit t = iTexUnit t
  assignments t = iAssignments t
  texID t = iID t

instance TextureClass LoadedVideo where
  texObj t = fst $ (lvFrames t) V.! (lvCurrentFrame t)
  texUnit t = lvTexUnit t
  assignments t = lvAssignments t
  texID t = lvID t

instance TextureClass Texture where
  texObj (Vid t) = texObj t
  texObj (Img t) = texObj t
  texObj (LVd t) = texObj t
  texUnit (Vid t) = texUnit t
  texUnit (Img t) = texUnit t
  texUnit (LVd t) = texUnit t
  assignments (Vid t) = assignments t
  assignments (Img t) = assignments t
  assignments (LVd t) = assignments t
  texID (Vid t) = texID t
  texID (Img t) = texID t
  texID (LVd t) = texID t


updateAssignments :: [(GraphID, UnitID, Int)] -> Texture -> Texture
updateAssignments assigns tex =
  if assigns == assignments tex then
    tex
  else case tex of
    Vid texture -> Vid $ texture { vAssignments = assigns }
    Img texture -> Img $ texture { iAssignments = assigns , iIsBound = False}
    LVd texture -> LVd $ texture { lvAssignments = assigns }


updateStartTime :: Maybe Double -> Texture -> Texture
updateStartTime timestamp (Vid texture) = Vid $ texture { vStartTime = timestamp }
updateStartTime timestamp (LVd texture) = LVd $ texture { lvStartTime = timestamp }
updateStartTime _ imageTexture = imageTexture
