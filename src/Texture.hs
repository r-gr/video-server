module Texture (
                 -- updateAssignments
               -- , updateStartTime
               )
               where


-- import RIO

-- import Types


-- updateAssignments :: [(NodeID, UnitID, Int)] -> Texture -> Texture
-- updateAssignments assigns tex =
--   if assigns == assignments tex then
--     tex
--   else case tex of
--     Vid texture -> Vid $ texture { vAssignments = assigns }
--     Img texture -> Img $ texture { iAssignments = assigns , iIsBound = False}
--     LVd texture -> LVd $ texture { lvAssignments = assigns }


-- updateStartTime :: Maybe Double -> Texture -> Texture
-- updateStartTime timestamp (Vid texture) = Vid $ texture { vStartTime = timestamp }
-- updateStartTime timestamp (LVd texture) = LVd $ texture { lvStartTime = timestamp }
-- updateStartTime _ imageTexture = imageTexture
