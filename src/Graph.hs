module Graph (partition) where

import Control.Monad
import Control.Monad.Loops
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.List (groupBy, nub, sortOn)
import Data.Maybe

import Types


-- theThing :: IO ()
-- theThing =
--   let units = [ SCUnit { scUnitName    = "Video"
--                        , scUnitID      = 1
--                        , scNodeID      = 1000
--                        , scUnitInputs  = []
--                        , scUnitOutputs = [1]
--                        }
--               , SCUnit { scUnitName    = "Noise"
--                        , scUnitID      = 2
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [999, 998]
--                        , scUnitOutputs = [2]
--                        }
--               , SCUnit { scUnitName    = "RGB"
--                        , scUnitID      = 3
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [2, 997, 996, 995]
--                        , scUnitOutputs = [3]
--                        }
--               , SCUnit { scUnitName    = "Alpha"
--                        , scUnitID      = 4
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [2, 994]
--                        , scUnitOutputs = [4]
--                        }
--               , SCUnit { scUnitName    = "Tex1Thing"
--                        , scUnitID      = 5
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [2, 4]
--                        , scUnitOutputs = [6]
--                        }
--               , SCUnit { scUnitName    = "Mix"
--                        , scUnitID      = 6
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [1, 3, 993]
--                        , scUnitOutputs = [5]
--                        }
--               , SCUnit { scUnitName    = "Red"
--                        , scUnitID      = 7
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [6, 992]
--                        , scUnitOutputs = [9]
--                        }
--               , SCUnit { scUnitName    = "Blend"
--                        , scUnitID      = 8
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [5, 3, 991, 990]
--                        , scUnitOutputs = [7]
--                        }
--               , SCUnit { scUnitName    = "Tex2Thing"
--                        , scUnitID      = 9
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [5, 7]
--                        , scUnitOutputs = [8]
--                        }
--               , SCUnit { scUnitName    = "Mix"
--                        , scUnitID      = 10
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [7, 8, 989]
--                        , scUnitOutputs = [10]
--                        }
--               , SCUnit { scUnitName    = "Mix"
--                        , scUnitID      = 11
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [9, 10, 988]
--                        , scUnitOutputs = [11]
--                        }
--               , SCUnit { scUnitName    = "Out"
--                        , scUnitID      = 12
--                        , scNodeID      = 1000
--                        , scUnitInputs  = [11]
--                        , scUnitOutputs = [12]
--                        }
--               ]
--   in do
--     subGraphs <- partition units
--     pPrint subGraphs





wiresMap :: [SCUnit] -> IntMap Link
wiresMap units =
  let unitOuts = IntMap.fromList
               $ map (\(outs, uID) -> (head outs, uID))
               $ filter (\(outs, _) -> length outs > 0)
               $ map (\u -> (scUnitOutputs u, scUnitID u)) units

      f :: (UnitID, [WireID]) -> IntMap Link -> IntMap Link
      f (uID, ins) intMap = foldr (\wireID intMap' ->
          let wire = IntMap.lookup wireID intMap'   -- :: Maybe Wire
              src  = IntMap.lookup wireID unitOuts -- :: Maybe UnitID
          in
            case wire of
              Nothing -> -- no extisting wire in the map
                case src of
                  Nothing    -> intMap'
                  Just srcID -> IntMap.insert wireID (Wire srcID [uID]) intMap'
              Just (Wire srcID dests) -> IntMap.insert wireID (Wire srcID $ uID : dests) intMap'
              Just (LBus srcID dests) -> IntMap.insert wireID (Wire srcID $ uID : dests) intMap'
        ) intMap ins
  in  foldr f IntMap.empty $ map (\u -> (scUnitID u, scUnitInputs u)) units


toGraph :: [SCUnit] -> NewGraph
toGraph units =
  flip map units $ \(SCUnit name uID nID ins outs) ->
    NewUnit { nuName = name
            , nuID = uID
            , nuNodeID = nID
            , nuInputs = ins
            , nuOutput = (head outs)
            , nuPartition = isJust $ partitionOn name
            }

-- Now have graph in a form where all links are Wires

partition :: [SCUnit] -> IO [NewSubGraph]
partition scUnits = do
  let units = toGraph scUnits
      unitsMap = IntMap.fromList $ map (\u -> (nuID u, u)) units

  -- Define the 'active list': all nodes which still need to be checked in
  -- the partition algorithm.
  activeListRef <- newIORef units

  linksRef <- newIORef $ wiresMap scUnits


  -- 1. Iterate through all units
  --    - cut any inputs as needed, leaving some units which don't require
  --      texture input/output but need a cut regardless
  forM_ units $ \u -> do
    if nuPartition u then do
      -- remove any units which require partition from the active list
      modifyIORef activeListRef $ filter (/= u)

      let wiresToChange = foldr (\index wires -> ((nuInputs u) !! index) : wires)
                                [] (fromJust $ partitionOn (nuName u))

      forM_ wiresToChange $ \w -> do
        links <- readIORef linksRef
        -- putStrLn "*** Debug: L168 in forM_ wiresToChange"
        let link = links IntMap.! w
            newLink = case link of
                        Wire from to -> LBus from to
                        LBus _ _     -> link

        writeIORef linksRef $ IntMap.insert w newLink links

      return ()
    else
      return ()


  -- 1.5 Drop units which output a texture from the 'active list'
  links <- readIORef linksRef
  activeList <- readIORef activeListRef
  let texOuts = map (\(wire, _) -> wire)
              $ IntMap.toList
              $ IntMap.filter (\l -> case l of LBus _ _ -> True; Wire _ _ -> False)
              $ links

  writeIORef activeListRef $ filter (\u -> not $ elem (nuOutput u) texOuts) activeList


  -- 2. For each unit with multiple output paths, ennumerate all paths which lead to
  --    unique texture outputs (the cut links/wires).
  --    - if > 1
  --        cut output of that node;
  --        remove from active list;
  --        restart 2.
  --      else continue
  whileM_ (fmap ((> 0).length) $ readIORef activeListRef) $ do
    activeList' <- readIORef activeListRef
    -- putStrLn "*** Debug: L201 in whileM_ (fmap ((> 0).length) $ readIORef activeListRef)"
    -- putStrLn $ "\n\n*** Debug: L202 activeList' = "
    -- pPrint activeList'
    let multiOutUnits = flip filter activeList' $ \u ->
          case IntMap.lookup (nuOutput u) links of
            Just (Wire _ dests) -> length dests > 1
            _ -> False
        len = length multiOutUnits

    modifyIORef activeListRef $ filter (\u -> elem u multiOutUnits)

    -- putStrLn $ "*** Debug: length multiOutUnits = " ++ (show len)

    index <- newIORef 0
    shouldContinue <- newIORef (len > 0)

    whileM_ (keepGoing shouldContinue index len) $ do
      links' <- readIORef linksRef
      index' <- readIORef index
      -- putStrLn "\n\n*** Debug: L214 in whileM_ (keepGoing shouldContinue index len)"
      let u = multiOutUnits !! index'
          outWireID = nuOutput u
          outWire = links' IntMap.! outWireID
          -- TODO: from this unit, recursively follow all output wires;
          --         if a texture output (LBus) is reached, add its wire ID to the list
          --         if path ends without reaching an LBus then leave list as-is
          --
          --       ensure no duplicate wire IDs are in the resultant list
          pathsToTextureOutputs = nub $ traverseWires outWire unitsMap links' []

      if length pathsToTextureOutputs > 1 then
        let Wire src dests = links' IntMap.! outWireID
            newLinks = IntMap.insert outWireID (LBus src dests) links'
        in do
          writeIORef linksRef newLinks
          modifyIORef activeListRef $ filter (/= u)
          writeIORef shouldContinue False
      else
        return ()

  readIORef linksRef >>= return.(groupUnitsInSubGraphs unitsMap)
  where
    -- TODO: I'm certain there's a better way to compose these IO results than
    --       needing to define a new function here.
    keepGoing continueRef indexRef len = do
      continue <- readIORef continueRef
      ind <- readIORef indexRef
      return (continue && ind < len)


traverseWires :: Link -> IntMap NewUnit -> IntMap Link -> [UnitID] -> [UnitID]
traverseWires (LBus src _) _ _ texOuts = src:texOuts
traverseWires (Wire _ [])  _ _ texOuts = texOuts
traverseWires (Wire _ dests) units links texOuts =
  let wires = map (links IntMap.!) -- Links
            $ map (nuOutput)       -- wire IDs
            $ map (units IntMap.!) -- units
            $ dests
  in  foldr (\w -> traverseWires w units links) texOuts wires


{- Split units into groups based on when the output is to a local bus.
-}
groupUnitsInSubGraphs :: IntMap NewUnit -> IntMap Link -> [NewSubGraph]
groupUnitsInSubGraphs units links =
  let unitList = IntMap.toAscList units
      -- i. start at lowest index unit, determine where all paths lead (they should
      --    converge on either an output with no further wires or a specific LBus?)
      -- ii. do that for all units
      unitPathDests = flip map unitList $ \(_, u) ->
                        nuOutput u                             -- :: WireID
                        |> (\wire -> IntMap.lookup wire links) -- :: Link
                        |> findDestination u                   -- :: NewUnit
                        |> \dest -> (u, dest)                  -- :: (NewUnit, NewUnit)
      -- iii. group units which end up at the same place
      unitGroups = unitPathDests
                   |> sortOn (nuID.snd)
                   |> groupBy (\(_, dest1) (_, dest2) -> dest1 == dest2) -- :: [[(NewUnit, NewUnit)]]
                   -- |> map (map (\(u, _) -> u)) -- [NewGraph] == [[NewUnit]]
  in do
    -- TODO: get inputs to the group of units. That is, links which come from other units in
    --       another SubGraph (or none)

    -- if a unit has no inputs then it won't be the output destination of any unit

    -- need to look at input wires of each unit in the SubGraph
    --   if of the LBus type then add to SubGraph's inputs
    --   if of the Wire type then ignore
    unitGroups
    |> map (\g -> NewSubGraph (map fst g) (subGraphInputs g) (subGraphOutput g))


  where
    findDestination :: NewUnit -> Maybe Link -> NewUnit
    findDestination _ (Just (LBus src _))   = units IntMap.! src
    findDestination _ (Just (Wire src []))  = units IntMap.! src
    -- findDestination _ (Just (Wire src [x])) = IntMap.lookup (nuOutput $ units IntMap.! x) links
    findDestination _ (Just (Wire _ (x:_))) = findDestination (units IntMap.! x) $ IntMap.lookup (nuOutput $ units IntMap.! x) links
    findDestination u Nothing = u

    subGraphInputs :: [(NewUnit, NewUnit)] -> [Link]
    subGraphInputs graph =
      graph
      |> concatMap (nuInputs.fst)
      |> filter (\link -> case IntMap.lookup link links of
                            Just (LBus _ _) -> True
                            _ -> False)
      |> map (links IntMap.!)
      |> nub

    subGraphOutput :: [(NewUnit, NewUnit)] -> Maybe Link
    subGraphOutput graph =
      graph
      |> head
      |> snd
      |> nuOutput
      |> flip IntMap.lookup links

(|>) :: a -> (a -> b) -> b
x |> f = f x
