module Graph (partition) where

import Control.Monad
import Control.Monad.Loops
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.List (groupBy, nub)
import Data.Maybe

import Types


-- tmp for ghcid
main :: IO ()
main = undefined


type NewGraph = [NewUnit]
data NewUnit = NewUnit { nuName :: String
                       , nuID :: UnitID
                       , nuNodeID :: NodeID
                       , nuInputs :: [WireID]
                       , nuOutput :: WireID
                       , nuPartition :: Bool
                       } deriving (Eq)
data Link = Wire UnitID [UnitID]
          | LBus UnitID [UnitID]
          deriving (Eq)

data NewSubGraph = NewSubGraph NewGraph [Link] Link


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
      let wiresToChange = foldr (\index wires -> ((nuInputs u) !! index) : wires)
                                [] (fromJust $ partitionOn (nuName u))

      forM_ wiresToChange $ \w -> do
        links <- readIORef linksRef
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
    let multiOutUnits = flip filter activeList' $ \u ->
          case links IntMap.! (nuOutput u) of
            Wire _ dests -> length dests > 1
            _ -> False
        len = length multiOutUnits

    index <- newIORef 0
    shouldContinue <- newIORef True

    whileM_ (keepGoing shouldContinue index len) $ do
      links' <- readIORef linksRef
      index' <- readIORef index
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

  -- now have wires of the correct types.

  -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  -- TODO: split units into groups based on when the output is to a local bus
  -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  -- assume lower ID units will _always_ output to higher ID units? I think that
  -- makes it simpler but is it necessary?
  -- could always throw in an extra check that the first units have no inputs from
  -- other video UGens?
  links' <- readIORef linksRef
  return $ groupUnitsInSubGraphs unitsMap links'
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

groupUnitsInSubGraphs :: IntMap NewUnit -> IntMap Link -> [NewSubGraph]
groupUnitsInSubGraphs units links =
  let unitList = IntMap.toAscList units
      -- i. start at lowest index unit, determine where all paths lead (they should
      --    converge on either an output with no further wires or a specific LBus?)
      -- ii. do that for all units
      unitPathDests = flip map unitList $ \(_, u) ->
                        nuOutput u            -- :: WireID
                        |> (links IntMap.!)   -- :: Link
                        |> findDestination    -- :: NewUnit
                        |> \dest -> (u, dest) -- :: (NewUnit, NewUnit)
      -- iii. group units which end up at the same place
      unitGroups = unitPathDests
                   |> groupBy (\(_, dest1) (_, dest2) -> dest1 == dest2) -- :: [[(NewUnit, NewUnit)]]
                   |> map (map (\(u, _) -> u)) -- [NewGraph] == [[NewUnit]]
  in
    -- TODO: get inputs to the group of units. That is, links which come from other units in
    --       another SubGraph (or none)

    -- if a unit has no inputs then it won't be the output destination of any unit

    -- need to look at input wires of each unit in the SubGraph
    --   if of the LBus type then add to SubGraph's inputs
    --   if of the Wire type then ignore
    unitGroups
    |> map (\g -> NewSubGraph g (subGraphInputs g) (links IntMap.! (nuOutput $ findDestination $ links IntMap.! (nuOutput $ head g))))

  where
    findDestination :: Link -> NewUnit
    findDestination (LBus src _)   = units IntMap.! src
    findDestination (Wire src [])  = units IntMap.! src
    findDestination (Wire _ (x:_)) = findDestination $ links IntMap.! (nuOutput $ units IntMap.! x)

    subGraphInputs :: [NewUnit] -> [Link]
    subGraphInputs graph =
      graph
      |> concatMap nuInputs
      |> filter (\link -> case IntMap.lookup link links of
                            Just (LBus _ _) -> True
                            _ -> False)
      |> map (links IntMap.!)
      |> nub

(|>) :: a -> (a -> b) -> b
x |> f = f x
