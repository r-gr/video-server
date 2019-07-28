module Graph (partition) where

import Control.Monad
import Control.Monad.Loops
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.List (groupBy, nub, sortOn)
import Data.Maybe

import Types


{- Construct a mapping from wire IDs to the links they form between UGens. At
   this point, each link is represented as a Wire but further analysis of the
   graph may require that some wires become a local bus (LBus) instead.
-}
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
                  Just srcID -> IntMap.insert wireID (Wire wireID srcID [uID]) intMap'
              Just (Wire _ srcID dests) -> IntMap.insert wireID (Wire wireID srcID $ uID : dests) intMap'
              Just (LBus _ srcID dests) -> IntMap.insert wireID (Wire wireID srcID $ uID : dests) intMap'
        ) intMap ins
  in  foldr f IntMap.empty $ map (\u -> (scUnitID u, scUnitInputs u)) units


toGraph :: [SCUnit] -> Graph
toGraph units =
  flip map units $ \(SCUnit name uID nID ins outs) ->
    Unit { unitName = name
         , unitID = uID
         , nodeID = nID
         , unitInputs = ins
         , unitOutput = (head outs)
         , unitPartitionOn = partitionOn name
         }


{- Partition a UGen graph based on where certain UGens require their input to
   be a texture for geometrical transformations such as rotations.

   TODO: convert this to a pure functional (non-IO) transformation. It was
         initially implemented in this iterative style to match the algorithm as
         worked out on paper.

   TODO: also ensure the output list is correctly ordered so that the shader
         programs for the SubGraphs can be executed in sequence. That is, for a
         certain SubGraph, if it takes any texture inputs from other SubGraphs,
         they must have been rendered first. I think this is already the case
         but need to check.
-}
partition :: [SCUnit] -> IO [SubGraph]
partition scUnits = do
  let units = toGraph scUnits
      unitsMap = IntMap.fromList $ map (\u -> (unitID u, u)) units

  -- Define the 'active list': all nodes which still need to be checked in
  -- the partition algorithm.
  activeListRef <- newIORef units

  linksRef <- newIORef $ wiresMap scUnits

  -- 1. Iterate through all units
  --    - cut any inputs as needed, leaving some units which don't require
  --      texture input/output but need a cut regardless
  forM_ units $ \u -> do
    if isJust $ unitPartitionOn u then do
      -- remove any units which require partition from the active list
      modifyIORef activeListRef $ filter (/= u)

      let wiresToChange = foldr (\index wires -> ((unitInputs u) !! index) : wires)
                                [] (fromJust $ partitionOn (unitName u))

      forM_ wiresToChange $ \w -> do
        links <- readIORef linksRef
        let link = links IntMap.! w
            newLink = case link of
                        Wire wireID from to -> LBus wireID from to
                        LBus _      _    _  -> link

        writeIORef linksRef $ IntMap.insert w newLink links

      return ()
    else
      return ()

  -- 1.5 Drop units which output a texture from the 'active list'
  links <- readIORef linksRef
  activeList <- readIORef activeListRef
  let texOuts = map (\(wire, _) -> wire)
              $ IntMap.toList
              $ IntMap.filter (\l -> case l of LBus _ _ _ -> True; Wire _ _ _ -> False)
              $ links

  writeIORef activeListRef $ filter (\u -> not $ elem (unitOutput u) texOuts) activeList

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
          case IntMap.lookup (unitOutput u) links of
            Just (Wire _ _ dests) -> length dests > 1
            _ -> False
        len = length multiOutUnits

    modifyIORef activeListRef $ filter (\u -> elem u multiOutUnits)

    index <- newIORef 0
    shouldContinue <- newIORef (len > 0)

    whileM_ (keepGoing shouldContinue index len) $ do
      links' <- readIORef linksRef
      index' <- readIORef index
      let u = multiOutUnits !! index'
          outWireID = unitOutput u
          outWire = links' IntMap.! outWireID
          -- from this unit, recursively follow all output wires;
          --   if a texture output (LBus) is reached, add its wire ID to the list
          --   if path ends without reaching an LBus then leave list as-is
          --
          -- ensure no duplicate wire IDs are in the resultant list
          pathsToTextureOutputs = nub $ traverseWires outWire unitsMap links' []

      if length pathsToTextureOutputs > 1 then
        let Wire _ src dests = links' IntMap.! outWireID
            newLinks = IntMap.insert outWireID (LBus outWireID src dests) links'
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


traverseWires :: Link -> IntMap Unit -> IntMap Link -> [UnitID] -> [UnitID]
traverseWires (LBus _ src _) _ _ texOuts = src:texOuts
traverseWires (Wire _ _ [])  _ _ texOuts = texOuts
traverseWires (Wire _ _ dests) units links texOuts =
  let wires = map (links IntMap.!) -- Links
            $ map (unitOutput)       -- wire IDs
            $ map (units IntMap.!) -- units
            $ dests
  in  foldr (\w -> traverseWires w units links) texOuts wires


{- Split units into groups based on when the output is to a local bus.
-}
groupUnitsInSubGraphs :: IntMap Unit -> IntMap Link -> [SubGraph]
groupUnitsInSubGraphs units links =
  let unitList = IntMap.toAscList units
      -- i. start at lowest index unit, determine where all paths lead (they should
      --    converge on either an output with no further wires or a specific LBus?)
      -- ii. do that for all units
      unitPathDests = flip map unitList $ \(_, u) ->
                        unitOutput u                           -- :: WireID
                        |> (\wire -> IntMap.lookup wire links) -- :: Link
                        |> findDestination u                   -- :: Unit
                        |> \dest -> (u, dest)                  -- :: (Unit, Unit)
      -- iii. group units which end up at the same place
      unitGroups = unitPathDests
                   |> sortOn (unitID.snd)
                   |> groupBy (\(_, dest1) (_, dest2) -> dest1 == dest2) -- :: [[(Unit, Unit)]]
  in do
    -- convert these groups of units to their SubGraph representations
    unitGroups
    |> map (\g -> SubGraph (map fst g) (subGraphInputs g) (subGraphOutput g))
  where
    (|>) :: a -> (a -> b) -> b
    x |> f = f x

    findDestination :: Unit -> Maybe Link -> Unit
    findDestination u Nothing = u
    findDestination _ (Just (LBus _ src _))   = units IntMap.! src
    findDestination _ (Just (Wire _ src []))  = units IntMap.! src
    findDestination _ (Just (Wire _ _ (x:_))) = findDestination (units IntMap.! x)
                                              $ IntMap.lookup (unitOutput $ units IntMap.! x) links

    subGraphInputs :: [(Unit, Unit)] -> [Link]
    subGraphInputs graph =
      graph
      |> concatMap (unitInputs.fst)
      |> filter (\link -> case IntMap.lookup link links of
                            Just (LBus _ _ _) -> True
                            _ -> False)
      |> map (links IntMap.!)
      |> nub

    subGraphOutput :: [(Unit, Unit)] -> Maybe Link
    subGraphOutput graph =
      graph
      |> head
      |> snd
      |> unitOutput
      |> flip IntMap.lookup links
