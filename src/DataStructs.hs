module DataStructs
  ( -- SetMap operations
    SetMap,
    emptySetMap,
    addToSetMap,
    addAllToSetMap,
    lookupSetMap,
    -- Queue operations
    Queue (..),
    emptyQueue,
    enqueue,
    dequeue,
    lastElement,
    queueToList,
    queueIntersection,
    hasQueueIntersection,
    -- MapOfQueues operations
    MapOfQueues (..),
    emptyMapOfQueues,
    lookupQueue,
    insertIntoQueue,
    customUpdate,
    -- RoleLabelMap operations
    RoleLabelMap,
    addLabelToRole,
    addLabelToRoles,
    lookupRoleLabel,
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set
import Core


type SetMap t = Map.Map t (Set t)

emptySetMap :: SetMap t
emptySetMap = Map.empty

addToSetMap :: Ord t => t -> t -> SetMap t -> SetMap t
addToSetMap key value =
  Map.alter (Just . maybe (singleton value) (insert value)) key

addAllToSetMap :: Ord t => t -> Set t -> SetMap t -> SetMap t
addAllToSetMap key values =
  Map.alter (Just . maybe values (union values)) key

lookupSetMap :: Ord t => t -> SetMap t -> Set t
lookupSetMap = Map.findWithDefault empty

-- above works

newtype Queue a = Queue [a] deriving (Show, Eq)

newtype MapOfQueues k a = MapOfQueues (Map.Map k (Queue a)) deriving (Show, Eq)

emptyQueue :: Queue a
emptyQueue = Queue []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs ++ [x])

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue []) = (Nothing, emptyQueue)
dequeue (Queue (x : xs)) = (Just x, Queue xs)

lastElement :: Queue a -> Maybe a
lastElement (Queue []) = Nothing
lastElement (Queue xs) = Just (last xs)

emptyMapOfQueues :: MapOfQueues k a
emptyMapOfQueues = MapOfQueues Map.empty

lookupQueue :: Ord k => k -> MapOfQueues k a -> Queue a
lookupQueue key (MapOfQueues m) = fromMaybe emptyQueue (Map.lookup key m)

insertIntoQueue :: Ord k => k -> a -> MapOfQueues k a -> MapOfQueues k a
insertIntoQueue key val (MapOfQueues m) =
  let queue = lookupQueue key (MapOfQueues m)
   in MapOfQueues (Map.insert key (enqueue val queue) m)

customUpdate :: (Ord k) => k -> k -> Set k -> MapOfQueues k k -> MapOfQueues k k
customUpdate p q crs aw
  | p `member` crs = insertIntoQueue p p (insertIntoQueue q p aw)
  | not (p `member` crs) && lookupQueue p aw /= emptyQueue =
      case lastElement (lookupQueue p aw) of
        Just val -> insertIntoQueue q val aw
        Nothing -> aw
  | otherwise = aw

queueToList :: Queue a -> [a]
queueToList (Queue xs) = xs

queueIntersection :: Eq a => Queue a -> Queue a -> Queue a
queueIntersection (Queue xs) (Queue ys) = Queue (Prelude.filter (`elem` ys) xs)

hasQueueIntersection :: Eq a => Queue a -> Queue a -> Bool
hasQueueIntersection q1 q2 = not . Prelude.null $ queueToList (queueIntersection q1 q2)

type RoleLabelMap = Map.Map Role [Label]
addLabelToRole :: Label -> Role -> RoleLabelMap -> RoleLabelMap
addLabelToRole l role rlm = Map.insert role (l : ls) rlm
  where ls = Map.findWithDefault [] role rlm

addLabelToRoles :: Label -> [Role] -> RoleLabelMap -> RoleLabelMap
addLabelToRoles l roles rlm = Prelude.foldr (addLabelToRole l) rlm roles

lookupRoleLabel :: Role -> RoleLabelMap -> [Label]
lookupRoleLabel role rlm = Map.findWithDefault [] role rlm
