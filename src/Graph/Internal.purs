module Graph.Internal where

import Prelude

import Control.Monad.State (execState, modify)
import Data.Either (Either(..), note)
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap, over)
import Data.Show (class Show)
import Data.Traversable (traverse)
import Graph.Class (class Id)
import Graph.Edge (Edge, EdgeConfig, EdgeId)
import Graph.Edge as Edge
import Graph.Node (Node, NodeConfig)
import Graph.Node as Node
import Partial.Unsafe (unsafePartial)


-- Graph properties: Directed, cyclic, simple (no loop, no multiple edges)  
--
-- TYPES
newtype Graph id n e
  = Graph
      { nodes :: HashMap id (Node id n)
      , edges :: HashMap (EdgeId id) (Edge id e)
      }

derive instance newtypeGraph ::
  Newtype (Graph id n e) _

data Error id n e
  = ErrLoop id
  | ErrNodeNotFound id
  | ErrCycle (Array (Node id n))

derive instance eqError :: (Eq id, Eq n, Eq e) => Eq (Error id n e)

derive instance genericError :: Generic (Error id n e) _

instance showError :: (Show id, Show n, Show e) => Show (Error id n e) where
  show = genericShow

derive instance genericGraph ::
  Generic (Graph id n e) _

instance showGraph ::
  ( Show id
  , Show n
  , Show e
  ) =>
  Show (Graph id n e) where
    show = genericShow

-- CREATE
empty ::
  forall id n e.
  Graph id n e
empty = Graph { nodes: HashMap.empty
              , edges: HashMap.empty
              }

-- MODIFY
-- | Insert node, update label if already exsits
insertNode ::
  forall id n e.
  Id id =>
  NodeConfig id n ->
  Graph id n e ->
  Graph id n e
insertNode nodeConfig = over wrap \graph ->
  graph { nodes = HashMap.upsert updateNode nodeConfig.id newNode graph.nodes
        }
  where
  newNode = Node.create nodeConfig
  updateNode node = map (const nodeConfig.label) node

insertEdge ::
  forall id n e.
  Id id =>
  EdgeConfig id e ->
  Graph id n e ->
  Either (Error id n e) (Graph id n e)
insertEdge { predNodeId, succNodeId } _
  | predNodeId == succNodeId = Left (ErrLoop predNodeId)

insertEdge edgeConfig@{ predNodeId, succNodeId } graph = ado
  _ <- getNode predNodeId graph # note (ErrNodeNotFound predNodeId)
  _ <- getNode succNodeId graph # note (ErrNodeNotFound succNodeId)
  in graph # unsafeInsertEdge edgeConfig # unsafeRegisterEdge edgeConfig

insertEdges ::
  forall id n e.
  Id id =>
  Array (EdgeConfig id e) ->
  Graph id n e ->
  Either (Error id n e) (Graph id n e)
insertEdges edgeConfigs graph =
  foldM (\st x -> insertEdge x st) graph edgeConfigs

unsafeInsertEdge ::
  forall id n e.
  Id id =>
  EdgeConfig id e ->
  Graph id n e ->
  Graph id n e
unsafeInsertEdge edgeConfig@{ predNodeId, succNodeId } = over wrap \graph ->
  graph { edges = HashMap.upsert updateEdge id edge graph.edges
        }
  where
  id = { predNodeId
       , succNodeId
       }
  edge = Edge.create edgeConfig
  updateEdge edge' = map (const edgeConfig.label) edge'

unsafeRegisterEdge ::
  forall id n e.
  Id id =>
  EdgeConfig id e ->
  Graph id n e ->
  Graph id n e
unsafeRegisterEdge { predNodeId, succNodeId } = over wrap \graph ->
  graph { nodes = graph.nodes # updatePredNode # updateSuccNode
        }
  where
  id = { predNodeId
       , succNodeId
       }
  updatePredNode nodes = HashMap.update (Node.insertPredNodeId predNodeId >>> Just) predNodeId nodes
  updateSuccNode nodes = HashMap.update (Node.insertSuccNodeId succNodeId >>> Just) succNodeId nodes

-- QUERY
getNode ::
  forall id n e.
  Id id =>
  id ->
  Graph id n e ->
  Maybe (Node id n)
getNode id graph = HashMap.lookup id (unwrap graph).nodes

getEdge ::
  forall id n e.
  Id id =>
  EdgeId id ->
  Graph id n e ->
  Maybe (Edge id e)
getEdge id graph = HashMap.lookup id (unwrap graph).edges

getNodes ::
  forall id n e.
  Id id =>
  Graph id n e ->
  Array (Node id n)
getNodes = unwrap >>> _.nodes >>> HashMap.values

getEdges ::
  forall id n e.
  Id id =>
  Graph id n e ->
  Array (Edge id e)
getEdges = unwrap >>> _.edges >>> HashMap.values

unsafeGetNode ::
  forall id n e.
  Id id =>
  id ->
  Graph id n e ->
  Node id n
unsafeGetNode id graph = getNode id graph # (unsafePartial fromJust)
