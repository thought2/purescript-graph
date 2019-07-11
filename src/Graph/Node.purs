module Graph.Node
  ( Node
  , NodeConfig
  , create
  , getId
  , getLabel
  , getPredEdgeIds
  , getPredNodeIds
  , getSuccEdgeIds
  , getSuccNodeIds
  , insertPredNodeId
  , insertSuccNodeId
  ) where

import Prelude
import Data.Set as Set
import Data.Set (Set)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Graph.Class (class Id)
import Graph.Edge (EdgeId)



-- TYPES
newtype Node id n
  = Node
      { id :: id
      , label :: n
      , predNodeIds :: Set id
      , succNodeIds :: Set id
      }

type NodeConfig id n
  =
    { id :: id
    , label :: n
    }

derive instance newtypeNode ::
  Newtype (Node id n) _

derive instance functorNode ::
  Functor (Node id)

derive instance genericNode ::
  Generic (Node id n) _

instance showNode ::
  ( Show id
  , Show n
  ) =>
  Show (Node id n) where
    show = genericShow

-- CREATE
create ::
  forall id n.
  Id id =>
  NodeConfig id n ->
  Node id n
create { id, label } = wrap { id
                            , label
                            , predNodeIds: Set.empty
                            , succNodeIds: Set.empty
                            }

-- UPDATE
insertPredNodeId ::
  forall id n.
  Id id =>
  id ->
  Node id n ->
  Node id n
insertPredNodeId id = over wrap \node ->
  node { predNodeIds = Set.insert id node.predNodeIds
       }

insertSuccNodeId ::
  forall id n.
  Id id =>
  id ->
  Node id n ->
  Node id n
insertSuccNodeId id = over wrap \node ->
  node { succNodeIds = Set.insert id node.succNodeIds
       }

-- QUERY
getPredEdgeIds ::
  forall id n.
  Id id =>
  Node id n ->
  Array (EdgeId id)
getPredEdgeIds node = map (\predNodeId ->
  { predNodeId
  , succNodeId: (unwrap node).id
  }) (getPredNodeIds node)

getPredNodeIds ::
  forall id n.
  Id id =>
  Node id n ->
  Array id
getPredNodeIds = unwrap >>> \node ->
  Set.toUnfoldable node.predNodeIds

getSuccEdgeIds ::
  forall id n.
  Id id =>
  Node id n ->
  Array (EdgeId id)
getSuccEdgeIds = unwrap >>> \node ->
  map (\succNodeId ->
    { predNodeId: node.id
    , succNodeId
    }) (Set.toUnfoldable node.succNodeIds)

getSuccNodeIds ::
  forall id n.
  Id id =>
  Node id n ->
  Array id
getSuccNodeIds = unwrap >>> \node ->
  Set.toUnfoldable node.succNodeIds

getId ::
  forall id n.
  Id id =>
  Node id n ->
  id
getId = unwrap >>> _.id

getLabel ::
  forall id n.
  Id id =>
  Node id n ->
  n
getLabel = unwrap >>> _.label
