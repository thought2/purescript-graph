module Graph.Edge
  ( Edge
  , EdgeConfig
  , EdgeData
  , EdgeId
  , create
  , getId
  , getLabel
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, wrap, unwrap)
import Graph.Class (class Id)

-- TYPES
newtype Edge id e
  = Edge (EdgeData id e)

type EdgeData id e
  =
    { label :: e
    , id :: EdgeId id
    }

type EdgeConfig id e
  =
    { label :: e
    , predNodeId :: id
    , succNodeId :: id
    }

type EdgeId id
  =
    { predNodeId :: id
    , succNodeId :: id
    }

derive instance newtypeEdge ::
  Newtype (Edge id e) _

derive instance functorEdge ::
  Functor (Edge id)

derive instance genericEdge ::
  Generic (Edge id e) _

instance showEdge ::
  ( Show id
  , Show e
  ) =>
  Show (Edge id e) where
    show = genericShow

derive instance eqEdge ::
  ( Eq id
  , Eq e
  ) =>
  Eq (Edge id e)

-- CREATE
create ::
  forall id e.
  Id id =>
  EdgeConfig id e ->
  Edge id e
create { label, predNodeId, succNodeId } = wrap { label
                                                , id: { predNodeId
                                                      , succNodeId
                                                      }
                                                }

getId ::
  forall id e.
  Id id =>
  Edge id e ->
  EdgeId id
getId = unwrap >>> _.id

getLabel ::
  forall id e.
  Id id =>
  Edge id e ->
  e
getLabel = unwrap >>> _.label
