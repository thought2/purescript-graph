module Graph.Edge
  ( Edge
  , EdgeConfig
  , EdgeData
  , EdgeId
  , create
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, wrap)
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
