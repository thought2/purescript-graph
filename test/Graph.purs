module Test.Graph where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Graph (Graph, Node, Edge)
import Graph as Graph
import Graph.Edge as Edge
import Graph.Node as Node
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (equal)


graphA :: Graph Char Unit Unit
graphA =
  Graph.empty
    # Graph.insertNode { id : 'A', label : unit}

graphAB :: Graph Char Unit Unit
graphAB =
  graphA
    # Graph.insertNode { id : 'B', label : unit}

graphABC :: Graph Char Unit Unit
graphABC =
  graphAB
    # Graph.insertNode { id : 'C', label : unit}

all :: TestSuite
all = do
  suite "Graph" do
    suite "empty" do
      test "nodes" do
        equal
          ([] :: Array (Node Int Unit))
          (Graph.getNodes Graph.empty)
      test "edges" do
        equal
          ([] :: Array (Edge Int Unit))
          (Graph.getEdges Graph.empty)

    test "insertNode" do
      equal
        ['A']
        ( graphA
            # Graph.getNodes
            # map Node.getId
        )

    suite "insertEdge" do
      test "ok" do
        let graph = graphAB
              # Graph.insertEdge { predNodeId : 'A', succNodeId : 'B', label : unit}
        equal
          (Right [{predNodeId : 'A', succNodeId: 'B' }])
          ((Graph.getEdges >>> map Edge.getId) <$> graph)

      test "fail" do
        let graph = graphAB
              # Graph.insertEdge { predNodeId : 'A', succNodeId : 'C', label : unit}
        equal
          (Left (Graph.ErrNodeNotFound 'C'))
          ((Graph.getEdges >>> map Edge.getId) <$> graph)

    suite "insertEdges" do
      test "ok" do
        let graph = graphABC
              # Graph.insertEdges
                  [ { predNodeId : 'A', succNodeId : 'B', label : unit}
                  , { predNodeId : 'B', succNodeId : 'C', label : unit}
                  ]
        equal
          (Right $ Array.sort
             [ { predNodeId : 'A', succNodeId: 'B' }
             , { predNodeId : 'B', succNodeId: 'C' }
             ]
          )
          ((Graph.getEdges >>> map Edge.getId >>> Array.sort) <$> graph)
