module Test.Graph where

import Prelude

import Graph (Graph, Node, Edge)
import Graph as Graph
import Graph.Node as Node
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (equal)

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
    suite "nodes" do
      test "insert" do
        equal
          ['A']
          ( Graph.empty
              # Graph.insertNode { id : 'A', label : 'A'}
              # Graph.getNodes
              # map Node.getId
          )