module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Graph as TestGraph
import Test.Unit (TestSuite)
import Test.Unit.Main (runTest)

all :: TestSuite
all = do
  TestGraph.all

main :: Effect Unit
main = runTest all