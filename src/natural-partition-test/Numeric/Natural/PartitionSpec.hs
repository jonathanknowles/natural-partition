-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Numeric.Natural.PartitionSpec where

import Test.Hspec
    ( Spec, describe, it, shouldBe )

spec :: Spec
spec = do
    describe "PartitionSpec" $ it "Partition" $ True `shouldBe` True
