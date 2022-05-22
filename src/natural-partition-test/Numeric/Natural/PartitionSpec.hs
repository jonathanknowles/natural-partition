-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Numeric.Natural.PartitionSpec where

import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( isJust, mapMaybe )
import Data.Ratio
    ( (%) )
import Numeric.Natural
    ( Natural )
import Numeric.Natural.Partition.Internal
    ( partitionIdeal )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , cover
    , listOf
    , property
    , shrinkList
    , (===)
    )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "partitionIdeal" $ do
        it "prop_partitionIdeal_isJust" $
            prop_partitionIdeal_isJust & property
        it "prop_partitionIdeal_length" $
            prop_partitionIdeal_length & property
        it "prop_partitionIdeal_sum" $
            prop_partitionIdeal_sum & property

prop_partitionIdeal_isJust :: Natural -> NonEmpty Natural -> Property
prop_partitionIdeal_isJust n ns =
    checkCoverage $
    cover 10
        (sum ns >= 1 && n >= 1 && length ns > 1)
        "sum ns >= 1 && n >= 1 && length ns > 1" $
    cover 1
        (sum ns == 0)
        "sum ns == 0" $
    isJust (partitionIdeal n ns) === (sum ns /= 0)

prop_partitionIdeal_length :: Natural -> NonEmpty Natural -> Property
prop_partitionIdeal_length n ns =
    checkCoverage $
    cover 10
        (sum ns >= 1 && n >= 1 && length ns > 1)
        "sum ns >= 1 && n >= 1 && length ns > 1" $
    cover 1
        (sum ns == 0)
        "sum ns == 0" $
    maybe (length ns) length (partitionIdeal n ns) === length ns

prop_partitionIdeal_sum :: Natural -> NonEmpty Natural -> Property
prop_partitionIdeal_sum n ns =
    checkCoverage $
    cover 10
        (sum ns >= 1 && n >= 1 && length ns > 1)
        "sum ns >= 1 && n >= 1 && length ns > 1" $
    cover 1
        (sum ns == 0)
        "sum ns == 0" $
    maybe (n % 1) sum (partitionIdeal n ns) === n % 1

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = genNonEmpty arbitrary
    shrink = shrinkNonEmpty shrink

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty genA = (:|) <$> genA <*> listOf genA

shrinkNonEmpty :: (a -> [a]) -> (NonEmpty a -> [NonEmpty a])
shrinkNonEmpty shrinkA = mapMaybe NE.nonEmpty . shrinkList shrinkA . NE.toList
