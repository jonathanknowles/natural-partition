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
    ( partition, partitionIdeal, partitionMin, partitionMax )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Large (..)
    , Positive (..)
    , Property
    , Small (..)
    , checkCoverage
    , cover
    , listOf
    , property
    , shrinkList
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "partition" $ do
        it "prop_partition_monotonic" $
            prop_partition_monotonic
                & property & withMaxSuccess 1000
        it "prop_partition_sum" $
            prop_partition_sum
                & property & withMaxSuccess 1000
        it "prop_partition_partitionMin" $
            prop_partition_partitionMin
                & property & withMaxSuccess 10000

    describe "partitionIdeal" $ do
        it "prop_partitionIdeal_isJust" $
            prop_partitionIdeal_isJust & property
        it "prop_partitionIdeal_length" $
            prop_partitionIdeal_length & property
        it "prop_partitionIdeal_sum" $
            prop_partitionIdeal_sum & property

prop_partition_monotonic
    :: (Large Int)
    -> NonEmpty Int
    -> Property
prop_partition_monotonic (Large i) is =
    property $
    maybe True and (NE.zipWith (<=) <$> partition n ns <*> partition (n + 1) ns)
  where
    toNatural = fromIntegral @Int @Natural . abs
    n :: Natural
    n = toNatural i
    ns :: NonEmpty Natural
    ns = toNatural <$> is

prop_partition_sum
    :: (Large Int)
    -> NonEmpty Int
    -> Property
prop_partition_sum (Large i) is =
    property $
    maybe (n % 1) sum (partitionIdeal n ns) === n % 1
  where
    toNatural = fromIntegral @Int @Natural . abs
    n :: Natural
    n = toNatural i
    ns :: NonEmpty Natural
    ns = toNatural <$> is

prop_partition_partitionMin
    :: (Large Int)
    -> NonEmpty Int
    -> Property
prop_partition_partitionMin (Large i) is =
    property $
    maybe True and (NE.zipWith (>=) <$> partition n ns <*> partitionMin n ns)
  where
    toNatural = fromIntegral @Int @Natural . abs
    n :: Natural
    n = toNatural i
    ns :: NonEmpty Natural
    ns = toNatural <$> is

prop_partition_partitionMax
    :: (Large Int)
    -> NonEmpty Int
    -> Property
prop_partition_partitionMax (Large i) is =
    property $
    maybe True and (NE.zipWith (<=) <$> partition n ns <*> partitionMax n ns)
  where
    toNatural = fromIntegral @Int @Natural . abs
    n :: Natural
    n = toNatural i
    ns :: NonEmpty Natural
    ns = toNatural <$> is

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
