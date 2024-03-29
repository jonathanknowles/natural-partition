-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Numeric.Natural.Partition.Internal where

import Data.Bifunctor
    ( bimap )
import Data.Functor
    ( (<&>) )
import Data.List.NonEmpty
    ( NonEmpty (..), zipWith )
import Data.Ratio
    ( Ratio, (%) )
import Numeric.Natural
    ( Natural )
import Prelude hiding
    ( zipWith )

partition
    :: Natural
    -> NonEmpty Natural
    -> Maybe (NonEmpty Natural)
partition =
    error "partition not implemented"

partitionIdeal
    :: Natural
    -> NonEmpty Natural
    -> Maybe (NonEmpty (Ratio Natural))
partitionIdeal n weights
    | weightSum == 0 = Nothing
    | otherwise = Just $ (* scaleFactor) . (% 1) <$> weights
  where
    scaleFactor = n % weightSum
    weightSum = sum weights

partitionMax
    :: Natural
    -> NonEmpty Natural
    -> Maybe (NonEmpty Natural)
partitionMax n ns =
    partitionIdeal n ns <&> fmap ceiling

partitionMin
    :: Natural
    -> NonEmpty Natural
    -> Maybe (NonEmpty Natural)
partitionMin n ns =
    partitionIdeal n ns <&> fmap floor

partitionMaxBounded
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionMaxBounded n ns =
    maybe True and (zipWith (<=) <$> partition n ns <*> partitionMax n ns)

partitionMinBounded
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionMinBounded n ns =
    maybe True and (zipWith (>=) <$> partition n ns <*> partitionMin n ns)
