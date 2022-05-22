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
    -> Maybe (NonEmpty Rational)
partitionIdeal =
    error "partitionIdeal not implemented"

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

partitionPreservesLength
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionPreservesLength n ns =
    maybe (length ns) length (partition n ns) == length ns

partitionPreservesSum
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionPreservesSum n ns =
    maybe n sum (partition n ns) == sum ns

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
