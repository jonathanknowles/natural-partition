-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Numeric.Natural.Partition.Internal where

import Data.Bifunctor
    ( bimap )
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

partitionCeiling
    :: Natural
    -> NonEmpty Natural
    -> Maybe (NonEmpty Natural)
partitionCeiling n ns =
    fmap ceiling <$> partitionIdeal n ns

partitionFloor
    :: Natural
    -> NonEmpty Natural
    -> Maybe (NonEmpty Natural)
partitionFloor n ns =
    fmap floor <$> partitionIdeal n ns

partitionIdeal
    :: Natural
    -> NonEmpty Natural
    -> Maybe (NonEmpty Rational)
partitionIdeal =
    error "partitionIdeal not implemented"

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

partitionBoundedByCeiling
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionBoundedByCeiling n ns =
    maybe True and (zipWith (<=) <$> partition n ns <*> partitionCeiling n ns)

partitionBoundedByFloor
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionBoundedByFloor n ns =
    maybe True and (zipWith (>=) <$> partition n ns <*> partitionFloor n ns)
