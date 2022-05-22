-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Numeric.Natural.Partition.Internal where

import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..), zipWith )
import Numeric.Natural
    ( Natural )
import Prelude hiding
    ( zipWith )

partition
    :: Natural
    -> NonEmpty Natural
    -> (Natural, NonEmpty Natural)
partition =
    error "partition not implemented"

partitionCeiling
    :: Natural
    -> NonEmpty Natural
    -> (Natural, NonEmpty Natural)
partitionCeiling n ns =
    bimap ceiling (fmap ceiling) (partitionIdeal n ns)

partitionFloor
    :: Natural
    -> NonEmpty Natural
    -> (Natural, NonEmpty Natural)
partitionFloor n ns =
    bimap floor (fmap floor) (partitionIdeal n ns)

partitionIdeal
    :: Natural
    -> NonEmpty Natural
    -> (Rational, NonEmpty Rational)
partitionIdeal =
    error "partitionIdeal not implemented"

partitionPreservesLength
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionPreservesLength n ns =
    length (snd (partition n ns)) == length ns

partitionPreservesSum
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionPreservesSum n ns =
    (partition n ns & \(r, rs) -> r + sum rs) == sum ns

partitionBoundedByCeiling
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionBoundedByCeiling n ns =
    and (zipWith (<=) (snd (partition n ns)) (snd (partitionCeiling n ns)))

partitionBoundedByFloor
    :: Natural
    -> NonEmpty Natural
    -> Bool
partitionBoundedByFloor n ns =
    and (zipWith (>=) (snd (partition n ns)) (snd (partitionFloor n ns)))
