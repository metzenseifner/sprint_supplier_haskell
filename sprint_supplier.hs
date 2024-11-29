#!/usr/bin/env stack
{- stack script --resolver lts-22.16 --optimize
  --package optparse-applicative
  --package time
-}

import Prelude

import Data.Time (CalendarDiffDays, Day, fromGregorian, toGregorian)

-- compare import Data.Time.Clock.System (getSystemTime)

import Data.Time.Calendar (diffDays)
import Data.Time.Clock (utctDay)
import Data.Time.Clock.POSIX (getCurrentTime)

-- calcDifference :: Day -> IO Integer
calcDifference t = fmap abs difference
 where
  difference = fmap (diffDays t) (fmap utctDay getCurrentTime)

-- calcSprint :: Int -> Int -> Int
calcSprint offset diff =
  let
    divisor = diff `div` 14
   in
    divisor + offset

run :: IO ()
run = do
  -- https://hendi.io/haskell-data-time-basic-examples/
  -- TODO make configurable
  let baseDate = fromGregorian 2024 11 29
  let offset = read "307" :: Integer

  now <- getCurrentTime
  let today = utctDay now

  diff <- calcDifference baseDate
  -- print diff
  let sprint = calcSprint offset diff
  print sprint

main :: IO ()
main = run
