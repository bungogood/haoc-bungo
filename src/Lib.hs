module Lib
  ( Solvable,
    part1,
    part2,
  )
where

class Solvable a where
  part1 :: a -> String -> String
  part2 :: a -> String -> String
