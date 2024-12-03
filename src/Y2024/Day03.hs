module Y2024.Day03 (Solution (..)) where

-- https://adventofcode.com/2024/day/3

import Data.Attoparsec.Text (Parser, anyChar, choice, decimal, many', parseOnly, string)
import qualified Data.Text as T
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . runProgram . parse
  part2 _ = show . runProgram' . parse

data Instr
  = Mul Int Int
  | Junk Char
  | Do
  | Dont
  deriving (Eq, Show)

parse :: String -> [Instr]
parse input =
  case parseOnly instrP (T.pack input) of
    Left err -> error $ "Parsing failed: " ++ err
    Right instr -> instr

junkP, mulP, doP, dontP :: Parser Instr
junkP = Junk <$> anyChar
mulP = Mul <$> (string (T.pack "mul(") *> decimal <* string (T.pack ",")) <*> (decimal <* string (T.pack ")"))
doP = Do <$ string (T.pack "do()")
dontP = Dont <$ string (T.pack "don't()")

instrP :: Parser [Instr]
instrP = many' (choice [mulP, doP, dontP, junkP])

runProgram :: [Instr] -> Int
runProgram = foldl step 0
  where
    step acc (Mul a b) = acc + a * b
    step acc _ = acc

runProgram' :: [Instr] -> Int
runProgram' = fst . foldl step (0, True)
  where
    step (acc, True) (Mul a b) = (acc + a * b, True)
    step (acc, _) Dont = (acc, False)
    step (acc, _) Do = (acc, True)
    step (acc, en) _ = (acc, en)
