module Aoc.Year2023.Day08 where

import           Data.List         (foldl1')
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Proxy
import           Data.Text         (Text)
import qualified Data.Text         as T
import           GHC.TypeLits

import           Control.Monad.Aoc

data Instruction = R | L deriving (Show, Eq)

type Instructions = [Instruction]

type Paths = Map Text (Text, Text)

parseInstructions :: Text -> (Instructions, Paths)
parseInstructions text =
  let [instructions, paths] = T.splitOn "\n\n" text in
  let instructions' = map parseInstruction $ T.unpack instructions in
  let paths' = Map.fromList $ map parsePath $ T.lines paths in
  (instructions', paths')
  where
    parseInstruction 'R' = R
    parseInstruction 'L' = L
    parseInstruction _   = error "Invalid instruction"

    parsePath line =
      let [from, to] = T.splitOn " = " line in
      let [left, right] = T.splitOn ", " to in
      (from, (T.tail left, T.init right))

findZZZ :: forall n. (KnownNat n) => Proxy n -> Instructions -> Paths -> Text -> Int
findZZZ _ instructions paths start = go (cycle instructions) paths start 0
  where
    go (instr:instructions) !paths !start !n =
      let partOne = natVal (Proxy @n) == 1 in
      if (partOne && start == "ZZZ") || T.isSuffixOf "Z" start then n
      else
        let (left, right) = paths Map.! start in
        case instr of
          R -> go instructions paths right (n + 1)
          L -> go instructions paths left (n + 1)
    go  _ _ _ _ = undefined

instance MonadAoc 8 2023 where
  type Result _ _ = Int

  partOne _ _ = do
    input <- getInput
    let (instructions, paths) = parseInstructions input
    return $ findZZZ (Proxy @1) instructions paths "AAA"

  partTwo _ _ = do
    input <- getInput
    let (instructions, paths) = parseInstructions input
    let nodes = filter (T.isSuffixOf "A") $ Map.keys paths
    let someZZZ = map (findZZZ (Proxy @2) instructions paths) nodes
    return $ foldl1' lcm someZZZ
