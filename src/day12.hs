{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Day12 (run) where

import Control.Applicative hiding (optional)
import Control.Lens
import Control.Monad
import Data.Either
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!), insert, fromList)
import Data.Maybe
import Text.Parsec (parse, try)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Text.Read

data Instruction = Copy String Char | Inc Char | Dec Char | Jump String Int deriving Show
data State = State { _instructions :: [Instruction], _pos :: Int, _registers :: Map Char Int } deriving Show
makeLenses ''State

-- Parsing

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\n\t "

copy :: Parser Instruction
copy = do
  void $ lexeme (string "cpy")
  num <- lexeme (many1 letter <|> many1 digit)
  r <- letter
  return $ Copy num r

jump :: Parser Instruction
jump = do
  void $ lexeme (string "jnz")
  r <- lexeme (many1 alphaNum)
  num <- read <$> lexeme (many1 letter <|> (many1 ((char '-') <|> digit)))
  return $ Jump r num

inc = Inc <$> registerInstruction "inc"
dec = Dec <$> registerInstruction "dec"

registerInstruction :: String -> Parser Char
registerInstruction s = (lexeme (string s) *> letter)

instruction :: Parser Instruction
instruction = copy <|> inc <|> dec <|> jump

-- Running

isEnd :: State -> Maybe State
isEnd s = if (s ^. pos) >= length (s ^. instructions)  then Just s else Nothing

runState :: State -> [State]
runState (isEnd -> Just s) = [s]
runState s = (runState $ runInstruction i s)
  where
    (State is ((!!) is -> i) regs) = s

runInstruction :: Instruction -> State -> State
runInstruction (Copy (readMaybe -> Just i) r) s = s & registers . at r . _Just .~ (i :: Int) & pos +~ 1
runInstruction (Copy (listToMaybe -> Just i) r) s = s & registers . at r . _Just .~ fromJust (s ^. registers . at i) & pos +~ 1
runInstruction (Inc r) s = s & registers . at r . _Just +~ 1 & pos +~ 1
runInstruction (Dec r) s = s & registers . at r . _Just -~ 1 & pos +~ 1
runInstruction (Jump (readMaybe -> Just r) i) s = if (r :: Integer) /= 0 then s & pos +~ i else s & pos +~ 1
runInstruction (Jump (listToMaybe -> Just r) i) s = if fromJust (s ^. registers . at r) /= 0 then s & pos +~ i else s & pos +~ 1


run = do
  print instructions
  print $ map (^. registers) $ runState (State instructions 0 rs)
  where
    rs = fromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)]
    instructions = rights $ (parseInstructions <$> splitOn "|" input)
    parseInstructions = parse instruction ""

sampleInput = "cpy 41 a|inc a|inc a|dec a|jnz a 2|dec a"

input = "cpy 1 a|cpy 1 b|cpy 26 d|jnz c 2|jnz 1 5|cpy 7 c|inc d|dec c|jnz c -2|cpy a c|inc a|dec b|jnz b -2|cpy c b|dec d|jnz d -6|cpy 14 c|cpy 14 d|inc a|dec d|jnz d -2|dec c|jnz c -5|"
