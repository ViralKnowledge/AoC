{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Day10 (run) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Either
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import Text.Parsec (parse, try)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

data Token = Bot Int | Output Int | Low | High | Value Int | Irrelevant deriving (Eq, Show)

data State = State { _bots :: [[Int]]
                   , _outputs :: [[Int]]
                   , _lowMap :: Map Int Token
                   , _highMap :: Map Int Token
                   } deriving Show

makeLenses ''State


emptyState (Bot n) (Output m) = State (replicate (n + 1) []) (replicate (m + 1) []) mempty mempty

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\n\t "

bot :: Parser Token
bot = Bot <$> numberToken "bot"

output :: Parser Token
output = Output <$> numberToken "output"

value :: Parser Token
value = Value <$> numberToken "value"

numberToken :: String -> Parser Int
numberToken s = read <$> (lexeme (string s) *> lexeme (many1 digit))

low :: Parser Token
low = stringToken Low "low"

high :: Parser Token
high = stringToken High "high"

irrelevant :: Parser Token
irrelevant = choice $ try . stringToken Irrelevant <$> ["gives", "goes", "and", "to"]

stringToken :: Token -> String -> Parser Token
stringToken t s = void (lexeme (string s)) *> pure t

term :: Parser Token
term = bot <|> output <|> low <|> high <|> value <|> irrelevant

terms :: Parser [Token]
terms = many1 term

parseTokens :: [String] -> [[Token]]
parseTokens = map (filter (/= Irrelevant)) . rights . map (parse terms "")

tokenState :: [Token] -> State -> State
tokenState [Value v, Bot b] s = s & (bots . ix b) %~ (sort . (:) v)
tokenState [Bot x, Low, a, High, b] s = s & (lowMap . at x .~ Just a) & (highMap . at x .~ Just b)
tokenState _ s = s

checkDone :: State -> Maybe (State, Int)
checkDone s =
  case (< 2) . maximum . map length $ view bots s of
    True -> Just (s, 0)
    False -> Nothing

runState :: State -> Either (State, Int) State
runState (checkDone -> Just i) = Left i
runState s = Right $ s & modMap (s ^. lowMap . at i) (s ^? bots . ix i . ix 0) & modMap (s ^. highMap . at i) (s ^? bots . ix i . ix 1) & bots . ix i .~ []
  where
    (Just i) = findIndex ((== 2) . length) $ s ^. bots
    modMap (Just (Bot l)) (Just n) = bots . ix l %~ (sort . (n:))
    modMap (Just (Output l)) (Just n) = outputs . ix l %~ (n:)

getAnswer :: State -> (State, Int)
getAnswer (runState -> Left i) = i
getAnswer (runState -> Right s) = getAnswer s

-- getAnswerSteps :: [State] -> ([State], Int)
-- getAnswerSteps ss = either (\a' -> (ss, a')) (\a' -> getAnswerSteps (a':ss)) (runState $ head ss)

run = do
  tokens' <- tokens
  -- let tokens' = splitOn "|" sampleInput
  print $ getAnswer $ foldr tokenState (emptyState (maximumBy compBot $ filter isBot $ concat tokens') (maximumBy compBot $ filter isOutput $ concat tokens')) tokens'
  where
    tokens = readFile "input10.txt" >>= \is -> return $ (parseTokens . lines) is
    compBot (Bot a) (Bot b) = compare a b
    compBot (Output a) (Output b) = compare a b
    isBot (Bot _) = True
    isBot _ = False
    isOutput (Output _) = True
    isOutput _ = False

sampleInput = "value 5 goes to bot 2|bot 2 gives low to bot 1 and high to bot 0|value 3 goes to bot 1|bot 1 gives low to output 1 and high to bot 0|bot 0 gives low to output 2 and high to output 0|value 2 goes to bot 2"
