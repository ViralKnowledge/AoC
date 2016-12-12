module Day7  (run) where

import Data.List
import Data.List.Split
import Data.Tuple.Select

checkPhrase :: String -> Bool
checkPhrase s = any id . map (checkStr . take 4) $ tails s
  where
    result b = (s, b)
    checkStr [a, b, c, d] = (a /= b) && ([a, b] == [d, c])
    checkStr _ = False

getAbas :: String -> [String]
getAbas = filter checkStr . map (take 3) . tails
  where
    checkStr [a, b, c] = (a /= b) && (a == c)
    checkStr _ = False

parseIpAbas :: (String, Bool) -> (String, [String], Bool)
parseIpAbas (s, b) = (s, getAbas s, b)

checkIp :: [(String, [String], Bool)] -> (([String], [String]), Bool)
checkIp ip = (foldr (\a (b, c) -> if sel3 a then (sel2 a ++ b, c) else (b, sel2 a ++ c)) ([], []) ip, checkSSL)
  where
    checkTLS = (\ss' -> elem (True, True) ss' && notElem (True, False) ss') $ map (\(s, _, b) -> (checkPhrase s, b)) ip
    checkSSL = (\(a, b) -> any id $ map (flip elem b . revAba) a) $ foldr (\a (b, c) -> if sel3 a then (sel2 a ++ b, c) else (b, sel2 a ++ c)) ([], []) ip
    revAba [a, b, c] = [b, a, b]
    revAba _ = ['-']


parseIp :: String -> [(String, [String], Bool)]
parseIp = map parseIpAbas . foldl (flip parseChar) [("", True)]

parseChar :: Char -> [(String, Bool)] -> [(String, Bool)]
parseChar '[' as = ("", False):as
parseChar ']' as = ("", True):as
parseChar x ((s, b):as) = (x:s, b):as

run = input >>= print . length . filter sel2 . map (checkIp . parseIp)

inputSample = splitOn "|" "abba[mnop]qrst|abcd[bddb]xyyx|aaaa[qwer]tyui|ioxxoj[asdfgh]zxcvbn"

inputSample2 = splitOn "|" "aba[bab]xyz|xyx[xyx]xyx|aaa[kek]eke|zazbz[bzb]cdb"

input = do
  content <- readFile "input7.txt"
  return $ lines content
