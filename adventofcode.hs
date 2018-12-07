{-# OPTIONS_GHC -Wall -Werror #-}
import qualified Data.Set as S hiding (Set)
import Data.Set (Set)
import Data.List (sort, sortOn, group, nub)
import Data.Char (ord)
--import Text.Regex.Posix

cleanInts :: String -> [Int]
cleanInts = map read . lines . filter (/= '+')

numberOne :: IO Int
numberOne = sum . cleanInts <$> readFile "/Users/nwest/1"

dupeFrequency :: Set Int -> Int -> [Int] -> Int
dupeFrequency _ _ [] = 0
dupeFrequency possible accum (x:xs) = let newAccum = accum + x
                                      in if S.member newAccum possible 
                                        then newAccum
                                        else dupeFrequency (S.insert newAccum possible) newAccum xs

numberOneB :: IO Int
numberOneB = dupeFrequency S.empty 0 . cycle . cleanInts <$> readFile "/Users/nwest/1"

-----------------------------------------------

frequency :: Ord a => [a] -> [Int]
frequency = map length . group . sort

numberTwo :: IO Int
numberTwo =  product . frequency . filter (/= 1) . concatMap (nub . frequency) . lines <$> readFile "/Users/nwest/2"

normalize :: [Int] -> [Int]
normalize = map (\x -> if x == 0 then 0 else 1)

hamming :: String -> String -> Int
hamming s1 = let f = map ord in sum . normalize . zipWith (-) (f s1) . f

combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x:xs) = pairAll x xs ++ combinations xs
                        where pairAll _ [] = []
                              pairAll a (y:ys) = (a, y) : pairAll a ys

combinations' :: [a] -> [(a, a)]
combinations' xs = concat [ zip (repeat b) . drop a $ xs | (a, b) <- zip [1..] xs ]

third :: (a, b, c) -> c
third (_, _, c) = c

numberTwoB :: IO String
numberTwoB = commonLetters . head . sortOn third . map distance . combinations' . lines <$> readFile "/Users/nwest/2"
               where distance set@(l, r) = (l, r, uncurry hamming set)
                     commonLetters (s1, s2, _) = concat $ zipWith (\a b -> if a == b then [a] else "") s1 s2

-----------------------------------------------

data Plan = Plan { xcoord   :: Int
                 , ycoord   :: Int
                 , width    :: Int
                 , height   :: Int } deriving (Eq, Show)

coordinates :: Plan -> [(Int, Int)]
coordinates plan = let top = ycoord plan + height plan
                       right = xcoord plan + width plan
                       x = xcoord plan
                       y = ycoord plan
                      in [(x, y), (x, top), (right, y), (right, top)]

hitTest :: Plan -> (Int, Int) -> Bool
hitTest plan coords = let testLeft    = fst coords > xcoord plan
                          testRight   = fst coords < xcoord plan + width plan
                          testTop     = snd coords < ycoord plan + height plan
                          testBottom  = snd coords > ycoord plan
                        in and [testLeft, testRight, testTop, testBottom]

overlaps :: Plan -> Plan -> Bool
overlaps p1 p2 = let coords = coordinates p2 in or $ map (hitTest p1) coords

parsePlans :: [String] -> [Plan]
parsePlans _ = Plan 0 0 10 10 : Plan 2 4 10 12 : []

numberThree :: IO [Plan]
numberThree = parsePlans . lines <$> readFile "/Users/nwest/3"

-----------------------------------------------
