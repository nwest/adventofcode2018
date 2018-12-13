{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Set as S hiding (Set)
import Data.Set (Set)
import Data.List (sort, sortOn, group, nub, minimumBy)
import Data.Char (ord, toUpper, isUpper, isLower)

cleanInts :: String -> [Int]
cleanInts = map read . lines . filter (/= '+')

numberOne :: IO Int
numberOne = sum . cleanInts <$> readFile "/Users/nwest/AoC/1"

dupeFrequency :: Set Int -> Int -> [Int] -> Int
dupeFrequency _ _ [] = 0
dupeFrequency possible accum (x:xs) = let newAccum = accum + x
                                      in if S.member newAccum possible 
                                        then newAccum
                                        else dupeFrequency (S.insert newAccum possible) newAccum xs

numberOneB :: IO Int
numberOneB = dupeFrequency S.empty 0 . cycle . cleanInts <$> readFile "/Users/nwest/AoC/1"

-----------------------------------------------

frequency :: Ord a => [a] -> [Int]
frequency = map length . group . sort

numberTwo :: IO Int
numberTwo =  product . frequency . filter (/= 1) . concatMap (nub . frequency) . lines <$> readFile "/Users/nwest/AoC/2"

normalize :: [Int] -> [Int]
normalize = map (\x -> if x == 0 then 0 else 1)

hamming :: String -> String -> Int
hamming s1 = let f = map ord in sum . normalize . zipWith (-) (f s1) . f

combinations :: [a] -> [(a, a)]
combinations xs = concat [ zip (repeat b) . drop a $ xs | (a, b) <- zip [1..] xs ]

third :: (a, b, c) -> c
third (_, _, c) = c

numberTwoB :: IO String
numberTwoB = commonLetters . head . sortOn third . map distance . combinations . lines <$> readFile "/Users/nwest/AoC/2"
               where distance set@(l, r) = (l, r, uncurry hamming set)
                     commonLetters (s1, s2, _) = concat $ zipWith (\a b -> if a == b then [a] else "") s1 s2

-----------------------------------------------

type Id = Int
type XCoord = Int
type YCoord = Int
type Width = Int
type Height = Int

data Coordinate = Coordinate XCoord YCoord deriving (Eq, Show, Ord)
data Plan = Plan Id XCoord YCoord Width Height deriving (Eq, Show)

splitOn :: Char -> String -> [String]
splitOn c s | c `notElem` s = [s]
            | otherwise = let f g = g (/= c) s
                          in [f takeWhile, tail . f $ dropWhile]

-- #1319 @ 627,639: 23x11
parsePlan :: String -> Plan
parsePlan s = let [(read . tail) -> num, _, init -> cs, wh] = words s
                  [x, y] = f ',' cs
                  [w, h] = f 'x' wh
              in Plan num x y w h
  where
    f x = map read . splitOn x

coordinateMap :: Plan -> [Coordinate]
coordinateMap (Plan _ x y w h) = [ Coordinate xc yc | xc <- [x..(x + w - 1)], yc <- [y..(y + h - 1)] ]

numberThree :: IO Int
numberThree = length . filter ((/=) 1 . length) . group . sort . concatMap (coordinateMap . parsePlan)  . lines <$> readFile "/Users/nwest/AoC/3"

checkOverlaps :: Ord a => [Set a] -> [Bool]
checkOverlaps set = map (\d -> overlapsWith d (filter (/= d) set)) set
                    where
                      overlapsWith a = any (not . S.disjoint a)

numberThreeB :: IO Plan
numberThreeB = let plans = map parsePlan . lines <$> readFile "/Users/nwest/AoC/3"
                   overlaps = checkOverlaps . map (S.fromList . coordinateMap) <$> plans
               in fst . head . filter (\(_, o) -> not o) <$> (zip <$> plans <*> overlaps)

-----------------------------------------------

-- data Event = Wake | Sleep | Start DateTime

numberFour :: IO [String]
numberFour = lines <$> readFile "/Users/nwest/AoC/4"

-----------------------------------------------

shouldReact :: Char -> Char -> Bool
shouldReact c1 c2 | isUpper c1, isLower c2 = c1 == toUpper c2
                  | isLower c1, isUpper c2 = toUpper c1 == c2 
                  | otherwise =  False

react :: String -> String
react str = let new = foldr f "" str
                f c [] = [c]
                f c s@(a:as) = if shouldReact c a then as else c:s
          in if new == str then str else react new

numberFive :: IO Int
numberFive = length . react . head . lines <$> readFile "/Users/nwest/AoC/5"

removePolymers :: String -> [String]
removePolymers s = map (\c -> filter (\ch -> not (ch == c || ch == toUpper c)) s) ['a'..'z']

numberFiveB :: IO Int
numberFiveB = minimum . map (length . react) . removePolymers . head . lines <$> readFile "/Users/nwest/AoC/5"

-----------------------------------------------
parseCoordinate :: String -> Coordinate
parseCoordinate s = let [read -> x, read -> y] = splitOn ',' s in Coordinate x y

manhattan :: Coordinate -> Coordinate -> Int
manhattan (Coordinate x1 y1) (Coordinate x2 y2) = max x1 x2 - min x1 x2 + max y1 y2 - min y1 y2

boundingPlan :: [Coordinate] -> Plan
boundingPlan cs = let (Coordinate left _)   = minimumBy (\(Coordinate x1 _) (Coordinate x2 _) -> compare x1 x2) cs
                      (Coordinate right _)  = minimumBy (\(Coordinate x1 _) (Coordinate x2 _) -> compare x2 x1) cs
                      (Coordinate _ bottom) = minimumBy (\(Coordinate _ y1) (Coordinate _ y2) -> compare y1 y2) cs
                      (Coordinate _ top)    = minimumBy (\(Coordinate _ y1) (Coordinate _ y2) -> compare y2 y1) cs
                  in Plan 1 left bottom (right - left) (top - bottom)

numberSix :: IO [Coordinate]
numberSix = let sites = map parseCoordinate . lines <$> readFile "/Users/nwest/AoC/6"
                --allCoordinates = coordinateMap . boundingPlan <$> sites
            in sites
