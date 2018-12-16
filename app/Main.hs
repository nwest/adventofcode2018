{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE TupleSections, ViewPatterns #-}

module Main where

import qualified Data.Set as S hiding (Set)
import Data.Set (Set)
import Data.List (sort, sortOn, group, nub, minimumBy)
import Data.List.Extra (groupOn)
import Data.Char (ord, toUpper, isUpper, isLower)
import Data.Maybe (isJust)
import Data.Time

main :: IO ()
main = numberSix

-----------------------------------------------

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
coordinateMap (Plan _ x y w h) = [ Coordinate xc yc | xc <- [x..(x + w)], yc <- [y..(y + h)] ]

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

data Event = Wake | Sleep | Work Int deriving (Show, Eq)
type Shift = [(LocalTime, Event)]

-- "Guard #2003 begins shift"
eventFromString :: String -> Event
eventFromString s | s == "wakes up" = Wake
                  | s == "falls asleep" = Sleep
                  | otherwise = Work (read . tail . (!! 1) . words $ s)

-- year-month-day hour:minute
parseEvent :: String -> (LocalTime, Event)
parseEvent s = let dateString = init . tail . unwords . take 2 . words $ s
                   event = eventFromString (unwords . drop 2 . words $ s)
                   localTime = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d %R" dateString :: LocalTime
               in (localTime, event)

shifts :: [(LocalTime, Event)] -> [Shift]
shifts = map reverse . reverse . foldl f []
  where
    f [] a = [[a]]
    f sh@(b:bs) ev@(_, e) | e == Wake || e == Sleep = (ev:b):bs
                          | otherwise = [ev] : sh

byTwo :: [a] -> [[a]]
byTwo = foldr f []
  where
    f x [] = [[x]]
    f x g@(a:as) = if length a == 1 then (x:a):as else [x] : g

sleepTime :: Shift -> (Int, Int)
sleepTime xs = let Work a = snd . head $ xs
                   pairs = byTwo . map (todMin . localTimeOfDay . fst) . tail $ xs
                   time = sum . map (\[t1, t2]-> t2 - t1) $ pairs
               in (a, time)

sleepiestGuard :: [Shift] -> Int
sleepiestGuard sh = let times = groupOn fst . sortOn fst . map sleepTime $ sh
                    in fst . last . sortOn snd . map f $ times
  where
    f [] = (0, 0)
    f a@(x:_) = (fst x, sum . map snd $ a)

isGuardsShift :: Int -> Shift -> Bool
isGuardsShift num s = let Work a = snd . head $ s
                      in a == num

shiftsForGuard :: Int -> [Shift] -> [Shift]
shiftsForGuard num = filter (isGuardsShift num)

sleepyMins :: Shift -> [Int]
sleepyMins s = let [start, end] = map (todMin . localTimeOfDay . fst) s
               in [start..(start + (end - start) - 1)]

sleepyShifts :: [Shift] -> [Shift]
sleepyShifts = byTwo . concatMap (drop 1)

sleepiestMinute :: [Shift] -> Int
sleepiestMinute = head . last . sortOn length . group . sort . concatMap sleepyMins

sleepiestMinuteFrequency :: [Shift] -> Int
sleepiestMinuteFrequency = length . last . sortOn length . group . sort . concatMap sleepyMins

numberFour :: IO ()
numberFour = do
  allShifts <- shifts . sortOn fst . map parseEvent . lines <$> readFile "/Users/nwest/AoC/4"
  let sleepiest = sleepiestGuard allShifts
      sleepyMinute = sleepiestMinute . sleepyShifts . shiftsForGuard sleepiest $ allShifts
  print (sleepiest * sleepyMinute)

guardForShift :: Shift -> Int
guardForShift sh = let Work a = snd . head $ sh in a

guards :: [Shift] -> [Int]
guards = nub . sort . map guardForShift

numberFourB :: IO ()
numberFourB = do
  allShifts <- shifts . sortOn fst . map parseEvent . lines <$> readFile "/Users/nwest/AoC/4"
  let allGuards = guards allShifts
      sleepShifts = map (sleepyShifts . flip shiftsForGuard allShifts) allGuards
      together = zip allGuards sleepShifts
      withoutSleepless = filter (\(_, xs)-> not (null xs)) together
      allTheThings =  map (\(num, xs) -> (num, sleepiestMinute xs, sleepiestMinuteFrequency xs)) withoutSleepless
      winningCombo = last . sortOn third $ allTheThings
  print . f $ winningCombo
  where
    f (a, b, _) = a * b

-----------------------------------------------

numberFive :: IO Int
numberFive = length . reaction . head . lines <$> readFile "/Users/nwest/AoC/5"

reaction :: String -> String
reaction input = let reacted = react input
                 in if reacted == input
                      then reacted
                      else react reacted

react :: String -> String
react input = foldr reactPolymers "" input
  where
    reactPolymers char "" = [char]
    reactPolymers char string@(firstChar:rest) = if shouldReact char firstChar
                                                   then rest
                                                   else char:string

shouldReact :: Char -> Char -> Bool
shouldReact c1 c2 | isUpper c1, isLower c2 = c1 == toUpper c2
                  | isLower c1, isUpper c2 = toUpper c1 == c2
                  | otherwise = False

removePolymers :: String -> [String]
removePolymers s = map (removePolymer s) ['a'..'z']
                     where removePolymer st c = filter (\ch -> not (ch == c || ch == toUpper c)) st

numberFiveB :: IO Int
numberFiveB = minimum . map (length . reaction) . removePolymers . head . lines <$> readFile "/Users/nwest/AoC/5"

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

closestCoord :: Coordinate -> [Coordinate] -> (Coordinate, Maybe Coordinate)
closestCoord c = (c, ) . g . sortOn snd . map f
  where
    f x = (x, manhattan c x)
    g []                            = Nothing
    g [(a, _)]                      = Just a
    g ((a, m):(_, n):_) | m == n    = Nothing
                        | otherwise = Just a

isOnEdge :: Plan -> Coordinate -> Bool
isOnEdge (Plan _ x y w h) (Coordinate x' y') = or [ x' == x
                                                  , y' == y
                                                  , x' == x + w
                                                  , y' == y + h ]

boundedCoords :: Plan -> [(Coordinate, Maybe Coordinate)] -> [(Coordinate, Maybe Coordinate)]
boundedCoords p tuples = helper (filter (isJust . snd) tuples) tuples
  where
    helper xs []                                          = xs
    helper xs ((coord, maybeCoord):ys) | isOnEdge p coord = helper (filter ((/= maybeCoord) . snd) xs) ys
                                       | otherwise        = helper xs ys

numberSix :: IO ()
numberSix = do
  sites <- map parseCoordinate . lines <$> readFile "/Users/nwest/AoC/6"
  let p              = boundingPlan sites
      allCoordinates = coordinateMap p
  print . maximum . map length . group . sort . map snd . boundedCoords p . map (`closestCoord` sites) $ allCoordinates

distanceToAll :: Coordinate -> [Coordinate] -> Int
distanceToAll c = sum . map (manhattan c)

numberSixB :: IO ()
numberSixB = do
  sites <- map parseCoordinate . lines <$> readFile "/Users/nwest/AoC/6"
  let p              = boundingPlan sites
      allCoordinates = coordinateMap p
  print . length . filter (< 10000) . map (`distanceToAll` sites) $ allCoordinates

-----------------------------------------------

parseRequirements :: String -> (Char, Char)
parseRequirements s = let wds = words s
                          a = head . head . drop 1 $ wds
                          b = head . head . drop 7 $ wds
                      in (a, b)

toList :: (a, a) -> [a]
toList (a, b) = [a,b]

reduction :: [(Char, Char)] -> [Char] -> [Char]
reduction [] a = a
reduction _ [] = []
reduction rules possible = let blocked = map snd rules
                               playable = filter (`notElem` blocked) possible
                           in if null playable
                           then ""
                           else let nextPlay = head playable
                                    newRules = filter (\(a, _)-> a /= nextPlay) rules
                                    newPossible = filter (/= nextPlay) possible
                                in nextPlay : reduction newRules newPossible

numberSeven :: IO ()
numberSeven = do
  requirements <- map parseRequirements . lines <$> readFile "/Users/nwest/AoC/7"
  let allPossible = nub . sort . concatMap toList $ requirements
  print . reduction requirements $ allPossible

-----------------------------------------------
