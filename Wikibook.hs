module Wikibook where

import Control.Applicative
import Control.Monad
import Data.Char
import System.Random
import Text.Read (readMaybe)

import My

factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' n = go n 1
  where go n acc | n > 1     = go (n - 1) (n * acc)
                 | otherwise = acc

myReplicate n x = go n []
  where go 0 acc = acc
        go n acc = go (n - 1) (x:acc)

index _ [] = error "index: out of bounds"
index 0 (x:_)  = x
index n (x:xs) = index (n - 1) xs

myZip xs ys = go xs ys []
  where go [] _ acc          = acc
        go _ [] acc          = acc
        go (x:xs) (y:ys) acc = go xs ys ((x, y):acc)

myLlength xs = go xs 0
  where go [] acc     = acc
        go (x:xs) acc = go xs (acc + 1)

myFoldr f acc []     = acc
myFoldr f acc (x:xs) = f x $ myFoldr f acc xs

myFoldl f acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myFoldl' f acc [] = acc
myFoldl' f acc (x:xs) = let acc' = f acc x
                        in seq acc' $ myFoldl' f acc' xs

myFoldr1 f [x]    = x
myFoldr1 f (x:xs) = f x $ myFoldr1 f xs
myFoldr1 _ []     = error "myFoldr1: empty list"

myFoldl1 f (x:xs) = myFoldl f x xs
myFoldl1 _ [] = error "myFoldl1: empty list"

echoes = myFoldr (\x xs -> (myReplicate x x) ++ xs) []

echoes2 = myFoldl (\xs x -> xs ++ (myReplicate x x)) []

myMap f = myFoldr (\x xs -> (f x):xs) []

myAnd = myFoldr (&&) True

myOr = myFoldr (||) False

myMaximum :: Ord a => [a] -> a
myMaximum = myFoldr1 max 

myMinimum :: Ord a => [a] -> a
myMinimum = myFoldr1 min

myReverse = myFoldl' (\acc x -> x:acc) []

-- myScanr f init []     = [init]
-- myScanr f init (x:xs) = let y = myScanr f init xs
--                         in (f x $ head y):y

myScanr f init = myFoldr (\x acc@(a:as) -> (f x a):acc) [init]

-- myScanl f init []     = [init]
-- myScanl f init (x:xs) = init:(myScanl f (f init x) xs)

myScanl f init = myReverse . myFoldl' (\acc@(a:as) x -> (f a x):acc) [init]

myFilter pred = myFoldr (\x acc -> if pred x then x:acc else acc) []

returnDivisible divider xs = myFilter (\x -> x `mod` divider == 0) xs

choosingTails ls = [tail l | l <- ls, not $ null l, head l > 5]

doubleOfFirstForEvenSeconds =
  map (\(x, _) -> 2 * x) . filter (\(_, y) -> y `mod` 2 == 0)

fakeIf pred thenExpr elseExpr = case pred of True  -> thenExpr
                                             False -> elseExpr

for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO()) -> IO ()
for init pred step io = go init
  where go i | pred i = do io i
                           go (step i)
             | otherwise = return ()

sequenceIO :: [IO a] -> IO [a]
sequenceIO []       = return []
sequenceIO (io:ios) = do x <- io
                         xs <- sequenceIO ios
                         return (x:xs)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO io [] = return []
mapIO io (x:xs) = do y <- io x
                     ys <- mapIO io xs
                     return (y:ys)

myCurry f a b = f (a, b)

myUncurry f (a, b) = f a b

myConst a _ = a

myFoldlUsingFoldr f init xs = myFoldr (.) id fs init
  where fs = myMap (\x a -> f a x) xs

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
   deriving (Show)

-- treeMap f (Leaf x) = Leaf $ f x
-- treeMap f (Branch l r) = Branch (treeMap f l) (treeMap f r)
treeMap f = go
  where go (Leaf x) = Leaf $ f x
        go (Branch l r) = Branch (go l) (go r)

treeFold foldBranch foldLeaf = go
  where go (Leaf x) = foldLeaf x
        go (Branch l r) = foldBranch (go l) (go r)

data Weird a b = First a
               | Second b
               | Third [(a, b)]
               | Fourth (Weird a b)
  deriving (Show)

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = go
  where go (First a)   = First $ fa a
        go (Second b)  = Second $ fb b
        go (Third abs) = Third $ map (\(a, b) -> (fa a, fb b)) abs
        go (Fourth w)  = Fourth $ go w

weirdFold :: (a -> c)
          -> (b -> c)
          -> ([(a, b)] -> c)
          -> (c -> c)
          -> Weird a b
          -> c
weirdFold f1 f2 f3 f4 = go
  where go (First a)   = f1 a
        go (Second b)  = f2 b
        go (Third abs) = f3 abs
        go (Fourth w)  = f4 $ go w

instance Functor Tree where
  fmap = treeMap

interactiveDoubling = do
  putStr "Choose a number: "
  s <- getLine
  let mx = readMaybe s :: Maybe Double
  case fmap (2*) mx of
    Just d -> putStrLn $ "The double of your number is " ++ (show d)
    Nothing -> do putStrLn "That was not a number, please try again"
                  interactiveDoubling

interactiveSumming = do
  putStrLn "Choose two numbers:"
  mx <- readMaybe <$> getLine
  my <- readMaybe <$> getLine
  case (+) <$> mx <*> my of
    Just z -> putStrLn $ "The sum of your numbers is " ++ (show z)
    Nothing -> do putStrLn "Invalid number, please try again"
                  interactiveSumming

interactiveConcatenating = do
  putStrLn "Choose two strings:"
  s <- (++) <$> getLine <*> getLine
  putStr "Concatenated: " *> putStrLn s

themselvesTimes :: [Int] -> [Int]
themselvesTimes xs = xs >>= (\x -> replicate x x)

printList :: Show a => [a] -> IO ()
printList xs = mapM_ putStrLn $ map show xs

generation :: Int -> [a] -> [a]
generation x bunnies = apply x (\bs -> bs >>= replicate 3) bunnies
  where apply 0 _ acc = acc
        apply n f acc = apply (n - 1) f (f acc)

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = replicateM n $ randomRIO (1, 6)

rollDie :: (RandomGen g) => MyState g Int
rollDie = myState $ randomR (1, 6)

rollDice :: (RandomGen g) => MyState g (Int, Int)
rollDice = myLiftA2 (,) rollDie rollDie

rollNDice :: (RandomGen g) => Int -> MyState g [Int]
rollNDice n = mySequence $ replicate n $ rollDie

getRandom :: Random a => MyState StdGen a
getRandom = myState random

allTypesRandom :: MyState StdGen (Int, Float, Char, Integer, Double, Bool, Int)
allTypesRandom = (,,,,,,) `myFmap` getRandom
                          `myApply` getRandom
                          `myApply` getRandom
                          `myApply` getRandom
                          `myApply` getRandom
                          `myApply` getRandom
                          `myApply` getRandom

isValidPassphrase :: String -> Bool
isValidPassphrase s = length s >= 8
                      && any isAlpha s
                      && any isNumber s
                      && any isPunctuation s

getPassphrase :: MyMaybeT IO String
getPassphrase = myLift getLine `myBind` \s ->
                myGuard (isValidPassphrase s) `myBind` \_ ->
                myReturn s

askPassphrase :: MyMaybeT IO ()
askPassphrase = (myLift $ putStr "Enter passphrase: ") `myBind` \_ ->
                getPassphrase `myBind` \_ ->
                myLift $ putStrLn "Saving to database..."
