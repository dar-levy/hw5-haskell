{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror Deque.hs HW5.hs
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Control.Applicative (liftA2)
import Data.Char (chr, ord, toLower, toUpper)
import Data.Either
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import Data.Set qualified as S
import Deque (Deque)
import Deque qualified as DQ
import System.Console.Haskeline

data FoldMapFunc a m result = FoldMapFunc {agg :: a -> m, finalize :: m -> result}

foldMap' :: (Foldable t, Monoid m) => FoldMapFunc a m result -> t a -> result
foldMap' FoldMapFunc{agg, finalize} = finalize . foldMap agg

-- Section 1: Foldable functions
fmsum :: Num a => FoldMapFunc a (Sum a) a
fmsum = FoldMapFunc Sum getSum

fmor :: FoldMapFunc Bool Any Bool
fmor = FoldMapFunc Any getAny

fmfold :: Monoid a => FoldMapFunc a a a
fmfold = FoldMapFunc id id

fmelem :: Eq a => a -> FoldMapFunc a Any Bool
fmelem x = FoldMapFunc (Any . (== x)) getAny

fmfind :: (a -> Bool) -> FoldMapFunc a (First a) (Maybe a)
fmfind p = FoldMapFunc (First . (\x -> if p x then Just x else Nothing)) getFirst

fmlength :: FoldMapFunc a (Sum Int) Int
fmlength = FoldMapFunc (const 1) getSum

fmnull :: FoldMapFunc a Any Bool
fmnull = FoldMapFunc (const (Any True)) (not . getAny)

fmmaximum :: Ord a => FoldMapFunc a (Max (Maybe a)) (Maybe a)
fmmaximum = FoldMapFunc (Max . Just) getMax

fmminimum :: Ord a => FoldMapFunc a (Min (Maybe a)) (Maybe a)
fmminimum = FoldMapFunc (Min . Just) getMin

fmmaxBy :: Ord b => (a -> b) -> FoldMapFunc a (Max (Maybe (b, a))) (Maybe a)
fmmaxBy f = FoldMapFunc (\x -> Max (Just (f x, x))) (fmap snd . getMax)

fmminBy :: Ord b => (a -> b) -> FoldMapFunc a (Min (Maybe (b, a))) (Maybe a)
fmminBy f = FoldMapFunc (\x -> Min (Just (f x, x))) (fmap snd . getMin)

fmtoList :: FoldMapFunc a [a] [a]
fmtoList = FoldMapFunc (:[]) id

-- Section 2: Deque instances (Don't forget to implement the instances in Deque.hs as well!)
newtype DequeWrapper a = DequeWrapper (Deque a) deriving (Show, Eq)

instance Semigroup (DequeWrapper a) where
  DequeWrapper q1 <> DequeWrapper q2 = DequeWrapper (q1 <> q2)

instance Monoid (DequeWrapper a) where
  mempty = DequeWrapper DQ.empty

instance Foldable DequeWrapper where
  foldMap f (DequeWrapper q) = foldMap f q

instance Functor DequeWrapper where
  fmap f (DequeWrapper q) = DequeWrapper (fmap f q)

instance Applicative DequeWrapper where
  pure x = DequeWrapper (pure x)
  DequeWrapper fs <*> DequeWrapper xs = DequeWrapper (fs <*> xs)

instance Monad DequeWrapper where
  DequeWrapper q >>= f = DequeWrapper (q >>= \x -> let DequeWrapper q' = f x in q')

---- Section 3: Calculator and traverse
class Monad f => CalculatorError f where
  divideByZero :: f Int
  missingVariable :: String -> f Int

runCalculator :: CalculatorError f => Map String Int -> Expr -> f Int
runCalculator _ (Val x) = return x
runCalculator vars (Var x) =
  case M.lookup x vars of
    Just val -> return val
    Nothing  -> missingVariable x
runCalculator vars (Add e1 e2) = liftA2 (+) (runCalculator vars e1) (runCalculator vars e2)
runCalculator vars (Sub e1 e2) = liftA2 (-) (runCalculator vars e1) (runCalculator vars e2)
runCalculator vars (Mul e1 e2) = liftA2 (*) (runCalculator vars e1) (runCalculator vars e2)
runCalculator vars (Div e1 e2) = do
  numerator <- runCalculator vars e1
  denominator <- runCalculator vars e2
  if denominator == 0
    then divideByZero
    else return (numerator `div` denominator)

-- Instances to implement:
instance CalculatorError Maybe where
  divideByZero = Nothing
  missingVariable _ = Nothing

data Err = DivByZero | MissingVar String deriving (Show, Eq)
instance CalculatorError (Either Err) where
  divideByZero = Left DivByZero
  missingVariable var = Left (MissingVar var)


data Defaults
  = Defaults
  -- This replaces the entire division result, not just the denominator!
  { defaultForDivisionByZero :: Int
  , defaultForVariable :: String -> Int
  }
instance CalculatorError (Reader Defaults) where
  divideByZero = Reader $ \d -> defaultForDivisionByZero d
  missingVariable var = Reader $ \d -> defaultForVariable d var

-- From the lectures:
newtype Reader r a = Reader {runReader :: r -> a}
instance Functor (Reader r) where
  fmap f r = Reader $ f . runReader r
instance Applicative (Reader r) where
  pure = Reader . const
  liftA2 f ra rb = Reader $ \r -> f (runReader ra r) (runReader rb r)
instance Monad (Reader r) where
  ra >>= f = Reader $ \r -> runReader (f $ runReader ra r) r

data Expr
  = Val Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

---- Section 4: Hangman
hangman :: String -> IO Int
hangman str = do

  let hidden_str = map (hideChar . toLower) str
  let optimal = S.size (S.delete ' ' (S.fromList str))
  num_guesses <- playGame str "Guess a letter: " hidden_str S.empty S.empty 0
  return (num_guesses - optimal)


playGame :: String -> String -> String -> Set Char -> Set Char -> Int -> IO Int
playGame str prompt hidden_str good_guessed_chars bad_guessed_chars num_guesses = do

  putStrLn hidden_str
  putStr prompt
  c <- getChar'
  let guess = toLower c
  --putStrLn ""

  -- Bonus char
  if guess == '?' then
    -- Here we create a set of the whole alphabet, and do diff set operation on the good and bad guesses sets
    -- i.e., ({"abcdefghijklmnopqrstuvwxy"} - {good_guessed_chars}) - {bad_guessed_chars}
    let remaining_chars = quicksort (S.toList (S.difference (S.difference (S.fromList "abcdefghijklmnopqrstuvwxyz") good_guessed_chars) bad_guessed_chars)) in do
      putStr "Remaining letters: ["
      putStr remaining_chars
      putStr "]"
      putStrLn ""
      playGame str "Guess a letter: " hidden_str good_guessed_chars bad_guessed_chars num_guesses
  
  -- Invalid char case
  else if not (isLowerLetter guess) then do
    --putStrLn ""
    putStr "Invalid letter guess "
    putStr [c]
    putStr "!"
    putStrLn ""
    playGame str "Try again: " hidden_str good_guessed_chars bad_guessed_chars num_guesses
  
  -- Character was already gussed correctly
  else if (S.member guess good_guessed_chars) then
    playGame str "Guess a letter: " hidden_str good_guessed_chars bad_guessed_chars num_guesses
  
  -- New or incorrect character guess
  else
    let new_hidden_str = unhideString str hidden_str guess in
    
    -- Full string was revealed
    if new_hidden_str == str then do
      putStrLn "Very good, the word is:"
      putStrLn new_hidden_str
      return (num_guesses + 1)
    
    -- Guess does not appear in the string, we add it to the bad guess set
    else if hidden_str == new_hidden_str then do
      putStrLn "Wrong guess!"
      playGame str "Try again: " new_hidden_str good_guessed_chars (S.insert guess bad_guessed_chars) (num_guesses + 1)
    
    -- Guess does appear, we add it to the good guess set
    else
      playGame str "Guess a letter: " new_hidden_str (S.insert guess good_guessed_chars) bad_guessed_chars (num_guesses + 1)

isLowerLetter :: Char -> Bool
isLowerLetter c = (code >= 97 && code <= 122)
  where code = ord c

hideChar :: Char -> Char
hideChar c = if isLowerLetter c then '_' else c

unhideString :: String -> String -> Char -> String
unhideString [] _ _ = []
unhideString _ [] _ = []
unhideString (c:original) (h:hidden) guess
 | (toLower c) == guess = c : (unhideString original hidden guess)
 | otherwise = h : (unhideString original hidden guess)

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

getChar' :: IO Char
getChar' = runInputT defaultSettings $ fromJust <$> getInputChar ""