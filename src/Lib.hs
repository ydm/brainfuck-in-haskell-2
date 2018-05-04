module Lib where

import Control.Exception (catch)
import Control.Monad.Trans.State
import Data.Char (chr, ord)
import Data.Int (Int8)


-- +------------+
-- + Tape & ops +
-- +------------+

type Stack = [Int8]
data Tape = Tape Stack Int deriving Show
type BFState = StateT Tape IO


-- | Apply a function to the i-th element of a given list.
_ap :: (a -> a) -> [a] -> Int -> [a]
_ap f (x : xs) 0 = f x : xs
_ap f (x : xs) i = x : _ap f xs (i - 1)

op :: (Monad m) => (Int8 -> Int8) -> StateT Tape m ()
op f = StateT $ \(Tape xs i) -> return ((), Tape (_ap f xs i) i)

plus :: (Monad m) => StateT Tape m ()
plus = op (+1)

minus :: (Monad m) => StateT Tape m ()
minus = op $ subtract 1

left :: (Monad m) => StateT Tape m ()
left = StateT $ \(Tape xs i) -> return ((), Tape xs (i - 1))

right :: (Monad m) => StateT Tape m ()
right = StateT $ \(Tape xs i) -> return ((), Tape xs (i + 1))

coerce :: (Num c, Integral a) => a -> c
coerce = fromInteger . toInteger

printInt8 :: Int8 -> IO ()
printInt8 x
  | 32 <= x && x <= 126 = (putChar . chr . coerce) x
  | otherwise         = putStr "\\" >> putStr (show x)

printAsInt8 :: StateT Tape IO ()
printAsInt8 = StateT $ \s@(Tape xs i) -> printInt8 (xs !! i) >> return ((), s)

readInt8 :: IO Int8
readInt8 = catch (coerce . ord <$> getChar) f
  where f :: (IOError -> IO Int8)
        f _ = return 0

readAsInt8 :: StateT Tape IO ()
readAsInt8 = StateT $ \(Tape xs i) -> do
  x <- readInt8
  return ((), Tape (_ap (const x) xs i) i)

cond :: StateT Tape IO Bool
cond = StateT $ \s@(Tape xs i) -> return (xs !! i /= 0, s)

printTape :: StateT Tape IO ()
printTape = StateT $ \s -> print s >> return ((), s)


-- +--------------------------+
-- + Parsing & interpretation |
-- +--------------------------+

-- | Split the given code string to loop body and what's after that.
extractLoop :: String -> (String, String)
extractLoop = rev <$> f "" 0
  where f :: String -> Int -> String -> (String, String)
        f  "" n ('[':xs) = f       ""  (n + 1) xs
        f acc n ('[':xs) = f ('[':acc) (n + 1) xs
        f acc 1 (']':xs) =  ((']':acc), xs)
        f acc n (']':xs) = f (']':acc) (n - 1) xs
        f acc n (x:xs)   = f (x:acc) n xs
        f acc _ ""       = (acc, "")
        rev (a, b) = (reverse a, b)

interpret :: String -> StateT Tape IO ()
interpret [] = StateT $ \s -> return ((), s)
interpret      ('<' : xs) = left >> interpret xs
interpret      ('>' : xs) = right >> interpret xs
interpret      ('+' : xs) = plus >> interpret xs
interpret      ('-' : xs) = minus >> interpret xs
interpret      ('.' : xs) = printAsInt8 >> interpret xs
interpret      (',' : xs) = readAsInt8 >> interpret xs
interpret      ('!' : xs) = printTape >> interpret xs
interpret code@('[' : xs) = interpretLoop (extractLoop code)
interpret      ( _  : xs) = interpret xs

interpretLoop :: (String, String) -> StateT Tape IO ()
interpretLoop tup@(body, rest) = do
  res <- cond
  if res
    then interpret body >> interpretLoop tup
    else interpret rest
