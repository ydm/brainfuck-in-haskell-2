module Main where

import Control.Monad.Trans.State -- (runStateT)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)
import Lib


initial n = Tape (replicate n 0) 0

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  code <- hGetContents handle
  end <- execStateT (interpret code) (initial 10)
  hClose handle

  putStr "\nEND STATE: "
  print end
