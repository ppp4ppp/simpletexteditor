module Main where

import SimpleTextEditor

import Control.Monad.State

import Data.List

-- | Read n and call 'processCommand' n times
--
-- > Please see usage at https://www.hackerrank.com/challenges/simple-text-editor
--
-- > In accordance with documnet above I will assume that all input is valid, therefor no input sanity check is performed
-- 
main :: IO ()
main = do
  n <- getLine 
  runStateT (sequence ( replicate (read n) processCommand )) $ EditorState "" [[]] []
  return ()