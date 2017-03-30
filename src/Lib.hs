{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Lib where

import Control.Lens
import Control.Monad.Writer
import Control.Monad.State

-- sanity
-- non empty cmd || args
-- any time sum delete <= sum of input
-- any time N undo <= N cmd

data EditorState = EditorState{_appended::String, _removed::[String], _undolog::[CommandFootprint]} deriving Show
data Command = Append String | Delete Int | Print Int | Undo
data CommandFootprint = Add Int | Remove deriving Show

makeLenses ''EditorState

-- | Take first element as command;
-- skip promissed whitespace;
-- put remaining array as command argument.
parsecommand :: String -> Command
parsecommand (x:xs) 
  | x == '1' = Append $ tail xs
  | x == '2' = Delete $ read $ tail xs
  | x == '3' = Print $ read $ tail xs
  | x == '4' = Undo

-- |  On delete (N) : move N symbols from appented to deleted
--    On print (I) : calulate reverse index in appended array and print
-- 

-- it is not MonadIO to make it usefull in QuickCheck testing
pushcommand :: (MonadState EditorState m) => Command -> m ()
pushcommand (Append s) = do 
  modify $ \ editorstate -> over undolog  ((Add (length s)):) $ over appended ((reverse s)++) editorstate
pushcommand (Delete i) = do 
  modify $ \ editorstate -> over undolog  (Remove:) $ over appended (drop i) $ over removed ([(take i (editorstate ^. appended))]++) editorstate
pushcommand Undo =  do
  editorstate <- get
  case head (editorstate ^. undolog) of
    (Add i) -> put $ over appended (drop i) $ over undolog tail editorstate
    Remove -> put $ over removed tail $ over appended ((head (editorstate ^. removed))++) $ over undolog tail editorstate

processIO :: (MonadState EditorState m, MonadIO m) => Int -> m ()
processIO 0 = return ()
processIO n = do
  input <- liftIO $ getLine
  case parsecommand input of
    (Print i) -> do
      editorstate <- get
      liftIO $ putStrLn $ [(editorstate ^. appended)!!((length (editorstate ^. appended)) - i)]
    othercommands -> pushcommand othercommands
  processIO (n - 1)