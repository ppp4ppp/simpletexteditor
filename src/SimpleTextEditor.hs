{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module SimpleTextEditor where

import Control.Lens
import Control.Monad.Writer
import Control.Monad.State


-- | Holds state of simple text editor
--
-- > '_text' is the reversed text in current state. It is reversed to simplify/speedup Delete 'Commnad'
-- > '_deleted' is list of strings containing removed symbols in order they where removed
-- > '_undolog' list of 'CommandFootprint' representing sequence and specs of previouse Append Delete 'Command'
-- 
data EditorState = EditorState{_text::String, _removed::[String], _undolog::[CommandFootprint]} deriving Show

-- | Represents 'Command' parsed from input
--
-- > see https://www.hackerrank.com/challenges/simple-text-editor for commands desc
--
data Command = Append String | Delete Int | Print Int | Undo Bool deriving (Eq, Show)

-- |  Append and Delete 'Command' footprint + info, (Add n) n - is number of added symbols
--
data CommandFootprint = Add Int | Remove deriving Show

makeLenses ''EditorState

-- | Take first element as command; skip promissed whitespace; put remaining array as command argument.
--
parsecommand :: String -> Command
parsecommand (x:xs) 
  | x == '1' = Append $ tail xs
  | x == '2' = Delete $ read $ tail xs
  | x == '3' = Print $ read $ tail xs
  | x == '4' = (Undo True)


-- | Modify 'EditorState' by appling 'Command' to EdirotState. Append will add string to '_text',
-- delete will remove specified number from '_text' and add what was removed to '_deleted',
-- undo append will remove symbols from '_text', undo delete will move one string from '_deleted' to '_text'.
--
-- > APPEND: "rebrab" ~~> "paradox" ~~> "xodaraprebrab" [Add 7]
-- > DELETE: "xodaraprebrab" ~~> delete 7 ~~> "rebrab" deleted:[["paradox"]] [Add 7, Delete 7]
-- > UNDO:   "rebrab" [Add 7, Delete 7] ~~> undo ~~> "xodaraprebrab" [Add 7]
--
pushcommand :: (MonadState EditorState m) => Command -> m ()
pushcommand (Append s) = do 
  modify $ \ editorstate -> over undolog  ((Add (length s)):) $ over text ((reverse s)++) editorstate
pushcommand (Delete i) = do 
  modify $ \ editorstate -> over undolog  (Remove:) $ over text (drop i) $ over removed ([(take i (editorstate ^. text))]++) editorstate
pushcommand (Undo _)=  do
  editorstate <- get
  case (editorstate ^. undolog) of
    [] -> return ()
    ((Add i):xs) -> put $ over text (drop i) $ over undolog tail editorstate
    (Remove:xs) -> put $ over removed tail $ over text ((head (editorstate ^. removed))++) $ over undolog tail editorstate

-- | Print out symbol from 'EditorState' '_text'  at specified index
--
printidx :: (MonadState EditorState m, MonadIO m) => Int -> m ()
printidx i = do
  editorstate <- get
  liftIO $ putStrLn $ [(editorstate ^. text)!!((length (editorstate ^. text)) - i)]

-- | Process 'Command' parsed from IO input line 
--
-- > 'Command' Print will print staff with 'printidx'
-- > all other commands will be applyed to EditorState by 'pushcommand'
--
processCommand :: (MonadState EditorState m, MonadIO m) => m ()
processCommand = do
  input <- liftIO $ getLine
  case parsecommand input of
    (Print i) -> printidx i
    othercommands -> pushcommand othercommands
  return ()
 