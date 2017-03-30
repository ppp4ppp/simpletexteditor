{-# LANGUAGE FlexibleContexts #-}

import ArbitraryInstances

import SimpleTextEditor

import Prelude hiding (print)

import Control.Lens hiding (elements)
import Control.Monad.State

import Data.List (intersperse, foldl', groupBy)

import Test.QuickCheck

-- | Will push all commands through 'pushcommand' and get 'EditorState' '_text' back
-- basicaly this will evaluate list of 'Command' the same way 'processCommand' does 
--
evalCommands ::[Command] -> String
evalCommands cmds = 
  let editorstate = snd $ runState (mapM_ pushcommand cmds) (EditorState "" [[]] [])
  in reverse $ editorstate ^. text

-- | Print 'Command' to String in described format
-- 
-- > see https://www.hackerrank.com/challenges/simple-text-editor
--
printcommnad :: Command -> String
printcommnad (Append s) = "1 " ++ s
printcommnad (Delete i) = "2 " ++ (show i)
printcommnad (Print i) = "3 " ++ (show i)
printcommnad (Undo _) = "4"


-- | Concatenate all Append 'Command' arguments
--
appnedall :: [Command] -> String
appnedall xs =  concatMap ap  xs 
      where ap (Append s) = s
            ap _ = []

-- | Add number off appned symbols, subtract number of removed symbols 
--                              
countchangesdiff :: [Command] -> Int
countchangesdiff xs = foldl' count 0 xs
    where
      count n (Append s) = (n + (length s))
      count n (Delete i) = if (n - i) < 0 then 0 else (n - i)
      count n _ = n

-- | Remove all commands that are affected by Undo 'Commnad' from list
-- 
removeundo :: [Command] -> [Command]
removeundo [] = []
removeundo xs = foldl' removebeforeundo [] $ removefirstundo $ groupBy ( \ a b -> (isUndo a && isUndo b) || ((not (isUndo a)) && (not (isUndo b)))) xs 
  where
    isUndo (Undo _) = True
    isUndo _ = False
    removebeforeundo acc xs = if (isUndo (head xs)) then (reverse (drop (length xs) (reverse acc))) else (acc ++ xs)
    removefirstundo [] = []
    removefirstundo (x:xs) = if (isUndo (head x)) then xs else (x:xs)


-- | Test command parsing
-- Print command, parse with 'parsecommand', comapre results
--
commnad_parse_inv :: Command -> Bool
commnad_parse_inv cmd = 
  cmd == parsecommand ((printcommnad cmd))


-- | Test list of Append 'Command' 
-- Concatenate all Append 'Command' arguments and compare result to 'evalCommands' output
--
appned_chain_inv :: AppendChain -> Bool
appned_chain_inv (AppendChain cmds) = 
  (appnedall cmds) == (evalCommands cmds)


-- | Test list of Append Delete 'Command'
-- Add number of appneded symbols, subtract number of removed symbols, compare result to length of 'evalCommands' output
--
appned_delete_chain_inv (AppendDeleteChain cmds) =
  (countchangesdiff cmds) == (length $ evalCommands cmds)

-- | Test if list of 'Command' without Undo affected commands will be evaluated the same as initial list of 'Commnad'
-- 
-- > check if 'evalCommands' [a, b, c, Undo, Undo, d] == 'evalCommands' [a, d]
--                             
appned_delete_undo_chain_inv (AppendDeleteUndoChain cmds) = 
  evalCommands cmds == evalCommands (removeundo cmds)

main = do 
  quickCheck appned_chain_inv
  quickCheck appned_delete_chain_inv
  quickCheck appned_delete_undo_chain_inv
