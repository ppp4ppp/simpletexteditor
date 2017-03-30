module ArbitraryInstances where

import Prelude hiding (print)

import SimpleTextEditor

import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(..))


data AppendChain = AppendChain [Command] deriving Show
data AppendDeleteChain = AppendDeleteChain [Command] deriving Show
data AppendDeleteUndoChain = AppendDeleteUndoChain [Command] deriving Show

instance Arbitrary Command where
  arbitrary = do
    oneof [append, delete, print , undo ]

instance Arbitrary AppendChain where
  arbitrary = do 
    rs <- (listOf $ oneof [append])
    return (AppendChain rs)
            
instance Arbitrary AppendDeleteChain where
  arbitrary = do 
    rs <- (listOf $ oneof [append, delete])
    return (AppendDeleteChain rs)

instance Arbitrary AppendDeleteUndoChain where
  arbitrary = do 
    rs <- (listOf $ oneof [append, delete, undo])
    return (AppendDeleteUndoChain rs)

positive = getPositive <$> arbitrary
append = (Append <$> (listOf (elements ['a'..'z'])))
delete = (Delete <$> positive)
print = (Print <$> positive)
undo = (Undo <$> (MkGen $ \ _ _ -> True))