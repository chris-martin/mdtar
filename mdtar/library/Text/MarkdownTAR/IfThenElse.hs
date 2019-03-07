{-# LANGUAGE LambdaCase #-}

module Text.MarkdownTAR.IfThenElse where

import Prelude (Bool (True, False), Monad, (>>=))

---

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM cond ifTrue ifFalse =

    cond >>= \case { True -> ifTrue; False -> ifFalse }

ifThenElseM' :: Monad m => [(m Bool, m a)] -> m a -> m a
ifThenElseM' ifs ifAllFalse =

    go ifs

  where
    go = \case
        []             -> ifAllFalse
        (cond, x) : xs -> ifThenElseM cond x (go xs)
