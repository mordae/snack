-- |
-- Module      :  Snack.Combinators
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Snack.Combinators
  ( provided
  , choice
  , count
  , eitherP
  , option
  , many1
  , manyTill
  , sepBy
  , sepBy1
  , wrap
  )
where
  import Control.Applicative
  import Data.Maybe

  import {-# SOURCE #-} qualified Data.ByteString.Parser as BSP
  import {-# SOURCE #-} qualified Data.Text.Parser as TP


  {-# INLINE CONLIKE provided #-}
  {-# SPECIALIZE provided :: (a -> Bool) -> BSP.Parser a -> BSP.Parser a #-}
  {-# SPECIALIZE provided :: (a -> Bool) -> TP.Parser a -> TP.Parser a #-}
  provided :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m a
  provided test par = do
    x <- par
    if test x
       then pure x
       else Control.Applicative.empty


  {-# INLINE CONLIKE choice #-}
  {-# SPECIALIZE choice :: [BSP.Parser a] -> BSP.Parser a #-}
  {-# SPECIALIZE choice :: [TP.Parser a] -> TP.Parser a #-}
  choice :: (Alternative f) => [f a] -> f a
  choice = asum


  {-# INLINE CONLIKE count #-}
  {-# SPECIALIZE count :: Int -> BSP.Parser a -> BSP.Parser [a] #-}
  {-# SPECIALIZE count :: Int -> TP.Parser a -> TP.Parser [a] #-}
  count :: (Monad m) => Int -> m a -> m [a]
  count n p = Prelude.sequence (Prelude.replicate n p)


  {-# INLINE CONLIKE eitherP #-}
  {-# SPECIALIZE eitherP :: BSP.Parser a -> BSP.Parser b -> BSP.Parser (Either a b) #-}
  {-# SPECIALIZE eitherP :: TP.Parser a -> TP.Parser b -> TP.Parser (Either a b) #-}
  eitherP :: (Alternative f) => f a -> f b -> f (Either a b)
  eitherP left right = (Left <$> left) <|> (Right <$> right)


  {-# INLINE CONLIKE option #-}
  {-# SPECIALIZE option :: a -> BSP.Parser a -> BSP.Parser a #-}
  {-# SPECIALIZE option :: a -> TP.Parser a -> TP.Parser a #-}
  option :: (Alternative f) => a -> f a -> f a
  option dfl par = fromMaybe dfl <$> optional par


  {-# INLINE many1 #-}
  {-# SPECIALIZE many1 :: BSP.Parser a -> BSP.Parser [a] #-}
  {-# SPECIALIZE many1 :: TP.Parser a -> TP.Parser [a] #-}
  many1 :: (Alternative f) => f a -> f [a]
  many1 = some


  {-# INLINE CONLIKE manyTill #-}
  {-# SPECIALIZE manyTill :: BSP.Parser a -> BSP.Parser a -> BSP.Parser [a] #-}
  {-# SPECIALIZE manyTill :: TP.Parser a -> TP.Parser a -> TP.Parser [a] #-}
  manyTill :: (Alternative f) => f a -> f a -> f [a]
  manyTill par stop = loop
    where loop = (stop *> pure []) <|> ((:) <$> par <*> loop)


  {-# INLINE CONLIKE sepBy #-}
  {-# SPECIALIZE sepBy :: BSP.Parser a -> BSP.Parser b -> BSP.Parser [a] #-}
  {-# SPECIALIZE sepBy :: TP.Parser a -> TP.Parser b -> TP.Parser [a] #-}
  sepBy :: (Alternative f) => f a -> f b -> f [a]
  sepBy par sep = sepBy1 par sep <|> pure []


  {-# INLINE CONLIKE sepBy1 #-}
  {-# SPECIALIZE sepBy1 :: BSP.Parser a -> BSP.Parser b -> BSP.Parser [a] #-}
  {-# SPECIALIZE sepBy1 :: TP.Parser a -> TP.Parser b -> TP.Parser [a] #-}
  sepBy1 :: (Alternative f) => f a -> f b -> f [a]
  sepBy1 par sep = loop
    where loop = (:) <$> par <*> ((sep *> loop) <|> pure [])


  {-# INLINE CONLIKE wrap #-}
  {-# SPECIALIZE wrap :: BSP.Parser a -> BSP.Parser b -> BSP.Parser a #-}
  {-# SPECIALIZE wrap :: TP.Parser a -> TP.Parser b -> TP.Parser a #-}
  wrap :: (Applicative f) => f a -> f b -> f a
  wrap par wrapper = wrapper *> par <* wrapper


-- vim:set ft=haskell sw=2 ts=2 et:
