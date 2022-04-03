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


  -- |
  -- Fails if the value returned by the parser does not conform to the
  -- predicate. Generalized form of 'Data.ByteString.Parser.Char8.string'.
  --
  -- Example:
  --
  -- @
  -- pInput = takeWhile isLetter `provided` (odd . length)
  -- @
  --
  {-# INLINE CONLIKE provided #-}
  {-# SPECIALIZE provided :: (a -> Bool) -> BSP.Parser a -> BSP.Parser a #-}
  {-# SPECIALIZE provided :: (a -> Bool) -> TP.Parser a -> TP.Parser a #-}
  provided :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m a
  provided test par = do
    x <- par
    if test x
       then pure x
       else Control.Applicative.empty


  -- |
  -- Tries various parsers, one by one. Alias for 'asum'.
  --
  -- Example:
  --
  -- @
  -- pExpression = choice [ pConstant
  --                      , pVariable
  --                      , pBinaryOperation
  --                      , pFunctionApplication
  --                      ]
  -- @
  --
  {-# INLINE CONLIKE choice #-}
  {-# SPECIALIZE choice :: [BSP.Parser a] -> BSP.Parser a #-}
  {-# SPECIALIZE choice :: [TP.Parser a] -> TP.Parser a #-}
  choice :: (Alternative f) => [f a] -> f a
  choice = asum


  -- |
  -- Replicates the parser given number of times, collecting the results
  -- in a list. Fails if any instance of the parser fails.
  --
  -- Example:
  --
  -- @
  -- pFourWords = (:) \<$\> word \<*\> count 3 (blank *> word)
  --   where word  = takeWhile1 isLetter
  --         blank = takeWhile1 isSpace
  -- @
  --
  {-# INLINE CONLIKE count #-}
  {-# SPECIALIZE count :: Int -> BSP.Parser a -> BSP.Parser [a] #-}
  {-# SPECIALIZE count :: Int -> TP.Parser a -> TP.Parser [a] #-}
  count :: (Monad m) => Int -> m a -> m [a]
  count n p = Prelude.sequence (Prelude.replicate n p)


  -- |
  -- Captures first parser as @Left@ or the second as @Right@.
  --
  {-# INLINE CONLIKE eitherP #-}
  {-# SPECIALIZE eitherP :: BSP.Parser a -> BSP.Parser b -> BSP.Parser (Either a b) #-}
  {-# SPECIALIZE eitherP :: TP.Parser a -> TP.Parser b -> TP.Parser (Either a b) #-}
  eitherP :: (Alternative f) => f a -> f b -> f (Either a b)
  eitherP left right = (Left <$> left) <|> (Right <$> right)


  -- |
  -- Shortcut for 'optional' with a default value.
  --
  -- Example:
  --
  -- @
  -- data Contact =
  --  Contact
  --    { contactName  :: Text
  --    , contactEmail :: Maybe Text
  --    }
  --
  -- pContact = Contact \<$\> pFullName \<*\> option pEmail
  -- @
  --
  {-# INLINE CONLIKE option #-}
  {-# SPECIALIZE option :: a -> BSP.Parser a -> BSP.Parser a #-}
  {-# SPECIALIZE option :: a -> TP.Parser a -> TP.Parser a #-}
  option :: (Alternative f) => a -> f a -> f a
  option dfl par = fromMaybe dfl <$> optional par


  -- |
  -- Like 'many1', but requires at least one match.
  --
  {-# INLINE many1 #-}
  {-# SPECIALIZE many1 :: BSP.Parser a -> BSP.Parser [a] #-}
  {-# SPECIALIZE many1 :: TP.Parser a -> TP.Parser [a] #-}
  many1 :: (Alternative f) => f a -> f [a]
  many1 = some


  -- |
  -- Like 'many', but stops once the second parser matches the input ahead.
  --
  -- Example:
  --
  -- @
  -- pBodyLines = pLine `manyTill` pEnd
  --   where pLine = takeTill (== '\n')
  --         pEnd  = string "\n.\n"
  -- @
  --
  {-# INLINE CONLIKE manyTill #-}
  {-# SPECIALIZE manyTill :: BSP.Parser a -> BSP.Parser a -> BSP.Parser [a] #-}
  {-# SPECIALIZE manyTill :: TP.Parser a -> TP.Parser a -> TP.Parser [a] #-}
  manyTill :: (Alternative f) => f a -> f a -> f [a]
  manyTill par stop = loop
    where loop = (stop *> pure []) <|> ((:) <$> par <*> loop)


  -- |
  -- Similar to 'many', but interleaves the first parser with the second.
  --
  -- Example:
  --
  -- @
  -- pLines = pLine `sepBy` char '\n'
  -- @
  --
  {-# INLINE CONLIKE sepBy #-}
  {-# SPECIALIZE sepBy :: BSP.Parser a -> BSP.Parser b -> BSP.Parser [a] #-}
  {-# SPECIALIZE sepBy :: TP.Parser a -> TP.Parser b -> TP.Parser [a] #-}
  sepBy :: (Alternative f) => f a -> f b -> f [a]
  sepBy par sep = sepBy1 par sep <|> pure []


  -- |
  -- Like 'sepBy', but requires at least one match.
  --
  {-# INLINE CONLIKE sepBy1 #-}
  {-# SPECIALIZE sepBy1 :: BSP.Parser a -> BSP.Parser b -> BSP.Parser [a] #-}
  {-# SPECIALIZE sepBy1 :: TP.Parser a -> TP.Parser b -> TP.Parser [a] #-}
  sepBy1 :: (Alternative f) => f a -> f b -> f [a]
  sepBy1 par sep = loop
    where loop = (:) <$> par <*> ((sep *> loop) <|> pure [])


  -- |
  -- Wraps the parser from both sides.
  --
  -- Example:
  --
  -- @
  -- pToken = takeWhile1 (inClass "A-Za-z0-9_") `wrap` takeWhile isSpace
  -- @
  --
  {-# INLINE CONLIKE wrap #-}
  {-# SPECIALIZE wrap :: BSP.Parser a -> BSP.Parser b -> BSP.Parser a #-}
  {-# SPECIALIZE wrap :: TP.Parser a -> TP.Parser b -> TP.Parser a #-}
  wrap :: (Applicative f) => f a -> f b -> f a
  wrap par wrapper = wrapper *> par <* wrapper


-- vim:set ft=haskell sw=2 ts=2 et:
