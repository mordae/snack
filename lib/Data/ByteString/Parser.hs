-- |
-- Module      :  Data.ByteString.Parser
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Data.ByteString.Parser
  ( Parser(..)
  , parseOnly

    -- * Bytes
  , byte
  , notByte
  , anyByte
  , satisfy
  , peekByte

    -- * Strings
  , string
  , Data.ByteString.Parser.take
  , scan
  , runScanner
  , Data.ByteString.Parser.takeWhile
  , takeWhile1
  , takeTill
  , takeTill1

    -- * Combinators
  , provided
  , choice
  , Data.ByteString.Parser.count
  , optional
  , eitherP
  , option
  , many
  , many1
  , manyTill
  , sepBy
  , sepBy1
  , wrap
  , match

    -- * End Of Input
  , takeByteString
  , endOfInput
  , atEnd

    -- * Miscelaneous
  , Control.Applicative.empty
  , pure
  , guard
  , when
  , unless
  , void
  )
where
  import Prelude hiding (null, length, splitAt, take)

  import Control.Applicative
  import Control.Monad

  import Data.Maybe
  import Data.Word

  import Data.ByteString as BS
  import Data.ByteString.Unsafe as BS


  newtype Parser a =
    Parser
      { runParser :: ByteString -> Maybe (a, ByteString)
      }

  instance Functor Parser where
    {-# INLINE fmap #-}
    fmap fn Parser{runParser} = Parser \inp ->
      case runParser inp of
        Just (res, rest) -> Just (fn res, rest)
        Nothing -> Nothing

  instance Applicative Parser where
    {-# INLINE pure #-}
    pure x = Parser \inp -> Just (x, inp)

    {-# INLINE (<*>) #-}
    (Parser runFn) <*> (Parser runArg) = Parser \inp ->
      case runFn inp of
        Nothing -> Nothing
        Just (fn, rest) ->
          case runArg rest of
            Nothing -> Nothing
            Just (x, rest') -> Just (fn x, rest')

  instance Alternative Parser where
    {-# INLINE empty #-}
    empty = Parser \_ -> Nothing

    {-# INLINE (<|>) #-}
    (Parser runLeft) <|> (Parser runRight) = Parser \inp ->
      case runLeft inp of
        Just r  -> Just r
        Nothing -> runRight inp

  instance Monad Parser where
    {-# INLINE (>>=) #-}
    (Parser runLeft) >>= right = Parser \inp ->
      case runLeft inp of
        Nothing -> Nothing
        Just (x, more) -> runParser (right x) more

  instance MonadPlus Parser

  instance MonadFail Parser where
    {-# INLINE CONLIKE fail #-}
    fail _ = mzero


  {-# INLINE CONLIKE parseOnly #-}
  parseOnly :: Parser a -> ByteString -> Maybe a
  parseOnly par = \inp -> fst <$> runParser par inp


  {-# INLINE CONLIKE byte #-}
  byte :: Word8 -> Parser Word8
  byte c = satisfy (c ==)


  {-# INLINE CONLIKE notByte #-}
  notByte :: Word8 -> Parser Word8
  notByte c = satisfy (c /=)


  {-# INLINE anyByte #-}
  anyByte :: Parser Word8
  anyByte = Parser \inp ->
    if null inp
       then Nothing
       else Just (unsafeHead inp, unsafeTail inp)


  {-# INLINE CONLIKE satisfy #-}
  satisfy :: (Word8 -> Bool) -> Parser Word8
  satisfy isOk = Parser \inp ->
    if null inp
       then Nothing
       else let c = unsafeHead inp
             in if isOk c
                   then Just (c, unsafeTail inp)
                   else Nothing


  {-# INLINE peekByte #-}
  peekByte :: Parser Word8
  peekByte = Parser \inp ->
    if null inp
       then Nothing
       else Just (unsafeHead inp, inp)


  {-# INLINE CONLIKE string #-}
  string :: ByteString -> Parser ByteString
  string str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case pfx == str of
          True -> Just (pfx, sfx)
          False -> Nothing


  {-# INLINE CONLIKE take #-}
  take :: Int -> Parser ByteString
  take n = Parser \inp ->
    if n > length inp
       then Nothing
       else Just (splitAt n inp)


  {-# INLINE CONLIKE scan #-}
  scan :: s -> (s -> Word8 -> Maybe s) -> Parser ByteString
  scan state scanner = fst <$> runScanner state scanner


  {-# INLINE CONLIKE runScanner #-}
  runScanner :: s -> (s -> Word8 -> Maybe s) -> Parser (ByteString, s)
  runScanner state scanner = Parser \inp ->
    let (state', n) = scanBytes state scanner 0 (unpack inp)
        (res, more) = splitAt n inp
     in Just ((res, state'), more)


  {-# INLINE scanBytes #-}
  scanBytes :: s -> (s -> Word8 -> Maybe s) -> Int -> [Word8] -> (s, Int)
  scanBytes !state _scanner !n [] = (state, n)
  scanBytes !state scanner !n (x:more) =
    case scanner state x of
      Just state' -> scanBytes state' scanner (succ n) more
      Nothing -> (state, n)


  {-# INLINE CONLIKE takeWhile #-}
  takeWhile :: (Word8 -> Bool) -> Parser ByteString
  takeWhile test = takeTill (not . test)


  {-# INLINE CONLIKE takeWhile1 #-}
  takeWhile1 :: (Word8 -> Bool) -> Parser ByteString
  takeWhile1 test = provided (not . null) $
                    Data.ByteString.Parser.takeWhile test


  {-# INLINE CONLIKE takeTill #-}
  takeTill :: (Word8 -> Bool) -> Parser ByteString
  takeTill test = Parser \inp ->
    let n = fromMaybe (length inp) $ findIndex test inp
     in Just (splitAt n inp)


  {-# INLINE CONLIKE takeTill1 #-}
  takeTill1 :: (Word8 -> Bool) -> Parser ByteString
  takeTill1 test = provided (not . null) $
                    Data.ByteString.Parser.takeTill test


  {-# INLINE CONLIKE provided #-}
  {-# SPECIALIZE provided :: (a -> Bool) -> Parser a -> Parser a #-}
  provided :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m a
  provided test par = do
    x <- par
    if test x
       then pure x
       else Control.Applicative.empty


  {-# INLINE CONLIKE choice #-}
  {-# SPECIALIZE choice :: [Parser a] -> Parser a #-}
  choice :: (Alternative f) => [f a] -> f a
  choice = asum


  {-# INLINE CONLIKE count #-}
  {-# SPECIALIZE Data.ByteString.Parser.count :: Int -> Parser a -> Parser [a] #-}
  count :: (Monad m) => Int -> m a -> m [a]
  count n p = Prelude.sequence (Prelude.replicate n p)


  {-# INLINE CONLIKE eitherP #-}
  {-# SPECIALIZE eitherP :: Parser a -> Parser b -> Parser (Either a b) #-}
  eitherP :: (Alternative f) => f a -> f b -> f (Either a b)
  eitherP left right = (Left <$> left) <|> (Right <$> right)


  {-# INLINE CONLIKE option #-}
  {-# SPECIALIZE option :: a -> Parser a -> Parser a #-}
  option :: (Alternative f) => a -> f a -> f a
  option dfl par = fromMaybe dfl <$> optional par


  {-# INLINE many1 #-}
  {-# SPECIALIZE many1 :: Parser a -> Parser [a] #-}
  many1 :: (Alternative f) => f a -> f [a]
  many1 = some


  {-# INLINE CONLIKE manyTill #-}
  {-# SPECIALIZE manyTill :: Parser a -> Parser a -> Parser [a] #-}
  manyTill :: (Alternative f) => f a -> f a -> f [a]
  manyTill par stop = loop
    where loop = (stop *> pure []) <|> ((:) <$> par <*> loop)


  {-# INLINE CONLIKE sepBy #-}
  {-# SPECIALIZE sepBy :: Parser a -> Parser b -> Parser [a] #-}
  sepBy :: (Alternative f) => f a -> f b -> f [a]
  sepBy par sep = sepBy1 par sep <|> pure []


  {-# INLINE CONLIKE sepBy1 #-}
  {-# SPECIALIZE sepBy1 :: Parser a -> Parser b -> Parser [a] #-}
  sepBy1 :: (Alternative f) => f a -> f b -> f [a]
  sepBy1 par sep = loop
    where loop = (:) <$> par <*> ((sep *> loop) <|> pure [])


  {-# INLINE CONLIKE wrap #-}
  {-# SPECIALIZE wrap :: Parser a -> Parser b -> Parser a #-}
  wrap :: (Applicative f) => f a -> f b -> f a
  wrap par wrapper = wrapper *> par <* wrapper


  {-# INLINE CONLIKE match #-}
  match :: Parser a -> Parser (ByteString, a)
  match par = Parser \inp ->
    case runParser par inp of
      Nothing -> Nothing
      Just (x, more) ->
        let n = length more
         in Just ((BS.take n inp, x), more)


  {-# INLINE takeByteString #-}
  takeByteString :: Parser ByteString
  takeByteString = Parser \inp -> Just (inp, mempty)


  {-# INLINE endOfInput #-}
  endOfInput :: Parser ()
  endOfInput = Parser \case
    inp | null inp  -> Just ((), inp)
    _otherwise      -> Nothing


  {-# INLINE atEnd #-}
  atEnd :: Parser Bool
  atEnd = Parser \inp -> Just (null inp, inp)


-- vim:set ft=haskell sw=2 ts=2 et:
