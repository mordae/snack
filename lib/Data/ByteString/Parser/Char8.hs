-- |
-- Module      :  Data.ByteString.Parser.Char8
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Data.ByteString.Parser.Char8
  ( Parser(..)
  , parseOnly

    -- * Characters
  , char
  , notChar
  , anyChar
  , satisfy
  , space
  , isSpace
  , skipSpace
  , peekChar

    -- * Strings
  , string
  , stringCI
  , Data.ByteString.Parser.Char8.take
  , scan
  , runScanner
  , inClass
  , notInClass
  , Data.ByteString.Parser.Char8.takeWhile
  , takeWhile1
  , takeTill
  , takeTill1

    -- * Numbers
  , signed
  , decimal
  , hexadecimal
  , octal
  , fractional

    -- * Combinators
  , provided
  , choice
  , Data.ByteString.Parser.Char8.count
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
  import GHC.Base (unsafeChr)

  import Data.ByteString as BS
  import Data.ByteString.Unsafe as BS

  import qualified Data.ByteString.Lex.Fractional as LF
  import qualified Data.ByteString.Lex.Integral as LI
  import qualified Data.CaseInsensitive as CI


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


  {-# INLINE CONLIKE char #-}
  char :: Char -> Parser Char
  char c = satisfy (c ==)


  {-# INLINE space #-}
  space :: Parser Char
  space = satisfy isSpace


  {-# INLINE skipSpace #-}
  skipSpace :: Parser ()
  skipSpace = void $ Data.ByteString.Parser.Char8.takeWhile isSpace


  {-# INLINE isSpace #-}
  isSpace :: Char -> Bool
  isSpace c = (c == ' ') || ('\t' <= c && c <= '\r')


  {-# INLINE CONLIKE notChar #-}
  notChar :: Char -> Parser Char
  notChar c = satisfy (c /=)


  {-# INLINE anyChar #-}
  anyChar :: Parser Char
  anyChar = Parser \inp ->
    if null inp
       then Nothing
       else Just (w2c (unsafeHead inp), unsafeTail inp)


  {-# INLINE CONLIKE satisfy #-}
  satisfy :: (Char -> Bool) -> Parser Char
  satisfy isOk = Parser \inp ->
    if null inp
       then Nothing
       else let c = w2c (unsafeHead inp)
             in if isOk c
                   then Just (c, unsafeTail inp)
                   else Nothing


  {-# INLINE peekChar #-}
  peekChar :: Parser Char
  peekChar = Parser \inp ->
    if null inp
       then Nothing
       else Just (w2c (unsafeHead inp), inp)


  {-# INLINE CONLIKE string #-}
  string :: ByteString -> Parser ByteString
  string str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case pfx == str of
          True -> Just (pfx, sfx)
          False -> Nothing


  {-# INLINE CONLIKE stringCI #-}
  stringCI :: ByteString -> Parser ByteString
  stringCI str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case CI.mk pfx == CI.mk str of
          True -> Just (pfx, sfx)
          False -> Nothing


  {-# INLINE CONLIKE take #-}
  take :: Int -> Parser ByteString
  take n = Parser \inp ->
    if n > length inp
       then Nothing
       else Just (splitAt n inp)


  {-# INLINE CONLIKE scan #-}
  scan :: s -> (s -> Char -> Maybe s) -> Parser ByteString
  scan state scanner = fst <$> runScanner state scanner


  {-# INLINE CONLIKE runScanner #-}
  runScanner :: s -> (s -> Char -> Maybe s) -> Parser (ByteString, s)
  runScanner state scanner = Parser \inp ->
    let (state', n) = scanBytes state scanner 0 (unpack inp)
        (res, more) = splitAt n inp
     in Just ((res, state'), more)


  {-# INLINE scanBytes #-}
  scanBytes :: s -> (s -> Char -> Maybe s) -> Int -> [Word8] -> (s, Int)
  scanBytes !state _scanner !n [] = (state, n)
  scanBytes !state scanner !n (x:more) =
    case scanner state (w2c x) of
      Just state' -> scanBytes state' scanner (succ n) more
      Nothing -> (state, n)


  {-# INLINE CONLIKE inClass #-}
  inClass :: [Char] -> Char -> Bool
  inClass (x:'-':y:rest)  = \c -> (x <= c && c <= y) || inClass rest c
  inClass (x:rest)        = \c -> (x == c) || inClass rest c
  inClass []              = \_ -> False


  {-# INLINE CONLIKE notInClass #-}
  notInClass :: [Char] -> Char -> Bool
  notInClass (x:'-':y:rest) = \c -> (x > c || c > y) || notInClass rest c
  notInClass (x:rest)       = \c -> (x /= c) || notInClass rest c
  notInClass []             = \_ -> False


  {-# INLINE CONLIKE takeWhile #-}
  takeWhile :: (Char -> Bool) -> Parser ByteString
  takeWhile test = takeTill (not . test)


  {-# INLINE CONLIKE takeWhile1 #-}
  takeWhile1 :: (Char -> Bool) -> Parser ByteString
  takeWhile1 test = provided (not . null) $
                    Data.ByteString.Parser.Char8.takeWhile test


  {-# INLINE CONLIKE takeTill #-}
  takeTill :: (Char -> Bool) -> Parser ByteString
  takeTill test = Parser \inp ->
    let n = fromMaybe (length inp) $ findIndex (test . w2c) inp
     in Just (splitAt n inp)


  {-# INLINE CONLIKE takeTill1 #-}
  takeTill1 :: (Char -> Bool) -> Parser ByteString
  takeTill1 test = provided (not . null) $
                    Data.ByteString.Parser.Char8.takeTill test


  {-# INLINE signed #-}
  signed :: (Num a) => Parser a -> Parser a
  signed runNumber = (char '-' *> fmap negate runNumber)
                 <|> (char '+' *> runNumber)
                 <|> (runNumber)


  {-# INLINE decimal #-}
  decimal :: (Integral a) => Parser a
  decimal = Parser LI.readDecimal


  {-# INLINE hexadecimal #-}
  hexadecimal :: (Integral a) => Parser a
  hexadecimal = Parser LI.readHexadecimal


  {-# INLINE octal #-}
  octal :: (Integral a) => Parser a
  octal = Parser LI.readOctal


  {-# INLINE fractional #-}
  fractional :: (Fractional a) => Parser a
  fractional = Parser LF.readDecimal


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
  {-# SPECIALIZE Data.ByteString.Parser.Char8.count :: Int -> Parser a -> Parser [a] #-}
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


  {-# INLINE w2c #-}
  w2c :: Word8 -> Char
  w2c = unsafeChr . fromIntegral


-- vim:set ft=haskell sw=2 ts=2 et:
