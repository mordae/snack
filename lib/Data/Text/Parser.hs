-- |
-- Module      :  Data.Text.Parser
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Data.Text.Parser
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
  , Data.Text.Parser.take
  , scan
  , runScanner
  , inClass
  , notInClass
  , Data.Text.Parser.takeWhile
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
  , Snack.Combinators.count
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
  , takeText
  , endOfInput
  , atEnd

    -- * Miscelaneous
    -- |
    -- These are all generic methods, but since I sometimes forget about them,
    -- it is nice to have them listed here for reference what writing parsers.
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

  import Data.Char
  import Data.Maybe

  import Data.Text as T
  import Data.Text.Unsafe as T
  import Data.Text.Encoding as T

  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Parser.Char8 as BSP

  import Snack.Combinators


  newtype Parser a =
    Parser
      { runParser :: Text -> Maybe (a, Text)
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
  parseOnly :: Parser a -> Text -> Maybe a
  parseOnly par = \inp -> fst <$> runParser par inp


  {-# INLINE CONLIKE satisfy #-}
  satisfy :: (Char -> Bool) -> Parser Char
  satisfy isOk = Parser \inp ->
    if null inp
       then Nothing
       else let c = unsafeHead inp
             in if isOk c
                   then Just (c, unsafeTail inp)
                   else Nothing


  {-# INLINE CONLIKE string #-}
  string :: Text -> Parser Text
  string str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case pfx == str of
          True -> Just (pfx, sfx)
          False -> Nothing


  {-# INLINE CONLIKE take #-}
  take :: Int -> Parser Text
  take n = Parser \inp ->
    if n > length inp
       then Nothing
       else Just (splitAt n inp)


  {-# INLINE CONLIKE scan #-}
  scan :: s -> (s -> Char -> Maybe s) -> Parser Text
  scan state scanner = fst <$> runScanner state scanner


  {-# INLINE CONLIKE runScanner #-}
  runScanner :: s -> (s -> Char -> Maybe s) -> Parser (Text, s)
  runScanner state scanner = Parser \inp ->
    let (state', n) = scanBytes state scanner 0 (unpack inp)
        (res, more) = splitAt n inp
     in Just ((res, state'), more)


  {-# INLINE scanBytes #-}
  scanBytes :: s -> (s -> Char -> Maybe s) -> Int -> [Char] -> (s, Int)
  scanBytes !state _scanner !n [] = (state, n)
  scanBytes !state scanner !n (x:more) =
    case scanner state x of
      Just state' -> scanBytes state' scanner (succ n) more
      Nothing -> (state, n)


  {-# INLINE CONLIKE takeWhile #-}
  takeWhile :: (Char -> Bool) -> Parser Text
  takeWhile test = takeTill (not . test)


  {-# INLINE CONLIKE takeWhile1 #-}
  takeWhile1 :: (Char -> Bool) -> Parser Text
  takeWhile1 test = provided (not . null) $
                    Data.Text.Parser.takeWhile test


  {-# INLINE CONLIKE takeTill #-}
  takeTill :: (Char -> Bool) -> Parser Text
  takeTill test = Parser \inp ->
    let n = fromMaybe (length inp) $ findIndex test inp
     in Just (splitAt n inp)


  {-# INLINE CONLIKE takeTill1 #-}
  takeTill1 :: (Char -> Bool) -> Parser Text
  takeTill1 test = provided (not . null) $
                    Data.Text.Parser.takeTill test


  {-# INLINE CONLIKE match #-}
  match :: Parser a -> Parser (Text, a)
  match par = Parser \inp ->
    case runParser par inp of
      Nothing -> Nothing
      Just (x, more) ->
        let n = length more
         in Just ((T.take n inp, x), more)


  {-# INLINE takeText #-}
  takeText :: Parser Text
  takeText = Parser \inp -> Just (inp, mempty)


  {-# INLINE endOfInput #-}
  endOfInput :: Parser ()
  endOfInput = Parser \case
    inp | null inp  -> Just ((), inp)
    _otherwise      -> Nothing


  {-# INLINE atEnd #-}
  atEnd :: Parser Bool
  atEnd = Parser \inp -> Just (null inp, inp)


  {-# INLINE CONLIKE char #-}
  char :: Char -> Parser Char
  char c = satisfy (c ==)


  {-# INLINE space #-}
  space :: Parser Char
  space = satisfy isSpace


  {-# INLINE skipSpace #-}
  skipSpace :: Parser ()
  skipSpace = void $ Data.Text.Parser.takeWhile isSpace


  {-# INLINE CONLIKE notChar #-}
  notChar :: Char -> Parser Char
  notChar c = satisfy (c /=)


  {-# INLINE anyChar #-}
  anyChar :: Parser Char
  anyChar = Parser \inp ->
    if null inp
       then Nothing
       else Just (unsafeHead inp, unsafeTail inp)


  {-# INLINE peekChar #-}
  peekChar :: Parser Char
  peekChar = Parser \inp ->
    if null inp
       then Nothing
       else Just (unsafeHead inp, inp)


  {-# INLINE CONLIKE stringCI #-}
  stringCI :: Text -> Parser Text
  stringCI str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case toCaseFold pfx == toCaseFold str of
          True -> Just (pfx, sfx)
          False -> Nothing


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


  {-# INLINE signed #-}
  signed :: (Num a) => Parser a -> Parser a
  signed runNumber = (char '-' *> fmap negate runNumber)
                 <|> (char '+' *> runNumber)
                 <|> (runNumber)


  {-# INLINE CONLIKE withUtf8 #-}
  withUtf8 :: BSP.Parser a -> Parser a
  withUtf8 bspar = Parser \inp ->
    let bstr = encodeUtf8 inp
     in case BSP.runParser bspar bstr of
          Nothing -> Nothing
          Just (x, more) ->
            let n = lengthWord8 inp - BS.length more
             in Just (x, dropWord8 n inp)


  {-# INLINE decimal #-}
  decimal :: (Integral a) => Parser a
  decimal = withUtf8 BSP.decimal


  {-# INLINE hexadecimal #-}
  hexadecimal :: (Integral a) => Parser a
  hexadecimal = withUtf8 BSP.hexadecimal


  {-# INLINE octal #-}
  octal :: (Integral a) => Parser a
  octal = withUtf8 BSP.octal


  {-# INLINE fractional #-}
  fractional :: (Fractional a) => Parser a
  fractional = withUtf8 BSP.fractional


-- vim:set ft=haskell sw=2 ts=2 et:
