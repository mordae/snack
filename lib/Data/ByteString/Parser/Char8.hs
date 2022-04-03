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

  import Data.Maybe
  import Data.Word
  import GHC.Base (unsafeChr)

  import Data.ByteString as BS
  import Data.ByteString.Unsafe as BS

  import Data.ByteString.Parser
    hiding ( satisfy, scan, runScanner
           , takeTill, takeWhile, takeTill1, takeWhile1
           )

  import qualified Data.ByteString.Lex.Fractional as LF
  import qualified Data.ByteString.Lex.Integral as LI
  import qualified Data.CaseInsensitive as CI


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


  {-# INLINE w2c #-}
  w2c :: Word8 -> Char
  w2c = unsafeChr . fromIntegral


-- vim:set ft=haskell sw=2 ts=2 et:
