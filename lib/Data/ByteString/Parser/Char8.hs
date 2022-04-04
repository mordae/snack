-- |
-- Module      :  Data.ByteString.Parser.Char8
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides a parser for ASCII 'ByteString'.
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


  -- |
  -- Accepts a single, matching ASCII character.
  --
  {-# INLINE CONLIKE char #-}
  char :: Char -> Parser Char
  char c = satisfy (c ==)


  -- |
  -- Accepts a single, differing ASCII character.
  --
  {-# INLINE CONLIKE notChar #-}
  notChar :: Char -> Parser Char
  notChar c = satisfy (c /=)


  -- |
  -- Accepts a single character.
  --
  {-# INLINE anyChar #-}
  anyChar :: Parser Char
  anyChar = Parser \inp ->
    if null inp
       then Nothing
       else Just (w2c (unsafeHead inp), unsafeTail inp)


  -- |
  -- Accepts a single character matching the predicate.
  --
  {-# INLINE CONLIKE satisfy #-}
  satisfy :: (Char -> Bool) -> Parser Char
  satisfy isOk = Parser \inp ->
    if null inp
       then Nothing
       else let c = w2c (unsafeHead inp)
             in if isOk c
                   then Just (c, unsafeTail inp)
                   else Nothing


  -- |
  -- Accepts a single ASCII white space character.
  -- See 'isSpace' for details.
  --
  {-# INLINE space #-}
  space :: Parser Char
  space = satisfy isSpace


  -- |
  -- Accepts multiple ASCII white space characters.
  -- See 'isSpace' for details.
  --
  {-# INLINE skipSpace #-}
  skipSpace :: Parser ()
  skipSpace = void $ Data.ByteString.Parser.Char8.takeWhile isSpace


  -- |
  -- True for any of the @[' ', '\\t', '\\n', '\\v', '\\f', '\\r']@ characters.
  --
  -- Please note that "Data.Text.Parser" re-exports 'Data.Char.isString', that
  -- considers more unicode codepoints, making it significantly slower.
  --
  {-# INLINE isSpace #-}
  isSpace :: Char -> Bool
  isSpace c = (c == ' ') || ('\t' <= c && c <= '\r')


  -- |
  -- Peeks ahead, but does not consume.
  --
  -- Be careful, peeking behind end of the input fails.
  -- You might want to check using 'atEnd' beforehand.
  --
  {-# INLINE peekChar #-}
  peekChar :: Parser Char
  peekChar = Parser \inp ->
    if null inp
       then Nothing
       else Just (w2c (unsafeHead inp), inp)


  -- |
  -- Accepts a matching string.
  -- Matching is performed in a case-insensitive manner under ASCII.
  --
  {-# INLINE CONLIKE stringCI #-}
  stringCI :: ByteString -> Parser ByteString
  stringCI str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case toCaseFold pfx == toCaseFold str of
          True -> Just (pfx, sfx)
          False -> Nothing


  -- |
  -- Perform simple ASCII case folding.
  --
  {-# INLINE toCaseFold #-}
  toCaseFold :: ByteString -> ByteString
  toCaseFold = BS.map foldCase
    where foldCase w | 65 <= w && w <= 90 = w + 32
          foldCase w = w


  -- |
  -- Accepts given number of characters.
  -- Fails when not enough characters are available.
  --
  {-# INLINE CONLIKE take #-}
  take :: Int -> Parser ByteString
  take n = Parser \inp ->
    if n > length inp
       then Nothing
       else Just (splitAt n inp)


  -- |
  -- Scans ahead statefully and then accepts whatever bytes the scanner liked.
  -- Scanner returns 'Nothing' to mark end of the acceptable extent.
  --
  {-# INLINE CONLIKE scan #-}
  scan :: s -> (s -> Char -> Maybe s) -> Parser ByteString
  scan state scanner = fst <$> runScanner state scanner


  -- |
  -- Like 'scan', but also returns the final scanner state.
  --
  {-# INLINE CONLIKE runScanner #-}
  runScanner :: s -> (s -> Char -> Maybe s) -> Parser (ByteString, s)
  runScanner state scanner = Parser \inp ->
    let (more, state') = scanStep scanner state inp
        res = unsafeTake (length inp - length more) inp
     in Just ((res, state'), more)


  scanStep :: (s -> Char -> Maybe s) -> s -> ByteString -> (ByteString, s)
  scanStep scanner !state !inp =
    case null inp of
      True -> (inp, state)
      False ->
        case scanner state (w2c (unsafeHead inp)) of
          Nothing -> (inp, state)
          Just state' ->
            scanStep scanner state' (unsafeDrop 1 inp)


  -- |
  -- Determine whether is the character member of the class specified
  -- like @\"A-Za-z0-9_-\"@. If you want to include @\'-\'@, put it either
  -- first or last.
  --
  -- Example:
  --
  -- @
  -- pValue = takeWhile1 (inClass "A-Za-z0-9_")
  -- @
  --
  {-# INLINE CONLIKE inClass #-}
  inClass :: [Char] -> Char -> Bool
  inClass (x:'-':y:rest)  = \c -> (x <= c && c <= y) || inClass rest c
  inClass (x:rest)        = \c -> (x == c) || inClass rest c
  inClass []              = \_ -> False


  -- |
  -- Negated 'inClass', faster than using @not . inClass@.
  --
  {-# INLINE CONLIKE notInClass #-}
  notInClass :: [Char] -> Char -> Bool
  notInClass (x:'-':y:rest) = \c -> (x > c || c > y) || notInClass rest c
  notInClass (x:rest)       = \c -> (x /= c) || notInClass rest c
  notInClass []             = \_ -> False


  -- |
  -- Efficiently consume as long as the input characters match the predicate.
  -- An inverse of 'takeTill'.
  --
  {-# INLINE CONLIKE takeWhile #-}
  takeWhile :: (Char -> Bool) -> Parser ByteString
  takeWhile test = takeTill (not . test)


  -- |
  -- Like 'takeWhile', but requires at least a single character.
  --
  {-# INLINE CONLIKE takeWhile1 #-}
  takeWhile1 :: (Char -> Bool) -> Parser ByteString
  takeWhile1 test = provided (not . null) $
                    Data.ByteString.Parser.Char8.takeWhile test


  -- |
  -- Efficiently consume until a character matching the predicate is found.
  -- An inverse of 'Data.ByteString.Parser.Char8.takeWhile'.
  --
  {-# INLINE CONLIKE takeTill #-}
  takeTill :: (Char -> Bool) -> Parser ByteString
  takeTill test = Parser \inp ->
    let n = fromMaybe (length inp) $ findIndex (test . w2c) inp
     in Just (splitAt n inp)


  -- |
  -- Same as 'takeTill', but requires at least a single character.
  --
  {-# INLINE CONLIKE takeTill1 #-}
  takeTill1 :: (Char -> Bool) -> Parser ByteString
  takeTill1 test = provided (not . null) $
                    Data.ByteString.Parser.Char8.takeTill test


  -- |
  -- Accepts optional @\'+\'@ or @\'-\'@ character and then applies it to
  -- the following parser result.
  --
  {-# INLINE signed #-}
  signed :: (Num a) => Parser a -> Parser a
  signed runNumber = (char '-' *> fmap negate runNumber)
                 <|> (char '+' *> runNumber)
                 <|> (runNumber)


  -- |
  -- Accepts an integral number in the decimal format.
  --
  {-# INLINE decimal #-}
  decimal :: (Integral a) => Parser a
  decimal = Parser LI.readDecimal


  -- |
  -- Accepts an integral number in the hexadecimal format in either case.
  -- Does not look for @0x@ or similar prefixes.
  --
  {-# INLINE hexadecimal #-}
  hexadecimal :: (Integral a) => Parser a
  hexadecimal = Parser LI.readHexadecimal


  -- |
  -- Accepts an integral number in the octal format.
  --
  {-# INLINE octal #-}
  octal :: (Integral a) => Parser a
  octal = Parser LI.readOctal


  -- |
  -- Accepts a fractional number as a decimal optinally followed by a colon
  -- and the fractional part. Does not support exponentiation.
  --
  {-# INLINE fractional #-}
  fractional :: (Fractional a) => Parser a
  fractional = Parser LF.readDecimal


  {-# INLINE w2c #-}
  w2c :: Word8 -> Char
  w2c = unsafeChr . fromIntegral


-- vim:set ft=haskell sw=2 ts=2 et:
