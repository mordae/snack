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
--   * If you\'d like to parse Unicode text, look instead at the
--     "Data.Text.Parser". Is is slower, but in a way more correct.
--
--   * If you\'d like to parse byte sequences, look instead at the
--     "Data.ByteString.Parser". It reuses the same 'Parser', but
--     provides functions working with 'Word8' instead of 'Char'.
--

module Data.ByteString.Parser.Char8
  ( Parser(..)
  , Result(..)
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
  , inRange
  , notInRange
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
  , label
  , unlabel
  , commit
  , validate

    -- * End Of Input
  , takeByteString
  , peekByteString
  , endOfInput
  , atEnd

    -- * Position
  , offset
  , position
  , explain
  , Explanation(..)

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
  import Data.List qualified as List
  import GHC.Base (unsafeChr)

  import Data.ByteString as BS
  import Data.ByteString.Unsafe as BS

  import Snack.Combinators

  import Data.ByteString.Parser ( Parser(..), Result(..), parseOnly
                                , string, count, match, label, unlabel, commit
                                , validate
                                , takeByteString, peekByteString
                                , endOfInput, atEnd, offset
                                )

  import Data.ByteString.Lex.Fractional qualified as LF
  import Data.ByteString.Lex.Integral qualified as LI


  -- |
  -- Accepts a single, matching ASCII character.
  --
  {-# INLINE CONLIKE char #-}
  char :: Char -> Parser Char
  char c = label (show c) $ satisfy (c ==)


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
       then Failure ["any character"] inp
       else Success (w2c (unsafeHead inp)) (unsafeTail inp)


  -- |
  -- Accepts a single character matching the predicate.
  --
  {-# INLINE CONLIKE satisfy #-}
  satisfy :: (Char -> Bool) -> Parser Char
  satisfy isOk = Parser \inp ->
    if null inp
       then Failure ["more input"] inp
       else let c = w2c (unsafeHead inp)
             in if isOk c
                   then Success c (unsafeTail inp)
                   else Failure [] inp


  -- |
  -- Accepts a single ASCII white space character.
  -- See 'isSpace' for details.
  --
  {-# INLINE space #-}
  space :: Parser Char
  space = label "space" $ satisfy isSpace


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
       then Failure ["more input"] inp
       else Success (w2c (unsafeHead inp)) inp


  -- |
  -- Accepts a matching string.
  -- Matching is performed in a case-insensitive manner under ASCII.
  --
  {-# INLINE CONLIKE stringCI #-}
  stringCI :: ByteString -> Parser ByteString
  stringCI str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case toCaseFold pfx == toCaseFold str of
          True -> Success pfx sfx
          False -> Failure [show str] inp


  -- |
  -- Perform simple ASCII case folding.
  --
  {-# INLINE toCaseFold #-}
  toCaseFold :: ByteString -> ByteString
  toCaseFold = BS.map foldCase
    where foldCase w | 65 <= w && w <= 90 = w + 32
          foldCase w = w


  -- |
  -- Accepts given number of bytes.
  -- Fails when not enough bytes are available.
  --
  {-# INLINE CONLIKE take #-}
  take :: Int -> Parser ByteString
  take n = Parser \inp ->
    if n > length inp
       then Failure [show n <> " more bytes"] inp
       else Success (unsafeTake n inp) (unsafeDrop n inp)


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
  runScanner state scanner = Parser \inp -> loop inp state 0
    where
      loop inp !st !n =
        case n >= length inp of
          True -> Success (inp, st) mempty
          False ->
            case unsafeIndex inp n of
              w ->
                case scanner st (w2c w) of
                  Nothing -> Success (unsafeTake n inp, st) (unsafeDrop n inp)
                  Just st' -> loop inp st' (succ n)


  -- |
  -- Efficiently consume as long as the input characters match the predicate.
  -- An inverse of 'takeTill'.
  --
  {-# INLINE CONLIKE takeWhile #-}
  takeWhile :: (Char -> Bool) -> Parser ByteString
  takeWhile test = takeTill (not . test)


  -- |
  -- Like 'Data.ByteString.Parser.Char8.takeWhile',
  -- but requires at least a single character.
  --
  {-# INLINE CONLIKE takeWhile1 #-}
  takeWhile1 :: (Char -> Bool) -> Parser ByteString
  takeWhile1 test = Data.ByteString.Parser.Char8.takeWhile test `provided` (not . null)


  -- |
  -- Efficiently consume until a character matching the predicate is found.
  -- An inverse of 'Data.ByteString.Parser.Char8.takeWhile'.
  --
  {-# INLINE CONLIKE takeTill #-}
  takeTill :: (Char -> Bool) -> Parser ByteString
  takeTill test = Parser \inp ->
    let n = fromMaybe (length inp) $ findIndex (test . w2c) inp
     in Success (unsafeTake n inp) (unsafeDrop n inp)


  -- |
  -- Same as 'takeTill', but requires at least a single character.
  --
  {-# INLINE CONLIKE takeTill1 #-}
  takeTill1 :: (Char -> Bool) -> Parser ByteString
  takeTill1 test = Data.ByteString.Parser.Char8.takeTill test `provided` (not . null)


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
  decimal = Parser \inp ->
    case LI.readDecimal inp of
      Just (res, more) -> Success res more
      Nothing -> Failure ["decimal"] inp


  -- |
  -- Accepts an integral number in the hexadecimal format in either case.
  -- Does not look for @0x@ or similar prefixes.
  --
  {-# INLINE hexadecimal #-}
  hexadecimal :: (Integral a) => Parser a
  hexadecimal = Parser \inp ->
    case LI.readHexadecimal inp of
      Just (res, more) -> Success res more
      Nothing -> Failure ["hexadecimal"] inp


  -- |
  -- Accepts an integral number in the octal format.
  --
  {-# INLINE octal #-}
  octal :: (Integral a) => Parser a
  octal = Parser \inp ->
    case LI.readOctal inp of
      Just (res, more) -> Success res more
      Nothing -> Failure ["octal"] inp


  -- |
  -- Accepts a fractional number as a decimal optinally followed by a colon
  -- and the fractional part. Does not support exponentiation.
  --
  {-# INLINE fractional #-}
  fractional :: (Fractional a) => Parser a
  fractional = Parser \inp ->
    case LF.readDecimal inp of
      Just (res, more) -> Success res more
      Nothing -> Failure ["fractional"] inp


  {-# INLINE w2c #-}
  w2c :: Word8 -> Char
  w2c = unsafeChr . fromIntegral


  -- |
  -- Determine @(line, column)@ from the original input and the remainder.
  --
  -- Counts line feed characters leading to the 'offset', so only use it
  -- on your slow path. For example when describing parsing errors.
  --
  position :: ByteString -> ByteString -> (Int, Int)
  position inp more = (succ line, succ column)
    where
      column = length lastLine
      lastLine = takeWhileEnd (10 /=) leader
      line = BS.count 10 leader
      leader = dropEnd (length more) inp


  -- |
  -- More precise 'Result' description produced by 'explain'.
  --
  data Explanation
    = Explanation
      { exSource       :: String
        -- ^ Name of the source file.
      , exSpanFrom     :: (Int, Int)
        -- ^ Line and column where the problem starts.
      , exSpanTo       :: (Int, Int)
        -- ^ Line and column where the problem ends.
      , exMessage      :: String
        -- ^ A message associated with the problem.
      }
    deriving (Eq, Show)


  -- |
  -- Process the result for showing it to the user.
  --
  explain :: String -> ByteString -> Result a -> Explanation
  explain src inp (Success _ more) =
    Explanation { exSource   = src
                , exSpanFrom = pos
                , exSpanTo   = pos
                , exMessage  = "Parsed successfully up to this point."
                }
      where
        pos = position inp more


  explain src inp (Failure expected more) =
    Explanation { exSource   = src
                , exSpanFrom = pos
                , exSpanTo   = pos
                , exMessage =
                    case expected of
                      [] -> "Unexpected input."
                      ex -> "Expected " <> List.intercalate ", " ex <> "."
                }
      where
        pos = position inp more

  explain src inp (Error reason more len) =
    Explanation { exSource   = src
                , exSpanFrom = from
                , exSpanTo   = to
                , exMessage  = reason
                }
      where
        from = position inp more
        to   = position inp (BS.drop len more)


-- vim:set ft=haskell sw=2 ts=2 et:
