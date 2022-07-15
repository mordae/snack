-- |
-- Module      :  Data.ByteString.Parser
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides a parser for 'ByteString'.
--
--   * If you\'d like to parse ASCII text, you might want to take a look at
--     "Data.ByteString.Parser.Char8". It reuses the same 'Parser', but
--     provides functions working with 'Char' instead of 'Word8' as well as
--     more string utilities.
--
--   * If you\'d like to parse Unicode text, look instead at the
--     "Data.Text.Parser". Is is slower, but in a way more correct.
--

module Data.ByteString.Parser
  ( Parser(..)
  , Result(..)
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
  , branch
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
  , label
  , unlabel
  , validate

    -- * End Of Input
  , takeByteString
  , peekByteString
  , endOfInput
  , atEnd

    -- * Position
  , offset

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

  import Data.List qualified as List
  import Data.Maybe
  import Data.Word

  import Data.ByteString as BS
  import Data.ByteString.Unsafe as BS

  import Snack.Combinators


  -- |
  -- Result represents either success or some kind of failure.
  --
  -- You can find the problematic offset by subtracting length of the
  -- remainder from length of the original input.
  --
  data Result a
    = Success a {-# UNPACK #-} !ByteString
      -- ^ Parser successfully matched the input.
      --   Produces the parsing result and the remainder of the input.

    | Failure [String] {-# UNPACK #-} !ByteString
      -- ^ Parser failed to match the input.
      --   Produces list of expected inputs and the corresponding remainder.

    | Error String {-# UNPACK #-} !ByteString {-# UNPACK #-} !Int
      -- ^ Parser ran into an error. Either syntactic or a validation one.

    deriving (Eq, Show)

  instance Functor Result where
    {-# INLINE fmap #-}
    fmap fn (Success res more) = Success (fn res) more
    fmap _  (Failure expected more) = Failure expected more
    fmap _  (Error reason more len) = Error reason more len


  -- |
  -- Parser for 'ByteString' inputs.
  --
  newtype Parser a =
    Parser
      { runParser :: ByteString -> Result a
        -- ^ Run the parser on specified input.
      }

  instance Functor Parser where
    {-# INLINE fmap #-}
    fmap fn Parser{runParser} = Parser \inp ->
      fmap fn (runParser inp)

  instance Applicative Parser where
    {-# INLINE pure #-}
    pure x = Parser \inp -> Success x inp

    {-# INLINE (<*>) #-}
    (Parser runFn) <*> (Parser runArg) = Parser \inp ->
      case runFn inp of
        Error reason more len -> Error reason more len
        Failure expected more -> Failure expected more
        Success fn rest -> fmap fn (runArg rest)

  instance Alternative Parser where
    {-# INLINE empty #-}
    empty = Parser \inp -> Failure [] inp

    -- |
    -- Tries the right branch only if the left brach produces Failure.
    -- Does not mask Error.
    --
    {-# INLINE (<|>) #-}
    (Parser runLeft) <|> (Parser runRight) = Parser \inp ->
      case runLeft inp of
        Success res more -> Success res more
        Error reason more len -> Error reason more len
        Failure expected more ->
          case runRight inp of
            Success res' more' -> Success res' more'
            Error reason' more' len' -> Error reason' more' len'
            Failure expected' more' ->
              -- Longer match (shorter remainder) wins.
              case length more `compare` length more' of
                LT -> Failure expected more
                EQ -> Failure (expected <> expected') more
                GT -> Failure expected' more'

  instance Monad Parser where
    {-# INLINE (>>=) #-}
    (Parser runLeft) >>= right = Parser \inp ->
      case runLeft inp of
        Error reason more len -> Error reason more len
        Failure expected more -> Failure expected more
        Success res more -> runParser (right res) more

  instance MonadPlus Parser

  instance MonadFail Parser where
    -- |
    -- Fail the whole parser with given reason.
    --
    -- If you want the best error report possible, fail at the end of a
    -- relevant 'extent'.
    --
    -- For example, if you are parsing a mapping that is syntactically valid,
    -- but does not contain some mandatory keys, fail after parsing the whole
    -- mapping and make sure that the maaping parser and the 'fail' call are
    -- enclosed in an 'extent'.
    --
    -- That way, the error will indicate the extent remainder and length.
    --
    {-# INLINE CONLIKE fail #-}
    fail reason = Parser \inp -> Error reason inp 0


  -- |
  -- Accepts a single, matching byte.
  --
  {-# INLINE CONLIKE byte #-}
  byte :: Word8 -> Parser Word8
  byte c = satisfy (c ==)


  -- |
  -- Accepts a single, differing byte.
  --
  {-# INLINE CONLIKE notByte #-}
  notByte :: Word8 -> Parser Word8
  notByte c = satisfy (c /=)


  -- |
  -- Discards the remaining input and returns just the parse result.
  -- You might want to combine it with 'endOfInput' for the best effect.
  --
  -- Example:
  --
  -- @
  -- parseOnly (pContacts \<* endOfInput) bstr
  -- @
  --
  {-# INLINE CONLIKE parseOnly #-}
  parseOnly :: Parser a -> ByteString -> Either String a
  parseOnly par = \inp ->
    case runParser par inp of
      Success res _ -> Right res
      Error reason _ _ -> Left reason
      Failure expected _ ->
        case expected of
          [] -> Left $ "Unexpected input."
          ex -> Left $ "Expected " <> List.intercalate ", " ex <> "."


  -- |
  -- Accepts a single byte.
  --
  {-# INLINE anyByte #-}
  anyByte :: Parser Word8
  anyByte = Parser \inp ->
    if null inp
       then Failure ["any byte"] inp
       else Success (unsafeHead inp) (unsafeTail inp)


  -- |
  -- Accepts a single byte matching the predicate.
  --
  {-# INLINE CONLIKE satisfy #-}
  satisfy :: (Word8 -> Bool) -> Parser Word8
  satisfy isOk = Parser \inp ->
    if null inp
       then Failure ["more input"] inp
       else let c = unsafeHead inp
             in if isOk c
                   then Success c (unsafeTail inp)
                   else Failure [] inp


  -- |
  -- Peeks ahead, but does not consume.
  --
  -- Be careful, peeking behind end of the input fails.
  -- You might want to check using 'atEnd' beforehand.
  --
  {-# INLINE peekByte #-}
  peekByte :: Parser Word8
  peekByte = Parser \inp ->
    if null inp
       then Failure ["more input"] inp
       else Success (unsafeHead inp) inp


  -- |
  -- Accepts a matching string.
  --
  {-# INLINE CONLIKE string #-}
  string :: ByteString -> Parser ByteString
  string str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case pfx == str of
          True -> Success pfx sfx
          False -> Failure [show str] inp


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
  scan :: s -> (s -> Word8 -> Maybe s) -> Parser ByteString
  scan state scanner = fst <$> runScanner state scanner


  -- |
  -- Like 'scan', but also returns the final scanner state.
  --
  {-# INLINE CONLIKE runScanner #-}
  runScanner :: s -> (s -> Word8 -> Maybe s) -> Parser (ByteString, s)
  runScanner state scanner = Parser \inp ->
    let (state', n) = scanBytes state scanner 0 (unpack inp)
        (res, more) = splitAt n inp
     in Success (res, state') more


  {-# INLINE scanBytes #-}
  scanBytes :: s -> (s -> Word8 -> Maybe s) -> Int -> [Word8] -> (s, Int)
  scanBytes !state _scanner !n [] = (state, n)
  scanBytes !state scanner !n (x:more) =
    case scanner state x of
      Just state' -> scanBytes state' scanner (succ n) more
      Nothing -> (state, n)


  -- |
  -- Efficiently consume as long as the input bytes match the predicate.
  -- An inverse of 'takeTill'.
  --
  {-# INLINE CONLIKE takeWhile #-}
  takeWhile :: (Word8 -> Bool) -> Parser ByteString
  takeWhile test = takeTill (not . test)


  -- |
  -- Like 'Data.ByteString.Parser.takeWhile', but requires at least a single byte.
  --
  {-# INLINE CONLIKE takeWhile1 #-}
  takeWhile1 :: (Word8 -> Bool) -> Parser ByteString
  takeWhile1 test = Data.ByteString.Parser.takeWhile test `provided` (not . null)


  -- |
  -- Efficiently consume until a byte matching the predicate is found.
  -- An inverse of 'Data.ByteString.Parser.takeWhile'.
  --
  {-# INLINE CONLIKE takeTill #-}
  takeTill :: (Word8 -> Bool) -> Parser ByteString
  takeTill test = Parser \inp ->
    let n = fromMaybe (length inp) $ findIndex test inp
     in Success (unsafeTake n inp) (unsafeDrop n inp)


  -- |
  -- Same as 'takeTill', but requires at least a single byte.
  --
  {-# INLINE CONLIKE takeTill1 #-}
  takeTill1 :: (Word8 -> Bool) -> Parser ByteString
  takeTill1 test = Data.ByteString.Parser.takeTill test `provided` (not . null)


  -- |
  -- Makes the parser not only return the result, but also the original
  -- matched extent.
  --
  {-# INLINE CONLIKE match #-}
  match :: Parser a -> Parser (ByteString, a)
  match par = Parser \inp ->
    case runParser par inp of
      Failure expected more -> Failure expected more
      Error reason more len -> Error reason more len
      Success res more ->
        let n = length more
         in Success (BS.take n inp, res) more


  -- |
  -- Names an extent of the parser.
  --
  -- When the extent returns a Failure, details are discarded and replaced
  -- with the extent as a whole.
  --
  -- When the extent returns an Error, it is adjusted to cover the whole
  -- extent, but the reason is left intact.
  --
  -- You should strive to make labeled extents as small as possible,
  -- approximately of a typical token size. For example:
  --
  -- @
  -- pString = label \"string\" $ pStringContents \`wrap\` char \'\"\'
  -- @
  --
  {-# INLINE CONLIKE label #-}
  label :: String -> Parser a -> Parser a
  label lbl par = Parser \inp ->
    case runParser par inp of
      Success res more -> Success res more
      Failure _expected _more -> Failure [lbl] inp
      Error reason more len ->
        let len' = len + (length inp - length more)
         in Error reason inp len'


  -- |
  -- Un-names an extent of the parser.
  --
  -- Same as 'label', but removes any expected values upon Failure.
  -- Very useful to mark comments and optional whitespace with.
  --
  {-# INLINE CONLIKE unlabel #-}
  unlabel :: Parser a -> Parser a
  unlabel par = Parser \inp ->
    case runParser par inp of
      Success res more -> Success res more
      Failure _expected _more -> Failure [] inp
      Error reason more len ->
        let len' = len + (length inp - length more)
         in Error reason inp len'


  -- |
  -- Validate parser result and turn it into an Error upon failure.
  --
  {-# INLINE CONLIKE validate #-}
  validate :: (a -> Either String b) -> Parser a -> Parser b
  validate test par = Parser \inp ->
    case runParser par inp of
      Failure expected more -> Failure expected more
      Error reason more len -> Error reason more len
      Success res more ->
        case test res of
          Right res' -> Success res' more
          Left reason -> Error reason inp (length inp - length more)


  -- |
  -- Given list of matchers and parsers, runs the first parser whose matcher
  -- succeeds on the input. This pattern makes for a simpler alternative to
  -- @try@ used in other parser combinator libraries.
  --
  -- Example:
  --
  -- @
  -- pProperty = branch [ ( string "public" <* skipSpace
  --                      , \_ -> Property Public <$> pToken
  --                      )
  --                    , ( string "private" <* skipSpace
  --                      , \_ -> Property Private <$> pToken
  --                      )
  --                    ]
  -- @
  --
  {-# INLINE CONLIKE branch #-}
  branch :: [(Parser a, a -> Parser b)] -> Parser b
  branch [] = Parser \inp -> Failure [] inp
  branch ((Parser test, finish) : alts) =
    Parser \inp ->
      case test inp of
        Success res more -> runParser (finish res) more
        Error reason more len -> Error reason more len
        Failure _expected _more -> runParser (branch alts) inp


  -- |
  -- Accept whatever input remains.
  --
  {-# INLINE takeByteString #-}
  takeByteString :: Parser ByteString
  takeByteString = Parser \inp -> Success inp mempty


  -- |
  -- Peek at whatever input remains.
  --
  {-# INLINE peekByteString #-}
  peekByteString :: Parser ByteString
  peekByteString = Parser \inp -> Success inp inp


  -- |
  -- Accepts end of input and fails if we are not there yet.
  --
  {-# INLINE endOfInput #-}
  endOfInput :: Parser ()
  endOfInput = Parser \case
    inp | null inp  -> Success () inp
    inp             -> Failure ["end of input"] inp


  -- |
  -- Returns whether we are at the end of the input yet.
  --
  {-# INLINE atEnd #-}
  atEnd :: Parser Bool
  atEnd = Parser \inp -> Success (null inp) inp


  -- |
  -- Calculate offset from the original input and the remainder.
  --
  offset :: ByteString -> ByteString -> Int
  offset inp more = length inp - length more


-- vim:set ft=haskell sw=2 ts=2 et:
