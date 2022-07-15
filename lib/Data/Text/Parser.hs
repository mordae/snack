-- |
-- Module      :  Data.Text.Parser
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides a parser for unicode 'Text'.
--
--   * If you\'d like to parse ASCII text, you might want to take a look at
--     "Data.ByteString.Parser.Char8". It is much, much faster.
--
--   * If you\'d like to parse byte sequences, look instead at the
--     "Data.ByteString.Parser".
--

module Data.Text.Parser
  ( Parser(..)
  , Result(..)
  , parseOnly

    -- * Chars
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
  , takeText
  , peekText
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

  import Data.Char
  import Data.Maybe
  import Data.List qualified as List

  import Data.Text as T
  import Data.Text.Unsafe as T
  import Data.Text.Encoding as T

  import Data.ByteString qualified as BS
  import Data.ByteString.Lex.Fractional qualified as LF
  import Data.ByteString.Lex.Integral qualified as LI

  import Snack.Combinators


  -- |
  -- Result represents either success or some kind of failure.
  --
  -- You can find the problematic offset by subtracting length of the
  -- remainder from length of the original input.
  --
  data Result a
    = Success a {-# UNPACK #-} !Text
      -- ^ Parser successfully matched the input.
      --   Produces the parsing result and the remainder of the input.

    | Failure [String] {-# UNPACK #-} !Text
      -- ^ Parser failed to match the input.
      --   Produces list of expected inputs and the corresponding remainder.

    | Error String {-# UNPACK #-} !Text {-# UNPACK #-} !Int
      -- ^ Parser ran into an error. Either syntactic or a validation one.

    deriving (Eq, Show)

  instance Functor Result where
    {-# INLINE fmap #-}
    fmap fn (Success res more) = Success (fn res) more
    fmap _  (Failure expected more) = Failure expected more
    fmap _  (Error reason more len) = Error reason more len


  -- |
  -- Parser for 'Text' inputs.
  --
  newtype Parser a =
    Parser
      { runParser :: Text -> Result a
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
        Success fn rest -> fmap fn (runArg rest)
        Failure expected more -> Failure expected more
        Error reason more len -> Error reason more len

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
        Success res more -> runParser (right res) more
        Failure expected more -> Failure expected more
        Error reason more len -> Error reason more len

  instance MonadPlus Parser


  -- |
  -- Accepts a single, matching character.
  --
  {-# INLINE CONLIKE char #-}
  char :: Char -> Parser Char
  char c = label (show c) $ satisfy (c ==)


  -- |
  -- Accepts a single, differing character.
  --
  {-# INLINE CONLIKE notChar #-}
  notChar :: Char -> Parser Char
  notChar c = satisfy (c /=)


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
  parseOnly :: Parser a -> Text -> Maybe a
  parseOnly par = \inp ->
    case runParser par inp of
      Success res _ -> Just res
      _otherwise    -> Nothing


  -- |
  -- Accepts a single character.
  --
  {-# INLINE anyChar #-}
  anyChar :: Parser Char
  anyChar = Parser \inp ->
    if null inp
       then Failure ["any char"] inp
       else Success (unsafeHead inp) (unsafeTail inp)


  -- |
  -- Accepts a single character matching the predicate.
  --
  {-# INLINE CONLIKE satisfy #-}
  satisfy :: (Char -> Bool) -> Parser Char
  satisfy isOk = Parser \inp ->
    if null inp
       then Failure ["more input"] inp
       else let c = unsafeHead inp
             in if isOk c
                   then Success c (unsafeTail inp)
                   else Failure [] inp


  -- |
  -- Accepts a single unicode white space character.
  -- See 'isSpace' for details.
  --
  {-# INLINE space #-}
  space :: Parser Char
  space = label "space" $ satisfy isSpace


  -- |
  -- Accepts multiple unicode white space characters.
  -- See 'isSpace' for details.
  --
  {-# INLINE skipSpace #-}
  skipSpace :: Parser ()
  skipSpace = void $ Data.Text.Parser.takeWhile isSpace


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
       else Success (unsafeHead inp) inp


  -- |
  -- Accepts a matching string.
  --
  {-# INLINE CONLIKE string #-}
  string :: Text -> Parser Text
  string str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case pfx == str of
          True -> Success pfx sfx
          False -> Failure [show str] inp


  -- |
  -- Same as 'string', but case insensitive.
  --
  {-# INLINE CONLIKE stringCI #-}
  stringCI :: Text -> Parser Text
  stringCI str = Parser \inp ->
    let (pfx, sfx) = splitAt (length str) inp
     in case toCaseFold pfx == toCaseFold str of
          True -> Success pfx sfx
          False -> Failure [show str] inp


  -- |
  -- Accepts given number of characters.
  -- Fails when not enough characters are available.
  --
  {-# INLINE CONLIKE take #-}
  take :: Int -> Parser Text
  take n = Parser \inp ->
    if n > length inp
       then Failure [show n <> " more characters"] inp
       else let (pfx, more) = splitAt n inp
             in Success pfx more


  -- |
  -- Scans ahead statefully and then accepts whatever characters the scanner liked.
  -- Scanner returns 'Nothing' to mark end of the acceptable extent.
  --
  {-# INLINE CONLIKE scan #-}
  scan :: s -> (s -> Char -> Maybe s) -> Parser Text
  scan state scanner = fst <$> runScanner state scanner


  -- |
  -- Like 'scan', but also returns the final scanner state.
  --
  {-# INLINE CONLIKE runScanner #-}
  runScanner :: s -> (s -> Char -> Maybe s) -> Parser (Text, s)
  runScanner state scanner = Parser \inp -> loop inp state 0
    where
      loop inp !st !n =
        case n >= lengthWord8 inp of
          True -> Success (inp, st) mempty
          False ->
            case iter inp n of
              Iter c n' ->
                case scanner st c of
                  Nothing -> Success (takeWord8 n inp, st) (dropWord8 n inp)
                  Just st' -> loop inp st' (n + n')


  -- |
  -- Efficiently consume as long as the input characters match the predicate.
  -- An inverse of 'takeTill'.
  --
  {-# INLINE CONLIKE takeWhile #-}
  takeWhile :: (Char -> Bool) -> Parser Text
  takeWhile test = takeTill (not . test)


  -- |
  -- Like 'Data.Text.Parser.takeWhile', but requires at least a single character.
  --
  {-# INLINE CONLIKE takeWhile1 #-}
  takeWhile1 :: (Char -> Bool) -> Parser Text
  takeWhile1 test = Data.Text.Parser.takeWhile test `provided` (not . null)


  -- |
  -- Efficiently consume until a character matching the predicate is found.
  -- An inverse of 'Data.Text.Parser.takeWhile'.
  --
  {-# INLINE CONLIKE takeTill #-}
  takeTill :: (Char -> Bool) -> Parser Text
  takeTill test = Parser \inp ->
    let n = fromMaybe (length inp) $ findIndex test inp
        (pfx, more) = splitAt n inp
     in Success pfx more


  -- |
  -- Same as 'takeTill', but requires at least a single character.
  --
  {-# INLINE CONLIKE takeTill1 #-}
  takeTill1 :: (Char -> Bool) -> Parser Text
  takeTill1 test = Data.Text.Parser.takeTill test `provided` (not . null)


  -- |
  -- Makes the parser not only return the result, but also the original
  -- matched extent.
  --
  {-# INLINE CONLIKE match #-}
  match :: Parser a -> Parser (Text, a)
  match par = Parser \inp ->
    case runParser par inp of
      Failure expected more -> Failure expected more
      Error reason more len -> Error reason more len
      Success res more ->
        let n = length more
         in Success (T.take n inp, res) more


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
  {-# INLINE takeText #-}
  takeText :: Parser Text
  takeText = Parser \inp -> Success inp mempty


  -- |
  -- Peek at whatever input remains.
  --
  {-# INLINE peekText #-}
  peekText :: Parser Text
  peekText = Parser \inp -> Success inp inp


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
  -- Accepts optional @\'+\'@ or @\'-\'@ character and then applies it to
  -- the following parser result.
  --
  {-# INLINE signed #-}
  signed :: (Num a) => Parser a -> Parser a
  signed runNumber = (char '-' *> fmap negate runNumber)
                 <|> (char '+' *> runNumber)
                 <|> (runNumber)


  {-# INLINE CONLIKE unsafeWithUtf8 #-}
  unsafeWithUtf8 :: (BS.ByteString -> Maybe (a, BS.ByteString))
                 -> Text -> Maybe (a, Text)
  unsafeWithUtf8 bspar = \inp ->
    let bstr = encodeUtf8 inp
     in case bspar bstr of
          Nothing -> Nothing
          Just (x, more) ->
            -- This should be perfectly safe as long as the embedded
            -- parser returns the actual remaining input and not some
            -- random chunk of bytes.
            let n = lengthWord8 inp - BS.length more
             in Just (x, dropWord8 n inp)


  -- |
  -- Accepts an integral number in the decimal format.
  --
  {-# INLINE decimal #-}
  decimal :: (Integral a) => Parser a
  decimal = Parser \inp ->
    case unsafeWithUtf8 LI.readDecimal inp of
      Just (res, more) -> Success res more
      Nothing -> Failure ["decimal"] inp


  -- |
  -- Accepts an integral number in the hexadecimal format in either case.
  -- Does not look for @0x@ or similar prefixes.
  --
  {-# INLINE hexadecimal #-}
  hexadecimal :: (Integral a) => Parser a
  hexadecimal = Parser \inp ->
    case unsafeWithUtf8 LI.readHexadecimal inp of
      Just (res, more) -> Success res more
      Nothing -> Failure ["hexadecimal"] inp


  -- |
  -- Accepts an integral number in the octal format.
  --
  {-# INLINE octal #-}
  octal :: (Integral a) => Parser a
  octal = Parser \inp ->
    case unsafeWithUtf8 LI.readOctal inp of
      Just (res, more) -> Success res more
      Nothing -> Failure ["octal"] inp


  -- |
  -- Accepts a fractional number as a decimal optinally followed by a colon
  -- and the fractional part. Does not support exponentiation.
  --
  {-# INLINE fractional #-}
  fractional :: (Fractional a) => Parser a
  fractional = Parser \inp ->
    case unsafeWithUtf8 LF.readDecimal inp of
      Just (res, more) -> Success res more
      Nothing -> Failure ["fractional"] inp



  -- |
  -- Calculate offset from the original input and the remainder.
  --
  offset :: Text -> Text -> Int
  offset inp more = length inp - length more


  -- |
  -- Determine @(line, column)@ from the original input and the remainder.
  --
  -- Counts line feed characters leading to the 'offset', so only use it
  -- on your slow path. For example when describing parsing errors.
  --
  position :: Text -> Text -> (Int, Int)
  position inp more = (succ line, succ column)
    where
      column = length lastLine
      lastLine = takeWhileEnd ('\n' /=) leader
      line = T.count "\n" leader
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
        -- ^ Message associated with the problem.
      }
    deriving (Eq, Show)


  -- |
  -- Process the result for showing it to the user.
  --
  explain :: String -> Text -> Result a -> Explanation
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
        to   = position inp (T.drop len more)


-- vim:set ft=haskell sw=2 ts=2 et:
