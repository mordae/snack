-- |
-- Module      :  Bench
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Main
  ( main
  )
where
  import Data.String.Conversions

  import Control.Applicative
  import Data.Either
  import Data.Maybe

  import Data.Text (Text)
  import Data.ByteString (ByteString)

  import Criterion.Main

  import Data.ByteString.Parser.Char8 qualified as SC
  import Data.Attoparsec.ByteString.Char8 qualified as AC
  import Data.Text.Parser qualified as ST
  import Data.Attoparsec.Text qualified as AT


  main :: IO ()
  main = defaultMain
    [ bgroup "media"
        [ bench "Data.ByteString.Parser.Char8" $ nf scMedia (cs $! sampleMedia)
        , bench "Data.Attoparsec.ByteString"   $ nf acMedia (cs $! sampleMedia)
        , bench "Data.Text.Parser"             $ nf stMedia (cs $! sampleMedia)
        , bench "Data.Attoparsec.Text"         $ nf atMedia (cs $! sampleMedia)
        ]
    , bgroup "kv"
        [ bench "Data.ByteString.Parser.Char8" $ nf scKeyValue (cs $! sampleKeyValue)
        , bench "Data.Attoparsec.ByteString"   $ nf acKeyValue (cs $! sampleKeyValue)
        , bench "Data.Text.Parser"             $ nf stKeyValue (cs $! sampleKeyValue)
        , bench "Data.Attoparsec.Text"         $ nf atKeyValue (cs $! sampleKeyValue)
        ]
    ]



  type Media a = (a, a, [(a, a)], Float)

  sampleMedia :: String
  sampleMedia = "text/html, text/plain;q=0.7"


  sampleKeyValue :: String
  sampleKeyValue = "lst = \"\\\"first\\\", \\\"second\\\", \\\"third\\\"\""


  {-# NOINLINE scMedia #-}
  scMedia :: ByteString -> Maybe [Media ByteString]
  scMedia = SC.parseOnly (pMediaList <* SC.endOfInput)
    where
      pMedia :: SC.Parser (Media ByteString)
      pMediaList = pMedia `SC.sepBy` pSeparator
      pMedia = do
        mainType <- pToken
        subType  <- (SC.char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> SC.many (SC.eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return (mainType, subType, params, quality)

      pParameter :: SC.Parser (ByteString, ByteString)
      pParameter = do
        _     <- SC.takeWhile SC.isSpace
        _     <- SC.char ';'
        name  <- pToken
        _     <- SC.char '='
        value <- pValue
        return (name, value)

      pQuality :: SC.Parser Float
      pQuality = do
        _ <- SC.takeWhile SC.isSpace
        _ <- SC.char ';'
        _ <- pSpaced $ SC.char 'q'
        _ <- SC.char '='
        SC.fractional

      pToken :: SC.Parser ByteString
      pToken = pSpaced $ SC.label "token" $ SC.takeTill1 isSpecial

      pSeparator :: SC.Parser Char
      pSeparator = pSpaced $ SC.char ','

      pValue :: SC.Parser ByteString
      pValue = pToken <|> pQuotedStr

      pQuotedStr :: SC.Parser ByteString
      pQuotedStr = pSpaced $ SC.label "quoted string" $ pQuoted $ SC.takeWhile isStrChar

      pSpaced :: SC.Parser a -> SC.Parser a
      pSpaced p = p `SC.wrap` SC.takeWhile SC.isSpace

      pQuoted :: SC.Parser a -> SC.Parser a
      pQuoted p = SC.char '"' *> p <* SC.char '"'

      isStrChar :: (Char -> Bool)
      isStrChar c = c /= '\\' && c /= '"'

      isSpecial :: (Char -> Bool)
      isSpecial c = c <= ' '
                 || c == '(' || c == ')'
                 || c == '<' || c == '>'
                 || c == '@' || c == ',' || c == ';'
                 || c == ':' || c == '\\'
                 || c == '"' || c == '/'
                 || c == '[' || c == ']'
                 || c == '?' || c == '='



  {-# NOINLINE acMedia #-}
  acMedia :: ByteString -> Either String [Media ByteString]
  acMedia = AC.parseOnly (pMediaList <* AC.endOfInput)
    where
      pMedia :: AC.Parser (Media ByteString)
      pMediaList = pMedia `AC.sepBy` pSeparator
      pMedia = do
        mainType <- pToken
        subType  <- (AC.char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> AC.many' (AC.eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return (mainType, subType, params, quality)

      pParameter :: AC.Parser (ByteString, ByteString)
      pParameter = do
        _     <- AC.skipSpace
        _     <- AC.char ';'
        name  <- pToken
        _     <- AC.char '='
        value <- pValue
        return (name, value)

      pQuality :: AC.Parser Float
      pQuality = do
        _ <- AC.skipSpace
        _ <- AC.char ';'
        _ <- pSpaced $ AC.char 'q'
        _ <- AC.char '='
        AC.rational

      pToken :: AC.Parser ByteString
      pToken = pSpaced $ AC.takeTill isSpecial

      pSeparator :: AC.Parser Char
      pSeparator = pSpaced $ AC.char ','

      pValue :: AC.Parser ByteString
      pValue = pToken <|> pQuotedStr

      pQuotedStr :: AC.Parser ByteString
      pQuotedStr = pSpaced $ pQuoted $ AC.takeWhile isStrChar

      pSpaced :: AC.Parser a -> AC.Parser a
      pSpaced p = AC.skipSpace *> p <* AC.skipSpace

      pQuoted :: AC.Parser a -> AC.Parser a
      pQuoted p = AC.char '"' *> p <* AC.char '"'

      isStrChar :: (Char -> Bool)
      isStrChar c = c /= '\\' && c /= '"'

      isSpecial :: (Char -> Bool)
      isSpecial c = c <= ' '
                 || c == '(' || c == ')'
                 || c == '<' || c == '>'
                 || c == '@' || c == ',' || c == ';'
                 || c == ':' || c == '\\'
                 || c == '"' || c == '/'
                 || c == '[' || c == ']'
                 || c == '?' || c == '='


  {-# NOINLINE stMedia #-}
  stMedia :: Text -> Maybe [Media Text]
  stMedia = ST.parseOnly (pMediaList <* ST.endOfInput)
    where
      pMedia :: ST.Parser (Media Text)
      pMediaList = pMedia `ST.sepBy` pSeparator
      pMedia = do
        mainType <- pToken
        subType  <- (ST.char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> ST.many (ST.eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return (mainType, subType, params, quality)

      pParameter :: ST.Parser (Text, Text)
      pParameter = do
        _     <- ST.takeWhile ST.isSpace
        _     <- ST.char ';'
        name  <- pToken
        _     <- ST.char '='
        value <- pValue
        return (name, value)

      pQuality :: ST.Parser Float
      pQuality = do
        _ <- ST.takeWhile ST.isSpace
        _ <- ST.char ';'
        _ <- pSpaced $ ST.char 'q'
        _ <- ST.char '='
        ST.fractional

      pToken :: ST.Parser Text
      pToken = pSpaced $ ST.takeTill1 isSpecial

      pSeparator :: ST.Parser Char
      pSeparator = pSpaced $ ST.char ','

      pValue :: ST.Parser Text
      pValue = pToken <|> pQuotedStr

      pQuotedStr :: ST.Parser Text
      pQuotedStr = pSpaced $ pQuoted $ ST.takeWhile isStrChar

      pSpaced :: ST.Parser a -> ST.Parser a
      pSpaced p = p `ST.wrap` ST.takeWhile ST.isSpace

      pQuoted :: ST.Parser a -> ST.Parser a
      pQuoted p = ST.char '"' *> p <* ST.char '"'

      isStrChar :: (Char -> Bool)
      isStrChar c = c /= '\\' && c /= '"'

      isSpecial :: (Char -> Bool)
      isSpecial c = c <= ' '
                 || c == '(' || c == ')'
                 || c == '<' || c == '>'
                 || c == '@' || c == ',' || c == ';'
                 || c == ':' || c == '\\'
                 || c == '"' || c == '/'
                 || c == '[' || c == ']'
                 || c == '?' || c == '='



  {-# NOINLINE atMedia #-}
  atMedia :: Text -> Either String [Media Text]
  atMedia = AT.parseOnly (pMediaList <* AT.endOfInput)
    where
      pMedia :: AT.Parser (Media Text)
      pMediaList = pMedia `AT.sepBy` pSeparator
      pMedia = do
        mainType <- pToken
        subType  <- (AT.char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> AT.many' (AT.eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return (mainType, subType, params, quality)

      pParameter :: AT.Parser (Text, Text)
      pParameter = do
        _     <- AT.skipSpace
        _     <- AT.char ';'
        name  <- pToken
        _     <- AT.char '='
        value <- pValue
        return (name, value)

      pQuality :: AT.Parser Float
      pQuality = do
        _ <- AT.skipSpace
        _ <- AT.char ';'
        _ <- pSpaced $ AT.char 'q'
        _ <- AT.char '='
        AT.rational

      pToken :: AT.Parser Text
      pToken = pSpaced $ AT.takeTill isSpecial

      pSeparator :: AT.Parser Char
      pSeparator = pSpaced $ AT.char ','

      pValue :: AT.Parser Text
      pValue = pToken <|> pQuotedStr

      pQuotedStr :: AT.Parser Text
      pQuotedStr = pSpaced $ pQuoted $ AT.takeWhile isStrChar

      pSpaced :: AT.Parser a -> AT.Parser a
      pSpaced p = AT.skipSpace *> p <* AT.skipSpace

      pQuoted :: AT.Parser a -> AT.Parser a
      pQuoted p = AT.char '"' *> p <* AT.char '"'

      isStrChar :: (Char -> Bool)
      isStrChar c = c /= '\\' && c /= '"'

      isSpecial :: (Char -> Bool)
      isSpecial c = c <= ' '
                 || c == '(' || c == ')'
                 || c == '<' || c == '>'
                 || c == '@' || c == ',' || c == ';'
                 || c == ':' || c == '\\'
                 || c == '"' || c == '/'
                 || c == '[' || c == ']'
                 || c == '?' || c == '='


  scKeyValue :: ByteString -> Maybe (ByteString, ByteString)
  scKeyValue = SC.parseOnly (pKeyValue <* SC.endOfInput)
    where
      pKeyValue = do
        _   <- SC.skipSpace
        key <- SC.takeWhile1 isToken
        _   <- SC.skipSpace
        _   <- SC.skipSpace
        _   <- SC.char '='
        _   <- SC.skipSpace
        _   <- SC.char '"'
        val <- SC.scan False scanString
        _   <- SC.char '"'
        return (key, val)


  acKeyValue :: ByteString -> Either String (ByteString, ByteString)
  acKeyValue = AC.parseOnly (pKeyValue <* AC.endOfInput)
    where
      pKeyValue = do
        _   <- AC.skipSpace
        key <- AC.takeWhile1 isToken
        _   <- AC.skipSpace
        _   <- AC.char '='
        _   <- AC.skipSpace
        _   <- AC.char '"'
        val <- AC.scan False scanString
        _   <- AC.char '"'
        return (key, val)


  stKeyValue :: Text -> Maybe (Text, Text)
  stKeyValue = ST.parseOnly (pKeyValue <* ST.endOfInput)
    where
      pKeyValue = do
        _   <- ST.skipSpace
        key <- ST.takeWhile1 isToken
        _   <- ST.skipSpace
        _   <- ST.skipSpace
        _   <- ST.char '='
        _   <- ST.skipSpace
        _   <- ST.char '"'
        val <- ST.scan False scanString
        _   <- ST.char '"'
        return (key, val)


  atKeyValue :: Text -> Either String (Text, Text)
  atKeyValue = AT.parseOnly (pKeyValue <* AT.endOfInput)
    where
      pKeyValue = do
        _   <- AT.skipSpace
        key <- AT.takeWhile1 isToken
        _   <- AT.skipSpace
        _   <- AT.char '='
        _   <- AT.skipSpace
        _   <- AT.char '"'
        val <- AT.scan False scanString
        _   <- AT.char '"'
        return (key, val)


  scanString :: Bool -> Char -> Maybe Bool
  scanString True _     = Just False
  scanString False '"'  = Nothing
  scanString False '\\' = Just True
  scanString False _    = Just False


  isToken :: Char -> Bool
  isToken c = ('A' <= c && c <= 'Z')
           || ('a' <= c && c <= 'z')
           || ('0' <= c && c <= '9')
           || ('_' == c)
           || ('-' == c)


-- vim:set ft=haskell sw=2 ts=2 et:
