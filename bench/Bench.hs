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
  scMedia = SC.parseOnly (SC.skipSpace *> pMediaList <* SC.endOfInput)
    where
      pMediaList :: SC.Parser [Media ByteString]
      pMediaList = pMedia `SC.sepBy` pSeparator

      pMedia :: SC.Parser (Media ByteString)
      pMedia = do
        mainType <- pToken
        subType  <- (SC.char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> SC.many (SC.eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return (mainType, subType, params, quality)

      pParameter :: SC.Parser (ByteString, ByteString)
      pParameter = do
        _     <- SC.char ';' <* SC.skipSpace
        name  <- pToken
        _     <- SC.char '=' <* SC.skipSpace
        value <- pValue
        return (name, value)

      pQuality :: SC.Parser Float
      pQuality = do
        _ <- SC.char ';' <* SC.skipSpace
        _ <- SC.char 'q' <* SC.skipSpace
        _ <- SC.char '=' <* SC.skipSpace
        SC.fractional

      pToken :: SC.Parser ByteString
      pToken = SC.label "token" $ SC.takeTill1 isSpecial <* SC.skipSpace

      pSeparator :: SC.Parser Char
      pSeparator = SC.char ',' <* SC.skipSpace

      pValue :: SC.Parser ByteString
      pValue = SC.branch [ (SC.char '"', \_ -> pQuotedStr)
                         , (   pure ' ', \_ -> pToken)
                         ]

      pQuotedStr :: SC.Parser ByteString
      pQuotedStr = pString <* SC.char '"' <* SC.skipSpace

      pString :: SC.Parser ByteString
      pString = SC.label "string" $ SC.takeWhile isStrChar

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
  acMedia = AC.parseOnly (AC.skipSpace *> pMediaList <* AC.endOfInput)
    where
      pMediaList :: AC.Parser [Media ByteString]
      pMediaList = pMedia `AC.sepBy` pSeparator

      pMedia :: AC.Parser (Media ByteString)
      pMedia = do
        mainType <- pToken
        subType  <- (AC.char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> AC.many' (AC.eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return (mainType, subType, params, quality)

      pParameter :: AC.Parser (ByteString, ByteString)
      pParameter = do
        _     <- AC.char ';' <* AC.skipSpace
        name  <- pToken
        _     <- AC.char '=' <* AC.skipSpace
        value <- pValue
        return (name, value)

      pQuality :: AC.Parser Float
      pQuality = do
        _ <- AC.char ';' <* AC.skipSpace
        _ <- AC.char 'q' <* AC.skipSpace
        _ <- AC.char '=' <* AC.skipSpace
        AC.rational

      pToken :: AC.Parser ByteString
      pToken = AC.takeTill isSpecial <* AC.skipSpace

      pSeparator :: AC.Parser Char
      pSeparator = AC.char ',' <* AC.skipSpace

      pValue :: AC.Parser ByteString
      pValue = pQuotedStr <|> pToken

      pQuotedStr :: AC.Parser ByteString
      pQuotedStr = pQuoted $ AC.takeWhile isStrChar

      pQuoted :: AC.Parser a -> AC.Parser a
      pQuoted p = AC.char '"' *> p <* AC.char '"' <* AC.skipSpace

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
  stMedia = ST.parseOnly (ST.skipSpace *> pMediaList <* ST.endOfInput)
    where
      pMediaList :: ST.Parser [Media Text]
      pMediaList = pMedia `ST.sepBy` pSeparator

      pMedia :: ST.Parser (Media Text)
      pMedia = do
        mainType <- pToken
        subType  <- (ST.char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> ST.many (ST.eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return (mainType, subType, params, quality)

      pParameter :: ST.Parser (Text, Text)
      pParameter = do
        _     <- ST.char ';' <* ST.skipSpace
        name  <- pToken
        _     <- ST.char '=' <* ST.skipSpace
        value <- pValue
        return (name, value)

      pQuality :: ST.Parser Float
      pQuality = do
        _ <- ST.char ';' <* ST.skipSpace
        _ <- ST.char 'q' <* ST.skipSpace
        _ <- ST.char '=' <* ST.skipSpace
        ST.fractional

      pToken :: ST.Parser Text
      pToken = ST.takeTill1 isSpecial <* ST.skipSpace

      pSeparator :: ST.Parser Char
      pSeparator = ST.char ',' <* ST.skipSpace

      pValue :: ST.Parser Text
      pValue = ST.branch [ (ST.char '"', \_ -> pQuotedStr)
                         , (   pure ' ', \_ -> pToken)
                         ]

      pQuotedStr :: ST.Parser Text
      pQuotedStr = pString <* ST.char '"' <* ST.skipSpace

      pString :: ST.Parser Text
      pString = ST.label "string" $ ST.takeWhile isStrChar

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
  atMedia = AT.parseOnly (AT.skipSpace *> pMediaList <* AT.endOfInput)
    where
      pMediaList :: AT.Parser [Media Text]
      pMediaList = pMedia `AT.sepBy` pSeparator

      pMedia :: AT.Parser (Media Text)
      pMedia = do
        mainType <- pToken
        subType  <- (AT.char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> AT.many' (AT.eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return (mainType, subType, params, quality)

      pParameter :: AT.Parser (Text, Text)
      pParameter = do
        _     <- AT.char ';' <* AT.skipSpace
        name  <- pToken
        _     <- AT.char '=' <* AT.skipSpace
        value <- pValue
        return (name, value)

      pQuality :: AT.Parser Float
      pQuality = do
        _ <- AT.char ';' <* AT.skipSpace
        _ <- AT.char 'q' <* AT.skipSpace
        _ <- AT.char '=' <* AT.skipSpace
        AT.rational

      pToken :: AT.Parser Text
      pToken = AT.takeTill isSpecial <* AT.skipSpace

      pSeparator :: AT.Parser Char
      pSeparator = AT.char ',' <* AT.skipSpace

      pValue :: AT.Parser Text
      pValue = pQuotedStr <|> pToken

      pQuotedStr :: AT.Parser Text
      pQuotedStr = pQuoted $ AT.takeWhile isStrChar

      pQuoted :: AT.Parser a -> AT.Parser a
      pQuoted p = AT.char '"' *> p <* AT.char '"' <* AT.skipSpace

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
