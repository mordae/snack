-- |
-- Module      :  Bench
-- License     :  CC0-1.0
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

{-# OPTIONS_GHC -ddump-to-file -ddump-stg-from-core -ddump-cmm-opt -dsuppress-all -dppr-cols=200 #-}

module Main
  ( main
  )
where
  import Data.String.Conversions

  import Control.Applicative
  import Data.Either
  import Data.Maybe

  import Data.ByteString (ByteString)

  import Criterion.Main

  import qualified Data.ByteString.Parser.Char8 as SC
  import qualified Data.Attoparsec.ByteString.Char8 as AC


  main :: IO ()
  main = defaultMain
    [ bgroup "media"
        [ bench "snack"      $ nf scMedia (cs $! sampleMedia)
        , bench "attoparsec" $ nf acMedia (cs $! sampleMedia)
        ]
    ]



  type Media a = (a, a, [(a, a)], Float)

  sampleMedia :: String
  sampleMedia = "text/html, text/plain;q=0.7"


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
      pToken = pSpaced $ SC.takeTill1 isSpecial

      pSeparator :: SC.Parser Char
      pSeparator = pSpaced $ SC.char ','

      pValue :: SC.Parser ByteString
      pValue = pToken <|> pQuotedStr

      pQuotedStr :: SC.Parser ByteString
      pQuotedStr = pSpaced $ pQuoted $ SC.takeWhile isStrChar

      pSpaced :: SC.Parser a -> SC.Parser a
      pSpaced p = p `SC.wrap` SC.takeWhile SC.isSpace

      pQuoted :: SC.Parser a -> SC.Parser a
      pQuoted p = SC.char '"' *> p <* SC.char '"'

      isStrChar :: (Char -> Bool)
      isStrChar c = c /= '\\' && c /= '"'

      isSpecial :: (Char -> Bool)
      --isSpecial = SC.inClass "\x00- ()<>@,;:\\\"/[]?="
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


-- vim:set ft=haskell sw=2 ts=2 et:
