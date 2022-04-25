# snack

**Strict ByteString Parser Combinator**

- Simple. Feel free to contribute.
- Fast. Sometimes faster then Attoparsec.
- ASCII. Good enough for IETF formats.
- Also Text. But quite slower.

Example:

```haskell
import Data.ByteString (ByteString)
import qualified Data.ByteString.Parser.Char8 as BSP

parseList :: BSP.Parser [ByteString]
parseList = (token `BSP.wrap` BSP.skipSpace) `BSP.sepBy` BSP.char ','
  where token = BSP.takeWhile isToken
        isToken c = inRange 'a' 'z' c ||
                    inRange 'A' 'Z' c ||
                    inRange '0' '9' c ||
                    c == '_' || c == '-'

main :: IO ()
main = do
  putStrLn $ show $ BSP.runParser parseList "monkey, wrench, bananas"
  putStrLn $ show $ BSP.runParser parseList "^quux"
  putStrLn $ show $ BSP.runParser (parseList <* BSP.endOfInput) "^quux"

-- Will output:
-- Success ["monkey","wrench","bananas"] ""
-- Success [""] "^quux"
-- Failure ["end of input"] "^quux"
```
