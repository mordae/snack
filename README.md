# snack

**Strict ByteString Parser Combinator**

- Simple. Feel free to contribute.
- Fast. Faster then Attoparsec.
- ASCII. Good enough for IETF formats.

Example:

```haskell
import Data.ByteString (ByteString)
import qualified Data.ByteString.Parser.Char8 as BSP

parseList :: BSP.Parser [ByteString]
parseList = (token `BSP.wrap` BSP.skipSpace) `BSP.sepBy` BSP.char ','
  where token = BSP.takeWhile (BSP.inClass "a-zA-Z0-9_-")

main :: IO ()
main = do
  putStrLn $ show $ BSP.runParser parseList "monkey, wrench, bananas"
  putStrLn $ show $ BSP.runParser parseList "^quux"
  putStrLn $ show $ BSP.runParser (parseList <* BSP.endOfInput) "^quux"

-- Will output:
-- Just (["monkey","wrench","bananas"],"")
-- Just ([""],"^quux")
-- Nothing
```
