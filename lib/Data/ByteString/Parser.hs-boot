module Data.ByteString.Parser
where
  import Control.Applicative
  import Data.ByteString (ByteString)

  newtype Parser a =
    Parser
      { runParser :: ByteString -> Maybe (a, ByteString)
      }

  instance Functor Parser
  instance Applicative Parser
  instance Alternative Parser
  instance Monad Parser
