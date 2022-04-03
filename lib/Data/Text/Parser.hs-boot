module Data.Text.Parser
where
  import Control.Applicative
  import Data.Text (Text)

  newtype Parser a =
    Parser
      { runParser :: Text -> Maybe (a, Text)
      }

  instance Functor Parser
  instance Applicative Parser
  instance Alternative Parser
  instance Monad Parser
