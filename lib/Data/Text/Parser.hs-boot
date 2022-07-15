module Data.Text.Parser
where
  import Control.Applicative
  import Control.Monad
  import Data.Text (Text)

  data Result a
    = Success a {-# UNPACK #-} !Text
    | Failure [String] {-# UNPACK #-} !Text
    | Error String {-# UNPACK #-} !Text {-# UNPACK #-} !Int

  instance Functor Result where

  newtype Parser a =
    Parser
      { runParser :: Text -> Result a
      }

  instance Functor Parser
  instance Applicative Parser
  instance Alternative Parser
  instance Monad Parser
  instance MonadPlus Parser
