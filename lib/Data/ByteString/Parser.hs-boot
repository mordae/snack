module Data.ByteString.Parser
where
  import Control.Applicative
  import Control.Monad
  import Data.ByteString (ByteString)

  data Result a
    = Success a {-# UNPACK #-} !ByteString
    | Failure [String] {-# UNPACK #-} !ByteString
    | Error String {-# UNPACK #-} !ByteString {-# UNPACK #-} !Int

  instance Functor Result where

  newtype Parser a =
    Parser
      { runParser :: ByteString -> Result a
      }

  instance Functor Parser
  instance Applicative Parser
  instance Alternative Parser
  instance Monad Parser
  instance MonadPlus Parser
