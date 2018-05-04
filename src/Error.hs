module Error where

type Error = String
type OrError = Either Error

fromOrError :: OrError a -> a
fromOrError (Right a) = a
fromOrError (Left e) = error e
