-- | Monadic utilities
--
-- @since 0.1.0
module Utils
  ( (>>|)
  ) where

(>>|) :: Monad m => m (Either a b) -> (b -> m (Either a c)) -> m (Either a c)
l >>| r = l >>= eitherR r

eitherR :: Monad m => (b -> m (Either a c)) -> Either a b -> m (Either a c)
eitherR = either (return . Left)
