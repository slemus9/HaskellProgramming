import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ms = join $ fmap f ms
-- bind f = join . fmap f
