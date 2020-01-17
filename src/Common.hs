module Common where

fromMaybeE :: b -> Maybe a -> Either b a
fromMaybeE l Nothing  = Left l
fromMaybeE _ (Just r) = Right r
