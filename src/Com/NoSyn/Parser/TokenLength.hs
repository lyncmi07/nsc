module Com.NoSyn.Parser.TokenLength where

class TokenLength a where
  tokenLength :: a -> Int

