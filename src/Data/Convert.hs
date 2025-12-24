module Data.Convert where

class Convert a b where
  convert :: a -> b
