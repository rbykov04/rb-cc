{- It is so usefull class to print :D -}

module Printable where

addTab :: Int -> [String] -> [String]
addTab tab arr =
  let
      prefix = replicate (tab*4) ' '
  in map (\x -> prefix ++ x) arr



class Printable a where
  pprint :: a -> [String]
  pprintOneLine :: a -> String
  pprintOneLine a = concat (pprint a)
  prettyCodePrint :: Int -> a -> [String]
  prettyCodePrint tab a =
    let
      code = pprint a
    in addTab tab code
