module Misc exposing (..)


twice : (a -> a -> b) -> a -> b
twice f x =
    f x x
