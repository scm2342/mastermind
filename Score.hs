{-# LANGUAGE TemplateHaskell #-}
module Score where

import Data.Function.Memoize

data Score = Score Int Int deriving (Eq, Show, Read)

deriveMemoizable ''Score
