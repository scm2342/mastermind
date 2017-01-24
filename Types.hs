{-# LANGUAGE NoMonomorphismRestriction, MonoLocalBinds, FlexibleInstances, UndecidableInstances, DeriveGeneric #-}
module Types where

import Score
import Control.DeepSeq
import System.Random
import Control.Monad.State
import GHC.Generics


class Pretty a where
    pretty :: a -> String

instance NFData Score where
    rnf s@(Score a b) = a `deepseq` b `deepseq` s `seq` ()

instance Pretty Score where
    pretty (Score a b) = "red: " ++ show a ++ " white: " ++ show b

instance Ord Score where
    compare (Score a1 b1) (Score a2 b2)
        | cmp == EQ = compare a1 a2
        | otherwise = cmp
      where
      cmp = compare (a1 + b1) (a2 + b2)

instance Bounded Score where
    minBound = Score 0 0
    maxBound = Score 4 0

instance Enum Score where
    succ (Score a b)
        | a < 4 && b > 0 = Score (succ a) (pred b)
        | b == 0 && a + b < 4 = Score 0 (succ (a + b))
        | otherwise = error "out of bound succ"
    pred (Score a b)
        | a > 0 && b < 4 = Score (pred a) (succ b)
        | a == 0 && a + b > 0 = Score (pred (a + b)) 0
        | otherwise = error "out of bound pred"
    toEnum = genToEnum
    fromEnum = genFromEnum
    enumFromTo = genEnumFromTo
    enumFrom = genEnumFrom
    enumFromThenTo = undefined

data Color = R | B | Y | W | G | P deriving (Eq, Show, Enum, Bounded, Ord, Read, Generic)

instance NFData Color

instance Pretty Color where
    pretty = show

instance Random Color where
    randomR (lo, hi) g = let (i, g') = randomR (fromEnum lo, fromEnum hi) g in (toEnum i, g')
    random g = randomR (minBound, maxBound) g

data Guess = Guess Color Color Color Color deriving (Eq, Show, Read)

instance NFData Guess where
    rnf g@(Guess a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` g `seq` ()

instance Pretty Guess where
    pretty (Guess a b c d) = pretty a ++ " " ++ pretty b ++ " " ++ pretty c ++ " " ++ pretty d

instance Random Guess where
    -- sloooow not used anyhow
    randomR (lo, hi) g = let (i, g') = randomR (fromEnum lo, fromEnum hi) g in (toEnum i, g')
    random = runState $ do
        a <- getR
        b <- getR
        c <- getR
        d <- getR
        return $ Guess a b c d
        where getR = get >>= \g -> let (r, g') = random g in put g' >> return r

instance Ord Guess where
    compare (Guess a1 b1 c1 d1) (Guess a2 b2 c2 d2) = compare [a1, b1, c1, d1] [a2, b2, c2, d2]

instance Bounded Guess where
    minBound = Guess R R R R
    maxBound = Guess P P P P

instance Enum Guess where
    succ (Guess a b c d)
        | d < maxBound = Guess a b c (succ d)
        | c < maxBound = Guess a b (succ c) minBound
        | b < maxBound = Guess a (succ b) minBound minBound
        | a < maxBound = Guess (succ a) minBound minBound minBound
        | otherwise = error "out of bounds succ"
    pred (Guess a b c d)
        | d > minBound = Guess a b c (pred d)
        | c > minBound = Guess a b (pred c) maxBound
        | b > minBound = Guess a (pred b) maxBound maxBound
        | a > minBound = Guess (pred a) maxBound maxBound maxBound
        | otherwise = error "out of bounds pred"
    toEnum = genToEnum
    fromEnum = genFromEnum
    enumFromTo = genEnumFromTo
    enumFrom = genEnumFrom
    enumFromThenTo = undefined

-- general enum funs. the default stuff seems worse since it does not
-- depend on Bounded and Ord instance
genToEnum x = helper minBound x
    where
    helper acc 0 = acc
    helper acc x = helper (succ acc) (pred x)
genFromEnum x = helper 0 x
    where
    helper acc x
        | x == minBound = acc
        | otherwise     = helper (succ acc) (pred x)
genEnumFromTo x y | x < y = x : enumFromTo (succ x) y
               | otherwise = [y]
genEnumFrom x = enumFromTo x maxBound
enumAll = enumFrom minBound
