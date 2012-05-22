{-# LANGUAGE NoMonomorphismRestriction, MonoLocalBinds, FlexibleInstances, UndecidableInstances, TemplateHaskell, OverloadedStrings #-}
module Main where

{-
   This is just a little messing around because I am bored... The code shows that...
   If you want a fast version, compile, run with -writemap, compile again
-}

import Prelude hiding (foldl, foldr, all, any)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable
import Data.List (sort)
import System.Random
import Control.Monad.State
import Data.Char
import Data.Function.Memoize
import System.Environment
import System.IO
import System.Directory
import Data.FileEmbed
import Language.Haskell.TH.Syntax
import qualified Data.ByteString.Char8 as BS
import Score


class Pretty a where
    pretty :: a -> String

data Color = R | B | Y | W | G | P deriving (Eq, Show, Enum, Bounded, Ord, Read)

instance Pretty Color where
    pretty = show

instance Random Color where
    randomR (lo, hi) g = let (i, g') = randomR (fromEnum lo, fromEnum hi) g in (toEnum i, g')
    random g = randomR (minBound, maxBound) g

data Guess = Guess Color Color Color Color deriving (Eq, Show, Read)

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

score (Guess s1 s2 s3 s4) (Guess g1 g2 g3 g4) = Score correct (color - correct)
    where
    xs = [s1, s2, s3, s4]
    ys = [g1, g2, g3, g4]
    correct = correct' xs ys
    color = color' (sort xs) (sort ys)
    correct' xs ys = length $ filter id $ zipWith (==) xs ys
    color' [] _ = 0
    color' _ [] = 0
    color' (x:xs) (y:ys) | x < y = color' xs (y:ys)
                         | y < x = color' (x:xs) ys
                         | otherwise = 1 + color' xs ys

valid guess score' solution = score guess solution == score'
invalid guess score' solution = not $ valid guess score' solution

expCache = read $ BS.unpack $(do
    fileThere <- runIO $ doesFileExist "map"
    if not fileThere then return $! LitE $! StringL $! "fromList []" else
        embedFile "map")
secondGuess :: Score -> Guess
secondGuess score = maybe (memoized score) id $ M.lookup score expCache
    where
    memoized = traceMemoize $ \score -> nextGuessNoErr $ reduceSolset initGuess score $ S.fromList enumAll
trainAll = foldlM (\num score -> putStrLn ("training " ++ show num ++ " out of " ++ show llstAll) >> (secondGuess score `seq` return (succ num))) 1 lstAll >> return ()
    where lstAll = enumAll :: [Score]
          llstAll = length lstAll
writeMap = do
    putStrLn "Generating and writing map. This may take a while"
    fileThere <- doesFileExist "map"
    when (fileThere) $ putStrLn "Warning - will override file \"map\". Press any key to continue" >> getChar >> return ()
    withFile "map" WriteMode $ \fh -> do
        let map = foldl' (\map score -> M.insert score (secondGuess score) map) M.empty enumAll
        hPrint fh map


nextGuess solset
    | S.null solset = error "solset empty - someone cheated"
    | otherwise = nextGuessAux solset
nextGuessNoErr solset
    | S.null solset = initGuess
    | otherwise = nextGuessAux solset
nextGuessAux solset = fst $ flippedFold (S.findMin solset, 0) enumAll $ \curr@(finalguess, redux) guess ->
        let newRedux = flippedFold (S.size solset) enumAll $ \redux score -> min redux $ wouldRemove guess score solset in
            if newRedux > redux then (guess, newRedux) else curr
    where flippedFold x y z = foldl' z x y
          wouldRemove guess score solset = foldl' (\num sol -> if invalid guess score sol then succ num else num) 0 solset

reduceSolset guess score solset = S.filter (valid guess score) solset

guess guess = do
    putStrLn "Working..."
    guess `seq` putStrLn $ "\nI guess: " ++ pretty guess

getScore = do
    putStr "correct? "
    correct <- getChar
    putStr "\ncolor? "
    color <- getChar
    putStrLn ""
    let score@(Score a b) = Score (read $ return correct) (read $ return color)
    if a + b > 4 then putStrLn "Huh?" >> getScore else return score

getGuess = do
    putStr "Your guess sir? "
    a <- get
    b <- get
    c <- get
    d <- get
    putStrLn ""
    return $ Guess a b c d
    where get = getChar >>= \r -> putStr " " >> (return $ read $ return $ toUpper r)

initGuess = Guess R R B B
main1 = do
    guess $ initGuess
    score <- getScore
    loop score 1 $ reduceSolset initGuess score (S.fromList enumAll)
    where
    loop lastScore try solset = do
        putStrLn $ "Possible solutions: " ++ show solset
        let curGuess = if try <= 1 then secondGuess lastScore else nextGuess solset
        guess curGuess
        score <- getScore
        if score == Score 4 0 then putStrLn $ "Only " ++ show try ++ " tries! Great" else
            loop score (succ try) $ reduceSolset curGuess score solset

-- ghci only - i pretty much messed up io code :)
main2 = do
    randomIO >>= loop
    where
    loop solution = do
        guess <- getGuess
        let score' = score solution guess
        putStrLn $ pretty score'
        if score' == Score 4 0 then putStrLn "Damn" else
            loop solution

-- ghci only - i pretty much messed up io code :)
main3 = do
    solution <- randomIO
    guess initGuess
    let score' = score solution initGuess
    putStrLn $ pretty score'
    loop score' (reduceSolset initGuess score' (S.fromList enumAll)) 1 solution
    where
    loop :: Score -> S.Set Guess -> Int -> Guess -> IO ()
    loop lastScore solset try solution = do
        putStrLn $ "Possible solutions: " ++ show solset
        let curGuess = if try <= 1 then secondGuess lastScore else nextGuess solset
        guess curGuess
        let score' = score solution curGuess
        putStrLn $ pretty score'
        if score' == Score 4 0 then putStrLn ("Only " ++ show try ++ " try? Nochmal!") >> if try > 5 then putStrLn "fucked" else main3 else
            loop lastScore (reduceSolset curGuess score' solset) (succ try) solution

main = do
    args <- getArgs
    when (any (== "-trainall") args) $ trainAll
    if any (== "-writemap") args then writeMap else
        main3
