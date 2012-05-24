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
import Data.Char
import Data.Function.Memoize
import System.Environment
import System.IO
import Control.DeepSeq
import Control.Monad
import System.Directory
import Types
import Score
import ExpCache

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

secondGuess :: Score -> Guess
secondGuess score = maybe (memoized score) id $ M.lookup score expCache
    where
    memoized = let res = memoize $ \score -> nextGuessNoErr $ reduceSolset initGuess score $ S.fromList enumAll in res `deepseq` res
trainAll = foldlM (\num score -> putStrLn ("training " ++ show num ++ " out of " ++ show llstAll) >> (secondGuess score `seq` return (succ num))) 1 lstAll >> return ()
    where lstAll = enumAll :: [Score]
          llstAll = length lstAll
writeMap = do
    putStrLn "Generating and writing map. This may take a while"
    fileThere <- doesFileExist "map"
    when (fileThere) $ putStrLn "Warning - will override file \"map\". Press any key to continue" >> getChar >> return ()
    withFile "map" WriteMode $ \fh -> do
        let map = foldl' (\map score -> M.insert score (secondGuess score) map) M.empty enumAll
        hPutStr fh $ show map


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
