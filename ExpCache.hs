{-# LANGUAGE TemplateHaskell #-}
module ExpCache where

import Language.Haskell.TH.Syntax
import System.Directory
import qualified Data.Map as M
import Control.Applicative
import System.IO
import Types
import Score

expCache :: M.Map Score Guess
expCache = $(do
    let filename = "map"
        emptyStr = show (M.empty :: M.Map Score Guess)
        checkStr str = case (reads str :: [(M.Map Score Guess, String)]) of
                           [(_, "")] -> do
                               putStrLn $ "Ran pre-flight checks for ExpCache \"" ++ filename ++ "\" file - looks ok"
                               return str
                           _ -> do
                               hPutStrLn stderr $ "\"" ++ filename ++ "\" file present but could not parse - would crash at runtime - regenerate with \"-writemap\"\nDisabling ExpCache"
                               return emptyStr

    fileThere <- runIO $ doesFileExist filename
    str <- runIO $ if fileThere then readFile filename >>= checkStr else do
        putStrLn $ "No \"" ++ filename ++ "\" file found - compiling withoug ExpCache\nRun with \"-writemap\" to generate \"" ++ filename ++ "\" and recompile"
        return emptyStr
    reader <- [| read |]
    return $ AppE reader $ LitE $ StringL str
    )
