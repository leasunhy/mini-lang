import Interpreter

import AbsMini
import LexMini
import ParMini

import ErrM
import Control.Monad.State
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        interact $ \s -> interpret s
    else do
        s <- readFile (args !! 0)
        putStr $ interpret s

interpret :: String -> String
interpret s = case pProgram (myLexer s) of
    Bad err -> err
    Ok  tree ->
        let (_, s) = runState (execProgram tree) initEnv
        in unlines (outputs s)

