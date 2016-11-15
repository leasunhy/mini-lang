import CodeGen

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
        interact $ \s -> codegen s
    else do
        s <- readFile (args !! 0)
        putStr $ codegen s

codegen :: String -> String
codegen s = case pProgram (myLexer s) of
    Bad err -> err
    Ok  tree ->
        let (_, s) = runState (genCodeForProgram tree) initEnv
        in unlines (instructions s)

