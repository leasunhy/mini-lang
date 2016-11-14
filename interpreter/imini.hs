import Interpreter

import AbsMini
import LexMini
import ParMini

import ErrM
import Control.Monad.State

main :: IO ()
main = do
    s <- getContents
    putStrLn (interpret s)

interpret :: String -> String
interpret s = case pProgram (myLexer s) of
    Bad err -> err
    Ok  tree ->
        let (_, s) = runState (execProgram tree) initEnv
        in unlines (output s)

