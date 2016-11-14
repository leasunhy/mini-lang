module Interpreter where

import AbsMini
import PrintMini

import Control.Monad.State
import qualified Data.Map.Strict as Map

-- the interpreter

execProgram :: Program -> Action Void
execProgram p = case p of
  PStms ss -> do
    forEach ss exec

exec :: Stm -> Action Void
exec s = case s of
  SAssign x e -> do
    v <- eval e
    updateVar x v
  SPrint e -> do
    v <- eval e
    output (show v)

eval :: Exp -> Action Integer
eval e = case e of
  EAdd exp1 exp2  -> do
    v1 <- eval exp1
    v2 <- eval exp2
    return (v1 + v2)
  ESub exp1 exp2  -> do
    v1 <- eval exp1
    v2 <- eval exp2
    return (v1 - v2)
  EMul exp1 exp2  -> do
    v1 <- eval exp1
    v2 <- eval exp2
    return (v1 * v2)
  EDiv exp1 exp2  -> do
    v1 <- eval exp1
    v2 <- eval exp2
    return (v1 `div` v2)
  EInt n -> return n
  EId  x -> lookupVar x

-- Actions: functions with side effects on a state

-- an Action is a State monad on Environment
type Action a = State Env a

-- a familiar name for Action whose return value is uninteresting
type Void = ()

-- iterate over a list of elementes
forEach :: [x] -> (x -> Action Void) -> Action Void
forEach ss comp = mapM_ comp ss

-- the environment

data Env = ENV {
  variables :: Map.Map Ident Integer,
  outputs   :: [String]
  }

-- auxiliary functions

-- initial environment
initEnv :: Env
initEnv = ENV Map.empty []

-- update the value of a variable
updateVar :: Ident -> Integer -> Action Void
updateVar x v = modify (\s ->
  s{variables = Map.insert x v (variables s)}
  )

-- lookup the value of a variable
lookupVar :: Ident -> Action Integer
lookupVar x = do
  vars <- gets variables
  case Map.lookup x vars of
    Just v  -> return v
    Nothing -> error ("unknown variable " ++ printTree x)

-- generate output
output :: String -> Action Void
output m = modify (\s ->
  s{outputs = outputs s ++ [m]}
  )

