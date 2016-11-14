module Interpreter where

import AbsMini
import PrintMini

import Data.Either
import Control.Monad.State
import qualified Data.Map.Strict as Map

-- the interpreter

execProgram :: Program -> Action Void
execProgram p = case p of
  PStms ss -> do
    forEach ss exec

exec :: Stm -> Action Void
exec s = case s of
  SDecl t vars -> do
    forEach vars (\x -> defineVar x Nothing)
  SDefi t assmts -> do
    -- TODO(leasunhy): add type checking!
    forEach assmts (\(Assign x e) -> do v <- eval e
                                        defineVar x v)
  SPrint e -> do
    v <- eval e
    output (show v)
  SAssign Assign x e -> do
    v <- eval e
    updateVar x v
  SWhile cond body -> do
    continue <- evalBoolean cond
    if continue then
        eval body
        exec s
    else return ()
  SIf cond body -> do
    condtrue <- evalBoolean cond
    if condtrue then exec body else return ()
  SIfElse cond tbody fbody -> do
    condtrue <- evalBoolean cond
    if condtrue then exec tbody else exec fbody
  SBlock stms -> do
    pushCtx
    forEach stms exec
    popCtx
  SExp exp -> eval exp
  SEmpty -> return ()

evalBoolean :: Exp -> Action Boolean
evalBoolean e = do
  val <- eval e
  case val of
    VInt int  -> return (int \= 0)
    otherwise -> return (True)

eval :: Exp -> Action Value
eval e = case e of
  EILit v  -> return (VInt v)
  EFLit v  -> return (VFlo v)
  ESLit v  -> return (VStr v)
  EBTLit   -> return (VInt 1)
  EBFLit   -> return (VInt 0)
  EVar v   -> lookupVar v
  EPoInc v -> do
    orival <- lookupVar v
    newval <- incval v
    updateVar v newval
    return (orival)
  EPoDec v -> do
    orival <- lookupVar v
    newval <- decval v
    updateVar v newval
    return (orival)
  EPrInc v -> do
    orival <- lookupVar v
    newval <- incval v
    updateVar v newval
    return (newval)
  EPrDec v -> do
    orival <- lookupVar v
    newval <- decval v
    updateVar v newval
    return (newval)
  ENegate e -> do
    val <- eval e
    case val of
      VInt n -> return (VInt (-n))
      VFlo n -> return (VFlo (-n))
      VStr _ -> error "type error"
  EAdd exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (VInt (i1 + i2))
      ((VFlo f1), (VFlo f2)) -> return (VFlo (f1 + f2))
      ((VStr s1), (VStr s2)) -> return (VStr (s1 ++ s2))
      otherwise -> error "operand type mismatch"
  ESub exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (VInt (i1 - i2))
      ((VFlo f1), (VFlo f2)) -> return (VFlo (f1 - f2))
      ((VStr s1), (VStr s2)) -> error "operator '-' is not defined on strings"
      otherwise -> error "operand type mismatch"
  EMul exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (VInt (i1 * i2))
      ((VFlo f1), (VFlo f2)) -> return (VFlo (f1 * f2))
      ((VStr s1), (VStr s2)) -> error "operator '*' is not defined on strings"
      otherwise -> error "operand type mismatch"
  EDiv exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (VInt (i1 `div` i2))
      ((VFlo f1), (VFlo f2)) -> return (VFlo (f1 / f2))
      ((VStr s1), (VStr s2)) -> error "operator '/' is not defined on strings"
      otherwise -> error "operand type mismatch"
  EOrdLT exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (booltoVInt $ i1 < i2)
      ((VFlo f1), (VFlo f2)) -> return (booltoVInt $ f1 < f2)
      ((VStr s1), (VStr s2)) -> return (booltoVInt $ s1 < s2)
      otherwise -> error "operand type mismatch"
  EOrdLE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (booltoVInt $ i1 <= i2)
      ((VFlo f1), (VFlo f2)) -> return (booltoVInt $ f1 <= f2)
      ((VStr s1), (VStr s2)) -> return (booltoVInt $ s1 <= s2)
      otherwise -> error "operand type mismatch"
  EOrdGT exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (booltoVInt $ i1 > i2)
      ((VFlo f1), (VFlo f2)) -> return (booltoVInt $ f1 > f2)
      ((VStr s1), (VStr s2)) -> return (booltoVInt $ s1 > s2)
      otherwise -> error "operand type mismatch"
  EOrdGE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (booltoVInt $ i1 >= i2)
      ((VFlo f1), (VFlo f2)) -> return (booltoVInt $ f1 >= f2)
      ((VStr s1), (VStr s2)) -> return (booltoVInt $ s1 >= s2)
      otherwise -> error "operand type mismatch"
  EOrdEQ exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (booltoVInt $ i1 == i2)
      ((VFlo f1), (VFlo f2)) -> return (booltoVInt $ f1 == f2)
      ((VStr s1), (VStr s2)) -> return (booltoVInt $ s1 == s2)
      otherwise -> error "operand type mismatch"
  EOrdNE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1, v2) of
      ((VInt i1), (VInt i2)) -> return (booltoVInt $ i1 /= i2)
      ((VFlo f1), (VFlo f2)) -> return (booltoVInt $ f1 /= f2)
      ((VStr s1), (VStr s2)) -> return (booltoVInt $ s1 /= s2)
      otherwise -> error "operand type mismatch"
  EConj exp1 exp2 -> do
    v1 <- evalBoolean exp1
    v2 <- evalBoolean exp2
    return (booltoVInt $ v1 && v2)
  EDisj exp1 exp2 -> do
    v1 <- evalBoolean exp1
    v2 <- evalBoolean exp2
    return (booltoVInt $ v1 || v2)
  EAssign (Assign v e) -> do
    val <- eval e
    updateVar v val
    return (val)

booltoVInt :: Boolean -> Action VInt
booltoVInt True  -> return (VInt 1)
booltoVInt False -> return (VInt 0)

incval :: Value -> Action Value
incval v = case v of
  VInt n = return (VInt (n + 1))
  VStr s = error "type error"
  VFlo n = return (VFlo (n + 1.0))

decval :: Value -> Action Value
decval v = case v of
  VInt n = return (VInt (n - 1))
  VStr s = error "type error"
  VFlo n = return (VFlo (n - 1.0))

-- Actions: functions with side effects on a state

-- an Action is a State monad on Environment
type Action a = State Env a

-- a familiar name for Action whose return value is uninteresting
type Void = ()

-- define the type of a context
type Context = Map.Map Var (Maybe Value)

-- iterate over a list of elementes
forEach :: [x] -> (x -> Action Void) -> Action Void
forEach ss comp = mapM_ comp ss

-- the environment

data Env = ENV {
  contexts  :: [Context],
  outputs   :: [String]
  }

-- auxiliary functions

-- initial environment
initEnv :: Env
initEnv = ENV [Map.empty] []

-- push a new context into the context stack
pushCtx :: Action Void
pushCtx = modify (\s -> s{contexts = Map.empty : (contexts s)})

-- pop a context from the context stack
popCtx :: Action Void
popCtx = modify (\s -> s{contexts = tail $ contexts s})

-- lookup the first (innermost) context that contains the specified variable
--   if found, returns a "partition" of the context; otherwise, returns (head, tail)
lookupCtx :: Var -> Action (Either ([Context], Context, [Context]) (Context, [Context]))
lookupCtx x = do
  cons <- gets contexts
  (inits, remains) <- span (Map.notMember x) cons
  case remains of
    (context:tails) -> return (Left  (inits, context, tails))
    []              -> return (Right (head cons, tail cons))

-- define a variable
defineVar :: Var -> Maybe Value -> Action Void
defineVar x mv = modify (\s ->
        s{contexts = Map.insert x mv (head $ contexts s) : (tail $ contexts s)})

-- update the value of a variable
updateVar :: Var -> Value -> Action Void
updateVar x v = do
  case lookupCtx x of
    Left  (inits, context, tails) ->
        modify (\s -> s{contexts = inits ++ ((Map.insert x (Just v) context) : tails)})
    Right (context, tails) ->
        modify (\s -> s{contexts = (Map.insert x (Just v) context : tails)})

-- lookup the value of a variable
lookupVar :: Var -> Action Value
lookupVar x = do
  case lookupCtx x of
    Left (_, context, _) -> case Map.lookup x context of
                              Just (Just v)  -> return v
                              otherwise -> error "uninitialized variable: " ++ show x
    Right _              -> error $ "uninitialized variable: " ++ show x

-- generate output
output :: String -> Action Void
output m = modify (\s ->
  s{outputs = outputs s ++ [m]}
  )

