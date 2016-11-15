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
                                        defineVar x (Just v))
  SPrint e -> do
    v <- eval e
    case v of
      VInt i -> output (show i)
      VFlo f -> output (show f)
      VStr s -> output s
  SAssign (Assign x e) -> do
    v <- eval e
    updateVar x v
  SWhile cond body -> do
    continue <- evalBool cond
    if continue then do
        exec body
        exec s
    else return ()
  SIf cond body -> do
    condtrue <- evalBool cond
    if condtrue then exec body else return ()
  SIfElse cond tbody fbody -> do
    condtrue <- evalBool cond
    if condtrue then exec tbody else exec fbody
  SBlock stms -> do
    pushCtx
    forEach stms exec
    popCtx
  SExp exp -> do
    eval exp
    return ()
  SEmpty -> return ()

evalBool :: Exp -> Action Bool
evalBool e = do
  val <- eval e
  case val of
    VInt int  -> return (int /= 0)
    otherwise -> return (True)

manipVar :: (Value -> Value) -> Var -> Action (Value, Value)
manipVar func x = do
  orival <- lookupVar x
  newval <- func orival
  updateVar x newval
  return (orival, newval)

operateOnVals forInt forFlo forStr v1 v2 = case (v1, v2) of
  ((VInt i1), (VInt i2)) -> forInt i1 i2
  ((VFlo f1), (VFlo f2)) -> forFlo f1 f2
  ((VStr s1), (VStr s2)) -> forStr s1 s2
  otherwise -> error "operand types mismatch"

returnVal :: (a -> Value) -> (a -> a -> a) -> a -> a -> Action Value
returnVal ctor func v1 v2 = return (ctor (func v1 v2))

errorVal :: String -> a -> a -> Action Value
errorVal msg v1 v2 = error msg

eval :: Exp -> Action Value
eval e = case e of
  EILit v  -> return (VInt v)
  EFLit v  -> return (VFlo v)
  ESLit v  -> return (VStr v)
  EBTLit   -> return (VInt 1)
  EBFLit   -> return (VInt 0)
  EVar v   -> lookupVar v
  EPoInc v -> fst $ manipVar incval v
  EPoDec v -> fst $ manipVar decval v
  EPrInc v -> snd $ manipVar incval v
  EPrDec v -> snd $ manipVar decval v
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
      ((VInt i), (VStr s)) -> return (VStr (show i ++ s))
      ((VStr s), (VInt i)) -> return (VStr (s ++ (show i)))
      otherwise -> operateOnVals (returnVal (+)) (returnVal (+)) (returnVal (++)) v1 v2
  ESub exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVal (-)) (returnVal (-)) (errorVal "operator '-' is not defined on strings") v1 v2
  EMul exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVal (*)) (returnVal (*)) (errorVal "operator '*' is not defined on strings") v1 v2
  EDiv exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVal div) (returnVal (/)) (errorVal "operator '/' is not defined on strings") v1 v2
  EOrdLT exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVal (<)) (returnVal (<)) (returnVal (<)) v1 v2
  EOrdLE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVal (<=)) (returnVal (<=)) (returnVal (<=)) v1 v2
  EOrdGT exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVal (>)) (returnVal (>)) (returnVal (>)) v1 v2
  EOrdGE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVal (>=)) (returnVal (>=)) (returnVal (>=)) v1 v2
  EOrdEQ exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    return (booltoVInt $ v1 == v2)
  EOrdNE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    return (booltoVInt $ i1 /= i2)
  EConj exp1 exp2 -> do
    v1 <- evalBool exp1
    if v1 then do
        v2 <- evalBool exp2
        return (booltoVInt v2)
    else
        return (booltoVInt v1)
  EDisj exp1 exp2 -> do
    v1 <- evalBool exp1
    if not v1 then do
        v2 <- evalBool exp2
        return (booltoVInt v2)
    else
        return (booltoVInt v1)
  EAssign (Assign v e) -> do
    val <- eval e
    updateVar v val
    return (val)

booltoVInt :: Bool -> Value
booltoVInt True  = VInt 1
booltoVInt False = VInt 0

incval :: Value -> Action Value
incval v = case v of
  VInt n -> return (VInt (n + 1))
  VStr s -> error "type error"
  VFlo n -> return (VFlo (n + 1.0))

decval :: Value -> Action Value
decval v = case v of
  VInt n -> return (VInt (n - 1))
  VStr s -> error "type error"
  VFlo n -> return (VFlo (n - 1.0))

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
  let (inits, remains) = span (Map.notMember x) cons in
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
  cons <- lookupCtx x
  case cons of
    Left  (inits, context, tails) ->
        modify (\s -> s{contexts = inits ++ ((Map.insert x (Just v) context) : tails)})
    Right (context, tails) ->
        modify (\s -> s{contexts = (Map.insert x (Just v) context : tails)})

-- lookup the value of a variable
lookupVar :: Var -> Action Value
lookupVar x = do
  cons <- lookupCtx x
  case cons of
    Left (_, context, _) -> case Map.lookup x context of
                              Just (Just v)  -> return v
                              otherwise -> error $ "uninitialized variable: " ++ show x
    Right _              -> error $ "uninitialized variable: " ++ show x

-- generate output
output :: String -> Action Void
output m = modify (\s ->
  s{outputs = outputs s ++ [m]}
  )

