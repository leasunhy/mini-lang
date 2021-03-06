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
    maybemain <- lookupFun (Ident "main")
    case maybemain of
      Just fun -> do invokeFun (Ident "main") []; return ()
      Nothing  -> return ()

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
  SRet exp -> do
    v <- eval exp
    modify (\s -> s{returnval = Just v, shouldret = True})
  SVRet -> do
    modify (\s -> s{returnval = Nothing, shouldret = True})
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
    pushFCtx
    forEach stms exec
    popFCtx
    popCtx
  SExp exp -> do
    eval exp
    return ()
  SEmpty -> return ()
  SFunDfn rettype funname paramlist body -> do
    defineFun funname (Function rettype paramlist body)

evalBool :: Exp -> Action Bool
evalBool e = do
  val <- eval e
  case val of
    VInt int  -> return (int /= 0)
    otherwise -> return (True)

manipVar :: (Value -> Action Value) -> Var -> Action (Value, Value)
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

returnVInt = returnVal VInt
returnVFlo = returnVal VFlo
returnVStr = returnVal VStr

returnBool :: (a -> a -> Bool) -> a -> a -> Action Value
returnBool func v1 v2 = return (booltoVInt (func v1 v2))

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
  EFunInv funname args -> do invokeFun funname args
  EPoInc v -> do (orival, newval) <- manipVar incval v; return newval
  EPoDec v -> do (orival, newval) <- manipVar decval v; return newval
  EPrInc v -> do (orival, newval) <- manipVar incval v; return orival
  EPrDec v -> do (orival, newval) <- manipVar decval v; return orival
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
      otherwise -> operateOnVals (returnVal VInt (+)) (returnVal VFlo (+)) (returnVal VStr (++)) v1 v2
  ESub exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVInt (-)) (returnVFlo (-)) (errorVal "operator '-' is not defined on strings") v1 v2
  EMul exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVInt (*)) (returnVFlo (*)) (errorVal "operator '*' is not defined on strings") v1 v2
  EDiv exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnVInt div) (returnVFlo (/)) (errorVal "operator '/' is not defined on strings") v1 v2
  EOrdLT exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnBool (<)) (returnBool (<)) (returnBool (<)) v1 v2
  EOrdLE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnBool (<=)) (returnBool (<=)) (returnBool (<=)) v1 v2
  EOrdGT exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnBool (>)) (returnBool (>)) (returnBool (>)) v1 v2
  EOrdGE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    operateOnVals (returnBool (>=)) (returnBool (>=)) (returnBool (>=)) v1 v2
  EOrdEQ exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    -- the values need to be in the same type to compare
    return (booltoVInt $ v1 == v2)
  EOrdNE exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    return (booltoVInt $ v1 /= v2)
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

-- define the type of a variable context
type Context = Map.Map Var (Maybe Value)

-- define the type of a function context
data Function = Function Type [Param] [Stm]

-- define the type of a function context
type FunContext = Map.Map Ident Function

-- iterate over a list of elementes
forEach :: [x] -> (x -> Action Void) -> Action Void
forEach ss comp = mapM_ comp ss

-- the environment

data Env = ENV {
  contexts  :: [Context],
  outputs   :: [String],
  functxs   :: [FunContext],
  returnval :: Maybe Value,
  shouldret :: Bool
  }

-- auxiliary functions

-- initial environment
initEnv :: Env
initEnv = ENV [Map.empty] [] [Map.empty] Nothing False

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

-- push a new context into the context stack
pushFCtx :: Action Void
pushFCtx = modify (\s -> s{functxs = Map.empty : (functxs s)})

-- pop a context from the context stack
popFCtx :: Action Void
popFCtx = modify (\s -> s{functxs = tail $ functxs s})

-- lookup the first (innermost) context that contains the specified function
--   if found, returns a "partition" of the context; otherwise, returns (head, tail)
lookupFCtx :: Ident -> Action (Either ([FunContext], FunContext, [FunContext]) (FunContext, [FunContext]))
lookupFCtx x = do
  cons <- gets functxs
  let (inits, remains) = span (Map.notMember x) cons in
    case remains of
      (context:tails) -> return (Left  (inits, context, tails))
      []              -> return (Right (head cons, tail cons))

-- define a function
defineFun :: Ident -> Function -> Action Void
defineFun x mv = modify (\s ->
        s{functxs = Map.insert x mv (head $ functxs s) : (tail $ functxs s)})

-- lookup a function
lookupFun :: Ident -> Action (Maybe Function)
lookupFun x = do
  cons <- lookupFCtx x
  case cons of
    Left (_, context, _) -> return $ Map.lookup x context
    Right _              -> return Nothing

-- invoke a function
invokeFun :: Ident -> [Exp] -> Action Value
invokeFun funname args = do
  maybef <- lookupFun funname
  case maybef of
    Just fun -> do
      -- check the number of arguments
      let (Function _ plist body) = fun in
        if length args == length plist then do
          pushCtx
          pushFCtx
          mapM_ (\((Param t v), exp) -> do val <- eval exp; updateVar v val) $ zip plist args
          forEach body (\stm -> do sret <- gets shouldret
                                   if not sret then exec stm else return ())
          popFCtx
          popCtx
          modify (\s -> s{shouldret = False})
          ret <- gets returnval
          case ret of
            Just v -> return v
            Nothing -> return (VInt 0)  -- dummy value
         else error $ "invalid number of arguments given for function: " ++ show funname
    Nothing -> error $ "undefined function: " ++ (show funname)

-- generate output
output :: String -> Action Void
output m = modify (\s ->
  s{outputs = outputs s ++ [m]}
  )

