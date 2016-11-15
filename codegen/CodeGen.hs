module CodeGen where

import AbsMini
import PrintMini

import Data.Either
import Control.Monad.State
import qualified Data.Map.Strict as Map

-- the codegen component

genCodeForProgram :: Program -> Action Void
genCodeForProgram p = case p of
  PStms ss -> do
    emit classPrelude
    emit mainMethodPrelude
    forEach ss compileStm
    emit "return"
    emit ".end method"
    -- maybemain <- lookupFun (Ident "main")
    -- case maybemain of
    --   Just fun -> do invokeFun (Ident "main") []; return ()
    --   Nothing  -> return ()

compileStm :: Stm -> Action Void
compileStm s = case s of
  SDecl t vars -> do
    forEach vars (\x -> declareVar x t)
  SDefi t assmts -> do
    -- TODO(leasunhy): add type checking!
    forEach assmts (\(Assign x e) -> do
        et <- compileExp e
        let vt = if t == Type_bool then Type_int else t in
          if et == vt then do
            declareVar x vt
            vi <- lookupVar x
            storevar vi
            return ()
          else
            error "trying to assign a value to a variable of different type in definition"
          )
  SPrint e -> do
    t <- compileExp e
    case t of
      Type_int -> emit "invokestatic MiniRuntime/print(I)V"
      Type_bool -> emit "invokestatic MiniRuntime/print(I)V"
      Type_double -> emit "invokestatic MiniRuntime/print(D)V"
      Type_string -> emit "invokestatic MiniRuntime/print(Ljava/lang/String;)V"
      Type_void -> error "can't print void values"
  SAssign (Assign x e) -> do
    et <- compileExp e
    -- vi <- lookupVar x
    vi <- lookupOrAddVar x et
    let (VarInfo vt _) = vi in
      if et == vt then do storevar vi; return ()
      else error "trying to assign a value to a variable of different type in assignment"
  SRet exp -> do
    t <- compileExp exp
    case t of
      Type_int -> emit "ireturn"
      Type_bool -> emit "ireturn"
      Type_double -> emit "dreturn"
      Type_string -> emit "areturn"
      Type_void -> error "trying to return a void value"
  SVRet -> emit "return"
  SWhile cond body -> do
    curlabel <- getnextlabel
    let startlabel = "CMINILWHILE" ++ show curlabel
        bodylabel  = startlabel ++ "B"
        endlabel   = startlabel ++ "E" in do
      emit $ startlabel ++ ":"
      compileExp cond
      emit "iconst_1"
      emit $ "if_icmpne " ++ endlabel
      emit $ bodylabel ++ ":"
      compileStm body
      emit $ "goto " ++ startlabel
      emit $ endlabel ++ ":"
  SIf cond body -> do
    curlabel <- getnextlabel
    let startlabel = "CMINILIF" ++ show curlabel
        bodylabel  = startlabel ++ "B"
        endlabel   = startlabel ++ "E" in do
      emit $ startlabel ++ ":"
      compileExp cond
      emit "iconst_1"
      emit $ "if_icmpne " ++ endlabel
      emit $ bodylabel ++ ":"
      compileStm body
      emit $ endlabel ++ ":"
  SIfElse cond tbody fbody -> do
    curlabel <- getnextlabel
    let startlabel = "CMINILIF" ++ show curlabel
        tbodylabel  = startlabel ++ "T"
        fbodylabel  = startlabel ++ "F"
        endlabel   = startlabel ++ "E" in do
      emit $ startlabel ++ ":"
      compileExp cond
      emit "iconst_1"
      emit $ "if_icmpne " ++ fbodylabel
      emit $ tbodylabel ++ ":"
      compileStm tbody
      emit $ "goto " ++ endlabel
      emit $ fbodylabel ++ ":"
      compileStm fbody
      emit $ endlabel ++ ":"
  SBlock stms -> do
    pushCtx
    pushFCtx
    forEach stms compileStm
    popFCtx
    popCtx
  SExp exp -> do
    compileExp exp
    -- discard the result
    emit "pop"
    return ()
  SEmpty -> return ()
  SFunDfn rettype funname paramlist body -> return ()
    -- defineFun funname (Function rettype paramlist body)

handleSameType forInt forFlo forStr t1 t2 = case (t1, t2) of
  (Type_int, Type_int) -> do forInt; return Type_int
  (Type_bool, Type_bool) -> do forInt; return Type_int
  (Type_double, Type_double) -> do forFlo; return Type_double
  (Type_string, Type_string) -> do forStr; return Type_string
  otherwise -> error $ "operand types mismatch: " ++ show t1 ++ " vs " ++ show t2

compileExp :: Exp -> Action Type
compileExp e = case e of
  EILit v  -> do emit ("ldc " ++ show v); return Type_int
  EFLit v  -> do emit ("ldc " ++ show v); return Type_double
  ESLit v  -> do emit ("ldc " ++ show v); return Type_string
  EBTLit   -> do emit "iconst_1"; return Type_int
  EBFLit   -> do emit "iconst_0"; return Type_int
  EVar v   -> do
    vi <- lookupVar v
    loadvar vi
  EFunInv funname args -> do
    fun <- lookupFun funname
    case fun of
      Just (Function rettype plist spec) -> do
        if length plist == length args then do
          mapM_ compileExp args
          emit $ "invokestatic Foo/" ++ spec
          return rettype
        else error $ "no viable overload for " ++ show funname ++ " is found"
      Nothing -> error $ "no function named " ++ show funname ++ " is found"
  EPoInc v -> do vi <- lookupVar v; incvar vi; loadvar vi
  EPoDec v -> do vi <- lookupVar v; decvar vi; loadvar vi
  EPrInc v -> do vi <- lookupVar v; loadvar vi; incvar vi
  EPrDec v -> do vi <- lookupVar v; loadvar vi; decvar vi
  ENegate e -> do
    t <- compileExp e
    case t of
      Type_int -> emit "ineg"
      Type_bool -> emit "ineg"
      Type_double -> emit "dneg"
      Type_string -> error "type error"
    return t
  EAdd exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    case (t1, t2) of
      (Type_int, Type_string) -> do
        emit ("invokestatic MiniRuntime/add(ILjava/lang/String;)Ljava/lang/String;")
        return Type_string
      (Type_string, Type_int) -> do
        emit ("invokestatic MiniRuntime/add(Ljava/lang/String;I)Ljava/lang/String;")
        return Type_string
      otherwise ->
        handleSameType (emit "iadd") (emit "dadd") (emit "invokestatic MiniRuntime/add(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;") t1 t2
  ESub exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "isub") (emit "dsub") (error "operator '-' is not defined on strings") t1 t2
  EMul exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "imul") (emit "dmul") (error "operator '*' is not defined on strings") t1 t2
  EDiv exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "idiv") (emit "ddiv") (error "operator '/' is not defined on strings") t1 t2
  EOrdLT exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "invokestatic MiniRuntime/lt(II)Z") (emit "invokestatic MiniRuntime/lt(DD)Z") (emit "invokestatic MiniRuntime/lt(Ljava/lang/String;Ljava/lang/String;)Z") t1 t2
  EOrdLE exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "invokestatic MiniRuntime/le(II)Z") (emit "invokestatic MiniRuntime/le(DD)Z") (emit "invokestatic MiniRuntime/le(Ljava/lang/String;Ljava/lang/String;)Z") t1 t2
  EOrdGT exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "invokestatic MiniRuntime/gt(II)Z") (emit "invokestatic MiniRuntime/gt(DD)Z") (emit "invokestatic MiniRuntime/gt(Ljava/lang/String;Ljava/lang/String;)Z") t1 t2
  EOrdGE exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "invokestatic MiniRuntime/ge(II)Z") (emit "invokestatic MiniRuntime/ge(DD)Z") (emit "invokestatic MiniRuntime/ge(Ljava/lang/String;Ljava/lang/String;)Z") t1 t2
  EOrdEQ exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "invokestatic MiniRuntime/eq(II)Z") (emit "invokestatic MiniRuntime/eq(DD)Z") (emit "invokestatic MiniRuntime/eq(Ljava/lang/String;Ljava/lang/String;)Z") t1 t2
  EOrdNE exp1 exp2 -> do
    t1 <- compileExp exp1
    t2 <- compileExp exp2
    handleSameType (emit "invokestatic MiniRuntime/ne(II)Z") (emit "invokestatic MiniRuntime/ne(DD)Z") (emit "invokestatic MiniRuntime/ne(Ljava/lang/String;Ljava/lang/String;)Z") t1 t2
  EConj exp1 exp2 -> do
    curlabel <- getnextlabel
    let label = "CMINIL" ++ show curlabel
        elabel = label ++ "E" in do
      -- TODO: handle string and double values
      compileExp exp1
      emit "iconst_1"
      emit $ "if_icmpne " ++ label
      compileExp exp2
      emit "iconst_1"
      emit $ "if_icmpne " ++ label
      emit "iconst_1"
      emit $ "goto " ++ elabel
      emit $ label ++ ":"
      emit "iconst_0"
      emit $ elabel ++ ":"
      return Type_int
  EDisj exp1 exp2 -> do
    curlabel <- getnextlabel
    let label = "CMINIL" ++ show curlabel
        elabel = label ++ "E" in do
      -- TODO: handle string and double values
      compileExp exp1
      emit "iconst_1"
      emit $ "if_icmpeq " ++ label
      compileExp exp2
      emit "iconst_1"
      emit $ "if_icmpeq " ++ label
      emit "iconst_0"
      emit $ "goto " ++ elabel
      emit $ label ++ ":"
      emit "iconst_1"
      emit $ elabel ++ ":"
      return Type_int
  EAssign (Assign v e) -> do
    vi <- lookupVar v
    et <- compileExp e
    let (VarInfo vt _) = vi in
      if vt == et then storevar vi
      else error "operands of an assignment are not of the same type"

incvar :: VarInfo -> Action Type
incvar (VarInfo t addr) = do
  case t of
    Type_int -> emit $ "iinc " ++ show addr ++ " 1"
    Type_bool -> emit $ "iinc " ++ show addr ++ " 1"
    Type_double ->
      emit $ unlines $ ["dstore " ++ show addr, "dconst_1", "dadd", "dload " ++ show addr]
    Type_string -> error $ "a string variable cannot be decremented"
  return t

decvar :: VarInfo -> Action Type
decvar (VarInfo t addr) = do
  case t of
    Type_int -> emit $ "iinc " ++ show addr ++ " -1"
    Type_bool -> emit $ "iinc " ++ show addr ++ " -1"
    Type_double ->
      emit $ unlines $ ["dstore " ++ show addr, "dconst_1", "dsub", "dload " ++ show addr]
    Type_string -> error $ "a string variable cannot be decremented"
  return t

storevar :: VarInfo -> Action Type
storevar (VarInfo t addr) = do
  case t of
    Type_int -> emit $ "istore " ++ show addr
    Type_bool -> emit $ "istore " ++ show addr
    Type_double -> emit $ "dstore " ++ show addr
    Type_string -> emit $ "astore " ++ show addr
  return t

loadvar :: VarInfo -> Action Type
loadvar (VarInfo t addr) = do
  case t of
    Type_int -> emit $ "iload " ++ show addr
    Type_bool -> emit $ "iload " ++ show addr
    Type_double -> emit $ "dload " ++ show addr
    Type_string -> emit $ "aload " ++ show addr
  return t

-- Actions: functions with side effects on a state

-- an Action is a State monad on Environment
type Action a = State Env a

-- a familiar name for Action whose return value is uninteresting
type Void = ()

-- a verbose name for expressing the address of a variable
type Address = Int

-- a type holding the info for a variable
data VarInfo = VarInfo Type Address

-- define the type of a variable context
type Context = Map.Map Var VarInfo

-- define the type of a function context
data Function = Function Type [Param] String

-- define the type of a function context
type FunContext = Map.Map Ident Function

-- iterate over a list of elementes
forEach :: [x] -> (x -> Action Void) -> Action Void
forEach ss comp = mapM_ comp ss

-- the environment

data Env = ENV {
  contexts       :: [Context],
  instructions   :: [String],
  functxs        :: [FunContext],
  nextvar        :: Int,
  nextlabel      :: Int
  }

-- auxiliary functions

-- initial environment
initEnv :: Env
initEnv = ENV [Map.empty] [] [Map.empty] 5 0

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

-- declare a variable
declareVar :: Var -> Type -> Action Void
declareVar _ Type_void = error "a variable can not be void!"
declareVar x Type_bool = declareVar x Type_int
declareVar x t = modify (\s ->
  s{contexts = Map.insert x (VarInfo t (nextvar s)) (head $ contexts s) : (tail $ contexts s),
    nextvar = nextvar s + 1
  })

-- lookup the value of a variable
lookupVar :: Var -> Action VarInfo
lookupVar x = do
  cons <- lookupCtx x
  case cons of
    Left (_, context, _) -> case Map.lookup x context of
                              Just vi   -> return vi
                              otherwise -> error $ "unknown variable: " ++ show x
    Right _              -> error $ "unknown variable: " ++ show x

-- lookup the value of a variable
lookupOrAddVar :: Var -> Type -> Action VarInfo
lookupOrAddVar x t = do
  cons <- lookupCtx x
  case cons of
    Left (_, context, _) -> case Map.lookup x context of
                              Just vi   -> return vi
                              otherwise -> do declareVar x t; lookupVar x
    Right _              -> do declareVar x t; lookupVar x

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

-- get next label
getnextlabel :: Action Int
getnextlabel = do
  cur <- gets nextlabel
  modify (\s -> s{nextlabel = cur + 1})
  return cur

-- emit instructions
emit :: String -> Action Void
emit i = modify (\s ->
  s{instructions = instructions s ++ [i]}
  )

-- boilerplate code

classPrelude = unlines [
  ".class public Foo",
  ".super java/lang/Object",
  ".method public <init>()V",
  "  aload_0",
  "  invokenonvirtual java/lang/Object/<init>()V",
  "  return",
  ".end method"
  ]

mainMethodPrelude = unlines [
  ".method public static main([Ljava/lang/String;)V",
  ".limit locals 100",
  ".limit stack 1000"
  ]

