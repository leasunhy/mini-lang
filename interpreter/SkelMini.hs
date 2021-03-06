module SkelMini where

-- Haskell module generated by the BNF converter

import AbsMini
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  PStms stms -> failure x
transType :: Type -> Result
transType x = case x of
  Type_double -> failure x
  Type_int -> failure x
  Type_string -> failure x
  Type_bool -> failure x
  Type_void -> failure x
transStm :: Stm -> Result
transStm x = case x of
  SDefi type_ assigns -> failure x
  SDecl type_ vars -> failure x
  SPrint exp -> failure x
  SAssign assign -> failure x
  SRet exp -> failure x
  SVRet -> failure x
  SWhile exp stm -> failure x
  SIf exp stm -> failure x
  SIfElse exp stm1 stm2 -> failure x
  SExp exp -> failure x
  SEmpty -> failure x
  SBlock stms -> failure x
  SFunDfn type_ ident params stms -> failure x
transParam :: Param -> Result
transParam x = case x of
  Param type_ var -> failure x
transAssign :: Assign -> Result
transAssign x = case x of
  Assign var exp -> failure x
transVar :: Var -> Result
transVar x = case x of
  Var ident -> failure x
transValue :: Value -> Result
transValue x = case x of
  VInt integer -> failure x
  VFlo double -> failure x
  VStr string -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EILit integer -> failure x
  EFLit double -> failure x
  ESLit string -> failure x
  EBTLit -> failure x
  EBFLit -> failure x
  EVar var -> failure x
  EFunInv ident exps -> failure x
  EPoInc var -> failure x
  EPoDec var -> failure x
  EPrInc var -> failure x
  EPrDec var -> failure x
  ENegate exp -> failure x
  EMul exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  EAdd exp1 exp2 -> failure x
  ESub exp1 exp2 -> failure x
  EOrdLT exp1 exp2 -> failure x
  EOrdLE exp1 exp2 -> failure x
  EOrdGT exp1 exp2 -> failure x
  EOrdGE exp1 exp2 -> failure x
  EOrdEQ exp1 exp2 -> failure x
  EOrdNE exp1 exp2 -> failure x
  EConj exp1 exp2 -> failure x
  EDisj exp1 exp2 -> failure x
  EAssign assign -> failure x

