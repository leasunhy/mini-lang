-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParMini where
import AbsMini
import LexMini
import ErrM

}

%name pProgram Program
%name pType Type
%name pStm Stm
%name pListStm ListStm
%name pAssign Assign
%name pListAssign ListAssign
%name pVar Var
%name pListVar ListVar
%name pValue Value
%name pExp15 Exp15
%name pExp14 Exp14
%name pExp13 Exp13
%name pExp12 Exp12
%name pExp11 Exp11
%name pExp9 Exp9
%name pExp8 Exp8
%name pExp4 Exp4
%name pExp3 Exp3
%name pExp2 Exp2
%name pExp Exp
%name pExp1 Exp1
%name pExp5 Exp5
%name pExp6 Exp6
%name pExp7 Exp7
%name pExp10 Exp10
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!=' { PT _ (TS _ 1) }
  '&&' { PT _ (TS _ 2) }
  '(' { PT _ (TS _ 3) }
  ')' { PT _ (TS _ 4) }
  '*' { PT _ (TS _ 5) }
  '+' { PT _ (TS _ 6) }
  '++' { PT _ (TS _ 7) }
  ',' { PT _ (TS _ 8) }
  '-' { PT _ (TS _ 9) }
  '--' { PT _ (TS _ 10) }
  '/' { PT _ (TS _ 11) }
  ';' { PT _ (TS _ 12) }
  '<' { PT _ (TS _ 13) }
  '<=' { PT _ (TS _ 14) }
  '=' { PT _ (TS _ 15) }
  '==' { PT _ (TS _ 16) }
  '>' { PT _ (TS _ 17) }
  '>=' { PT _ (TS _ 18) }
  'bool' { PT _ (TS _ 19) }
  'double' { PT _ (TS _ 20) }
  'else' { PT _ (TS _ 21) }
  'false' { PT _ (TS _ 22) }
  'if' { PT _ (TS _ 23) }
  'int' { PT _ (TS _ 24) }
  'print' { PT _ (TS _ 25) }
  'string' { PT _ (TS _ 26) }
  'true' { PT _ (TS _ 27) }
  'void' { PT _ (TS _ 28) }
  'while' { PT _ (TS _ 29) }
  '{' { PT _ (TS _ 30) }
  '||' { PT _ (TS _ 31) }
  '}' { PT _ (TS _ 32) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }
L_quoted { PT _ (TL $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
String  :: { String }  : L_quoted {  $1 }

Program :: { Program }
Program : ListStm { AbsMini.PStms (reverse $1) }
Type :: { Type }
Type : 'double' { AbsMini.Type_double }
     | 'int' { AbsMini.Type_int }
     | 'string' { AbsMini.Type_string }
     | 'bool' { AbsMini.Type_bool }
     | 'void' { AbsMini.Type_void }
Stm :: { Stm }
Stm : Type ListAssign ';' { AbsMini.SDefi $1 $2 }
    | Type ListVar ';' { AbsMini.SDecl $1 $2 }
    | 'print' Exp ';' { AbsMini.SPrint $2 }
    | Assign ';' { AbsMini.SAssign $1 }
    | 'while' '(' Exp ')' Stm { AbsMini.SWhile $3 $5 }
    | 'if' '(' Exp ')' Stm { AbsMini.SIf $3 $5 }
    | 'if' '(' Exp ')' Stm 'else' Stm { AbsMini.SIfElse $3 $5 $7 }
    | '{' ListStm '}' { AbsMini.SBlock (reverse $2) }
    | Exp ';' { AbsMini.SExp $1 }
    | ';' { AbsMini.SEmpty }
ListStm :: { [Stm] }
ListStm : {- empty -} { [] } | ListStm Stm { flip (:) $1 $2 }
Assign :: { Assign }
Assign : Var '=' Exp { AbsMini.Assign $1 $3 }
ListAssign :: { [Assign] }
ListAssign : {- empty -} { [] }
           | Assign { (:[]) $1 }
           | Assign ',' ListAssign { (:) $1 $3 }
Var :: { Var }
Var : Ident { AbsMini.Var $1 }
ListVar :: { [Var] }
ListVar : {- empty -} { [] }
        | Var { (:[]) $1 }
        | Var ',' ListVar { (:) $1 $3 }
Value :: { Value }
Value : Integer { AbsMini.VInt $1 }
      | Double { AbsMini.VFlo $1 }
      | String { AbsMini.VStr $1 }
Exp15 :: { Exp }
Exp15 : Integer { AbsMini.EILit $1 }
      | Double { AbsMini.EFLit $1 }
      | String { AbsMini.ESLit $1 }
      | 'true' { AbsMini.EBTLit }
      | 'false' { AbsMini.EBFLit }
      | Var { AbsMini.EVar $1 }
      | '(' Exp ')' { $2 }
Exp14 :: { Exp }
Exp14 : Var '++' { AbsMini.EPoInc $1 }
      | Var '--' { AbsMini.EPoDec $1 }
      | '++' Var { AbsMini.EPrInc $2 }
      | '--' Var { AbsMini.EPrDec $2 }
      | Exp15 { $1 }
Exp13 :: { Exp }
Exp13 : '-' Exp14 { AbsMini.ENegate $2 } | Exp14 { $1 }
Exp12 :: { Exp }
Exp12 : Exp12 '*' Exp13 { AbsMini.EMul $1 $3 }
      | Exp12 '/' Exp13 { AbsMini.EDiv $1 $3 }
      | Exp13 { $1 }
Exp11 :: { Exp }
Exp11 : Exp11 '+' Exp12 { AbsMini.EAdd $1 $3 }
      | Exp11 '-' Exp12 { AbsMini.ESub $1 $3 }
      | Exp12 { $1 }
Exp9 :: { Exp }
Exp9 : Exp9 '<' Exp11 { AbsMini.EOrdLT $1 $3 }
     | Exp9 '<=' Exp11 { AbsMini.EOrdLE $1 $3 }
     | Exp9 '>' Exp11 { AbsMini.EOrdGT $1 $3 }
     | Exp9 '>=' Exp11 { AbsMini.EOrdGE $1 $3 }
     | Exp10 { $1 }
Exp8 :: { Exp }
Exp8 : Exp8 '==' Exp9 { AbsMini.EOrdEQ $1 $3 }
     | Exp8 '!=' Exp9 { AbsMini.EOrdNE $1 $3 }
     | Exp9 { $1 }
Exp4 :: { Exp }
Exp4 : Exp4 '&&' Exp8 { AbsMini.EConj $1 $3 } | Exp5 { $1 }
Exp3 :: { Exp }
Exp3 : Exp3 '||' Exp4 { AbsMini.EDisj $1 $3 } | Exp4 { $1 }
Exp2 :: { Exp }
Exp2 : Assign { AbsMini.EAssign $1 } | Exp3 { $1 }
Exp :: { Exp }
Exp : Exp1 { $1 }
Exp1 :: { Exp }
Exp1 : Exp2 { $1 }
Exp5 :: { Exp }
Exp5 : Exp6 { $1 }
Exp6 :: { Exp }
Exp6 : Exp7 { $1 }
Exp7 :: { Exp }
Exp7 : Exp8 { $1 }
Exp10 :: { Exp }
Exp10 : Exp11 { $1 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}
