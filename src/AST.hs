module AST where

type File a = [Def a]
type Ident = String

type Def a = (Ident , Expr a)

data Expr a = EApp a (Expr a) (Expr a) | EAbstr a Ident (Expr a)
          | ELet a [Def a] (Expr a)
          | EITE a (Expr a) (Expr a) (Expr a)
          | ECase a (Expr a) (Expr a) (Ident, Ident, (Expr a))
            -- ^ ECase a x ("l","ls",y) is case a of [] -> x; l:ls -> y, as only
            -- lists must be matched
          | EDo a [(Expr a)]
          | Return a
          | EIdent a Ident 
          | EConst a Value 
          | EList a [(Expr a)]
          | EOp a Op
          | EUnaryMinus a (Expr a)
            deriving (Show)

posOf (EApp p _ _) = p
posOf (EAbstr p _ _) = p
posOf (ELet p _ _ ) = p
posOf (EITE p _ _ _) = p
posOf (ECase p _ _ _) = p
posOf (EDo p _) = p
posOf (Return p) = p
posOf (EIdent p _) = p
posOf (EConst p _) = p
posOf (EList p _) = p
posOf (EOp p _) = p
posOf (EUnaryMinus p _) = p

data Tokens =
       Telse | Tif | Tthen
     | Tlet  | Tin | Tcase
     |  Tof  | Tdo | Treturn
     | TBinOp Op      
     | TStr String
     | TInt Integer
     | TBool Bool
     | TLPar | TRPar
     | TRBrkt | TLBrkt
     | TComma
     | TBck
     | TArrow
     | TAffect
     | TIdent1 String
     | TIdent0 String
     | TConst Value
     | TMinus
     | TColon
     | TSemiColon
     | TRCurl
     | TLCurl
     | TEOF
     deriving  (Eq,Show)


data Op = 
       Plus | Minus | Times
     | Leq  | Geq   | G 
     | L    | Neq   | Eq
     | And  | Or    | Cons
     deriving (Eq, Show)

data Value =
      VString String
     | VInteger Integer
     | VChar Char
     | VBool Bool
     deriving (Eq,Show)



