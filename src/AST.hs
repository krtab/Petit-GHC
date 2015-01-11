module AST where

type File a = [Def a]
type Ident = String

type Def a = (Ident , Expr a)

data Expr a = EApp a (Expr a) (Expr a) --
            | EAbstr a Ident (Expr a) --
            | ELet a [Def a] (Expr a)
            | EITE a (Expr a) (Expr a) (Expr a) --
            | ECase a --
            | EIdent a Ident --
            | EConst a Value --
            | EOp a Op --
            | EUnaryMinus a --
            | EEList a
            | Return a 
            deriving (Show)

data SExpr a = SEApp (SExpr a) (SExpr a)--
             | SEClos  [SExpr a] Ident --
             | SEIdent  Ident 
             | SEConst  Value --
             | SEClotElem Int --
             | SELocal Int Int --
             | SEArg0 --
             | SELet [SExpr a] (SExpr a)
               deriving (Show)

posOf (EApp p _ _) = p
posOf (EAbstr p _ _) = p
posOf (ELet p _ _ ) = p
posOf (EITE p _ _ _) = p
posOf (ECase p) = p
posOf (Return p) = p
posOf (EIdent p _) = p
posOf (EConst p _) = p
posOf (EEList p) = p
posOf (EOp p _) = p
posOf (EUnaryMinus p) = p

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
     | MonadSeq
     deriving (Eq, Show)

data Value =
      VString String
     | VInteger Integer
     | VChar Char
     | VBool Bool
     deriving (Eq,Show)




