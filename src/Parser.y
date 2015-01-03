{
module Parser where
import AST
import Lexer
}
%monad {Either (Int,Int)}
%tokentype { (AlexPosn,Tokens) }
%error { \(((AlexPn _ line column),_):_) -> Left (line,column)}
%name parser

%nonassoc in
%nonassoc else
%nonassoc '->'
%left '||'
%left '&&'
%left '<' '>' '<=' '>=' '==' '/='
%right ':'
%left '+' '-'
%nonassoc NEG
%left '*'
%left APP

%token
        else    {(_,Telse)}
        if      {(_,Tif)}
        then    {(_,Tthen)}
        let     {(_,Tlet)}
        in      {(_,Tin)}
        case    {(_,Tcase)}
        of      {(_,Tof)}
        do      {(_,Tdo)}
        return  {(_,Treturn)}
        '('     {(_,TLPar)}
        ')'     {(_,TRPar)}
        '['     {(_,TLBrkt)}
        ']'     {(_,TRBrkt)}
        '{'     {(_,TLCurl)}
        '}'     {(_,TRCurl)}
        '='     {(_,TAffect)}
        ','     {(_,TComma)}
        '->'    {(_,TArrow)}
        ':'     {(_,TColon)}
        ';'     {(_,TSemiColon)}
        lambda  {(_,TBck)}
        '+'      {(_,TBinOp Plus)}
        '*'      {(_,TBinOp Times)}
        '<='      {(_,TBinOp Leq)}
        '>='      {(_,TBinOp Geq)}
        '>'      {(_,TBinOp G)}
        '<'      {(_,TBinOp L)}
        '/='      {(_,TBinOp Neq)}
        '=='      {(_,TBinOp Eq)}
        '&&'      {(_,TBinOp And)}
        '||'      {(_,TBinOp Or)}
        '-'     {(_,TMinus)}
        ident0  {(_,TIdent0 _)}
        ident1  {(_,TIdent1 _)}
        const   {(_,TConst _)}



%%

rev_enum1(p):        
         p     { [$1] }
        | rev_enum1(p) p {$2 : $1}

rev_enum(p) :        { [] }
           | rev_enum1(p) {$1}

rev_enum_sep1(p,s):     
        p              {[$1]}
       | rev_enum_sep1(p,s) s p {$3 : $1}

rev_enum_sep(p,s) :         { [] }
           | rev_enum_sep1(p,s) {$1}


option(p):                 {Nothing}
        | p                {Just $1}



file 
     : rev_enum(def0)           {reverse $1}


def0 :: { Def AlexPosn }
     : ident0 rev_enum(ident1) '=' expr     {((\(_,TIdent0 s) -> s) $1,foldr (\(pos,TIdent1 s) exp -> EAbstr pos s exp) $4 (reverse $2) )}

def1 :: { Def AlexPosn }
     : ident1 rev_enum(ident1) '=' expr     {((\(_,TIdent1 s) -> s) $1,foldr (\(pos,TIdent1 s) exp -> EAbstr pos s exp) $4 (reverse $2) )}


simple_expr :: { Expr AlexPosn }
            : '(' expr ')' {$2}
            | '[' rev_enum_sep(expr,',') ']' {foldr (\x xs -> let l = posOf x in EApp l (EApp l (EOp l Cons) x) xs) (EEList (fst $3)) $2 }
            | ident1     {(\(pos,TIdent1 s) -> EIdent pos s) $1}
            | const      {(\(pos,TConst v) ->  EConst pos v) $1}


expr :: { Expr AlexPosn}
   : rev_enum1(simple_expr)                                                                   {foldl1 (\e1 e2 -> EApp (posOf e1) e1 e2) (reverse $1)}
   
     | lambda rev_enum1(ident1) '->' expr                                                         {foldr (\(pos,TIdent1 s) exp -> EAbstr pos s exp) $4 (reverse $2) }
| '-' expr   %prec NEG                                                                                {EApp (fst $1) (EUnaryMinus (fst $1)) $2 }
     | expr '-' expr                                                                              {appOp  $1 $2 $3 Minus}
     | expr '+' expr                                                                              {appOp $1 $2 $3 Plus}
     | expr '*' expr                                                                              {appOp $1 $2 $3 Times}
     | expr '<=' expr                                                                             {appOp $1 $2 $3 Leq}
     | expr '>=' expr                                                                             {appOp $1 $2 $3 Geq}
     | expr '>' expr                                                                              {appOp $1 $2 $3 G}
     | expr '<' expr                                                                              {appOp $1 $2 $3 L}
     | expr '/=' expr                                                                             {appOp $1 $2 $3 Neq}
     | expr '==' expr                                                                             {appOp $1 $2 $3 Eq}
     | expr '&&' expr                                                                             {appOp $1 $2 $3 And}
     | expr '||' expr                                                                             {appOp $1 $2 $3 Or}
     | if expr then expr else expr                                                                {EITE (fst $1) $2 $4 $6 }
     | let liaisons in expr                                                                       {ELet (fst $1) $2 $4}
     | case expr of '{' '[' ']' '->' expr ';' ident1 ':' ident1 '->' expr option(';') '}'         {EApp (fst $1) (EApp (fst $1) (EApp (fst $1) (ECase (fst $1)) $2) $8) (EAbstr (fst $10) ((\(_,TIdent1 s) -> s) $10) (EAbstr (fst $12) ((\(_,TIdent1 s) -> s) $12) $14))}
     | expr ':' expr                                                                              {EApp (fst $2) (EApp (fst $2) (EOp (fst $2) Cons) $1) $3}
| do '{' rev_enum_sep1(expr,';') option(';') '}'                                             {foldl (\sequence newInstr -> let newInsPos = posOf newInstr in EApp (newInsPos) (EApp (newInsPos) (EOp newInsPos MonadSeq) newInstr) sequence) (Return (fst $5)) $3}
     | return '(' ')'                                                                             {Return (fst $1)} 

liaisons 
        : def1      {[$1]}
        | '{' rev_enum_sep1(def1,';') option(';') '}' { reverse $2 }


{
appOp e1 e2 e3 op = EApp (fst e2) (EApp (fst e2) (EOp (fst e2) op) e1)  e3
}
