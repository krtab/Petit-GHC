{
module Lexer where
import AST(Tokens(..),Op(..),Value(..))
import qualified Data.Bits
import Control.Applicative (Applicative (..), liftA)
import Data.Word (Word8)
}



$chiffre = 0-9
$alpha = [a-z A-Z]
$basecar = [\x20 - \x7E] # [ \x5C \x22 ]
$backs = \x5C
$guill = \x022

@car = ($basecar |  $backs "n" |  $backs "t" | $backs $backs | $backs $guill)

@entier = $chiffre+
@caractere = ' @car '
@chaine = \"((@car)*)\"
@ident = [a - z]($alpha | _ | "'" | $chiffre)*

tokens :-

  $white+       ;
  "--" $printable* $     ;
  "else"        { constToken Telse }
  "if"          { constToken Tif }
  "then"        { constToken Tthen }
  "let"         { constToken Tlet }
  "in"          { constToken Tin }
  "case"        { constToken Tcase }
  "of"          { constToken Tof }
  "do"          { constToken Tdo }
  "return"      { constToken Treturn }
  "("           { constToken TLPar }
  ")"           { constToken TRPar }
  "["           { constToken TLBrkt }
  "]"           { constToken TRBrkt }
  "{"           { constToken TLCurl}
  "}"           { constToken TRCurl}
  "+"           { constOp Plus }
  "-"           { constToken TMinus }
  "*"           { constOp Times }
  "<="          { constOp Leq }
  ">="          { constOp Geq }
  ">"           { constOp G }
  "<"           { constOp L }
  "/="          { constOp Neq }
  "=="          { constOp Eq }     
  "&&"          { constOp And }
  "||"          { constOp Or }  
  "="           { constToken TAffect }
  ","           { constToken TComma}
  "\"          { constToken TBck }
  "->"          { constToken TArrow }
  ":"           { constToken TColon}
  ";"           { constToken TSemiColon }
  "True"        { \posn str -> (posn,TConst $ VBool True)}
  "False"        { \posn str -> (posn,TConst $ VBool False)}
  ^ @ident      {\posn str -> (posn,TIdent0 str)}
  @ident        {\posn str -> (posn, TIdent1 str)}
  @chaine       {\posn str -> (posn,TConst . VString . read $ str)}
  @entier       {\posn num -> (posn,TConst . VInteger . read $ num)}
  @caractere    {\posn car -> (posn, TConst . VChar . read $ car)}

{
constToken token posn _ = (posn, token)
constOp op posn _ = (posn, TBinOp op)



-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
data AlexPosn =
     AlexPn !Int !Int !Int
     deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1) l (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1) 1
alexMove (AlexPn a l c) _ = AlexPn (a+1) l (c+1)

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string


type Byte = Word8

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))



alexScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return $ []
                AlexError ((AlexPn _ line column),_,_,_) -> Left (line,column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> liftA ((act pos (take len str)):)  (go inp')


}
