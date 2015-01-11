module MIPS where

import Data.Char (toLower)
import Data.List (intersperse)
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative ((<$>))

class MIPSAble a where
  toMIPS :: a -> String


data Instruction =
   SW Register Address 
 | LW Register Address
 | JALR Register
 | JAL Label
 | JR Register
 | J Label

 | LI Register Int
 | LA Register Label

 | ADDI Register Register Int
 | ADD Register Register Register
 | REM Register Register Register
 | SUB Register Register Register
 | SUBI Register Register Int
 | MUL Register Register Register
 | DIV Register Register Register
 | NEG Register Register

 | MOVE Register Register

 | BEQ Register Register Label
 | BEQZ Register Label
 | BNE Register Register Label
 | BLT Register Register Label
 | BLE Register Register Label
 | BGT Register Register Label
 | BGE Register Register Label

 | SYSCALL
 | LABEL String

 | SLT Register Register Register
   | Comment String
   deriving Show

instance MIPSAble Instruction where
  toMIPS (LABEL s) = s ++ ":\n"
  toMIPS (Comment s) = "\t # " ++ s ++ "\n"
  toMIPS x = "\t" ++ 
    let (command:args) = words . (toLower <$>) . show $ x in
    command ++ " " ++ (concat $ intersperse "," args) ++ "\n"

newtype Register = Register String

instance Show Register where
  show (Register s) = '$':s

data Label = Label String

instance Show Label where
  show (Label s) = s

data Address =
    At Register
  | Int :> Register

instance Show Address where
  show (At r) = '(':((show r) ++ ")")
  show (n :> r) = (show $ 4*n) ++ (show (At r))

_zero = Register "zero"
_v0 = Register "v0"
_v1 = Register "v1"
_a0 = Register "a0"
_a1 = Register "a1"
_a2 = Register "a2"
_a3 = Register "a3"
_t0 = Register "t0"
_t1 = Register "t1"
_t2 = Register "t2"
_t3 = Register "t3"
_t4 = Register "t4"
_t5 = Register "t5"
_t6 = Register "t6"
_t7 = Register "t7"
_t8 = Register "t8"
_t9 = Register "t9"
_s0 = Register "t0"
_s1 = Register "s1"
_s2 = Register "s2"
_s3 = Register "s3"
_s4 = Register "s4"
_s5 = Register "s5"
_s6 = Register "s6"
_s7 = Register "s7"
_sp = Register "sp"
_fp = Register "fp"
_ra = Register "ra"

data DataStore =
  Ascii Label String |
  Word Label [String]
  deriving Show
           
instance MIPSAble DataStore where
  toMIPS (Ascii (Label l) s) =
    l ++ ":    .asciiz " ++ (show s) ++ "\n"
  toMIPS (Word (Label l) ss) =
    l ++ ": .word " ++ (concat . (intersperse ",") $ ss) ++ "\n"

type Lol = WriterT (String,String) (State Int) ()

instr x = (toMIPS x,[])

move r1 r2 = tell . instr $ MOVE r1 r2 :: Lol

lw r a = tell . instr $ LW r a :: Lol
sw r a = tell . instr $ SW r a :: Lol
li r x = tell . instr $ LI r x :: Lol
la r x = tell . instr $ LA r x :: Lol

subi r r' x = tell . instr $ SUBI r r' x :: Lol
addi r r' x = tell . instr $ ADDI r r' x :: Lol
neg r r' = tell . instr $ NEG r r' :: Lol
sub r1 r2 r3 = tell . instr $ SUB r1 r2 r3
add r1 r2 r3 = tell . instr $ ADD r1 r2 r3
slt r1 r2 r3 = tell . instr $ SLT r1 r2 r3
mul r1 r2 r3 = tell . instr $ MUL r1 r2 r3
div r1 r2 r3 = tell . instr $ DIV r1 r2 r3
rem r1 r2 r3 = tell . instr $ REM r1 r2 r3

jal l = tell . instr $ JAL l
jr r = tell . instr $ JR r
jalr r = tell . instr $ JALR r

syscall :: Lol
syscall = tell . instr $ SYSCALL

label s = tell . instr $ LABEL s

beqz r l = tell .instr $ BEQZ r l

comment s = tell .instr $ Comment s


putOnData x = tell ([],toMIPS x)

saving :: [Register] -> Lol -> Lol
saving l c =
  let n = length l in
  let indexes = zip [0..(n-1)] l in
  let begining =
        decreaseSP n >>
        mapM_ (\(i,r) -> sw r (i :> _sp)) indexes in
  let end =
        mapM_ (\(i,r) -> lw r (i :> _sp)) indexes >>
        increaseSP n in
  let com x = comment $ x ++ "saving :" ++ show l in
  com "Begin" >> begining >> c >> end >> com "End"

requestHeap :: Int -> Lol
requestHeap n = 
  saving [_a0] $ do
    li _v0 9
    li _a0 (4*n)
    syscall


decreaseSP n = do
  subi _sp _sp (4*n) :: Lol


increaseSP n = do
  addi _sp _sp (4*n) :: Lol

newLabel =
  do i <- get
     modify (+1) :: Lol
     return . Label $ "l_" ++ (show i)
                    

storeCalculated x = do
  (saving [_s1] $ do
      li _s1 0
      requestHeap 2
      sw _s1 (At _v0))
  sw x ( 1 :> _v0)
  

storeApp f x = do
   (saving [_s1] $ do
       li _s1 1
       requestHeap 4
       sw _s1 (At _v0) )
   sw f (2 :> _v0)
   sw x (3 :> _v0) 
