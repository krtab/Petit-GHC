module Core where
import MIPS
import AST
import Prelude hiding (div,rem)
getCode = let get = Label "get" in do
  label "getforced"
  lw _v1 (1 :> _a0)
  jr _ra
  label "get"
  lw _t1 (At _a0)
  beqz _t1 (Label "getforced")
  (saving [_ra,_s1,_s2,_a1] $ do
    move _s1 _a0
    lw _a0 (2 :> _s1)
    jal get
    lw _s2 (At _v1)
    lw _a1 (1 :> _v1)
    lw _a0 (3 :> _s1)
    jalr _s2
    move _a0 _v0
    jal get
    sw _v1 (1 :> _s1)
    li _t1 0
    sw _t1 (At _s1)
    )
  jr _ra



  label "id1"
  move _v0 _a0
  jr _ra

  putOnData ( Word (Label "idclot") ["id1","0"])
  putOnData ( Word (Label "coreid") ["0","idclot","0","0"])
  

binOp o =  let get = Label "get" in do
  label ((show o) ++ "1")
  (saving [_ra, _s1, _s2] $ do 
      move _s2 _a0
      lw _a0 (At _a1)
      jal get
      move _s1 _v1
      move _a0 _s2
      jal get
      (instr o) _s1 _s1 _v1
      storeCalculated _s1 )
  jr _ra

  label ((show o) ++ "2")
  (saving [_ra, _s1] $ do
      move _s1 _a0
      requestHeap 1
      sw _s1 (At _v0)
      move _s1 _v0
      requestHeap 2
      la _t1 (Label $ (show o) ++ "1")
      sw _t1 (At _v0)
      sw _s1 (1 :> _v0)
      move _s1 _v0
      storeCalculated _s1 )
  jr _ra 
  putOnData (Word (Label ("Core" ++ (show o)))
             ["0",((show o) ++ "2Clot"),"0","0"])
  putOnData (Word (Label ((show o) ++ "2Clot")) [(show o) ++ "2","0"])	
  where
    instr o = case o of
      Plus -> add
      Minus -> sub
      Times -> mul
      Geq -> \r1 r2 r3 -> addi _t1 r2 1 >> slt r1 r3 _t1
      Leq -> \r1 r2 r3 -> instr Geq r1 r3 r2
      L -> slt
      G -> \r1 r2 r3 -> instr L r1 r3 r2
      Neq -> sub
      Cons -> undefined
      MonadSeq -> add -- doesn't matter
      And -> mul


listValues = let get = Label "get" in do
  label "case1"
  decreaseSP 1
  sw _s1 (At _sp)
  move _s1 _a0
  lw _a0 (At _a1)
  saving [_ra] (jal get)
  lw _t1 (At _v1)
  beqz _t1 (Label "caseEmpty")
  move _a0 _s1
  lw _s1 (At _sp)
  increaseSP 1
  (saving [_s1,_s2,_s3,_s4] $ do
      -- s1 : x ; s2 : xs; s3 : f
      move _s3 _a0
      lw _s1 (1 :> _v1)
      lw _s2 (2 :> _v1)
      storeApp _s3 _s1
      move _s4 _v0
      storeApp _s4 _s2)
  jr _ra
      
  label "caseEmpty"
  lw _v0 (1 :> _a1)
  lw _s1 (At _sp)
  increaseSP 1
  jr _ra

  label "case2"
  (saving [_ra, _s1] $ do
      move _s1 _a0
      requestHeap 2
      sw _s1 (1 :> _v0)
      lw _t1 (At _a1)
      sw _t1 (0 :> _v0)
      move _s1 _v0
      requestHeap 2
      la _t1 (Label $ "case1")
      sw _t1 (At _v0)
      sw _s1 (1 :> _v0)
      move _s1 _v0
      storeCalculated _s1 )
  jr _ra

  label "case3"
  (saving [_ra, _s1] $ do
      move _s1 _a0
      requestHeap 1
      sw _s1 (At _v0)
      move _s1 _v0
      requestHeap 2
      la _t1 (Label "case2")
      sw _t1 (At _v0)
      sw _s1 (1 :> _v0)
      move _s1 _v0
      storeCalculated _s1 )
  jr _ra
  putOnData (Word (Label "CoreCase")
             ["0","case3Clot","0","0"])
  putOnData (Word (Label "case3Clot") ["case3","0"])



  putOnData (Word (Label "CoreEmptyList") ["0","corezero","0","0"] )
  putOnData (Word (Label "corezero") ["0"])

  
  label  "cons1"
  (saving [_ra, _s1] $ do
      requestHeap 3
      li _t1 1
      sw _t1 (At _v0)
      lw _t1 (At _a1)
      sw  _t1 (1 :> _v0)
      sw _a0 (2 :> _v0)
      move _s1 _v0
      storeCalculated _s1
    )
  jr _ra

  label "cons2"
  (saving [_ra, _s1] $ do
      move _s1 _a0
      requestHeap 1
      sw _s1 (At _v0)
      move _s1 _v0
      requestHeap 2
      la _t1 (Label "cons1")
      sw _t1 (At _v0)
      sw _s1 (1 :> _v0)
      move _s1 _v0
      storeCalculated _s1 )
  jr _ra 
  putOnData (Word (Label "Corecons")
             ["0","cons2Clot","0","0"])
  putOnData (Word (Label "cons2Clot") ["cons2","0"])


misc = let get = Label "get" in do
  label "putChar1"
  (saving [_ra] $ do
      jal get
      move _a0 _v1
      li _v0 11
      syscall
      storeCalculated _zero
    )
  jr _ra
      
  putOnData (Word (Label "putchar")
             ["0","putchar1clot","0","0"])
  putOnData (Word (Label "putchar1clot") ["putchar1","0"])



  putOnData (Word (Label "corereturn") ["0","0","0","0"])

  label "exiterror1"
  li _v0 17
  syscall

  putOnData (Word (Label "coreexiterror") ["0","exiterrorclot","0","0"])
  putOnData (Word (Label "exiterrorclot") ["exiterror1","0"])




  label "ite1"
  decreaseSP 1
  sw _s1 (At _sp)
  move _s1 _a0
  lw _a0 (At _a1)
  saving [_ra] (jal get)
  beqz _v1 (Label "itefalse")
  lw _v0 (1 :> _a1)
  lw _s1 (At _sp)
  increaseSP 1
  jr _ra
      
  label "itefalse"
  move _v0 _s1
  lw _s1 (At _sp)
  increaseSP 1
  jr _ra

  label "ite2"
  (saving [_ra, _s1] $ do
      move _s1 _a0
      requestHeap 2
      sw _s1 (1 :> _v0)
      lw _t1 (At _a1)
      sw _t1 (0 :> _v0)
      move _s1 _v0
      requestHeap 2
      la _t1 (Label $ "ite1")
      sw _t1 (At _v0)
      sw _s1 (1 :> _v0)
      move _s1 _v0
      storeCalculated _s1 )
  jr _ra

  label "ite3"
  (saving [_ra, _s1] $ do
      move _s1 _a0
      requestHeap 1
      sw _s1 (At _v0)
      move _s1 _v0
      requestHeap 2
      la _t1 (Label "ite2")
      sw _t1 (At _v0)
      sw _s1 (1 :> _v0)
      move _s1 _v0
      storeCalculated _s1 )
  jr _ra
  putOnData (Word (Label "Coreite")
             ["0","ite3Clot","0","0"])
  putOnData (Word (Label "ite3Clot") ["ite3","0"])

  label "unaryminus1"
  (saving [_s1,_ra] $ do
      jal get
      sub _s1 _zero _v1
      storeCalculated _s1
    )
  jr _ra
  putOnData (Word (Label "Coreunaryminus")
             ["0","unaryminusclot","0","0"])
  putOnData (Word (Label "unaryminusclot") ["unaryminus1","0"])
  

divRem s= 
  let get = Label "get" in do
  label (s ++ "1")
  (saving [_ra, _s1, _s2] $ do 
      move _s2 _a0
      lw _a0 (At _a1)
      jal get
      move _s1 _v1
      move _a0 _s2
      jal get
      (if s == "div" then div else rem)  _s1 _s1 _v1
      storeCalculated _s1 )
  jr _ra

  label (s ++ "2")
  (saving [_ra, _s1] $ do
      move _s1 _a0
      requestHeap 1
      sw _s1 (At _v0)
      move _s1 _v0
      requestHeap 2
      la _t1 (Label $ s ++ "1")
      sw _t1 (At _v0)
      sw _s1 (1 :> _v0)
      move _s1 _v0
      storeCalculated _s1 )
  jr _ra 
  putOnData (Word (Label $ s )
             ["0",s ++ "Clot","0","0"])
  putOnData (Word (Label $s ++ "Clot") [s ++ "2","0"])




  
primitives = do
  getCode
  binOp Plus
  binOp Times
  binOp Minus
  binOp MonadSeq
  binOp Neq
  binOp Geq
  binOp Leq
  binOp L
  binOp G
  binOp And
  listValues
  misc
  divRem "rem"
  divRem "div"
