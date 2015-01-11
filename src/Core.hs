module Core where
import MIPS
import AST

getCode = let get = Label "get" in do
  label "getforced"
  lw _v1 (1 :> _a0)
  jr _ra
  label "get"
  lw _t1 (At _a0)
  beqz _t1 (Label "getforced")
  (saving [_ra,_s1,_s2] $ do
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

binOp o =  let get = Label "get" in do
  label ((show o) ++ "1")
  (saving [_ra, _s1, _s2] $ do 
      move _s2 _a0
      lw _a0 (At _a1)
      jal get
      move _s1 _v1
      move _a0 _s2
      jal get
      (instr o) _s1 _v1 _s1
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
      G -> \r1 r2 r3 -> instr o r1 r3 r2
      Neq -> sub
      Cons -> undefined


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


primitives = do
  getCode
  binOp Plus
  binOp Times
--  binOp Cons
  listValues
