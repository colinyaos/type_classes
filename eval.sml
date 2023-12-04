structure Eval = struct

  structure TC = TypeClass

  fun eval (Int i) = Int i
    | eval (Float f) = Float f
    | eval (Add (t1, t2)) = case (t1, t2) of 
          (Int i1, Int i2) => Int (i1 + i2)
        | (Float f1, Float f2) => Float (f1 + f2)
        | _ => raise Fail "type mismatch"
    | eval (Mult (t1, t2)) = case (t1, t2) of 
          (Int i1, Int i2) => Int (i1 * i2)
        | (Float f1, Float f2) => Float (f1 + f2)
        | _ => raise Fail "type mismatch"
    | eval (Negate t1) = case t1 of 
          Int i1 => Int (-t1)
        | Float f1 => Float (-f1)
        | _ => raise Fail "type mismatch"