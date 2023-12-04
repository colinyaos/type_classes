structure Eval = struct

  structure TC = TypeClass

  fun addInt (TC.Int i1, TC.Int i2) = TC.Int (i1 + i2)
  fun mulInt (TC.Int i1, TC.Int i2) = TC.Int (i1 * i2)
  fun negInt (TC.Int i1) = TC.Int (-i1)

  fun addFlt (TC.Float f1, TC.Float f2) = TC.Float (f1 + f2)
  fun mulFlt (TC.Float f1, TC.Float f2) = TC.Float (f1 * f2)
  fun negFlt (TC.Float f1) = TC.Float (-f1)


  num_dict = [
    (TC.Int 0, [addInt, mulInt, negInt])
    (TC.Float (0.0), [addFlt, mulFlt, negFlt])
    ]


  fun type_applicator ([], _, _) = raise Fail "no types left"
    | type_applicator ((typ, imp_list)::rdict, i, t1) = 
        if TC.same_type(typ, t1) then imp_list[i] else type_applicator (rdict, i, t1)


  fun eval (TC.Int i) = TC.Int i
    | eval (TC.Float f) = TC.Float f
    | eval (TC.Add (t1, t2)) = type_applicator (num_dict, 0, t1)
    | eval (TC.Mult (t1, t2)) = type_applicator (num_dict, 1, t1)
    | eval (TC.Negate t1) = type_applicator (num_dict, 2, t1)