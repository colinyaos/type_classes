structure Eval = struct

  structure TC = TypeClass
  structure NumD = NumD


  fun addInt (TC.Int i1) (TC.Int i2) = TC.Int (i1 + i2)
  fun mulInt (TC.Int i1) (TC.Int i2) = TC.Int (i1 * i2)
  fun negInt (TC.Int i1) = TC.Int (~i1)

  fun addFlt (TC.Float f1) (TC.Float f2) = TC.Float (f1 + f2)
  fun mulFlt (TC.Float f1) (TC.Float f2) = TC.Float (f1 * f2)
  fun negFlt (TC.Float f1) = TC.Float (~f1)

  fun type_applicator ([], _, _) = raise Fail "no types left"
    | type_applicator ((typ, numd)::rdict, op_name, t1) = 
        if TC.same_type(typ, t1) then 
          (case numd of 
          NumD.NumD (a,m,n) => (case op_name of
            "add" => (a t1)
          | "mul" => (m t1)
          | "neg" => n
          | _ => raise Fail "not valid key"
          )
        (* | _ => raise Fail "not a numd" *)
          )
        else type_applicator (rdict, op_name, t1)


  val num_dict = [
    (TC.Int 0, NumD.NumD (addInt, mulInt, negInt)),
    (TC.Float 0.0, NumD.NumD (addFlt, mulFlt, negFlt))
  ]

  (* val num_dict = [
    (TC.Int 0, (NumD addInt mulInt negInt)),
    (TC.Float (0.0), (NumD addFlt mulFlt negFlt))
    ]; *)
  

  fun eval (TC.Int i) = TC.Int i
    | eval (TC.Float f) = TC.Float f
    | eval (TC.Add (t1, t2)) = type_applicator (num_dict, "add", (eval t1)) (eval t2)
    | eval (TC.Mult (t1, t2)) = type_applicator (num_dict, "mul", (eval t1)) (eval t2)
    | eval (TC.Negate t1) = type_applicator (num_dict, "neg", (eval t1)) (eval t1)
  
  end
