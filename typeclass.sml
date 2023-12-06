structure TypeClass = struct

    datatype term 
        = Int of int
        | Float of real
        | Complex of real * real
        | Add of term * term
        | Mult of term * term
        | Negate of term


    fun same_type (t1, t2) = case (t1, t2) of 
            (Int _, Int _) => true
        |   (Float _, Float _) => true
        |   (Complex _, Complex _) => true
        | _  => false


        (* | App of term * term
        | Lam of term * term
        | Over of string * type_class * term
        | Inst of string * type_class * term * term *)

    (* datatype type_class
        =  *)

    fun tos (Int i) = Int.toString i
      | tos (Float f) = Real.toString f
      | tos (Add (t1, t2)) = "(" ^ tos(t1) ^ " + " ^ tos(t2) ^ ")"
      | tos (Mult (t1, t2)) = "(" ^ tos(t1) ^ " * " ^ tos(t2) ^ ")"
      | tos (Negate t1) = "~" ^ tos(t1)

end
