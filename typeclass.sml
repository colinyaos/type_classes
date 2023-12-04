structure TypeClass = struct

    datatype term 
        = Int of int
        | Float of real
        | Add of term * term
        | Mult of term * term
        | Negate of term


        (* | App of term * term
        | Lam of term * term
        | Over of string * type_class * term
        | Inst of string * type_class * term * term *)

    (* datatype type_class
        =  *)
end
