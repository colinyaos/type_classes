structure TypeClass = struct

    datatype term 
        = Int_term of int
        | Real_term of real
        | App of term * term
        | Lam of term * term
        | Over of string * type_class * term
        | Inst of string * type_class * term * term

    (* datatype type_class
        =  *)
end
