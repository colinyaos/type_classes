structure Token = struct

  datatype token
    = Int of int
    | Float of real
    | LParen
    | RParen
    | Plus
    | Minus
    | Times
    | Negate

end
