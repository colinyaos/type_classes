structure Token = struct

  datatype token
    = Int of int
    | Float of real
    | LParen
    | RParen
    | Comma
    | Plus
    | Minus
    | Times
    | Negate

end
