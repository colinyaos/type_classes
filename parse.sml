structure Parse : sig

  val next  : Token.token list -> (TypeClass.term * Token.token list) option
  val parse : Token.token list -> TypeClass.term

end = struct

  structure T = Token
  structure A = TypeClass
  
  fun next [] = NONE
    | next ((T.Int i) :: toks) = SOME ((A.Int i), toks)
    | next ((T.Float f) :: toks) = SOME ((A.Float f), toks)
    | next (T.LParen :: toks) =
      let
        val t1 = next toks
      in
        (case t1
          of SOME (term1, toks') => 
            (case toks'
              of (T.Plus :: toks') =>
                let
                  val t2 = next toks'
                in
                  (case t2
                    of SOME (term2, T.RParen :: toks'') => SOME (A.Add(term1, term2), toks'')
                    | NONE => NONE
                    | _ => raise Fail "Invalid Parentheses"
                  )
                end
              | (T.Minus :: toks') =>
                let
                  val t2 = next toks'
                in
                  (case t2
                    of SOME (term2, T.RParen :: toks'') => SOME (A.Add(term1, A.Negate (term2)), toks'')
                    | NONE => NONE
                    | _ => raise Fail "Invalid Parentheses"
                  )
                end
              | (T.Times :: toks') =>
                let
                  val t2 = next toks'
                in
                  (case t2
                    of SOME (term2, T.RParen :: toks'') => SOME (A.Mult(term1, term2), toks'')
                    | NONE => NONE
                    | _ => raise Fail "Invalid Parentheses"
                  )
                end
              | _ => raise Fail "Invalid operator"
            )
          | NONE => NONE
        )
      end
    | next _ = raise Fail "Invalid format"

  fun parse toklist =
    let
      val out = next toklist
    in
      (case out
        of SOME (term, _) => term
        | NONE => raise Fail "oopsie"
      )
    end
		     
end
 