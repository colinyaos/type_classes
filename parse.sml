structure Parse : sig

  val next  : Token.token list -> (TypeClass.term * Token.token list) option
  val parse : Token.token list -> TypeClass.term

end = struct

  structure T = Token
  structure A = TypeClass

  fun next [] = NONE
    | next ((T.Int i) :: toks) = SOME ((A.Int i), toks)
    | next ((T.Float f) :: toks) = SOME ((A.Float f), toks)
    | next ((T.Negate) :: toks) =
      let
        val t1 = next toks
      in
        (case t1 of
           SOME (term1, toks') => SOME ((A.Negate term1), toks')
         | NONE => raise Fail "error")
      end
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
              | (T.Comma :: toks') =>
                let
                  val t2 = next toks'
                in
                  (case t2
                    of SOME (term2, T.RParen :: toks'') => (case (term1, term2)
                      of (A.Int x, A.Int y) => SOME (A.Complex (Real.fromInt x, Real.fromInt y), toks'')
                       | (A.Int x, A.Float y) => SOME (A.Complex (Real.fromInt x, y), toks'')
                       | (A.Float x, A.Int y) => SOME (A.Complex (x, Real.fromInt y), toks'')
                       | (A.Float x, A.Float y) => SOME (A.Complex (x, y), toks'')
                       | (A.Negate (A.Int x), A.Int y) => SOME (A.Complex (Real.fromInt (~x), Real.fromInt y), toks'')
                       | (A.Negate (A.Int x), A.Float y) => SOME (A.Complex (Real.fromInt (~x), y), toks'')
                       | (A.Negate (A.Float x), A.Int y) => SOME (A.Complex (~x, Real.fromInt y), toks'')
                       | (A.Negate (A.Float x), A.Float y) => SOME (A.Complex (~x, y), toks'')
                       | (A.Int x, A.Negate (A.Int y)) => SOME (A.Complex (Real.fromInt x, Real.fromInt (~y)), toks'')
                       | (A.Int x, A.Negate (A.Float y)) => SOME (A.Complex (Real.fromInt x, ~y), toks'')
                       | (A.Float x, A.Negate (A.Int y)) => SOME (A.Complex (x, Real.fromInt (~y)), toks'')
                       | (A.Float x, A.Negate (A.Float y)) => SOME (A.Complex (x, ~y), toks'')
                       | _ => raise Fail "Invalid component in complex number"
                       )
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
        | NONE => raise Fail "parse error"
      )
    end
		     
end
 