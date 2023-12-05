structure Scan : sig

  val next : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
	    
end = struct

    structure T = Token

    fun isDigit c = (ord c > 47) andalso (ord c < 58)

    fun scanNumberHelper ([], b) = ([], b, [])
      | scanNumberHelper ((c :: cs), b) =
        if isDigit c
        then let
          val (scanned, isFloat, rest) = scanNumberHelper (cs, b)
        in
          ((c :: scanned), isFloat, rest)
        end
        else if c = #"."
        then let
          val (scanned, isFloat, rest) = scanNumberHelper (cs, b)
        in
          ((c :: scanned), true, rest)
        end
        else ([], b, (c :: cs))
    
    fun scanNumber cs = scanNumberHelper (cs, false)

    fun next [] = NONE
      | next (#"(" :: cs) = SOME (T.LParen, cs)
      | next (#")" :: cs) = SOME (T.RParen, cs)
      | next (#"+" :: cs) = SOME (T.Plus, cs)
      | next (#"-" :: cs) = SOME (T.Minus, cs)
      | next (#"*" :: cs) = SOME (T.Times, cs)
      | next (#"~" :: cs) = SOME (T.Negate, cs)
      | next (c :: cs) =
        if Char.isSpace c
        then next cs
        else if isDigit c
        then let
            val (ds, isFloat, rest) = scanNumber (c :: cs)
            val numString = String.implode ds
            val maybeFloat = Real.fromString numString
            val maybeInt = Int.fromString numString
          in
            if isFloat
            then case maybeFloat of
              SOME float => SOME (T.Float float, rest)
            | NONE => raise Fail "error"
            else case maybeInt of
              SOME integer => SOME (T.Int integer, rest)
            | NONE => raise Fail "error"
          end
        else raise Fail "Invalid token"

  fun scan code =
    let
      fun lp [] = []
      | lp chars =
        (case next chars
          of SOME (tok, chars') => tok :: lp chars'
          | NONE => [])
    in
      lp (explode code)
    end
      
end
