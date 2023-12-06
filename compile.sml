structure Compile : sig

    val compile : string -> TypeClass.term
    val compileAndPrint : string -> unit

end = struct

    fun compile expr = (Eval.eval (Parse.parse (Scan.scan expr)))

    fun compileAndPrint expr = print (TypeClass.tos (compile expr) ^ "\n")

end