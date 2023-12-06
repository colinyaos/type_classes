structure Compile : sig

    val compile : string -> TypeClass.term
    val compileAndPrint : string -> unit
    val printParsed : string -> unit

end = struct

    fun compile expr = (Eval.eval (Parse.parse (Scan.scan expr)))

    fun compileAndPrint expr = print (TypeClass.tos (compile expr) ^ "\n")

    fun printParsed expr = print (TypeClass.tos (Parse.parse (Scan.scan expr)) ^ "\n")

end