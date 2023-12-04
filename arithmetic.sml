(* Definition and Translation of Arithmetic Operations ?????*)
(* not sure ab sig vs. struct usage here *)

(* Define the Num signature *)
signature NUM =
  sig
    type t
    val add : t * t -> t
    val mul : t * t -> t
    val negate : t -> t
end;

(* Functor to instantiate Num for Int *)
functor NumInt() : NUM =
  struct
    type t = int
    val add = op+
    val mul = op*
    val negate = ~
end;

(* Functor to instantiate Num for Float *)
functor NumFloat() : NUM =
  struct
    fun fplus (f1:real, f2:real) = (f1 + f2);
    fun fmul (f1:real, f2:real) = (f1 * f2);
    fun fneg (f1:real) = (~f1);
    type t = real
    val add = fplus
    val mul = fmul
    val negate = fneg
end;

(* Example usage of the NumInt functor *)
structure IntNum = NumInt();
val resultInt = IntNum.add (3,4); (* resultInt should be 7 *)

(* Example usage of the NumFloat functor *)
structure FloatNum = NumFloat();
val resultFloat = FloatNum.mul (2.5,3.0); (* resultFloat should be 7.5 *)

(* Define the square function using a Num record *)
fun square {add, mul} x = mul (x,x);

(* Define a record for IntNum and use the square function *)
val resultSquareInt = square IntNum 5; (* resultSquareInt should be 25 *)

(* Define a record for FloatNum and use the square function *)
val resultSquareFloat = square FloatNum 2.5; (* resultSquareFloat should be 6.25 *)
