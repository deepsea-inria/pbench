

(************************************************************************)
(** Basic operations *)

(** Returns the value of a to the power b *)

let rec pow a b =
   if b = 0 then 1 else a * pow a (b-1)

(** Returns the value of a to the power b, where a is an int and b is a float, as an int *)

let pow_float a b =
   int_of_float (XFloat.pow (float_of_int a) b)

(** Returns the value of sqrt a, where a is an int, floored to an int *)

let sqrt a  =
   int_of_float (sqrt (float_of_int a))

(** Returns the value of 10 to the power n *)

let pow10 n =
   pow 10 n

(** Returns the value of 2 to the power 2 *)

let pow2 n =
   pow 2 n

(** Returns the value of the log base 2 of an int, as an int, recursively *)

let rec log2 n =
   if n <= 1 then 0 else 1 + (log2 (n/2))

(** Returns the value of the log base 2 of an int, as an int *)

let log2_via_float x =
   int_of_float (XFloat.log2 (float_of_int x))
   
(** Returns the value of the log base 10 of an int, as an int *)

let log10_via_float x =
   int_of_float (XFloat.log10 (float_of_int x))


   
(************************************************************************)
(** Sequences *)

(** Builds the list made of integers between a and b (inclusive), 
    with step of d integers *)

let rec seq_step a b d =
  if a > b then [] else a :: (seq_step (a+d) b d)

(** Builds the list of consecutive integers between a and b, inclusive *)

let seq a b =
  seq_step a b 1

(** Builds the list of the n first consecutive natural numbers, 
    i.e., between 0 and n-1, inclusive *)

let nat_seq n =
   seq 0 (n-1)
