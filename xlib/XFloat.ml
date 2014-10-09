

(************************************************************************)
(** Basic operations *)

(** Computes the square *)

let square x =
  x *. x

(** Computes the square root *)

let sqrt x =
  Pervasives.sqrt x

(** Returns the value of a to the power b, as a float *)

let pow a b =
   exp (b *. log a)

(** Returns the value of the log base 2 of a float, as a float *)

let log2 x =
   log x /. log 2.

(** Returns the value of the log base 10 of a float, as a float *)

let log10 x =
   log x /. log 10.



(************************************************************************)
(** Statistics *)

(** Returns the mean value of a non-empty list of float *)

let mean_of l =
   let nb = List.length l in
   let sum = List.fold_left (+.) 0. l in
   sum /. (float_of_int nb)

(** Returns the mean value and standard deviation of a non-empty list of float *)

let list_mean_and_stddev l =  
  let mean = mean_of l in
  let variance = mean_of (List.map (fun x -> square (x -. mean)) l) in
  mean, sqrt variance

let list_stddev l =
  let (_, stddev) = list_mean_and_stddev l in
  stddev
