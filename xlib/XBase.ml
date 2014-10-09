
(************************************************************************)
(** Function calls *)

(** A generic operator for swapping the order of the two first arguments 
    of a function *)

let ( ~~ ) = fun f x y -> f y x 

(** A generic operator for swapping the order of the two three arguments 
    of a function *)

let ( ~~~ ) = fun f x y z -> f z y x 


(************************************************************************)
(** Debugging *)

let printf = Printf.printf
let sprintf = Printf.sprintf


(************************************************************************)
(** Control-flow *)

(* Break, Continue and Return exceptions *)

exception Break
exception Continue
exception Return


(** Make [n] calls to a given function [f] *)

let repeat n f =
   for i = 0 to pred n do f () done

(** Make [n] calls to a given function [f], passing the index as argument *)

let repeati n f =
   for i = 0 to pred n do f i done


(************************************************************************)
(** Interactions *)

(** Runs a unix command and ignores the result *)

let unix_command s =
   ignore (Unix.system s)

(** Prints a message on stdout, and flush *)

let message_no_endln str =
   print_string str; flush stdout

(** Prints a message on stdout, with a newline, and flush *)

let message str =
   print_string str; print_newline(); flush stdout


(************************************************************************)
(** References *)

(** [add_to_list_ref r x] adds an item [x] to the total of a reference on an integer *)

let add_to_int_ref r x =
  r := x + !r

(** [add_to_list_ref r x] adds an item [x] to the total of a reference on a float *)

let add_to_float_ref r x =
  r := x +. !r

(** [add_to_list_ref r x] adds an item [x] to the front of a reference [r] on a list *)

let add_to_list_ref r x =
  r := x :: !r


(************************************************************************)
(** Conversions *)

let bool_of_int x =
   (x <> 0)


(************************************************************************)
(** Counter *)

(** Returns a counter in the form of a pair of a "get" and an "incr" function *)

let new_counter () =
   let r = ref 0 in
   let get () = !r in
   let incr () = (r := !r +1) in
   (get,incr)


(** Returns a counter in the form of a pair of a "get" function and an "add" function *)

let new_cumulative_counter i =
   let r = ref i in
   let get () = !r in
   let add x = (r := !r + x) in
   (get,add)
