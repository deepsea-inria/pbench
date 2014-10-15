
(************************************************************************)
(** Unsome *)

(** Extract the value from an option, or fails with a custom message *)

let unsome_or_failwith msg = function
   | None -> failwith msg
   | Some x -> x

(** Extract the value from an option known to be Some *)

let unsome o = 
  unsome_or_failwith "unsome of none" o

(** Extract the value from an option, or return a default value *)

let unsome_or default = function
  | None -> default
  | Some x -> x

(** Convert to a list of length zero or one *)

let to_list = function
  | None -> []
  | Some x -> [x]


(************************************************************************)
(** Iterators *)

(** Iter applies a function to the item stored in a Some, and do nothing
    otherwise *)

let iter o f =
   match o with
   | None -> ()
   | Some x -> f x

(** Map applies a function to the item stored in a Some, and returns a 
    given default value otherwise *)
let map o r f =
   match o with
   | None -> r
   | Some x -> f x

