



(************************************************************************)
(** Basic statistics *)

(** Returns the (lower) median item of a non-empty sorted list *)

let median_of_sorted l =
   let nb = List.length l in
   assert (nb > 0);
   List.nth l (nb / 2)

(** Returns the (lower) median item of a non-empty list, 
    not necessarily sorted *)

let median_of l =
   let s = List.sort (fun a b -> if a < b then -1 else 1) l in
   median_of_sorted s

(** Returns the (lower) median item of a non-empty list, or nan. *)

let median_of_or_nan l =
  if l = [] then nan else median_of l 

(** Returns the maximum value of a non-empty list *)

let max_of l =
   match l with 
   | [] -> assert false
   | x::ls -> List.fold_left max x ls

(** Returns the minimum value of a non-empty list *)

let min_of l =
   match l with 
   | [] -> assert false
   | x::ls -> List.fold_left min x ls




(************************************************************************)
(** Random numbers *)

(** Obtain a float following a random gaussian 
    (computed using a Box-Muller transform) *)

let rec random_normalized_gaussian () = 
   (* Generate two uniform numbers from -1 to 1 *) 
   let x = Random.float 2.0 -. 1.0 in 
   let y = Random.float 2.0 -. 1.0 in 
   let s = x*.x +. y*.y in 
   if s > 1.0 then random_normalized_gaussian () 
   else x *. sqrt (-2.0 *. (log s) /. s) 

(** Obtain a float following a random gaussian of given mean and standard deviation *)

let random_gaussian mean stddev = 
   let x = random_normalized_gaussian() in
   mean +. stddev *. x

