open XBase

(** Tools for selecting values in list of heteregeneous arguments.

    Running example: 
      type arg = Foo of int | Bar of bool | Sub of arg2 list
      type args = arg list
*)


(************************************************************************)
(** Get *)

(* Calls [f] on each item of the list, and return the "unsome" valued
   of the first result that is not None; Raises Not_found if no match.
   Example: [get (function Foo n -> Some n | _ -> None) args]. *)

let get opts f =
   let rec aux = function
      | [] -> raise Not_found
      | x::l ->  
         match f x with
         | None -> aux l
         | Some v -> v
      in
  aux opts

(** Same as get, but returns a default value when no item matches.
    Example: [get args (function Foo n -> Some n | _ -> None) 3]. *)

let get_default opts f def = 
   try get opts f 
   with Not_found -> def 

(** Same as get, but raises an error when no item matches.
    Example: [get args (function Foo n -> Some n | _ -> None) "missing foo"]. *)

let get_error opts f s = 
   try get opts f 
   with Not_found -> failwith s

(** Same as get, but returns an option.
    Example: [get args (function Foo n -> Some n | _ -> None)]. *)

let get_option opts f = 
   try Some (get opts f)
   with Not_found -> None

(** [get_default_2 opts f1 f2 def] returns [get opts f1] 
    if it produces a result and otherwise returns [get_default opts f2 def] *)

let get_default_2 opts f1 f2 def = 
   try get opts f1
   with Not_found -> 
      get_default opts f2 def



(************************************************************************)
(** Filter *)

(** [project f l] applies [f] on each item from [l],
    and keep the values which are wrapped by a Some. *)

let project opts f = 
   let rec aux acc = function
      | [] -> List.rev acc
      | x::t -> match f x with 
                | None -> aux acc t
                | Some v -> aux (v::acc) t
      in
   aux [] opts

(** [projects f l] is the same as [List.concat (project f l)].
    Example: [projects opts (function Sub x -> Some x | _ -> None)]
    is of type [arg2 list]. *)

let projects opts f = 
   List.concat (project opts f)

