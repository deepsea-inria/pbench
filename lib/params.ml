open XBase


(***************************************************************)

(** Each run of the benchmarked program is performed on a given
    set of parameters. We use the name "env" as shorthand for
    "Env.t". *)
    
type env = Env.t    
    
(** To describe a set of runs, we need to consider a set of sets 
    of parameters, represented using the type "envs", implemented 
    as a list of set of parameters of type "env". *)

type envs = env list

(** A parameter sometimes depend on the value of another paramter.
    To allow expressing these dependencies in a convenient way, we
    construct the "set of sets of parameters" (type "envs") using
    combinators that manipulate functions of type "env -> envs",
    that is, the "envs" are described in some context, from which
    the value of previously-defined parameters may be read. The type
    "env -> envs" is called "Params.t". *)

type t = env -> envs


(***************************************************************)
(** * Shorthands for constuctors of values *)

let bool x = Env.Vbool x
let int x = Env.Vint x
let float x = Env.Vfloat x
let string x = Env.Vstring x
let value x = x


(***************************************************************)
(** * Smart constructors *)

(** Empty denotes the unit operation for cross product: 
    it consists of a singleton set with an empty params *)

let mk_unit = fun e -> 
   [ Env.empty ]

(** Smart constructor for building a constant params, based on 
    an environment *)

let from_env e = fun _ ->
   [ e ] 

(** *)

let from_envs es = fun _ ->
   es

(** Smart constructor for constant values; for example,
    [mk int "foo" 3] binds the key "foo" to the int value 3. *)

let mk ty k v : t = 
   from_env (Env.add Env.empty k (ty v))
 
(** Smart constructor for constant list of values; for example,
    [mk_list int "foo" [3;4]] binds the key "foo" to the int 
    values 3 and 4. *)

let mk_list ty k vs : t = fun e ->
   List.map (fun v -> Env.add Env.empty k (ty v)) vs

(** Smart constructor for dynamically-computed value; for example, 
    [mk_eval int "foo" (fun e -> 2 * (get_int e n))] binds "foo"
    to the value obtained by doubling the value of the integer
    bound to "n" in the context *)

let mk_eval ty k mk_value : t = fun e ->
   mk ty k (mk_value e) e

(** Smart constructor for dynamically-computed list of values; for example, 
    [mk_eval_list int "foo" (fun e -> let x = get_int e n in [ x; 2*x; 3*x])]
    binds "foo" to the values obtained by multiplying the value of the integer
    bound to "n" in the context by 1, 2 and 3. *)

let mk_eval_list ty k mk_values : t = fun e ->
   mk_list ty k (mk_values e) e


(***************************************************************)
(** * Pre-defined constructors *)

(** Specialized constructor for program names; for example,
    [mk_prog "foo.out"] binds the key "prog" to the name "foo.out" *)

let mk_prog s =
  mk string "prog" s

let mk_progs ss =
  mk_list string "prog" ss


(***************************************************************)
(** * Printing (for debugging) *)
  
let to_string m =  
  let es = m Env.empty in
  XList.to_string "\n" Env.to_string es


(***************************************************************)
(** * Combinators *)

(** Cross-product operator: [m1 & m2] builds the cross product of
    parameters in m1 with the parameters in m2, in such a way that
    definitions in m2 may refer to values bound in m1. *)

let cross m1 m2 = fun e0 ->
   let es1 = m1 e0 in
   ~~ XList.concat_map es1 (fun e1 -> 
      let e01 = Env.append e0 e1 in
      let es2 = m2 e01 in 
      ~~ List.map es2 (fun e2 -> 
         Env.append e1 e2))

(** Shorthand notation for cross product *)

let (&) = cross

(** Concatenation operator: [m1 ++ m2] builds the union of two sets 
    of sets of parameters. *)

let append m1 m2 = fun e ->
   m1 e @ m2 e

(** Shorthand notation for concatenation *)

let (++) = append

(** Iterated concatenation *)

let concat ms = fun e ->
   List.fold_left (fun acc m -> acc @ m e) [] ms

(** Filtering, provided a function of type env->bool *)

let filter f m = fun e ->
   List.filter f (m e)

(** Evaluate on the empty environment, to get a list of environments *)

let to_envs m =
   m Env.empty

(** Evaluate on the empty environment, and extract the unique environment
    in the list *)

let to_env m =
   match to_envs m with
   | [e] -> e
   | _ -> Pbench.error ("to_env fails, because nb env is not one in: "  ^ 
            try to_string m with _ -> "<params could not be evaluated>")

(** Map a function to all the environments associated with a params 
    evaluated on the empty environment *)

let map_envs f m =
    List.map f (to_envs m)

(** Map a function to to all the environments associated with a params 
    evaluated on the empty environment, and rebuild a params from it *)

let map_concat f m =
    concat (List.map f (to_envs m))

(** [eval m] evaluates once and for all [m] in the empty environment,
    so as to avoid evaluating its body at every use site. *)

let eval m =
   from_envs (to_envs m)


(***************************************************************)
(** * Environment lookup *)

exception Not_in_env of string

(** Shorthand for lookup: "get conv e k" returns the value (of type 
    Env.values) to which the key "k" is bound. *)

let get conv e k =
  let v = Env.get e k in
  try conv v 
  with Env.Cast_error ty -> 
     raise (Env.Cast_error (sprintf "key %s not convertible to type %s" k ty))
   
    
(***************************************************************)
(** * Prefixes *)

let strip_prefixes_for ks m = fun e ->
   List.map (Env.strip_prefixes_for ks) (m e)
   
