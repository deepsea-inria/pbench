open XBase


(***************************************************************)
(** * Types *)

(** A set of parameters for a benchmark run is described as an 
    finite map from keys to values. Keys are represented as strings. 
    Values are of type bool or int or float or string,
    and they may also consists of list of values. The type "Env.t"
    describes the type of a set of parameters, currently implemented
    as association lists from keys to values. 
    
    There are two kind of special keys. Keys whose name starts with 
    a "-" character are "hidden" keys that will not show up in plots.
    Those whose name starts with a "!" character are phantom keys: 
    they will not show up in command lines nor graph plots. *)


type key = string

type keys = key list

type value =
   | Vbool of bool
   | Vint of int
   | Vfloat of float
   | Vstring of string
   | Vlist of value list

type t = (key * value) list

type formatter = t -> string


(***************************************************************)
(** * Formatting and Printing *)

(** Function to convert a value into a string
    --list of values are currently not supported *)

let string_of_value = function
   | Vbool true -> "1"
   | Vbool false -> "0"
   | Vint n -> string_of_int n
   | Vfloat x -> string_of_float x
   | Vstring s -> s (* for debugging: sprintf "'%s'" s *)
   | _ -> Pbench.error "unsupported value type for argument_of_param"

(** [is_ghost_key k] computes whether [k] is a ghost key, i.e.,
    its name starts with "!" *)

let is_ghost_key k =
   k <> "" && k.[0] = '!'

(** [strip_prefix k] removes the first character from [k] if it is 
    a special tag *)

let strip_prefix k =
   if k <> "" && k.[0] = '!' || k.[0] = '-' 
      then String.sub k 1 (String.length k - 1) 
      else k

(** [strip_prefixes e] strips the prefix of all the keys in the environment
    using [strip_prefix] *)

let strip_prefixes e =
   ~~ List.map e (fun (k,v) -> (strip_prefix k, v))

(** [strip_prefixes_for ks e] strips the prefix of the keys mentioned in ks
    using [strip_prefix] *)

let strip_prefixes_for ks e =
   ~~ List.map e (fun (k,v) -> 
      let k2 = if List.mem k ks then strip_prefix k else k in
      (k2, v))

(** Function to convert a params to a list of (string*string),
    converting the values using  string_of_value *)

let to_string_pairs e = 
   ~~ List.map e (fun (k,v) -> (k, string_of_value v)) 
   
(** Printing function *)

let to_string e =
   XList.to_string ", " (fun (k,v) -> Printf.sprintf "%s=%s" k (string_of_value v)) e

(** Printing of values *)

let rec fprintf_value ppf v =
   let fprintf = Format.fprintf in
   match v with
   | Vbool b -> fprintf ppf "%s" (if b then "true" else "false")
   | Vint n -> fprintf ppf "%d" n
   | Vfloat f -> fprintf ppf "%f" f
   | Vstring s -> fprintf ppf "\"%s\"" s
   | Vlist vs -> fprintf ppf "(%a)" (XList.fprintf "," fprintf_value) vs


(***************************************************************)
(** * Operations *)


(** The empty params *)

let empty = 
   []

(** lookup for the values associated with a key; raises Not_found if not found 
    --deprecated, use get instead *)

let lookup e k =
   List.assoc k e 

(** lookup for the values associated with a key; raises an error if not found *)

let get e k =
   try lookup e k
   with Not_found -> (* raise (Key_not_found_in_env k (XList.keys e)) *)
     Pbench.error (sprintf "Env.lookup: not found key '%s' in env:\n %s" k (to_string e)) 

(** Adding a key-value binding *)

let add e k v =
   (k,v)::e

(** Concatenation of two params (assumes distinct keys) *)

let append e1 e2 =
   ~~ List.iter e2 (fun (k,v) ->
      if List.mem_assoc k e1 
         then Pbench.error (Printf.sprintf "Shadowing in the environment for key %s in append environments:\n-- %s\n-- %s" k (to_string e1) (to_string e2)));
   e1 @ e2

(** Concatenation of a list params (assumes distinct keys) *)

let concat es =
   List.fold_right (fun e e2 -> append e2 e) es empty 

(** Map a function to the key value pairs *)

let map f e =
   List.map (fun (k,v) -> f k v) e

(** Map a function to the value bound to a particular key *)

let map_for_key k f e =
   List.map (fun (k2,v) -> let v2 = if k = k2 then f v else v in (k,v2)) e

(** Compose a list of operations on environments (left-to-right order) *)

let compose fs e = 
   List.fold_left (fun acc f -> f acc) e fs
   
(** Test equivalence of two environments *)

let equiv e1 e2 =
   XList.equiv e1 e2

(** Test equivalence of the string representation of two environments 
    (e.g. values "1" as string and 1 as int are considered equivalent) *)

let equiv_when_stringified e1 e2 =
   let stringify k v = (k, string_of_value v) in
   equiv (map stringify e1) (map stringify e2)



(***************************************************************)
(** * Projections and conversions *)

(** Conversions from association lists to params 
    (currently the identity, but might later change) *)

let from_assoc kvs : t =
   kvs

(** Conversions to association lists from params 
    (currently the identity, but might later change) *)

let to_assoc e : t =
   e

(** Get the list of keys *)

let keys e =
   List.map fst e 


(***************************************************************)
(** * Conversion of values *)

exception Cast_error of string

let val_bool = function
   | Vbool b -> b
   | _ -> raise (Cast_error "bool")

let val_int = function
   | Vint n -> n
   | _ -> raise (Cast_error "int")

let val_float = function
   | Vfloat f -> f
   | _ -> raise (Cast_error "float")

let val_string = function
   | Vfloat s -> s
   | _ -> raise (Cast_error "string")


let as_bool = function
   | Vbool b -> b
   | Vint n -> n <> 0
   | Vstring s ->    
       begin try (int_of_string s) <> 0
       with _ -> raise (Cast_error "bool") end
   | _ -> raise (Cast_error "bool")

let as_int = function
   | Vbool b -> if b then 1 else 0
   | Vint n -> n
   | Vfloat f -> int_of_float f
   | Vstring s ->
       begin try int_of_string s
       with _ -> raise (Cast_error "int") end
   | _ -> raise (Cast_error "int")

let as_float = function
   | Vbool b -> raise (Cast_error "float")
   | Vint n -> float_of_int n
   | Vfloat f -> f
   | Vstring s ->
       begin try float_of_string s
       with _ -> raise (Cast_error "float") end
   | _ -> raise (Cast_error "float")

let as_string = function
   | Vbool b -> string_of_int (if b then 1 else 0)
   | Vint n -> string_of_int n
   | Vfloat f -> string_of_float f
   | Vstring s -> s
   | _ -> raise (Cast_error "string")


let as_list_of ty = function
   | Vlist vs -> List.map ty vs
   | _ -> raise (Cast_error "list")

let as_keys = as_list_of as_string

(* todo: add errors *)
let get_as_bool e k = as_bool (get e k)
let get_as_int e k = as_int (get e k)
let get_as_float e k = as_float (get e k)
let get_as_string e k = as_string (get e k)


(***************************************************************)
(** * Filtering *)

(** Filter particular keys *)

let filter f e =
   List.filter (fun (k,v) -> f k) e

(** Partition particular keys *)

let partition f e =
   List.partition (fun (k,v) -> f k) e

(** Removes hidden keys from the environment *)

let filter_hidden_keys e =
   List.filter (fun (k,v) -> k <> "" && k.[0] <> '-') e

(** To_string combined with removed hidden keys *)

let filter_hidden_to_string e =
   to_string (filter_hidden_keys e)

(** Filter an environment according to the keys in a given list *)

let filter_keys ks e =
   List.filter (fun (k,v) -> List.mem k ks) e

(** Filter an environment according to the list of keys bound 
    to a particular ghost key -- not used

let filter_keys_from_ghost k e =
   let ks =
      try as_keys (get e k) 
      with Cast_error _ -> raise (Cast_error ("did not find a list of string for ghost key: " ^ k))
      in
   filter_keys ks e
*)


(***************************************************************)
(** * Formatter *)

(** String of environment in the form "k1=v1, k2=v2" *)

let formatter_key_values e = 
   XList.to_string ", " (fun (k,v) -> Printf.sprintf "%s=%s" k (string_of_value v)) e

(** String of environment in the form "v1, v2" *)

let formatter_values e = 
   XList.to_string ", " (fun (k,v) -> Printf.sprintf "%s" (string_of_value v)) e



(***************************************************************)
(** * Formatter *)

type format =
   | Format_key_eq_value 
   | Format_value 
   | Format_custom of (string -> string)
   | Format_custom_value of (value -> string)
   | Format_hidden
   
let format_one fo k v =
   match fo with 
   | Format_key_eq_value -> Printf.sprintf "%s=%s" k (string_of_value v)
   | Format_value -> string_of_value v
   | Format_custom f -> f (string_of_value v)
   | Format_custom_value f -> f v
   | Format_hidden -> ""

let format ?delim:(d=", ") fos e = 
   let string_of_item (k,v) =
      let fo = 
         match XList.assoc_option k fos with
         | None -> Format_key_eq_value
         | Some fo -> fo
         in
      format_one fo k v
      in
   let ss = List.map string_of_item e in
   XList.to_string d (fun x -> x) (List.filter (fun s -> s <> "") ss)

let format_values ks =
   List.map (fun k -> (k, Format_value)) ks

let format_hiddens ks =
   List.map (fun k -> (k, Format_hidden)) ks
