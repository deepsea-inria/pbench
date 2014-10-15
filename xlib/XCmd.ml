
(** On loading, the module parses the command line, and creates a list
    of key-pair arguments, given in the form "-key value", a list 
    of activated flags, given in the form "--key", and a list of other
    values provided on the command line. *)

exception Illegal_command_line of string

type element = 
  | Arg of string * string   (* -key value *)
  | Flag of string           (* --key *)
  | Other of string          (* value *)

let elements_from_string_list ls = (* todo: generalize the exception in a better way *)
   let rec aux acc = function
     | [] -> List.rev acc
     | arg::ls2 ->
       let n = String.length arg in
       if n = 0
          then raise (Illegal_command_line ("empty string as argument"));
       if arg.[0] <> '-' then begin
          aux ((Other arg)::acc) ls2
       end else begin
         if n <= 1 
            then raise (Illegal_command_line ("dash symbol on its own"));
         if arg.[1] = '-' then begin
            let key = Str.string_after arg 2 in       
            aux ((Flag key)::acc) ls2
         end else begin
            let key = Str.string_after arg 1 in
            match ls2 with
            | [] -> raise (Illegal_command_line ("missing argument after " ^ key));
            | value::ls3 -> aux (Arg(key, value)::acc) ls3
         end 
      end
    in
 aux [] ls

let elements =
   let argv = Sys.argv in
   let n = Array.length argv in
   let rec aux acc i =
     if i = n then List.rev acc else aux (argv.(i)::acc) (i+1)
     in
   let ls = aux [] 1 in
   elements_from_string_list ls


let args,flags,args_and_flags,others =
   (* todo: reimplement in more functional style *)
   let args = ref [] in
   let flags = ref [] in
   let args_and_flags = ref [] in
   let others = ref [] in
   List.iter (function
     | Arg (key,value) -> 
        XBase.add_to_list_ref args (key,value);
        XBase.add_to_list_ref args_and_flags (key,value)
     | Flag key -> 
        XBase.add_to_list_ref flags key;
        XBase.add_to_list_ref args_and_flags (key, "1")
     | Other value -> 
        XBase.add_to_list_ref others value
     ) elements;
   List.rev !args, List.rev !flags, List.rev !args_and_flags, List.rev !others

(** Returns the path the current binary program *)

let program () =
   Sys.argv.(0)
   (* same as: Sys.executable_name *)

(** Accessors *) 

let get_elements () =
   elements

let get_args () =
   args
   (*depreacted: XList.build (fun add -> iter_args (fun x v -> add (x,v))) *)

let get_flags () =
   flags 
   (*depreacted: XList.build (fun add -> iter_flags (fun x -> add x)) *)

let get_args_and_flags () =
   args @ (List.map (fun k -> (k,"1")) flags)

let get_others () =
   others

(** Iterators *) 

let iter_args f =
   List.iter (fun (k,v) -> f k v) args  

let iter_flags f =
   List.iter f flags

let iter_others f =
   List.iter f others


(** Functions for removing args and flags

let remove_flag name =
   Hashtbl.remove flags name

let remove_arg name =
   Hashtbl.remove args name
 *)

(** Test whether a flag was activated *)

let mem_flag name =
   List.mem name flags 

(** Test wheter an argument was provided *)

let mem_arg name = 
   List.mem_assoc name args  

(** Obtain the value bound to a key in args or flags, as a string;
    flags are mapped to "1".  In not found, raise Argument_not_found *)

exception Argument_not_found of string

let find_arg name = 
   try List.assoc name args
   with Not_found ->
     if List.mem name flags
     then "1" 
     else raise (Argument_not_found name)

(** Obtain the value bound to a key, at a specific type,
    or raise Argument_not_found *)

let bool_of_string s =
   match s with
   | "true" | "1" -> true
   | "false" | "0" -> false
   | _ -> failwith ("invalid argument for bool_of_string: " ^ s)

let parse_bool name =
   bool_of_string (find_arg name)

let parse_int name =
   int_of_string (find_arg name)

let parse_float name =
   float_of_string (find_arg name)

let parse_string name =
   (find_arg name)

let parse_list_string name = 
  Str.split (Str.regexp_string ",") (find_arg name)

let parse_list_int name =
   List.map int_of_string (parse_list_string name)

let parse_list_float name =
   List.map float_of_string (parse_list_string name)

(** Obtain the value bound to a key, at a specific type, 
    or return a default value of this type. *)

let parse_or_default parsing name default =
   try parsing name with Argument_not_found _ -> default

let parse_or_default_bool n d =
   parse_or_default parse_bool n d

let parse_or_default_int n d =
   parse_or_default parse_int n d

let parse_or_default_float n d =
   parse_or_default parse_float n d

let parse_or_default_string n d =
   parse_or_default parse_string n d

let parse_or_default_list_string n d =
   parse_or_default parse_list_string n d

let parse_or_default_list_int n d =
   parse_or_default parse_list_int n d

let parse_or_default_list_float n d =
   parse_or_default parse_list_float n d

(** Obtain "Some" applied to the value bound to a key, at a specific type, 
    or "None". *)

let parse_optional parsing name =
   try Some (parsing name) with Argument_not_found _ -> None

let parse_optional_bool n =
   parse_optional parse_bool n

let parse_optional_int n =
   parse_optional parse_int n

let parse_optional_float n =
   parse_optional parse_float n

let parse_optional_string n =
   parse_optional parse_string n

let parse_optional_list_string n =
   parse_optional parse_list_string n

let parse_optional_list_int n =
   parse_optional parse_list_int n

let parse_optional_list_float n =
   parse_optional parse_list_float n



(** Pretty printing *)

let string_of_flags flags =
   XList.to_string " " (fun x -> Printf.sprintf "--%s" x) flags
   
let string_of_args args =
   XList.to_string " " (fun (k,v) -> Printf.sprintf "-%s %s" k v) args