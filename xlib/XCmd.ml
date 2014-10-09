
(** On loading, the module parses the command line, and creates a list
    of key-pair arguments, given in the form "-key value", a list 
    of activated flags, given in the form "--key", and a list of other
    values provided on the command line. *)

exception Illegal_command_line of string

let args,flags,others =
   let args = ref [] in
   let flags = ref [] in
   let others = ref [] in
   let argv = Sys.argv in
   let n = Array.length argv in
   let rec aux i =
      if i = n then () else begin
         let arg = argv.(i) in
         if arg.[0] <> '-' then begin
            XBase.add_to_list_ref others arg;
            aux (i+1)
         end else if arg.[1] = '-' then begin
            let key = Str.string_after arg 2 in       
            XBase.add_to_list_ref flags key;
            aux (i+1)
         end else begin
            let key = Str.string_after arg 1 in
            if i+1 = n then raise (Illegal_command_line ("missing argument after " ^ key));
            let value = argv.(i+1) in
            XBase.add_to_list_ref args (key,value);
            aux (i+2)
         end 
      end in
   aux 1;
   List.rev !args, List.rev !flags, List.rev !others

(** Returns the path the current binary program *)

let program () =
   Sys.argv.(0)
   (* same as: Sys.executable_name *)

(** Accessors and iterators for args and flags *) 

let iter_args f =
   List.iter (fun (k,v) -> f k v) args  

let iter_flags f =
   List.iter f flags

let iter_others f =
   List.iter f others

let get_args () =
   XList.build (fun add -> iter_args (fun x v -> add (x,v)))

let get_flags () =
   XList.build (fun add -> iter_flags (fun x -> add x))

let get_others () =
   others

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

(** Obtain the value bound to a key, as a string;
    or raise Argument_not_found *)

exception Argument_not_found of string

let find_arg name = 
   try List.assoc name args
   with Not_found -> raise (Argument_not_found name)

(** Obtain the value bound to a key, at a specific type,
    or raise Argument_not_found *)

let parse_bool name =
   let bool_of_string s = (int_of_string s != 0) in
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