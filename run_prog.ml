open Printf
open Sys_tools
open Env
open Params

(** Usage, e.g.:
     run prog=foo.out,bar.out n=12,20 --verbose

    General pattern:
     run [key1=value11,value12,..] [run_options] 

*)

let debug = true
let sprintf = Printf.sprintf 

let parse_key_values s =
   try 
      let lens = String.length s in
      let k = String.index s '=' in
      assert (k < lens-1);
      let key = String.sub s 0 k in
      let svalues = String.sub s (k+1) (lens - k - 1) in
      let values = XList.parse_coma_separated_list (fun x -> x) svalues in 
      (key, values)
   with Not_found -> 
      failwith ("could not parse in the form key=value1,..,valueN the string: " ^ s)

let build_args key_eq_values =
   let kvs_list = List.map parse_key_values key_eq_values in
   List.fold_left (fun acc (k,vs) -> 
      Params.cross acc (Params.mk_list string k vs)) Params.mk_unit kvs_list

let bool_of_string s =
   match s with
   | "true" | "1" -> true
   | "false" | "0" -> false
   | _ -> failwith ("invalid argument for bool_of_string: " ^ s)

let build_other_options args =
   let opt key conv =
      match XList.assoc_option key args with
      | None -> []
      | Some svalue ->
         try [conv svalue] with _ -> failwith ("not the expected type for option %s" ^ key) 
      in
   Run_tools.Run.(List.concat [
     opt "output" (fun x -> Output x);
     opt "append" (fun x -> Append (bool_of_string x));
     opt "timeout" (fun x -> Timeout (int_of_string x));
     opt "dummy" (fun x -> Dummy (bool_of_string x));
     opt "runs" (fun x -> Runs (int_of_string x));
     opt "attempts" (fun x -> Attempts (int_of_string x));
     opt "silent" (fun x -> Silent (bool_of_string x));
     opt "verbose" (fun x -> Verbose (bool_of_string x));
     opt "virtual" (fun x -> Virtual (bool_of_string x)); ])


let _ = 
   let args = build_args (XCmd.get_others()) in
   let opts = build_other_options ((XCmd.get_args()) @ (List.map (fun k -> (k, "1")) (XCmd.get_flags()))) in
   Run_tools.Run.(run ([Args args] @ opts))


(* Example:
   make run_prog.byte && ./run_prog.byte prog=foo.out,bar.out n=4,5 m=2,3 -runs 2 --virtual -output results.txt
 *)
