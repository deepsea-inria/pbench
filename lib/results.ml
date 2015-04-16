open XBase


(***************************************************************)
(** * Representation of results *)

(** A result consists of a set of key-value bindings, represented
    using a Env.t, and a subset of the keys which correspond to
    input arguments provided to the benchmarked program. *)

type result = string list * Env.t

(** A set of results *)

type t = result list


(***************************************************************)
(** * Printing of results *)

let lines_of_result (inputs,env) =
   let kvs = Env.to_assoc env in
   let (kvsi,kvso) = List.partition (fun (k,v) -> List.mem k inputs) kvs in
   let string_of_kv (k,v) =
      sprintf "%s %s\n" k (Env.string_of_value v) in
   ( (List.map string_of_kv kvsi)
   @ ["---\n"]
   @ (List.map string_of_kv kvso)
   @ ["==========\n"]
   )

let string_of_result result =
   String.concat "" (lines_of_result result)

let to_files file results =
   XFile.put_contents file (String.concat "" (List.map string_of_result results))


(***************************************************************)
(** * Parsing of results *)

(** Underlying functions for parsing results *)

exception Cannot_parse of string

let value_of_string s =
   try Env.Vint (int_of_string s)
   with _ ->
   try Env.Vfloat (float_of_string s)
   with _ -> Env.Vstring s

(** Parsing of the lines of a result file *)

let from_lines lines : t =
   XList.build (fun (add_result:result->unit) ->
      let current_params = ref [] in
      let current_inputs = ref [] in
      let in_inputs = ref true in
      ~~ XList.iteri lines (fun id_line line ->
         if line = "==========" then begin
            add_result (List.rev !current_inputs, Env.from_assoc (List.rev !current_params));
            current_params := [];
            current_inputs := [];
            in_inputs := true
         end else if line = "" then begin
            ()
         end else if line = "---" then begin
            in_inputs := false
         end else begin
            let space_index =
               begin try String.index line ' '
               with Not_found ->
               try String.index line '\t'
               with Not_found -> raise (Cannot_parse (sprintf "no space or tab on line %d: %s" (id_line+1) line))
               end
               in
            try
               let key = Str.string_before line space_index in
               let svalue = Str.string_after line (space_index+1) in
               if key = "sError"
                  then Pbench.info (sprintf "Error in result: %s\n" svalue);
               add_to_list_ref current_params (key, value_of_string svalue);
               if !in_inputs
                  then add_to_list_ref current_inputs key;
            with _ -> raise (Cannot_parse (sprintf "error line %d: %s" (id_line+1) line))
            (* msg (sprintf "Error: could not parse line %d:\n%s\n" (id_line+1) line) *)
         end
         );
      if !current_params <> []
         then Pbench.warning "incomplete benchmark results at end of file";
      )

(** Reading results from filename *)

let from_file filename =
   let lines = XFile.get_lines filename in
   try from_lines lines
   with Cannot_parse s -> raise (Cannot_parse (sprintf "%s: %s" filename s))

(** Reading results from filename, if it exists *)

let from_file_or_empty filename =
   let lines = XFile.get_lines_or_empty filename in
   try from_lines lines
   with Cannot_parse s -> raise (Cannot_parse (sprintf "%s: %s" filename s))

(** Reading results from multiple filenames *)

let from_files filenames =
   XList.concat_map from_file filenames


(***************************************************************)
(** * Filtering *)

(** Filtering the results that satisfy the requirements specified by
    a function of type [(key*value) list -> bool] *)

let filter_by_fun f results =
   ~~ List.filter results (fun (inputs, ps_result) ->
      let kvs_result = Env.to_assoc ps_result in
      f kvs_result)

(** Filtering the results that contain all of the specified key-value
    bindings (represented as a Env.t); Prefixes in keys are removed. *)

let filter_nostrip env results =
   let kvs_filter = Env.to_assoc env in
   ~~ List.filter results (fun (inputs, ps_result) ->
      let kvs_result = Env.to_assoc ps_result in
      ~~ List.for_all kvs_filter (fun kv -> List.mem kv kvs_result))

let filter env results =
   let env = Env.strip_prefixes env in
   filter_nostrip env results

(** Filtering the results based on a singleton params *)

let filter_by_params ps results =
   filter (Params.to_env ps) results


(***************************************************************)
(** * Extraction of input environments *)

(** Takes a result and returns an environment
    corresponding to the input parameters *)

let result_get_input_env (inputs,env) =
  Env.filter (fun k -> List.mem k inputs) env

(** Takes a list of results and returns a list of environments
    corresponding to the input parameters *)

let get_input_envs results =
   List.map result_get_input_env results


(***************************************************************)
(** * Projection *)

exception Missing_key of string * string list (* missing key, available keys *)

(** Get the value associated with a given key for a given result *)

let get_one conv k (inkeys,ps) =
   let v =
      try Env.lookup ps k
      with Not_found -> raise (Missing_key (k, Env.keys ps))
      in
   try conv v
   with Env.Cast_error ty ->
      raise (Env.Cast_error (sprintf "result value bound to key %s is not convertible to type %s" k ty))

(** Get the set of values associated with a given key for a set of results *)

let get conv k results =
   List.map (get_one conv k) results

(** Specialized getters *)

let get_float k results = get Env.as_float k results
let get_int k results = get Env.as_int k results
let get_string k results = get Env.as_string k results

(** High-level getters *)

exception Missing_data

let get_mean_of k results =
   try  XFloat.mean_of (get Env.as_float k results)
      (* let vs = (get Env.as_float k results) in
      if List.mem infinity vs 
         then infinity 
         else XFloat.mean_of vs *)
   with Missing_key _ -> nan

let get_median_of k results =
   try XMath.median_of_or_nan (get Env.as_float k results)
      (* let vs = (get Env.as_float k results) in
      if List.mem infinity vs 
         then infinity 
         else XFloat.mean_of vs *)
   with Missing_key _ -> nan

let get_mean_and_stddev_of k results =
  try XFloat.list_mean_and_stddev (get Env.as_float k results)
  with Missing_key _ -> (nan, nan)

let get_stddev_of k results =
  let (_, stddev) = get_mean_and_stddev_of k results in
  stddev

(* todo: rename *)
let get_unique_of_as conv k results =
   try
      let vs = get conv k results in
      if vs = []
         then raise (Missing_key ("", []));
      if not (XList.same_items vs) then begin
        let m = String.concat "," (List.map Env.string_of_value (get (fun x->x) k results)) in
        Pbench.error (sprintf "Results.get_unique_of: not all the same values for key %s, values:\n%s" k m);
      end;
      List.hd vs
   with Missing_key _ -> raise (Missing_key ("", []))

(* todo: obtain by specializing above *)
let get_unique_of k results =
   try
      let vs = get Env.as_float k results in
      if vs = []
         then raise (Missing_key ("", []));
      if not (XList.same_items vs)
         then Pbench.error (sprintf "Results.get_unique_of: not all the same values for key %s" k);
      List.hd vs
   with Missing_key _ -> nan

let get_distinct_values_for k results =
  let vs = ~~ List.map results (fun (inputs,ps_results) -> Env.get ps_results k) in
  XList.remove_duplicate vs

let get_distinct_values_for_several ks results =
  let vss : Env.t list = ~~ List.map results (fun (inputs,ps_results) -> 
    List.map (fun k -> (k, Env.get ps_results k)) ks) in
  List.map Env.from_assoc (XList.remove_duplicate vss)
  (* note: returns a list of environments *)



(***************************************************************)
(** * Check consistency of inputs *)

(** [check_consistent_inputs group_by results] ensures that all the results
    have the same input values for every input key that is not mentioned
    in the group_by list of keys. *)

let check_consistent_inputs group_by results =
   let envs = ~~ List.map results (fun (inkeys,e) ->
      ~~ List.filter (Env.to_assoc e) (fun (k,v) ->
         List.mem k inkeys && not (List.mem k group_by))) in
   match envs with
   | [] -> ()
   | e0::es ->
      ~~ List.iter es (fun e ->
         if not (XList.equiv e0 e)
            then begin
            Pbench.info (sprintf "Trying to build a value for incompatible input environments:\n--- %s\n--- %s\n"
               (Env.to_string e0) (Env.to_string e));
            Pbench.error "Terminating."
            end)


(***************************************************************)
(** * Check consistency of outputs *)

(** [check_consistent_outputs match_by results] ensures that all the results
    have the same output values for every key from the list match_by *)

let check_consistent_outputs match_by_keys results =
   let envs_outputs = ~~ List.map results (fun (inkeys,e) ->
      let os = ~~ List.filter (Env.to_assoc e) (fun (k,v) ->
                    List.mem k match_by_keys) in
      (e,os)) in
   match envs_outputs with
   | [] -> ()
   | (e0,os0)::eos ->
      ~~ List.iter eos (fun (e,os) ->
         if not (XList.equiv os0 os)
            then Pbench.error (sprintf "Inconsistent outputs:\n--- %s\n--- %s\n"
               (Env.to_string e0) (Env.to_string e)))

(** [check_consistent_outputs_filter_by env match_by results] ensures that all the results
    filtered by "env" have the same output values for every key from the list match_by *)

let check_consistent_outputs_filter_by_env match_by_keys env results =
    check_consistent_outputs match_by_keys (filter env results)

let check_consistent_outputs_filter_by_params match_by_keys ps results =
   ~~ List.iter (Params.to_envs ps) (fun e ->
       check_consistent_outputs_filter_by_env match_by_keys e results)


(** Shorthand for providing a single field *)

let check_consistent_outputs match_by_key results =
   check_consistent_outputs [match_by_key] results

let check_consistent_output_filter_by_env match_by_key env results =
    check_consistent_outputs_filter_by_env [match_by_key] env results

let check_consistent_output_filter_by_params match_by_key ps results =
    check_consistent_outputs_filter_by_params [match_by_key] ps results

let check_consistent_output_filter_by_params_from_file match_by_key ps filename =
   let results = from_file filename in
   check_consistent_output_filter_by_params match_by_key ps results
