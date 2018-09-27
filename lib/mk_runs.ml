open XBase


type mode = Normal | Append | Complete | Replace

type arg =
   | Runs_opt of Runs.arg list (* options for bench runs; only verbose should be there *)
   | Output of string (* name of the output file where to append results; default=results.txt *)
   | Virtual of bool  (* whether to skip the runs and only print the command line; default=false *)
   | Args of Params.t (* combination of parameters to consider; mendatory argument *)
   | Dummy of bool    (* whether to make a dummy run before performing runs; default=false *)
   | Runs of int      (* number of times to run each combination of parameters; default=1 *)
   | Timeout of int   (* number of seconds after which to kill runs (to override the timeout keys in args, if any) *)
   | Attempts of int  (* number of trial for each run; default=1 --> should be in args too; default=1 *)
   | Mode of mode

let get_runs_opt args =
   let args2 = XOpt.projects args (function Runs_opt x -> Some x | _ -> None) in
   ~~ List.iter args2 (function Runs.Verbose x -> () | _ -> Pbench.error "Mk_runs does not accept runs_opt other than 'verbose'");
   args2
let get_output args = XOpt.get_default args (function Output x -> Some x | _ -> None) "results.txt"
let get_virtual args = XOpt.get_default args (function Virtual x -> Some x | _ -> None) false
let get_args args = XOpt.get_error args (function Args x -> Some x | _ -> None) "Mk_runs.call needs args"
let get_dummy args = XOpt.get_default args (function Dummy x -> Some x | _ -> None) false
let get_runs args = XOpt.get_default args (function Runs x -> Some x | _ -> None) 1
let get_timeout_opt args = XOpt.get_option args (function Timeout x -> Some x | _ -> None)
let get_attempts args = XOpt.get_default args (function Attempts x -> Some x | _ -> None) 1
let get_mode args = XOpt.get_default args (function Mode x -> Some x | _ -> None) Normal

let call args =
   let is_virtual = get_virtual args in
   let output_file = get_output args in
   let runs = get_runs args in
   if runs < 0 then Pbench.error "runs should be non-negative";
   let build_runs e = (* todo: cleanup this code, by filtering special keys all at once *)
      let (e_prog,e) = Env.partition (fun k -> k = "prog") e in
      let (e_timeout,e) = Env.partition (fun k -> k = "timeout") e in
      let (e_runs,e) = Env.partition (fun k -> k = "runs") e in
      let (e_ghost,e_normal) = Env.partition Env.is_ghost_key e in
      let local_runs =
        try Env.as_int (Env.lookup e_runs "runs")
        with Not_found -> 1
        in
      let timeout =
         match get_timeout_opt args with
         | Some n -> n
         | _ ->
            try Env.as_int (Env.lookup e_timeout "timeout")
            with Not_found -> -1 (* Pbench.error "missing 'timeout' in args for run" *)
         in
      let prog =
         try Env.as_string (Env.lookup e_prog "prog")
         with Not_found -> Pbench.error "missing 'prog' in args for run" in
      let prog = (* add the leading './' if appropriate *)
        let dot_slash_prog = "./" ^ prog in
        if not (String.contains prog '/')
           && (Sys.file_exists dot_slash_prog)
           then dot_slash_prog
           else prog
        in
      let r = Runs.({
         run_prog = prog;
         run_args = Env.to_string_pairs (Env.strip_prefixes e_normal);
         run_ghost_args = Env.to_string_pairs (Env.strip_prefixes e_ghost);
         run_timeout = timeout;
         run_attempts = (get_attempts args);
         run_dummy = false; }) in
      XList.init local_runs (fun i -> r)
     in
   let requested_es = Params.to_envs (get_args args) in
   let clear_output_file () =
      if not is_virtual
         then XFile.put_contents output_file ""
      in
   let machine = Pbench.get_localhost_name() in
   let equiv existing_e requested_e =
      let requested_e = Env.add requested_e "machine" (Env.Vstring machine) in
      let requested_e = Env.filter (fun k -> not (Env.is_ghost_key k) && k <> "timeout" && k <> "runs") requested_e in
      let b = Env.equiv_when_stringified existing_e requested_e in
      (* for debugging: printf "-- compare: \n\t%s \n\t%s\n%s\n" (Env.to_string existing_e) (Env.to_string requested_e) (if b then "same" else "diff"); *)
      b in
   let requested_es = match get_mode args with
      | Normal ->
         clear_output_file();
         if requested_es = []
            then Pbench.warning (sprintf "No run were requested.");
         requested_es
      | Complete ->
         if runs > 1
            then Pbench.warning "Interaction of --complete and -runs n for n > 1 is not properly supported.";
         if not (Sys.file_exists output_file) then clear_output_file();
         let results = Results.from_file_or_empty output_file in
         let existing_es = Results.get_input_envs results in
         let keep_es = ~~ List.filter requested_es (fun requested_e ->
            ~~ List.for_all existing_es (fun existing_e -> not (equiv existing_e requested_e))) in
            (* TODO: avoid the quadratic complexity above *)
            (* TODO: handle properly the interaction between --complete and -runs n *)
         (* for debugging:
         printf "existing ==>\n";
         ~~ List.iter existing_es  (fun e -> Pbench.info (Env.to_string e));
         printf "requested ==>\n";
         ~~ List.iter requested_es  (fun e -> Pbench.info (Env.to_string e));
         printf "kept ==>\n";
         ~~ List.iter keep_es  (fun e -> Pbench.info (Env.to_string e));
         exit 1; *)
         if keep_es = []
            then Pbench.info (sprintf "No runs needed to complete %s." output_file);
         keep_es
      | Append ->
         ();
         requested_es
      | Replace ->
         if runs > 1
            then Pbench.warning "Interaction of --replace and -runs n for n > 1 is not properly supported.";
         if not (Sys.file_exists output_file) then clear_output_file();
         let results = Results.from_file_or_empty output_file in
         let keep_results = ~~ List.filter results (fun r ->
            let existing_e = Results.result_get_input_env r in
             ~~ List.for_all requested_es (fun requested_e -> not (equiv existing_e requested_e))
            ) in
         Results.to_files output_file keep_results;
         requested_es
      in
   let bs1 = XList.concat_map build_runs requested_es in
   let bs = List.concat ((XList.init runs) (fun i -> bs1)) in
   let mk_dummy () =
      Runs.({ (List.hd bs) with run_attempts = 1; run_dummy = true }) in
   let bs2 = if (get_dummy args) && bs <> [] then mk_dummy()::bs else bs in
   Runs.call (Runs.([Output output_file; Virtual is_virtual; Runs bs2]) @ (get_runs_opt args));
   Pbench.info (sprintf "Results written to %s." output_file)


(************************************************************************)
(** Helper functions *)

let valid_modes = [ "normal"; "append"; "complete"; "replace" ]

let mode_of_string = function
   | "normal" -> Normal
   | "append" -> Append
   | "complete" -> Complete
   | "replace" -> Replace
   | _ -> Pbench.error "Mk_runs.string_of_mode: invalid argument (not one of 'normal', 'append', 'complete' or 'replace'.)"

let mode_string_from_command_line key =
   let m = XCmd.parse_optional_string key in
   let mn = XCmd.mem_flag "normal" in
   let ma = XCmd.mem_flag "append" in
   let mc = XCmd.mem_flag "complete" in
   let mr = XCmd.mem_flag "replace" in
   if XList.count (fun x -> x) [mn; ma; mr; mc; m<>None] > 1
      then Pbench.error "only one mode should be specified among: normal, append, complete, replace.";
   match m with
   | Some mode -> mode
   | None ->
     if ma then "append"
     else if mc then "complete"
     else if mr then "replace"
     else "normal"

let mode_from_command_line key =
  let mode_string = mode_string_from_command_line key in
  mode_of_string mode_string

