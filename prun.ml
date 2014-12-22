open XBase

(************************************************************************)
(** Options *)

let arg_mode = XCmd.parse_optional_string "mode" 
let arg_verbose = XCmd.parse_or_default_bool "verbose" false
let arg_output = XCmd.parse_or_default_string "output" "results.txt"
let arg_virtual = XCmd.parse_or_default_bool "virtual" false
let arg_args = XCmd.parse_or_default_string "args" ""
let arg_dummy = XCmd.parse_or_default_bool "dummy" false
let arg_runs = XCmd.parse_or_default_int "runs" 1
let arg_timeout = XCmd.parse_or_default_int "timeout" Runs.cst_no_timeout
let arg_attempts = XCmd.parse_or_default_int "attempts" 1
let arg_output_mode = Mk_runs.mode_from_command_line "output-mode"

let arg_baseline = XCmd.parse_or_default_string "baseline" ""
let arg_parallel = XCmd.parse_or_default_string "parallel" ""
let arg_elision = XCmd.parse_or_default_string "elision" ""
let arg_baseline_runs = XCmd.parse_or_default_int "baseline-runs" arg_runs
let arg_baseline_timeout = XCmd.parse_or_default_int "baseline-timeout" arg_timeout


(************************************************************************)
(** Keys *)

let reserved_keys = 
    ["verbose"; "output"; "virtual"; "args"; "dummy";
     "runs"; "timeout"; "attempts"; "mode" ]
  @ ["baseline"; "parallel"; "baseline-runs"; "baseline-timeout"; "elision" ]
  @ Mk_runs.valid_modes

let supported_modes = 
   [ "default"; "speedup" ]


(************************************************************************)
(** Helpers *)

let arguments_of_elements elements =
  let count_others = ref 0 in
  let key_coma_separated_values =
    ~~ List.map elements (function
      | XCmd.Arg (key, value) -> (key, value)
      | XCmd.Flag key -> (key, "1")
      | XCmd.Other value ->
          if !count_others > 0 
            then Pbench.error "Multiple non-named arguments";
          incr count_others;
          ("prog", value))
    in
  let key_values = ~~ List.map key_coma_separated_values (fun (k,v) ->
    let vs = Str.split (Str.regexp ",") v in
    (k,vs)) in
  key_values

let arguments_implicit =
  let elements = ~~ List.filter (XCmd.get_elements()) (function
    | XCmd.Arg (key, _) when List.mem key reserved_keys -> false 
    | XCmd.Flag key when List.mem key reserved_keys -> false 
    | XCmd.Other value when List.mem value supported_modes -> false 
    | _ -> true) 
    in
  arguments_of_elements elements

let arguments_of_string str = 
    let ls = Str.split (Str.regexp "[ \t]+") str in
    let elements = XCmd.elements_from_string_list ls in
    arguments_of_elements elements

let args_or_arguments arguments = 
  Params.(
    List.fold_left (&) mk_unit 
      (~~ List.map arguments (fun (k,vs) -> mk_list string k vs))
  )

let args_of_string str =
  args_or_arguments (arguments_of_string str)


(************************************************************************)
(** Common *)

let get_arguments_shared () =
  match arguments_implicit, arguments_of_string arg_args with
  | [], [] -> []
  | [], a | a, [] -> a
  | _ -> Pbench.error "When -args is used, only prun-specific arguments can be provided outside of the -args parameter."

let common_options () =
  Mk_runs.([
    Runs_opt ([Runs.(Verbose arg_verbose)]);
    Output arg_output;
    Virtual arg_virtual;
    Dummy arg_dummy;
    Attempts arg_attempts;
    Mode arg_output_mode;
  ])


(************************************************************************)
(** Default *)

let default () =
  let arguments_shared = get_arguments_shared() in
  if arguments_shared = [] 
    then Pbench.warning "no benchmark arguments provided";
  let args_shared = args_or_arguments arguments_shared in
  let args_shared = Params.( args_shared ) in
  Mk_runs.(call (common_options() @ [
    Args args_shared;
    Runs arg_runs;
    Timeout arg_timeout;
    ]))


(************************************************************************)
(** Speedup *)

let speedup () =
  let args_shared = args_or_arguments (get_arguments_shared()) in
  let args_baseline = args_of_string arg_baseline in
  let args_parallel = args_of_string arg_parallel in
  let use_elision = XCmd.mem_arg "elision" in
  let args_elision = if use_elision then args_of_string arg_elision else Params.mk_unit in
  let extra prun_speedup timeout runs  =
    Params.(
        (mk string "!prun_speedup" prun_speedup)
      & (mk int "timeout" timeout) 
      & (mk int "runs" runs)) in
  Params.(
    let b = args_baseline & extra "baseline" arg_baseline_timeout arg_baseline_runs in
    let p = args_parallel & extra "parallel" arg_timeout arg_runs in
    let e = args_elision  & extra "elision" arg_timeout arg_runs in
    let args = 
        args_shared 
        & (if use_elision then (b ++ p ++ e) else (b ++ p))
    in 
  Mk_runs.(call (common_options() @ [ Args args; ])) )



(************************************************************************)
(** Main *)

let _ = 
   let implicit_mode = 
     match XList.inter supported_modes (XCmd.get_others()) with
     | [] -> None
     | [m] -> Some m
     | _ -> Pbench.error "Multiple modes specified"
     in
  let mode = 
     match implicit_mode, arg_mode with
     | None, None -> "default"
     | None, Some m | Some m, None -> m
     | _ -> Pbench.error "Multiple modes specified"
     in
  let run_fct = match mode with
    | "default" -> default
    | "speedup" -> speedup
    | _ -> Pbench.error "Unknown mode (should be 'default' or 'speedup')."
    in
  run_fct()

