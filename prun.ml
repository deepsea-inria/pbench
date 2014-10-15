open XBase

let arg_verbose = XCmd.parse_or_default_bool "verbose" false
let arg_output = XCmd.parse_or_default_string "output" "results.txt"
let arg_virtual = XCmd.parse_or_default_bool "virtual" false
let arg_args = XCmd.parse_optional_string "args" 
let arg_dummy = XCmd.parse_or_default_bool "dummy" false
let arg_runs = XCmd.parse_or_default_int "runs" 1
let arg_timeout = XCmd.parse_or_default_int "timeout" Runs.cst_no_timeout
let arg_attempts = XCmd.parse_or_default_int "attempts" 1
let arg_mode = Mk_runs.mode_from_command_line()

let reserved_keys = 
    ["verbose"; "output"; "virtual"; "args"; "dummy";
     "runs"; "timeout"; "attempts"; "mode" ]
  @ Mk_runs.valid_modes

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
    | XCmd.Arg (key, _) | XCmd.Flag key when List.mem key reserved_keys -> false 
    | _ -> true) 
    in
  arguments_of_elements elements

let arguments_explicit = 
  match arg_args with
  | None -> []
  | Some s -> 
      let ls = Str.split (Str.regexp "[ \t]+") s in
      let elements = XCmd.elements_from_string_list ls in
      arguments_of_elements elements

let arguments =
  match arguments_implicit, arguments_explicit with
  | [], [] -> Pbench.error "No benchmark arguments provided"
  | [], a | a, [] -> a
  | _ -> Pbench.error "Conflict between -args argument and other arguments not associated with prun."

let args = 
  List.fold_left Params.(&) Params.mk_unit 
    (~~ List.map arguments (fun (k,vs) -> Params.mk_list Params.string k vs))

let () =
  Mk_runs.(call ([
    Runs_opt ([Runs.(Verbose arg_verbose)]);
    Output arg_output;
    Virtual arg_virtual;
    Args args;
    Dummy arg_dummy;
    Runs arg_runs;
    Timeout arg_timeout;
    Attempts arg_attempts;
    Mode arg_mode;
    ]))
