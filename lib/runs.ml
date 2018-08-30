open XBase
let info = Pbench.info

(** Description of the arguments of a program (list of "-key value" to give it) *)

type arguments = (string * string) list

(** Constant to encode "no timeout" *)

let cst_no_timeout = -1

(** Description of the parameters of a single run
    -- Note: representation might change *)

type run = {
   run_prog : string;      (* name of the program, without arguments *)
   run_args : arguments;   (* arguments to be provided to the program *)
   run_ghost_args : arguments; (* arguments stored in results filed but not passed to the program *)
   run_timeout : int;      (* number of seconds after which to kill the program; (-1 means infinity) *)
   run_attempts : int;     (* maximal number of times to try running this benchmark; (-1 means infinity) *)
   run_dummy : bool;       (* indicates whether the run is a dummy whose result should be discarded *)
   }

(** Create a backup of a given file into _results/results_[date].txt *)

let create_backup_of_results source_file =
   let tm = Unix.gmtime (Unix.time()) in
   let target = sprintf "%s/results_%d-%d-%d_%d-%d-%d.txt" (Pbench.get_results_folder())
      (1900+tm.Unix.tm_year) (1+tm.Unix.tm_mon) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
   unix_command (sprintf "cp %s %s" source_file target)

(** Build lines that describe the benchmark in the output file *)

let string_of_arguments args =
   XList.to_string "" (fun (k,v) -> sprintf "%s %s\n" k v) args

let run_params machine b =
   let args = ("machine", machine) :: ("prog", b.run_prog) :: b.run_args in
   string_of_arguments args

(** Build the command line for executing a benchrun,
    and the command line to be displayed for this run *)

let bench_command result_file b =
   let prog = b.run_prog in
   let args = XList.to_string " " (fun (k,v) -> sprintf "-%s %s" k v) b.run_args in
   if b.run_timeout <= 0 && b.run_timeout <> cst_no_timeout
    then Pbench.error "invalid value for timeout (shoud be positive or cst_no_timeout)";
   let timeout_path = sprintf "prun_timeout" in
   let timeout =
      if b.run_timeout = cst_no_timeout
        then ""
        else sprintf "%s %d" timeout_path b.run_timeout
      in
   (* let utime = sprintf "/usr/bin/time -f %s %s" "\"fUsertime %U\"" in *)
   let disp_cmd = sprintf "%s %s" prog args in
   let full_cmd = sprintf "%s %s > %s" timeout disp_cmd result_file in
   (full_cmd, disp_cmd)

(** Extract the "exectime" field from the result file *)

let result_extract_and_report_exectime content =
   let reg = Str.regexp "exectime .*" in
   let s =
      try
         let _ = Str.search_forward reg content 0 in
         Str.matched_string content (* use matched_group *)
      with Not_found -> "exectime NA"
     in
   info s

(** Extract whether the "error" word occurs in the result file
   -- TODO: improve using better regexp *)

let result_contains_error content =
   begin
   let reg = Str.regexp "Error" in
   try let _ = Str.search_forward reg content 0 in true
   with Not_found -> false
   end ||
   begin
   let reg = Str.regexp "error" in
   try let _ = Str.search_forward reg content 0 in true
   with Not_found -> false
   end ||
   begin
   let reg = Str.regexp "exectime ERROR" in
   try let _ = Str.search_forward reg content 0 in true
   with Not_found -> false
   end
   (* TODO: catch the case of a time out *)

let result_contains_killed content =
   begin
   let reg = Str.regexp "killed" in
   try let _ = Str.search_forward reg content 0 in true
   with Not_found -> false
   end


(** Execute a benchrun; this function invokes
    - "add_output" on the liens to be dumped in the result file
    - "add_error" on command lines that failed to succeed *)

(* TODO: svn_version is deprecated *)

let execute is_verbose is_virtual (*svn_revision*) machine add_output add_error b =
   let result_file = sprintf "%s/run.txt" (Pbench.get_results_folder()) in
   let (full_cmd, disp_cmd) = bench_command result_file b in
   begin
      if b.run_dummy
         then info "[dummy]"
         else info (sprintf "%s" disp_cmd);
      if is_verbose
         then info (sprintf "==> %s" full_cmd);
   end;
   if not is_virtual then begin
      XFile.put_contents result_file "";
      let attempts_left = ref b.run_attempts in
      if !attempts_left <= 0 then Pbench.error "attempts should be positive";
      let result = ref "" in
      let timed_out = ref false in
      begin try while !attempts_left > 0 do
         let exit_code = Unix.system full_cmd in
         if exit_code = Unix.WEXITED 0 then begin
            result := XFile.get_contents result_file;
            if !result <> "" && not (result_contains_error !result)
               then raise Break;
         end else if exit_code = Unix.WEXITED 137 then begin
            timed_out := true;
            raise Break;
         end;
         decr attempts_left;
         if !attempts_left = 0
            then info "==========> failed... giving up."
            else info "==========> failed... retyring...";
      done with Break -> () end;
      if !attempts_left = 0 then begin
         add_error disp_cmd; (* complete failure *)
      end else begin
         if !attempts_left < !attempts_left
            then add_error disp_cmd; (* partial failure *)
         if not b.run_dummy then begin
            add_output (run_params machine b);
            add_output "---\n";
            add_output (string_of_arguments ["timeout", string_of_int b.run_timeout]);
            add_output (string_of_arguments b.run_ghost_args);
            (* add_output (sprintf "svninfo %s" svn_revision); *)
            if !timed_out then begin
               add_output ("killed 1\n");
               add_output ("exectime inf\n");
            end else begin
               add_output (!result);
               result_extract_and_report_exectime !result;
            end;
            add_output "==========\n";
         end
      end
   end

(** Description of the parameters of a set of runs *)

type arg =
   | Runs of run list (* list of runs to perform *)
   | Output of string (* name of the output file where to append results; default=results.txt *)
   | Verbose of bool  (* whether to report extra details on stdout; default=false *)
   | Virtual of bool  (* whether to skip the runs and only print the command line; default=false *)

let get_runs args = XOpt.get_error args (function Runs x -> Some x | _ -> None) "Run.call needs argument runs"
let get_verbose args = XOpt.get_default args (function Verbose x -> Some x | _ -> None) false
let get_virtual args = XOpt.get_default args (function Virtual x -> Some x | _ -> None) false
let get_output args = XOpt.get_default args (function Output x -> Some x | _ -> None) "results.txt"

(** Execute a set of benchruns; appends results to the file if it already exists *)

let call args =
   let is_virtual = get_virtual args in
   let output_file = get_output args in
   Pbench.ensure_results_folder_exists();
   (* let svn_revision = Pbench.get_subversion_revision() in *)
   let machine = Pbench.get_localhost_name() in
   let exec = execute (get_verbose args) is_virtual (*svn_revision*) machine in
   let (add_output,end_output) =
      if is_virtual then begin
         ((fun s -> ()), (fun () -> ()))
      end else begin
         let old_contents = XFile.get_contents_or_empty output_file in
         let ch = open_out output_file in
         if old_contents <> "" then begin
            output_string ch old_contents;
            flush ch;
         end;
         let add_output content =
            output_string ch content;
            flush ch;
            in
         let end_output () =
            close_out ch;
            create_backup_of_results output_file;
            in
         (add_output, end_output)
      end in
   let bs = get_runs args in
   let nb = List.length bs in
   let errors = XList.build (fun add_error ->
      ~~ XList.iteri bs (fun id b ->
         let progress = sprintf "[%d/%d]" (id+1) nb in
         info progress;
         exec add_output add_error b))
      in
    end_output();
    if is_virtual then begin
       info "Simulation completed."
    end else begin
      if errors = [] then begin
        info "Benchmark successful."
      end else begin
        info "Benchmark completed, but encountered errors on the following commands:";
        info (XList.to_string "" (fun s -> sprintf "\t%s\n" s) errors);
      end
   end

