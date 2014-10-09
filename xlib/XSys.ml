
let command_as_bool s =
   match Unix.system s with
   | Unix.WEXITED 0 -> true
   | Unix.WEXITED n -> false
   | Unix.WSIGNALED n -> false
   | Unix.WSTOPPED n -> false

exception Command_failure of string

let command_must_succeed s =
   let r = command_as_bool s in
   if not r 
      then raise (Command_failure s)

(*
      (let r = Unix.system c in r)
      (let n = Sys.command c in Unix.WEXITED n)
*)


let absolute_path path =
   command_must_succeed (Printf.sprintf "pwd %s > .absolutepath" path);
   String.trim (XFile.get_contents ".absolutepath")

let command_or_virtual cmd s is_virtual =
  if not is_virtual then (ignore (cmd s);()) else Printf.printf "%s\n" s;
  ()

(**********************************)
(** Commands below were added by Mike *)

(** Execute system command s only if is_virtual is true; 
    otherwise just print the command to stdout. *)
let command_must_succeed_or_virtual s is_virtual =
  command_or_virtual command_must_succeed s is_virtual

let command_as_bool_or_virtual s is_virtual =
  command_or_virtual command_as_bool s is_virtual

(* surl: source url string, sdst: path to destination file *)
let wget ?no_clobber:(nc=false) surl sdst is_virtual =
  let ncs = if nc then "--no-clobber" else "" in
  (* by default wget returns a nonzero exit code when wget bypasses a download because the file already exists *)
  command_as_bool_or_virtual (Printf.sprintf "wget %s %s -O %s" ncs surl sdst) is_virtual

(* spath: path of directory of source file, sfname: name of file*)
let gunzip spath sfname is_virtual =
  command_must_succeed_or_virtual (Printf.sprintf "(cd %s; yes|gunzip %s)" spath sfname) is_virtual

