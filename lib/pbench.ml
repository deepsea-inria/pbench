open XBase


(************************************************************************)
(** User interactions *)

(** Reports an error *)

let error s =
   failwith s

(** Reports a warning *)

let warning str =
   XBase.message ("===> Warning: " ^ str)

(** Print an information message *)

let info s =
   XBase.message s


(************************************************************************)
(** System calls *)

(** System call that must succeed *)

let system s =
   try XSys.command_must_succeed s
   with XSys.Command_failure s ->
      error (sprintf "Error when executing command: %s" s)



(************************************************************************)
(** Host machine configuration *)

(** Compute the localhost name *)

let get_localhost_name () =
   let name = Unix.gethostname() in
   List.hd (Str.split (Str.regexp_string ".") name)

(** Compute the local directory *)

let get_local_directory () =
   Sys.getcwd() ^ "/"

(** Compute the current svn revision, if possible *)

let get_subversion_revision () =
  let r = Unix.system "svnversion . > .svninfo" in
  if r <> Unix.WEXITED 0 then begin
    warning "failed to get subversion number";
    "NA"
  end else begin
    try
      XFile.get_contents ".svninfo"
    with _ -> failwith "cannot read svninfo"
  end


(***************************************************************)
(** * Results output files *)

(** Give the name of the results folder *)

let arg_path_to_results = XCmd.parse_or_default_string "path_to_results" "_results"

let get_results_folder () =
   arg_path_to_results

(** Ensures that a _results folder exists for producing output *)

let ensure_results_folder_exists () =
   let folder = get_results_folder() in
   if not (Sys.file_exists folder)
      then system (Printf.sprintf "mkdir %s" folder)


(***************************************************************)
(** * Execution *)

(** Execute actions based on "only" and "skip"  list of keys *)

let execute_from_only_skip onlys skips keyfuncs =
   let keys = XList.keys keyfuncs in
   let check_incl l1 l2 =
      let show ss = String.concat "," ss in
      if not (XList.is_included l1 l2)
         then error (sprintf "Keys (%s) are not included in (%s)" (show l1) (show l2));
      in
   check_incl onlys keys;
   check_incl skips keys;
   let acts =
      if onlys <> []
         then onlys
         else XList.substract keys skips
      in
   ~~ List.iter keyfuncs (fun (k,f) -> if List.mem k acts then f())

