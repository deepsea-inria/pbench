open XBase
open Params

let path_to s = "path_to_" ^ s

let emit_allocator add =
  let allocator = XCmd.parse_or_default_string "allocator" "" in
  let use_allocator n = add (sprintf "USE_ALLOCATOR=%s" n) in
  let bindings = [
    ("jemalloc", fun () -> (
                   use_allocator "jemalloc";
                   let path = XCmd.parse_or_default_string (path_to "jemalloc") "" in
                   if path <> "" then
                     add (sprintf "JEMALLOC_PATH=%s" path)
                   else ()));
    ("tcmalloc", fun () -> (
                   use_allocator "tcmalloc";
                   let path = XCmd.parse_or_default_string (path_to "tcmalloc") "" in
                   if path <> "" then
                     add (sprintf "TCMALLOC_PATH=%s" path)
                   else ()));
  ]
  in
  match XList.assoc_option allocator bindings with
  | Some f -> f()
  | None -> 
     let all_allocator_names = String.concat "," (List.map fst bindings) in 
     if allocator <> "" then (
       Pbench.warning (sprintf "Unknown allocator: %s." allocator);
       Pbench.warning (sprintf "Supported allocators: %s." all_allocator_names))
     else ()

let emit_hwloc add =
  if XCmd.mem_flag "use_hwloc" then
    let _ = add "USE_HWLOC=1" in
    let path = XCmd.parse_or_default_string  (path_to "hwloc") "" in
    if path <> "" then
      add (sprintf "HWLOC_PATH=%s" path)
    else ()
  else ()

let all mode target_paths =
  let emitters = [
    emit_allocator;
    emit_hwloc;
  ]
  in
  let emit_all add = List.iter (fun e -> e add) emitters in
  let lines = XList.build emit_all in
  let output = String.concat "\n" lines in
(*  let _ = printf "%s\n" output in*)
  ~~ List.iter target_paths (fun path ->
                             let filename = path ^ "/settings.sh" in
                             printf "Writing configuration to %s\n" filename;
                             if mode = "append" then
                               XFile.append_contents filename output
                             else (* replace *)
                               XFile.put_contents filename output)
