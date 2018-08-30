
(** Write the string [str] into a file of given name *)

let put_contents filename str =
  let channel = open_out filename in
  output_string channel str;
  close_out channel

(** Write a list of lines into a file of given name *)

let put_lines filename ?(sep="\n") lines =
   put_contents filename (String.concat sep (lines @ [""]))

(** Read the lines of a file; raise FileNotFound if no such file *)

exception FileNotFound of string

let get_lines file =
   if not (Sys.file_exists file)
      then raise (FileNotFound file);
   let lines = ref [] in
   let f =
      try open_in file with End_of_file -> raise (FileNotFound file);
      in
   begin try while true do
      lines := input_line f :: !lines
   done with End_of_file -> () end;
   close_in f;
   XList.rev_not_rec !lines

(** Read the content of a file as a list of lines;
    returns an empty list if no such file exists *)

let get_lines_or_empty file =
   try get_lines file
   with FileNotFound _ -> []

(** Read the content of a file as a string, terminated with a newline;
    raise FileNotFound if no such file exists *)

let get_contents file =
   let lines = get_lines file in
   (String.concat "\n" lines) ^ "\n"

(** Read the content of a file as a string, terminated with a newline;
    returns an empty string if no such file exists *)

let get_contents_or_empty file =
   try get_contents file
   with FileNotFound _ -> ""

(** Append a string to the end of an existing file *)

let append_contents filename str =
  let contents = get_contents filename in
  put_contents filename (contents^str)


(****************************************************)

(** Get the extension of a filepath *)

let get_extension f =
   let n = String.length f in
   let p = String.rindex f '.' + 1 in
   String.sub f p (n - p)



(****************************************************)


(** Read a character from an input channel *)

let read_char ch =
  Char.chr (read_int ch)

(** Read a string from an input channel, character by character *)

let read_string ch =
  let len = read_int ch in
  let str = Bytes.make len ' ' in
  for i = 0 to pred len do
     Bytes.set str i (read_char ch)
  done;
  Bytes.to_string str

(** Read real 32-bit integers from an input channel *)

let read_int32 ch =
   let ch1 = input_byte ch in
   let ch2 = input_byte ch in
   let ch3 = input_byte ch in
   let ch4 = input_byte ch in
   let small = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
   Int32.logor (Int32.shift_left (Int32.of_int ch4) 24) small

(** Read real 64-bit integers from an input channel *)

let read_int64 ch =
   let small = Int64.of_int32 (read_int32 ch) in
   let big = Int64.of_int32 (read_int32 ch) in
   Int64.logor (Int64.shift_left big 32) small

(** Read an OCaml int from an input channel *)

let read_int ch =
   Int64.to_int (read_int64 ch)

(* TODO: does not work
let read_double ch =
	Int64.float_of_bits (read_int64 ch)
 *)

(* DEBUG
   print_int ch1;print_newline();
   print_int ch2;print_newline();
   print_int ch3;print_newline();
   print_int ch4; print_newline();

let _ =
   let ch = open_in_bin "LOG_BIN" in
   let a = read_double ch in
   Printf.printf "%f\n" a;
   let b = read_int64 ch in
   Printf.printf "%s\n" (Int64.to_string b);
   let c = read_int64 ch in
   Printf.printf "%s\n" (Int64.to_string c);
   close_in ch;
   exit 0
*)
