open XBase

(** Latex escape *)

let escape add s =
   add (Latex.escape s)

(** Auxiliary function to build a cell of a table *)

let cell ?(escape=true) ?(last=false) add s =
   add (if escape then Latex.escape s else s);
   if not last then add " & "

let build_table tex_file pdf_file body =
   let s = Buffer.create 1 in
   let add x = Buffer.add_string s x in
   body add;
   let latex = Buffer.contents s in
   XFile.put_contents tex_file latex;
   Latex.build pdf_file (Latex.basic_document latex)

