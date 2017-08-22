open XBase


(** Escape several latex characters: underscore, tilde, percent. *)

let escape r =
   let r = Str.global_replace (Str.regexp "_") "-" r in
   let r = Str.global_replace (Str.regexp "~") "{\\raise.17ex\\hbox{$\\scriptstyle\\mathtt{\\sim}$}}"  r in (* "\\char`\\~"   "$\\sim$"*)
   let r = Str.global_replace (Str.regexp "%") "\\%" r in
   (*let r = Str.global_replace (Str.regexp "\\\\") "\\" r in*)
   r

(** Escape latex characters in a list of strings *)

let escapes rs = 
  List.map escape rs

(** Function to a coma-separated string into a multiline latex comments *)

let coma_to_newlines s = 
   escape (Str.global_replace (Str.regexp ",") "\\\\\\\\" s)

(** Build basic document with given contents *)
(* \usepackage[margin=0cm, paperwidth=23.5cm, paperheight=23.1cm]{geometry} *)

let basic_document contents =
   "\\documentclass[12pt]{article}\n \
   \\usepackage{graphicx}\n \
   \\usepackage{verbatim}\n \
   \\usepackage{amssymb}\n \
   \\usepackage{dcolumn}\n \
   \\newcolumntype{d}[1]{D{.}{.}{#1}}\n \
   \\usepackage[margin=0.5cm, paperwidth=30cm, paperheight=30cm]{geometry}
   \\setlength{\\parindent}{0pt}
   \\pagestyle{empty}\n \
   \\begin{document}\n"
   ^ contents
   ^ "\\end{document}\n"

(** Build a pdf from a tex source string *)

let build output_file tex_source =
   Pbench.ensure_results_folder_exists();
   let folder = Pbench.get_results_folder() in
   XFile.put_contents (folder ^ "/latex.tex") tex_source;
   Pbench.system (sprintf "cd %s; pdflatex -interaction=batchmode latex.tex > null" folder);
   Pbench.system (sprintf "mv %s/latex.pdf %s" folder output_file);
   Pbench.info (sprintf "Produced file %s." output_file)

(** Helper functions *)

let tabular_begin s = sprintf "\\begin{tabular}{%s} \\hline \n" s
let tabular_end = "\\end{tabular}\n"
let tabular_col = " & "
let tabular_multicol nb format content = sprintf "\\multicolumn{%d}{%s}{%s}" nb format content
let tabular_newline = " \\\\ \\hline \n"
let new_page = " \\newpage \n"
let new_line = " \\\\ \n"
let vspace x = sprintf "\\vspace{%.3fem}" x
let bold s = sprintf "\\bf{%s}" s
