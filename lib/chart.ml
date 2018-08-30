open XBase


(******************************************************************************)
(** * Template for latex files --taken from plots.tex, removing % signs, and escaping *)

let latex_plots = "\
\\documentclass[12pt]{article}\
\\usepackage{graphicx}\
\\usepackage{verbatim}\
\\usepackage[margin=0cm, paperwidth=30cm, paperheight=15cm]{geometry}\
\\pagestyle{empty}\
\
\\begin{document}\
\\newcommand{\\myfig}[1]{ \
\\includegraphics[width=10.0cm]{#1} \
}\
\
\\newcommand{\\mytable}[1]{ \
\\tiny\
\\input{#1}\
\\newpage\
}\
\\input{list}\
\\end{document}\
"



(******************************************************************************)
(** * Chart *)

type arg =
  | Rscript of string
  | Dimensions of (float * float)
  | Legend_opt of Legend.t
  | Title of string
  | Comments of string

  (* not yet supported:
        | Colors of string list (* color of the curves *)
        | Pchs of string list   (* style of the curve points *)
  *)

type t = arg list

let get_rscript args = XOpt.get_error args (function Rscript x -> Some x | _ -> None) "Chart.build needs Rscript"
let get_dimensions args = XOpt.get_default args (function Dimensions x -> Some x | _ -> None) (6.,6.)
let get_legend_opt args = XOpt.projects args (function Legend_opt x -> Some x | _ -> None)
let get_title args = XOpt.get_default args (function Title x -> Some x | _ -> None) ""
let get_comments args = XOpt.get_default args (function Comments x -> Some x | _ -> None) ""

(** Option to activate debugging of R scripts *)

let build_debug = false

exception Cannot_build of string

(** Given a list of charts, described as chart options and R-scripts,
    generate a PDF in a target output file. *)

let build_for prefix folder output_file (charts : t list) =
   Pbench.ensure_results_folder_exists();
   let rscripts = ref [] in
   let latexs = ref [] in
   if prefix = "chart" then Pbench.info (sprintf "Starting to generate %d charts." (List.length charts));
   ~~ List.iteri charts (fun id_chart chart ->
      let rscript_core = get_rscript chart in
      let basename = sprintf "%s-%d" prefix(id_chart+1) in
      let basepath = sprintf "%s/%s" folder basename in
      let rscript = Rtool.wrap_image rscript_core (get_dimensions chart) basepath "pdf" in
      if build_debug then begin
         let rfilename = basename ^ ".r" in
         Rtool.execute rscript rfilename;
         Pbench.info (sprintf "Graph printed: %s.pdf\n" basename);
      end else begin
         add_to_list_ref rscripts rscript;
      end;
      let latex = sprintf "\\myfig{%s.pdf}\n\\\\ \\small %s\\newpage\n" basename (get_comments chart) in
      add_to_list_ref latexs latex;
      );
  if !rscripts <> [] then begin
     let fullscript = String.concat "\n" (List.rev !rscripts) in
     let rfile_path = folder ^ (sprintf "/%s-all.r" prefix) in
     Rtool.execute fullscript rfile_path;
     ()
  end ;
  latexs

let build output_file (charts : t list) =
  let folder = Pbench.get_results_folder() in
  let _ = build_for output_file folder output_file charts in
  let latexs = build_for "chart" folder output_file charts in
  if !latexs = []
      then Pbench.warning "no plots to output!\n";
   XFile.put_contents (folder ^ "/plots.tex") latex_plots;
   XFile.put_lines (folder ^ "/list.tex") (List.rev !latexs);
   Pbench.system (sprintf "cd %s; pdflatex -interaction=batchmode plots.tex > null" folder);
   Pbench.system (sprintf "mv %s/plots.pdf %s" folder output_file);
   Pbench.info (sprintf "Produced file %s." output_file)

