open XBase

(************************************************************************)
(** Common parameters *)

let arg_width = XCmd.parse_or_default_float "width" 6.0
let arg_height = XCmd.parse_or_default_float "height" 6.0
let arg_dimensions = (arg_width, arg_height)
let arg_title = XCmd.parse_or_default_string "title" "" 
let arg_input = XCmd.parse_or_default_string "input" "results.txt"
let arg_output = XCmd.parse_or_default_string "input" "plots.pdf"


(************************************************************************)
(** Helper functions *)

let make_axis smin smax szero slog slabel =
  List.concat [
  ~~ List.map (XOption.to_list (XCmd.parse_optional_string slabel)) (fun s -> Axis.Label s);
  ~~ List.map (XOption.to_list (XCmd.parse_optional_bool slog)) (fun b -> Axis.Is_log b);
  ~~ List.map (XOption.to_list (XCmd.parse_optional_float smin)) (fun v -> Axis.Lower (Some v));
  ~~ List.map (XOption.to_list (XCmd.parse_optional_float smax)) (fun v -> Axis.Upper (Some v));
  ~~ List.map (XOption.to_list (XCmd.parse_optional_bool szero)) (fun b -> 
    if not b then Pbench.error "should not be using -xzero=false, but only --xzero or nothing";
    Axis.Lower (Some 0.));
  ]

let scatter_and_bar_options () =

  let arg_series = XCmd.parse_or_default_list_string "series" [] in
  let arg_chart = XCmd.parse_or_default_list_string "chart" [] in

  let arg_y = XCmd.parse_string "y" in
  let yaxis = make_axis "ymin" "ymax" "yzero" "ylog" "ylabel" in

  let group_by = XCmd.parse_or_default_list_string "group-by" [] in

  let ylabel_def =
    match XCmd.parse_optional_string "ylabel" with
    | Some _ -> []
    | None -> [arg_y]
     in

  let all_results = Results.from_file arg_input in
  let mk_charts = Params.from_envs (Results.get_distinct_values_for_several arg_chart all_results) in
  let mk_series = Params.from_envs (Results.get_distinct_values_for_several arg_series all_results) in

  let eval_y env all_results results =
    Results.get_mean_of arg_y results
    in

  let arg_legend_pos = Legend.legend_pos_of_string (XCmd.parse_or_default_string "legend-pos" "topright") in

  let chart_opt = Chart.([
      Legend_opt Legend.([Legend_pos arg_legend_pos]);
      Title arg_title;
      ]) in

  (all_results, chart_opt, mk_charts, mk_series, group_by, yaxis, eval_y, ylabel_def)



(************************************************************************)
(** Scatter plot *)

let plot_scatter () =
  let (all_results, chart_opt, mk_charts, mk_series, group_by, yaxis, eval_y, ylabel_def) = 
    scatter_and_bar_options () in

  let arg_x = XCmd.parse_string "x" in
  let xaxis = make_axis "xmin" "xmax" "xzero" "xlog" "xlabel" in

  let mk_x =
    let vs = Results.get_distinct_values_for arg_x all_results in
    Params.mk_list Params.value arg_x vs 
    in

  let arg_drawline = XCmd.parse_or_default_bool "drawline" true in

  Mk_scatter_plot.(call ([
    Chart_opt chart_opt;
    Scatter_plot_opt Scatter_plot.([
      X_axis xaxis;
      Y_axis yaxis;
      Draw_lines arg_drawline;    
    ]);
    Charts mk_charts;
    Series mk_series;
    X mk_x;
    Group_by group_by;
    Y eval_y;
    Input arg_input;
    Output arg_output;
    ] @ (~~ List.map ylabel_def (fun s -> Y_label s)) ))


(************************************************************************)
(** Bar plot *)

let plot_bar () =
  let (all_results, chart_opt, mk_charts, mk_series, group_by, yaxis, eval_y, ylabel_def) = 
    scatter_and_bar_options () in

  let arg_x = XCmd.parse_or_default_list_string "x" [] in
  
  let x_label = 
     XCmd.parse_or_default_string "xlabel" "" in

  let x_label_direction =
     if XCmd.mem_flag "xtitles-vertical" then Bar_plot.Vertical else
     Bar_plot.label_direction_of_string (XCmd.parse_or_default_string "xtitles-dir" "horizontal") 
     in

  let mk_x = Params.from_envs (Results.get_distinct_values_for_several arg_x all_results) in

  Mk_bar_plot.(call ([
    Chart_opt chart_opt;
    Bar_plot_opt Bar_plot.([
      X_label x_label;
      Y_axis yaxis;
      X_titles_dir x_label_direction;
    ]);
    Charts mk_charts;
    Series mk_series;
    X mk_x;
    Group_by group_by;
    Y eval_y;
    Input arg_input;
    Output arg_output;
    ] @ (~~ List.map ylabel_def (fun s -> Y_label s)) ))


(************************************************************************)
(** Table generation *)

let plot_table () =
  let arg_tables = XCmd.parse_or_default_list_string "table" [] in
  let arg_rows = XCmd.parse_or_default_list_string "row" [] in
  let arg_cols = XCmd.parse_or_default_list_string "col" [] in
  let arg_cell = XCmd.parse_string "cell" in
  let group_by = XCmd.parse_or_default_list_string "group-by" [] in
  let all_results = Results.from_file arg_input in

  let mk_tables = Params.from_envs (Results.get_distinct_values_for_several arg_tables all_results) in
  let mk_rows = Params.from_envs (Results.get_distinct_values_for_several arg_rows all_results) in
  let mk_cols = Params.from_envs (Results.get_distinct_values_for_several arg_cols all_results) in

  let eval_cell env all_results results =
    Results.get_mean_of arg_cell results
    in
  let all_results = Results.from_file arg_input in
  let formatter = [] in

  let s = Buffer.create 1 in
  let add x = Buffer.add_string s x in
  
  let results = all_results in
  let env = Env.empty in
  let envs_tables = mk_tables env in
  (* todo: gérer plusieurs tables séparer en remontant la traduction matrice *)
  ~~ List.iter envs_tables (fun env_tables ->
     let table_title = Env.format formatter env_tables in
     let cells = ref [] in
     let results = Results.filter env_tables results in
     let env = Env.append env env_tables in
     let envs_rows = mk_rows env in
     let first_row = ref [] in
     let envs_cols = mk_cols env in
     XBase.add_to_list_ref first_row "";
     ~~ List.iter envs_cols (fun env_cols ->
         let col_title = Env.format formatter env_cols in
         XBase.add_to_list_ref first_row col_title;
       );
     XBase.add_to_list_ref cells (List.rev !first_row); 
     ~~ List.iter envs_rows (fun env_rows ->
       let row = ref [] in
       let results = Results.filter env_rows results in
       let env = Env.append env env_rows in
       let row_title = Env.format formatter env_rows in
       XBase.add_to_list_ref row row_title;
       ~~ List.iter envs_cols (fun env_cols ->
         let env = Env.append env env_cols in
         let results = Results.filter env_cols results in
         Results.check_consistent_inputs group_by results;
         let v = eval_cell env all_results results in
         let c = sprintf "%.5f" v in
         XBase.add_to_list_ref row c;
       );
       XBase.add_to_list_ref cells (List.rev !row)
    );
    let matrix = Mk_table.matrix_of_list_of_lists (List.rev !cells) in
    let latex_table = Mk_table.latex_of_matrix ~escape:true matrix in
    if arg_title <> "" || table_title <> "" then begin
      let title = if arg_title <> "" then arg_title else table_title in
      add (Latex.escape title ^ Latex.new_line ^ Latex.vspace 1.);
    end;
    add latex_table;
  );
  let latex = Buffer.contents s in
  Latex.build arg_output (Latex.basic_document latex)



(************************************************************************)
(** Speedup plot *)

let plot_speedup () =
  let arg_series = XCmd.parse_or_default_list_string "series" [] in
  let arg_chart = XCmd.parse_or_default_list_string "chart" [] in
  let arg_shared = arg_chart @ arg_series in 
  (* Note: the code implicitly assumes that "arg_shared" covers all the arguments that are common to the baseline and the parallel command lines. *)
  let group_by = XCmd.parse_or_default_list_string "group-by" [] in
  let arg_log = XCmd.parse_or_default_bool "log" false in
  let arg_legend_pos = Legend.legend_pos_of_string (XCmd.parse_or_default_string "legend-pos" "topleft") in

  let all_results = Results.from_file arg_input in
  let all_procs = List.map Env.as_int (Results.get_distinct_values_for "proc" all_results) in
  let max_proc = XMath.max_of all_procs in

  let mk_charts = Params.from_envs (Results.get_distinct_values_for_several arg_chart all_results) in
  let mk_series = Params.from_envs (Results.get_distinct_values_for_several arg_series all_results) in
  let mk_x = Params.mk_list Params.int "proc" (~~ List.filter all_procs (fun p -> p <> 0)) in 

  let axis = Axis.([
    Is_log arg_log;
    Lower (Some (if arg_log then 1. else 0.));
    Upper (Some (float_of_int max_proc)); ]) in

  let eval_y env all_results results =
    let results = ~~ Results.filter_by_params results Params.(mk string "prun_speedup" "parallel") in
    let baseline_results = ~~ Results.filter_by_params all_results Params.(mk string "prun_speedup" "baseline") in
    let baseline_env = ~~ Env.filter env (fun k -> List.mem k arg_shared) in
    let baseline_results = ~~ Results.filter baseline_results baseline_env in
    (* alternative: 
    let baseline_env = Env.add baseline_env "prun_speedup" (Env.Vstring "baseline") in
    let baseline_results = ~~ Results.filter all_results baseline_env in
    *)
    if baseline_results = [] then Pbench.warning ("no results for baseline: " ^ Env.to_string env);
    let tp = Results.get_mean_of "exectime" results in
    let tb = Results.get_mean_of "exectime" baseline_results in
    tb /. tp
    in

  Mk_scatter_plot.(call ([
    Chart_opt Chart.([
      Legend_opt Legend.([Legend_pos arg_legend_pos]);
      Title arg_title;
      ]);
    Scatter_plot_opt Scatter_plot.([
      X_axis axis;
      Y_axis axis;
      Draw_lines true;    
      Extra ["abline(a=0, b=1, col='gray')"];
    ]);
    Charts mk_charts;
    Series mk_series;
    X mk_x;
    Group_by group_by;
    Y eval_y;
    Input arg_input;
    Output arg_output;
    Y_label "speedup";
    (*X_label "processors";*)
    ]))



(************************************************************************)
(** Main *)

let () =
  let arg_type =
    match XCmd.parse_optional_string "mode", XCmd.get_others() with
    | None, [] -> "bar"
    | Some t, [] -> t
    | None, [t] -> t
    | _, _::_::_ -> Pbench.error "only one non-named argument is allowed; it should be the mode"
    | Some _, _::_ -> Pbench.error "multiple modes specified for the chart"
    in
  let plot_fct = match arg_type with
    | "bar" -> plot_bar
    | "scatter" -> plot_scatter
    | "table" -> plot_table
    | "speedup" -> plot_speedup
    | _ -> Pbench.error "unsupported type of graph"
    in
  plot_fct()


