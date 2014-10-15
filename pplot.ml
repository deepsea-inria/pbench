open XBase

let arg_width = XCmd.parse_or_default_float "width" 6.0
let arg_height = XCmd.parse_or_default_float "width" 6.0
let arg_dimensions = (arg_width, arg_height)
let arg_legend_pos =
  let s = XCmd.parse_or_default_string "legendpos" "topright" in
  Legend.legend_pos_of_string s
let arg_title = XCmd.parse_or_default_string "title" "" 
let arg_input = XCmd.parse_or_default_string "input" "results.txt"
let arg_output = XCmd.parse_or_default_string "input" "plots.pdf"

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

  let group_by_y = XCmd.parse_or_default_list_string "group_by" [] in

  let ylabel_def =
    match XCmd.parse_optional_string "ylabel" with
    | Some _ -> []
    | None -> [arg_y]
     in

  let all_results = 
    Results.from_file arg_input 
    in

  let mk_charts =
    let es = Results.get_distinct_values_for_several arg_chart all_results in
    Params.from_envs es  
    in

  let mk_series =
    let es = Results.get_distinct_values_for_several arg_series all_results in
    Params.from_envs es
    in

  let eval_y env all_results results =
    Results.get_mean_of arg_y results
    in

  let chart_opt = Chart.([
      Legend_opt Legend.([Legend_pos arg_legend_pos]);
      Title arg_title;
      ]) in

  (all_results, chart_opt, mk_charts, mk_series, group_by_y, yaxis, eval_y, ylabel_def)



let plot_scatter () =
  let (all_results, chart_opt, mk_charts, mk_series, group_by_y, yaxis, eval_y, ylabel_def) = 
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
    Group_by group_by_y;
    Y eval_y;
    Input arg_input;
    Output arg_output;
    ] @ (~~ List.map ylabel_def (fun s -> Y_label s)) ))


let plot_bar () =
  let (all_results, chart_opt, mk_charts, mk_series, group_by_y, yaxis, eval_y, ylabel_def) = 
    scatter_and_bar_options () in

  let arg_x = XCmd.parse_or_default_list_string "x" [] in
  
  let x_label = 
     XCmd.parse_or_default_string "xlabel" "" in

  let x_label_direction =
     Bar_plot.label_direction_of_string (XCmd.parse_or_default_string "x_titles_dir" "horizontal") in

  let mk_x =
    let es = Results.get_distinct_values_for_several arg_x all_results in
    Params.from_envs es
    in

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
    Group_by group_by_y;
    Y eval_y;
    Input arg_input;
    Output arg_output;
    ] @ (~~ List.map ylabel_def (fun s -> Y_label s)) ))



let () =
  let arg_type =
    match XCmd.parse_optional_string "type", XCmd.get_others() with
    | None, [] -> "bar"
    | Some t, [] -> t
    | None, [t] -> t
    | _, _::_::_ -> Pbench.error "only one non-named argument is allowed; it should be the type"
    | Some _, _::_ -> Pbench.error "multiple types specified for the chart"
    in
  let plot_fct = match arg_type with
    | "bar" -> plot_bar
    | "scatter" -> plot_scatter
    | _ -> Pbench.error "unsupported type of graph"
    in
  plot_fct()

