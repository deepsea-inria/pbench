open XBase

let arg_width = XCmd.parse_or_default_float "width" 6.0
let arg_height = XCmd.parse_or_default_float "width" 6.0
let arg_dimensions = (arg_width, arg_height)
let arg_legend_pos =
  let s = XCmd.parse_or_default_string "legendpos" "topright" in
  Legend.legend_pos_of_string s
let arg_title = XCmd.parse_or_default_string "title" "" 
let arg_group_by = XCmd.parse_or_default_list_string "group_by" []
let arg_x = XCmd.parse_string "x" 
let arg_y = XCmd.parse_string "y" 
let arg_series = XCmd.parse_or_default_list_string "series" []
let arg_chart = XCmd.parse_or_default_list_string "chart" []
let arg_input = XCmd.parse_or_default_string "input" "results.txt"
let arg_output = XCmd.parse_or_default_string "input" "plots.pdf"
let arg_drawline = XCmd.parse_or_default_bool "drawline" true

let make_arg_axis smin smax szero slog slabel =
  List.concat [
  ~~ List.map (XOption.to_list (XCmd.parse_optional_string slabel)) (fun s -> Axis.Label s);
  ~~ List.map (XOption.to_list (XCmd.parse_optional_bool slog)) (fun b -> Axis.Is_log b);
  ~~ List.map (XOption.to_list (XCmd.parse_optional_float smin)) (fun v -> Axis.Lower (Some v));
  ~~ List.map (XOption.to_list (XCmd.parse_optional_float smax)) (fun v -> Axis.Upper (Some v));
  ~~ List.map (XOption.to_list (XCmd.parse_optional_bool szero)) (fun b -> 
    if not b then Pbench.error "should not be using -xzero=false, but only --xzero or nothing";
    Axis.Lower (Some 0.));
  ]
let arg_xaxis = make_arg_axis "xmin" "xmax" "xzero" "xlog" "xlabel"
let arg_yaxis = make_arg_axis "ymin" "ymax" "yzero" "ylog" "ylabel"

let ylabel_def =
  match XCmd.parse_optional_string "ylabel" with
  | Some _ -> []
  | None -> Mk_scatter_plot.([ Y_label arg_y ])

let all_results = 
  Results.from_file arg_input

let mk_x =
  let vs = Results.get_distinct_values_for arg_x all_results in
  Params.mk_list Params.value arg_x vs

let mk_charts =
  let es = Results.get_distinct_values_for_several arg_chart all_results in
  Params.from_envs es

let mk_series =
  let es = Results.get_distinct_values_for_several arg_series all_results in
  Params.from_envs es

let eval_y env all_results results =
  Results.get_mean_of arg_y results

let plot_scatter () =
  Mk_scatter_plot.(call ([
    Chart_opt Chart.([
      Legend_opt Legend.([Legend_pos arg_legend_pos]);
      Title arg_title;
      ]);
    Scatter_plot_opt Scatter_plot.([
      X_axis arg_xaxis;
      Y_axis arg_yaxis;
      Draw_lines arg_drawline;    
    ]);
    Charts mk_charts;
    Series mk_series;
    X mk_x;
    Group_by arg_group_by;
    Y eval_y;
    Input arg_input;
    Output arg_output;
    ] @ ylabel_def))


let () =
  plot_scatter()

