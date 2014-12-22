open XBase

exception Missing_point

type arg = 
   | Chart_opt of Chart.t (* redundant with Scatter_plot_opt, on purpose *)
   | Scatter_plot_opt of Scatter_plot.t
   | Charts of Params.t
   | Series of Params.t
   | X of Params.t
   | Group_by of Env.keys (* indicate which input keys may differ for a same point *)
   | Y of (Env.t -> Results.t -> Results.t -> float)
   | Y_label of string
   | X_label of string
   | Input of string  (* filename *)
   | Results of Results.t 
   | Output of string  (* filename *)
   | Formatter of Env.formatter
   | Series_formatter of Env.formatter
   | Charts_formatter of Env.formatter

type t = arg list

let get_scatter_plot_opt args =
     [Scatter_plot.Chart_opt (XOpt.get_default args (function Chart_opt x -> Some x | _ -> None) [])]
   @ (XOpt.projects args (function Scatter_plot_opt x -> Some x | _ -> None))

let get_xaxis_label_opt args = 
  let scatter_args = get_scatter_plot_opt args in
  let axis_args = Scatter_plot.get_xaxis scatter_args in
  Axis.get_label_opt axis_args

let get_charts args = XOpt.get_default args (function Charts x -> Some x | _ -> None) Params.mk_unit
let get_series args = XOpt.get_default args (function Series x -> Some x | _ -> None) Params.mk_unit
let get_x args = XOpt.get_error args (function X x -> Some x | _ -> None) "barplot needs x" 
let get_group_by args = XOpt.get_default args (function Group_by x -> Some x | _ -> None) []
let get_y args = XOpt.get_error args (function Y x -> Some x | _ -> None) "barplot needs y" 
let get_ylabel_opt args = XOpt.get_option args (function Y_label x -> Some x | _ -> None)
let get_xlabel_opt args = XOpt.get_option args (function X_label x -> Some x | _ -> None)
let get_results args = 
   let input = XOpt.get_option args (function Input x -> Some x | _ -> None) in
   let results = XOpt.get_option args (function Results x -> Some x | _ -> None) in
   match results, input with
   | None, None -> Results.from_file "results.txt" 
   | None, Some f -> Results.from_file f
   | Some rs, None -> rs
   | Some _, Some _ -> Pbench.error "Bar_plot was given both Input and Results"
let get_output args = XOpt.get_default args (function Output x -> Some x | _ -> None) "plots.pdf" 
let get_formatter_common args f =
   XOpt.get_default_2 args f (function Formatter x -> Some x | _ -> None) Env.formatter_key_values
let get_series_formatter args = get_formatter_common args (function Series_formatter x -> Some x | _ -> None)
let get_charts_formatter args = get_formatter_common args (function Charts_formatter x -> Some x | _ -> None)

(** Smart constructor for building the Y evaluation function and label from a single key *)

let y_as_mean k = 
   let eval env all_results results =
      let vs = Results.get_float k results in
      XFloat.mean_of vs
      in
   [ Y_label k; Y eval ]

(** Builder *)

let call' args results_global =
   let group_by = get_group_by args in
   let build_x env x_key results_serie x_value =
      let env_x = Env.add Env.empty x_key x_value in
      let env = Env.append env env_x in
      let results_x = Results.filter env_x results_serie in
      Results.check_consistent_inputs group_by results_x;
      if results_x = [] then begin
         Pbench.warning ("no point to plot for: " ^ Env.to_string env);
         []
      end else begin
         let eval = get_y args in
         let x_value = 
            try Env.as_float x_value 
            with Env.Cast_error s -> Pbench.error ("Scatter plot x-axis key must be convertible to float values") in
         try 
            let y_value = eval env results_global results_x in
            [(x_value, y_value)]
         with Results.Missing_data ->
            Pbench.warning ("Missing data for: \n --" ^ Env.to_string env);
            [] 
      end
      in
   let build_serie env x_key x_values results_charts env_serie =
      let title_serie = (get_series_formatter args) env_serie in
      let results_serie = Results.filter env_serie results_charts in
      let env = Env.append env env_serie in
      let points = ~~ XList.concat_map x_values (build_x env x_key results_serie) in
      Scatter_plot.({
        serie_title = title_serie; 
        serie_points = points })
      in
   let build_chart env results_global env_charts =
      let title_chart = (get_charts_formatter args) env_charts in
      let results_charts = Results.filter env_charts results_global in
      let env = Env.append env env_charts in
      let envs_series = (get_series args) env in
      let envs_x = (get_x args) env in
      let (x_key, x_values) =
         if envs_x = [] then Pbench.error ("empty params for field x, in plot");
         let (keys, values) = List.split (~~ List.map envs_x (fun env_x ->
            match Env.to_assoc env_x with 
            | [(k,v)] -> (k,v)
            | _ -> Pbench.error "Mk_scatter_plot needs x-axis params to have a unique key"))
            in
         if not (XList.same_items keys)
            then Pbench.error "Mk_scatter_plot needs x-axis params to have a unique key";
         (List.hd keys, values)
         in
      let series = ~~ List.map envs_series (build_serie env x_key x_values results_charts) in
      let xlabel =
        match get_xaxis_label_opt args, get_xlabel_opt args with 
        | None, None -> x_key
        | Some s, None | None, Some s -> s
        | Some s1, Some s2 -> Pbench.warning "multiple settings for xlabel"; s2
        in
      Scatter_plot.([
         Chart_opt [ Chart.Title title_chart ];
         X_axis [ Axis.Label xlabel ];
         Series series ] 
      @ (match get_ylabel_opt args with None -> [] | Some label -> [Y_axis [Axis.Label label]])
      @ (get_scatter_plot_opt args))
      in
   let env_global = Env.empty in
   let chart_envs = (get_charts args) env_global in
   let by_charts = ~~ List.map chart_envs (build_chart env_global results_global) in
   let charts = XList.build (fun add_chart -> 
      ~~ List.iter by_charts (fun scatter_plot ->
         try 
            let rscript = Scatter_plot.call scatter_plot in
            let chart_opt = Scatter_plot.get_chart_opt scatter_plot in
            add_chart ([Chart.Rscript rscript] @ chart_opt) 
         with Chart.Cannot_build s ->
            Pbench.warning (sprintf "unable to build chart for %s: %s\n" (Chart.get_title (Scatter_plot.get_chart_opt scatter_plot)) s) 
      )) in
   Chart.build (get_output args) charts

let call args = call' args (get_results args)
