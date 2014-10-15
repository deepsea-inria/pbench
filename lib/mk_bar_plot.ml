open XBase
let warning = Pbench.warning


type arg = 
   | Chart_opt of Chart.t (* redundant with Bar_plot_opt, on purpose *)
   | Bar_plot_opt of Bar_plot.t
   | Charts of Params.t
   | Series of Params.t
   | X of Params.t
   | Group_by of Env.keys  (* indicate which input keys may differ for a same point *)
   | Y of (Env.t -> Results.t -> Results.t -> float)
   | Y_whiskers of (Env.t -> Results.t -> Results.t -> float)
   | X_label of string
   | Y_label of string
   | Input of string  (* filename *)
   | Results of Results.t 
   | Output of string  (* filename *)
   | Formatter of Env.formatter
   | X_formatter of Env.formatter
   | Series_formatter of Env.formatter
   | Charts_formatter of Env.formatter

type t = arg list

let get_bar_plot_opt args =
     [Bar_plot.Chart_opt (XOpt.get_default args (function Chart_opt x -> Some x | _ -> None) [])]
   @ (XOpt.projects args (function Bar_plot_opt x -> Some x | _ -> None))

let get_charts args = XOpt.get_default args (function Charts x -> Some x | _ -> None) Params.mk_unit
let get_series args = XOpt.get_default args (function Series x -> Some x | _ -> None) Params.mk_unit
let get_x args = XOpt.get_error args (function X x -> Some x | _ -> None) "barplot needs x" 
let get_group_by args = XOpt.get_default args (function Group_by x -> Some x | _ -> None) []
let get_y args = XOpt.get_error args (function Y x -> Some x | _ -> None) "barplot needs y" 
let get_y_whiskers args = XOpt.get_option args (function Y_whiskers x -> Some x | _ -> None)
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
let get_x_formatter args = get_formatter_common args (function X_formatter x -> Some x | _ -> None)
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

exception Missing_chart of string

let get_charts args =
   let results_global = get_results args in
   let group_by = get_group_by args in
   let eval_whiskers_opt = get_y_whiskers args in
   let build_x env results_serie env_x =
      let env = Env.append env env_x in 
      let results_x = Results.filter env_x results_serie in
      Results.check_consistent_inputs group_by results_x;
      if results_x = [] then begin
         warning ("No results for x: \n --" ^ Env.to_string env); 
         None
      end else begin
         try 
            let eval = get_y args in
            let y_value = eval env results_global results_x in
	    let y_whisker_value = (match eval_whiskers_opt 
				   with None -> None
				      | Some f -> Some (f env results_global results_x)) in
            Some (y_value, y_whisker_value)
         with Results.Missing_data ->
            warning ("Missing data for: \n --" ^ Env.to_string env);
            None 
      end
      in
   let build_serie env envs_x results_charts env_serie =
      let title_serie = (get_series_formatter args) env_serie in
      let results_serie = Results.filter env_serie results_charts in
      let env = Env.append env env_serie in
      let heights =
         if results_serie = [] then begin
            warning ("No results for serie: \n --" ^ Env.to_string env);
            ~~ List.map envs_x (fun _ -> None) 
         end else begin
            ~~ List.map envs_x (build_x env results_serie) 
         end in
      Bar_plot.({
        serie_title = title_serie; 
        serie_heights = heights })
      in
   let build_chart env results_global env_charts =
      let title_chart = (get_charts_formatter args) env_charts in
      let results_charts = Results.filter env_charts results_global in
      let env = Env.append env env_charts in
      let envs_series = (get_series args) env in
      if envs_series = [] then raise (Missing_chart ("Mk_bar_plot: no series for chart " ^ Env.to_string env));
      let envs_x = (get_x args) env in
      if envs_x = [] then raise (Missing_chart ("Mk_bar_plot: no x-axis for chart " ^ Env.to_string env));
      let x_titles = ~~ List.map envs_x (fun env_x -> (get_x_formatter args) env_x) in
      let x_label = XOption.unsome_or "" (get_xlabel_opt args) in
      (* -- optimization of display for regular cases 
      let x_titles = ~~ List.map envs_x (fun env_x -> (get_x_formatter args) env_x) in
      let (x_titles, sxlabel) = 
         let keys = ~~ List.map envs_x (fun env_x -> 
            ~~ Env.map env_x (fun k v -> k)) in
         if not (XList.same_items keys) then (x_titles, "") else begin
            let envs_x_txt = ~~ List.map envs_x (fun env_x -> 
               ~~ Env.map env_x (fun k v -> Env.string_of_value v)) in 
            let x_titles = ~~ List.map envs_x_txt (fun vals -> String.concat "," vals) in 
            let sxlabel = String.concat "," (List.hd keys) in
            (x_titles, sxlabel)
         end
         in
      *)
      if results_charts = [] then 
         warning ("No results for chart: \n --" ^ Env.to_string env);
      let series = ~~ List.map envs_series (build_serie env envs_x results_charts) in
      Bar_plot.([
         Chart_opt [ Chart.Title title_chart ];
         X_titles x_titles;
         X_label x_label;
         Series series;
         Error_bars (eval_whiskers_opt <> None);
         ] 
      @ (match get_ylabel_opt args with None -> [] | Some label -> [Y_axis [Axis.Label label]])
      @ (get_bar_plot_opt args))
      in
   let env_global = Env.empty in
   if results_global = [] then begin
      warning ("No results for plot");
      [] 
   end else begin
      let chart_envs = (get_charts args) env_global in
      let by_charts = ~~ XList.concat_map chart_envs (fun env_chart ->
         try [build_chart env_global results_global env_chart]
         with Missing_chart s -> Pbench.warning s; []) in
      XList.build (fun add_chart -> 
         ~~ List.iter by_charts (fun bar_plot ->
            try 
               let rscript = Bar_plot.call bar_plot in
               let chart_opt = Bar_plot.get_chart_opt bar_plot in
               add_chart ([Chart.Rscript rscript] @ chart_opt) 
            with Chart.Cannot_build s ->
               Pbench.warning (sprintf "unable to build chart for %s: %s\n" (Chart.get_title (Bar_plot.get_chart_opt bar_plot)) s) 
         )) 
   end

let call args =
   let charts = get_charts args in
   Chart.build (get_output args) charts

