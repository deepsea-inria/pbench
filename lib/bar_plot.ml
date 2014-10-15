open XBase

(** Representation of the series;
    There should be one height bar for each x-axis title
    (i.e. length series_height = length (get_xtitles args))
    and note that a None value indicates not available data. *)

type series = {
   mutable serie_title : string;
   mutable serie_heights : ((float * float option) option) list; 
   }
   
type label_direction = Horizontal | Vertical

type arg = 
   | Chart_opt of Chart.t
   | X_label of string
   | X_titles of string list
   | X_titles_dir of label_direction
   | Y_axis of Axis.t
   | Series of series list
   | Error_bars of bool (* todo: generalize *)

type t = arg list

let get_chart_opt args = XOpt.projects args (function Chart_opt x -> Some x | _ -> None) 
let get_xlabel args = XOpt.get_default args (function X_label x -> Some x | _ -> None) "" 
let get_xtitles args = XOpt.get_error args (function X_titles x -> Some x | _ -> None) "barplot needs xtitles" 
let get_xtitles_dir args = XOpt.get_default args (function X_titles_dir x -> Some x | _ -> None) Horizontal
let get_yaxis args = XOpt.projects args (function Y_axis x -> Some x | _ -> None)
let get_series args = XOpt.get_error args (function Series x -> Some x | _ -> None) "barplot needs series" 
let get_error_bars args = XOpt.get_default args (function Error_bars x -> Some x | _ -> None) false

let out_error_bar_fct = 
  "error.bar <- function(x, y, upper, lower=upper, length=0.1,...){ \n \
   if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper)) \n \
   stop(\"vectors must be same length\") \n \
   arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...) \n \
   } \n "

(** Returns the R script associated with a bar chart *)

let call args = 
   let error_bars = get_error_bars args in
   let series = get_series args in
   let chart = get_chart_opt args in
   let title = Chart.get_title chart in
   let legend_opt = Chart.get_legend_opt chart in
   let legend_pos = Legend.get_legend_pos legend_opt in
   let yaxis = get_yaxis args in
   let islogy = Axis.get_is_log yaxis in
   let logoption = if islogy then ", log='y'" else "" in
   let nb_series = List.length series in
   if nb_series = 0 then Pbench.error "Bar_plot.call needs nb_series > 0";
   let max_height = ref (-1.e20) in (* todo: use min float *)
   let find_value v = 
      if v > !max_height then max_height := v;
      in
   let series_rvalues_and_whiskers = List.concat (~~ List.map series (fun serie -> 
      ~~ List.map serie.serie_heights (fun optval ->
         match optval with
         | None -> (nan,nan)
         | Some (v,e_opt) -> 
	    let e = match e_opt with
		None -> 0.0
	      | Some e -> e in
	    find_value v; (v,e)))) in
   let (series_rvalues, series_whiskers) = List.split series_rvalues_and_whiskers in
   let out_data =
      sprintf "data0 <- matrix(c(%s),ncol=%d)\n" 
        (Rtool.of_list (List.map Rtool.of_float series_rvalues)) 
        nb_series
      ^ (if error_bars then
          sprintf "data0err <- matrix(c(%s),ncol=%d)\n" 
            (Rtool.of_list (List.map Rtool.of_float series_whiskers))
            nb_series
         else "")
   in
   let serie_titles = ~~ List.map series (fun serie -> serie.serie_title) in
   let out_serie_titles = sprintf "colnames(data0) <- c(%s)\n" (Rtool.of_string_list serie_titles) in
   let out_x_titles = sprintf "rownames(data0) <- c(%s)\n" (Rtool.of_string_list (get_xtitles args)) in
   let out_data_table = sprintf "table0 <- t(as.table(data0))\n" in 
   let out_ylim =
      let ymax = 
         match Axis.get_upper yaxis with
         | None -> if islogy then 10. *. !max_height else 1.5 *. !max_height
         | Some v -> v
         in
      let ymin = 
         match Axis.get_lower yaxis with
         | None -> 0.
         | Some v -> v
         in
      let ymin =
         if ymin = 0. && islogy then 1. else ymin in
      sprintf "ylim=range(%s,%s)" (Rtool.of_float ymin) (Rtool.of_float ymax) 
      in
   let (slegend_in_plot, slegend_out_plot) =
      match legend_pos with
      | Legend.Nowhere -> "", ""
      | Legend.Outside_below -> "", "plot.new() \n legend('center', rownames(table0), col=cols, pch=15) \n"
      | _ -> sprintf " legend=rownames(table0), args.legend = list(x = '%s')," (Rtool.string_of_legend_pos legend_pos), ""
      in
   let out_plot = 
      sprintf "bp <- barplot(table0, beside=TRUE, space=c(0,2), las=%d, col=cols, names.arg=colnames(table0), xlab='%s', ylab='%s',%s %s %s)"
       (if get_xtitles_dir args = Horizontal then 1 else 2)
       (get_xlabel args)
       (Axis.get_label yaxis)
       slegend_in_plot
       out_ylim
       logoption
   in
   let out_whiskers = if error_bars then sprintf "error.bar(t(bp),data0,data0err)" else "" in
   let out_title = Rtool.chart_title title in
   String.concat "\n" [
     out_error_bar_fct;
     Rtool.colors_define Rtool.colors_default nb_series;
     out_data; 
     out_serie_titles;
     out_x_titles;
     out_data_table;
     "par(mar=c(12, 4, 4, 2) + 0.4)";
     out_plot;
     out_whiskers;
     out_title;
     slegend_out_plot
     ]

     

   (* Later: horizontal line:  "abline(1.,0,lty=2,col='black')" *)
   (* Later: let out_text_labels = sprintf "text(bp, 0, round(table0, 1), cex=1, pos=3)" in*)
   (*  Later: "cols <- as.vector(c(\"white\", \"red\",\"blue\",\"yellow\"))"; *) (* Rtool.colors 16; *)
   (* Remark: space argument to barplot = # Amount of space between i) bars within a group, ii) bars between groups   *)
   (* Remark: in barplot, use `las=2` to render series labels vertically *)
   (* Remark: in barplot, use   font.lab=2  border="black"*)




(************************************************************************)
(** Helper functions *)

let label_direction_of_string s = 
  match s with
  | "horizontal" -> Horizontal 
  | "vertical" -> Vertical
  | _ -> Pbench.error "invalid name for label direction (should be 'horizontal' or 'vertical')"
  
