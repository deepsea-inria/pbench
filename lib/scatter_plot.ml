open XBase

type points = (float * float) list

type serie = {
   serie_title : string;
   serie_points : points; }

type arg = 
   | Chart_opt of Chart.t
   | X_axis of Axis.t
   | Y_axis of Axis.t
   | Series of serie list
   | Draw_lines of bool
   | Extra of string list

type t = arg list

let get_chart_opt args = XOpt.projects args (function Chart_opt x -> Some x | _ -> None) 
let get_xaxis args = XOpt.projects args (function X_axis x -> Some x | _ -> None)
let get_yaxis args = XOpt.projects args (function Y_axis x -> Some x | _ -> None)
let get_series args = XOpt.get_error args (function Series x -> Some x | _ -> None) "scatter needs series" 
let get_draw_lines args = XOpt.get_default args (function Draw_lines x -> Some x | _ -> None) true
let get_extra args = XOpt.get_default args (function Extra x -> Some x | _ -> None) []

(** Auxiliary function used for the next function *)

   (* see: http://www.statmethods.net/advgraphs/axes.html for implementing more features *)
   (* trick for bars: arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3) *)

let to_rscript_all_but_series batches scatter =
   let xaxis = get_xaxis scatter in
   let yaxis = get_yaxis scatter in
   let islogx = Axis.get_is_log xaxis in
   let islogy = Axis.get_is_log yaxis in
   let logoption = 
      if islogx && islogy then ", log='xy'" 
      else if islogx then ", log='x'"
      else if islogy then ", log='y'"
      else "" in
   let define_data i points =
      (*if points = [] then [ sprintf "%s <- matrix()" (Rtool.batch_name i) ] else *)
      (* let points = sort_points_x points in *)
      let xs,ys = List.split points in
      let list_of_values islogscale vs =
         String.concat "," (List.map (Rtool.of_float ~islogscale:islogscale) vs) in
      [ sprintf "%s <- matrix(c(%s,%s),ncol=2)" (Rtool.batch_name i) (list_of_values islogx xs) (list_of_values islogy ys) ] in
   let bound optvalue islog =
      match optvalue with
      | Some 0. when islog -> "1."
      | Some v -> sprintf "%f" v
      | None -> "NA" 
      in
   let corner1 = 
      sprintf "corner1 <- matrix(c(%s,%s),ncol=2)"
         (bound (Axis.get_lower xaxis) islogx) 
         (bound (Axis.get_lower yaxis) islogy) in
   let corner2 = 
      sprintf "corner2 <- matrix(c(%s,%s),ncol=2)"
         (bound (Axis.get_upper xaxis) islogx) 
         (bound (Axis.get_upper yaxis) islogy) in
   let xaxis_pos_option =
      if Axis.get_lower xaxis <> None then ", xaxs='i'" else "" in
   let yaxis_pos_option = 
      if Axis.get_lower yaxis <> None then ", yaxs='i'" else "" in
   let plot =
      sprintf "plot(rbind(%s), type='n', xlab='%s', ylab='%s'%s%s%s%s%s)" 
        (String.concat "," (XList.mapi (fun i _ -> Rtool.batch_name i) batches) ^ ",corner1,corner2")
        (Axis.get_label xaxis)
        (Axis.get_label yaxis)
        "" (* add support for xlim=c(a,b) here *)
        "" (* add support for ylim=c(a,b) here *)
        logoption
        xaxis_pos_option
        yaxis_pos_option
        in
   List.concat (XList.mapi define_data batches)
   @ [ corner1;
       corner2;
       plot ]

(** Returns the R script associated with a scatter chart *)

let call args =
   let series = get_series args in
   let chart_opt = get_chart_opt args in
   let legend_opt = Chart.get_legend_opt chart_opt in
   let legend_pos = Legend.get_legend_pos legend_opt in
   let title = Chart.get_title chart_opt in
   let series =
      let nb_series = List.length series in
      if nb_series = 0 then raise (Chart.Cannot_build "no series")
      else if nb_series > 16 then begin
         Pbench.warning (sprintf "cannot plot more than 16 series on one graph; trimming data for %s\n" title);
         XList.take 16 series
      end else series in
   let series = List.filter (fun serie -> serie.serie_points <> []) series in
   let nb_series = List.length series in
   let legends = List.map (fun serie -> serie.serie_title) series in
   let batches = List.map (fun serie -> serie.serie_points) series in
   let legend = 
      if legends = [""] || legend_pos = Legend.Nowhere then "" else
         sprintf "legend('%s', c(%s), col=cols, pch=pchs)" 
         (Rtool.string_of_legend_pos legend_pos)
         (String.concat "," (List.map (fun s -> sprintf "'%s'" s) legends)) in
   let draw_batch_mode mode i points =
      (*if points = [] then [] else*)
      [ sprintf "points(%s,col=cols[%d],pch=pchs[%d],type='%s')" (Rtool.batch_name i) (i+1) (i+1) mode ] in
   let draw_batch i points =
      if (get_draw_lines args)
         then draw_batch_mode "l" i points @ draw_batch_mode "p" i points
         else draw_batch_mode "p" i points
      in
   let core = to_rscript_all_but_series batches args in
   let series = List.concat (XList.mapi draw_batch batches) in
   let out_title = [ Rtool.chart_title title ] in
   let style = [ Rtool.colors_define Rtool.colors_default nb_series; Rtool.pchs_define Rtool.pchs_default; ] in
   String.concat "\n" (style @ core @ [ legend ] @ series @ out_title @ get_extra args )
