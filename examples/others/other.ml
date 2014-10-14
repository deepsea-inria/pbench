open Printf
open XBase
open Env
open Params


(* running the demo:
   make demo.byte && ./demo.byte -test first && evince barplots.pdf && evince plots.pdf && evince table.pdf &

   arthur:
   make demo.byte && ./demo.byte -test first && evince barplots.pdf
*)

(*-------------------------------------------------------------------*)
(* first demo *)

let first() = 
   let n = "n" in
   (* let m = "m" in *)
   let s = "s" in
   let t = "t" in
   (* let p = "p" in *)
   let mk_programs = mk_progs ["test.sh"; "test2.sh"] in
   (*  mk_programs = fun e -> [[("prog", Pstring "test.sh")]; [("prog", Pstring "test2.sh")]]) *)
   let mk_series = 
         (mk string s "a") 
      ++ ((mk string s "b") & (mk int t 0)) 
      in
   (*  mk_series = fun e -> [[(s, Pstring "a")]; [(s, Pstring "b"); (t, Vint 0)]]) *)
   let mk_x = mk_list int n [5; 10] in
   Mk_runs.(call [
      Output "results.txt";
      Virtual false;
      Runs 2;
      Args (
       (
            mk_prog "test.sh"
          & mk_series
          & mk_x
          (* LATER:
          & mk_eval int m (fun e -> 100 / (get val_int e n))
          & mk_eval_list int p (fun e -> List.map (fun x -> x * get as_int e m) [2;3]) *)
       )
       ++
       (
            mk_prog "test2.sh"
          & mk_series
          & mk_x
          (* LATER:
          & mk_list float n [5.; 10.]
          & mk_eval float m (fun e -> 1000. /. (get as_float e n)) *)
       )
      )]);
   let results_all = Results.from_file "results.txt" in

   
   begin
      Results.check_consistent_outputs_filter_by_params ["bla"] mk_programs results_all
   end;

   let charts1 = Mk_bar_plot.(get_charts ([
      Bar_plot_opt Bar_plot.([
         X_titles_dir Vertical; 
         Y_axis [Axis.Lower (Some 0.)] ]);
      Charts mk_programs;
      Series mk_series;
      X mk_x;
      Input "results.txt";
      ] 
      @ (y_as_mean "exectime")
      )) in
   let charts2 = Mk_bar_plot.(get_charts ([
      Bar_plot_opt Bar_plot.([
         X_titles_dir Vertical; 
         Y_axis [Axis.Lower (Some 0.)] ]);
      Series (mk_series);
      X (mk_programs & mk_x);
      Input "results.txt";
      ] 
      @ (y_as_mean "exectime")
      )) in
   let charts3 = Mk_bar_plot.(get_charts ([
      Bar_plot_opt Bar_plot.([
         X_titles_dir Vertical; 
         Y_axis [Axis.Lower (Some 0.)] ]);
      Series (mk_programs & mk_series);
      X (mk_x);
      Input "results.txt";
      ] 
      @ (y_as_mean "exectime")
      )) in
   Chart.build "barplots.pdf" (charts1 @ charts2 @ charts3);

   Mk_scatter_plot.(call ([
      Scatter_plot_opt Scatter_plot.([
         Draw_lines true; 
         X_axis [Axis.Label "demo_xaxis"];
         Y_axis [Axis.Lower (Some 0.)] ]);
      Charts mk_programs;
      Series mk_series;
      X mk_x;
      Input "results.txt";
      Output "plots.pdf";
      ] 
      @ (y_as_mean "exectime")
      ));
    begin (* draw table *)
       let s = Buffer.create 1 in
       let add x = Buffer.add_string s x in
       let env = Env.empty in
       let results = results_all in
       let envs_prog = mk_programs env in
       let envs_serie = mk_series env in
       let nb_series = List.length envs_serie in
       add (sprintf "\\begin{tabular}{|l|%s} \\hline \n" (String.concat "" (XList.init nb_series (fun i -> "c|"))));
       ~~ List.iter envs_serie (fun env_serie ->
          add " & ";
          add (Env.to_string env_serie);
       );
       add " \\\\ \\hline \n";
       ~~ List.iter envs_prog (fun env_prog ->
         let results = Results.filter env_prog results in
         let env = Env.append env env_prog in
         add (Env.to_string env_prog);
         ~~ List.iter envs_serie (fun env_serie ->
            add " & ";
            let results = Results.filter env_serie results in
            let _env = Env.append env env_serie in
            let vs = Results.get_float "exectime" results in
            let v = XFloat.mean_of vs in
            let s = match classify_float v with
               | FP_normal | FP_subnormal | FP_zero -> sprintf "%.2f" v
               | FP_infinite | FP_nan -> "NA"
               in
            add s;
            );
         add " \\\\\ \\hline \n";
         );
       add "\\end{tabular}\n";
       Latex.build "table.pdf" (Latex.basic_document (Buffer.contents s))
    end

(*-------------------------------------------------------------------*)

let _ =
   let test = XCmd.parse_or_default_string "test" "first" in
   match test with
   | "first" -> first()
   | _ -> failwith "unknown test"

