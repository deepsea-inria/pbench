open Printf
open XBase
open Env
open Params


(* running the demo:
   make
   ./other.pbench -test first
   evince barplots.pdf &
   evince plots.pdf &
   evince table.pdf &

   ./other.pbench -test second
   evince compare.pdf &

   ./other.pbench -test third
    evince improvement.pdf &
    rm results_mainlines.txt
   ./other.pbench -test third --skip_baseline
    evince improvement.pdf &

*)


(*-------------------------------------------------------------------*)
(* First demo: compare two programs, 
   with argument "-n v" for some value v
   and argument "-s a" or "-s b -t 0" 
*)

let first() = 
   let n = "n" in
   (* let m = "m" in *)
   let s = "s" in
   let t = "t" in
   (* let p = "p" in *)
   let mk_programs = mk_progs ["test.sh"; "test2.sh"] in
   (*  same as:
       mk_programs = fun e -> [[("prog", Pstring "test.sh")]; [("prog", Pstring "test2.sh")]]) *)
   let mk_series = 
         (mk string s "a") 
      ++ ((mk string s "b") & (mk int t 0)) 
      in
   (*  same as:
       mk_series = fun e -> [[(s, Pstring "a")]; [(s, Pstring "b"); (t, Vint 0)]]) *)
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
       )
       ++
       (
            mk_prog "test2.sh"
          & mk_series
          & mk_x
       )
      )]);
   let results_all = Results.from_file "results.txt" in

          (* LATER:
            & mk_eval int m (fun e -> 100 / (get val_int e n))
               & mk_eval_list int p (fun e -> List.map (fun x -> x * get as_int e m) [2;3]) *)
          (* LATER:
            & mk_list float n [5.; 10.]
            & mk_eval float m (fun e -> 1000. /. (get as_float e n)) *)
 
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
(* Second demo: compare two binaries, each with its own parameters
   on a range of arguments
*)

let second() = 
   let mk_x = mk_list int "n" [5; 10] in
   (*  same as:
       mk_programs = fun e -> [[("prog", Pstring "test.sh")]; [("prog", Pstring "test2.sh")]]) *)
   let mk_series = 
         ((mk_prog "test.sh") & (mk string "s" "a")) 
      ++ ((mk_prog "test2.sh") & (mk string "s" "b") & (mk int "t" 0)) 
      in
   (*  same as:
       mk_series = fun e -> [[(s, Pstring "a")]; [(s, Pstring "b"); (t, Vint 0)]]) *)
   Mk_runs.(call [
      Output "second.txt";
      Args (mk_series & mk_x)
      ]);
   
   begin
      let results_all = Results.from_file "second.txt" in
      Results.check_consistent_outputs_filter_by_params ["bla"] mk_series results_all
   end;
  
   let eval_mean k = (fun env all_results results -> Results.get_mean_of k results) in

   let formatter = (* used to beautify the name of the series *)
     Env.format (Env.(
       [ ("prog", Format_custom (fun s -> sprintf "%s" s));
         ("t", Format_custom (fun s -> sprintf "t = %s" s));
         (* ("prog", Format_custom (fun s -> 
            if s = "test.sh" then "A" 
            else if s = "test2.sh" then "B" else sprintf "%s" s)); *)
         ("t", Format_hidden);
         ("s", Format_custom (fun x -> sprintf "s is %s" (
            match x with
            | "a" -> "the_a_param"
            | "b" -> "the_b_param"
            | _ -> "unknown_param"))); ]
      )) in

   Mk_bar_plot.(call ([
      Bar_plot_opt Bar_plot.([
         X_titles_dir Vertical; 
         Y_axis [Axis.Lower (Some 0.)] ]);
      Charts mk_unit;
      Formatter formatter;
      Series mk_series;
      X mk_x;
      Y (eval_mean "exectime");
      Y_label "execution time";
      Input "results.txt"; (* equivalent to: Results results_all *)
      Output "compare.pdf"
      ]))

(*-------------------------------------------------------------------*)
(* Third demo: build a baseline to compare the main binary against,
   and use an option to skip the execution of baseline program
   (provide --skip_baseline on the command line)
*)

let third() = 
  let mk_baseline = 
    (mk_prog "test.sh") & (mk string "s" "a") in
  let mk_mainline = 
    ((mk_prog "test2.sh") & (mk string "s" "b") & (mk int "t" 0)) in
  let mk_n = 
     (mk_list int "n" [5; 10]) in
  let mk_m =
     (mk_list int "m" [1;2]) in
  let mk_args =
     mk_n & mk_m in

  let skip_baseline = XCmd.mem_flag "skip_baseline" in
  if not skip_baseline then begin
     Mk_runs.(call [
        Output "results_baselines.txt";
        Args (mk_baseline & mk_args)
        ]);
  end;

  Mk_runs.(call [
    Output "results_mainlines.txt";
    Args (mk_mainline & mk_args)
    ]);


  let results_baselines = Results.from_file "results_baselines.txt" in

  let eval_relative = fun env all_results results ->
    let baseline_results =  ~~ Results.filter_by_params results_baselines (
         from_env (Env.filter_keys ["n"; "m"] env)
         (* here we select only the baseline runs with matching n and m arguments *)
       ) in
    if baseline_results = [] then Pbench.warning ("no results for baseline: " ^ Env.to_string env);
    let v = Results.get_mean_of "exectime" results in
    let b = Results.get_mean_of "exectime" baseline_results in
    v /. b
    in

   Mk_bar_plot.(call ([
      Bar_plot_opt Bar_plot.([
         X_titles_dir Vertical; 
         Y_axis [Axis.Lower (Some 0.)] ]);
      Charts mk_unit;
      Series mk_m;
      X mk_n;
      Y eval_relative;
      Y_label "relative execution time w.r.t. baseline";
      Input "results_mainlines.txt";
      Output "improvement.pdf"
      ]))



(*-------------------------------------------------------------------*)

let _ =
   let test = XCmd.parse_or_default_string "test" "first" in
   match test with
   | "first" -> first()
   | "second" -> second()
   | "third" -> third()
   | _ -> failwith "unknown test"

