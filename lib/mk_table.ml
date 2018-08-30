open XBase

(** Latex escape *)

let escape add s =
   add (Latex.escape s)

(** Auxiliary function to build a cell of a table *)

let cell ?(escape=true) ?(last=false) add s =
   add (if escape then Latex.escape s else s);
   if not last then add " & "

let build_table tex_file pdf_file body =
   let s = Buffer.create 1 in
   let add x = Buffer.add_string s x in
   body add;
   let latex = Buffer.contents s in
   XFile.put_contents tex_file latex;
   Latex.build pdf_file (Latex.basic_document latex)

(** Helper functions: create 2d array from list of lists of strings *)

let matrix_of_list_of_lists cells =
  let nb_rows = List.length cells in
  if nb_rows = 0 then [| |] else begin
     let nb_cols = List.length (List.hd cells) in
     let matrix = Array.make_matrix nb_rows nb_cols "" in
     ~~ XList.iteri cells (fun x row ->
       ~~ XList.iteri row (fun y col ->
         matrix.(x).(y) <- col;
       )
     );
     matrix
  end

(** Create a basic table given a 2d array of data *)

let latex_of_matrix ?(escape=true) matrix =
  let nb_rows = Array.length matrix in
  if nb_rows = 0 then "" else begin
    let nb_cols = Array.length matrix.(0) in
    if nb_cols < 2 then Pbench.error "latex_of_matrix requires at least 2 columns";
    let s = Buffer.create 1 in
    let add x = Buffer.add_string s x in
    add (Latex.tabular_begin (String.concat "" (["|l|"] @ XList.init (nb_cols-1) (fun i -> "c|") )));
    ~~ Array.iter matrix (fun row ->
        ~~ Array.iteri row (fun icol col ->
          cell ~escape:escape ~last:(icol=nb_cols-1) add col;
        );
        add Latex.tabular_newline;
    );
    add Latex.tabular_end;
    add Latex.new_page;
    Buffer.contents s
  end


