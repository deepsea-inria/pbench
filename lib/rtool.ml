open XBase

(** String representation of a R string *)

type script = string

(** Converts a string into its R description, by adding double quotes *)

let of_string s = 
  sprintf "\"%s\"" s

(** Converts a list of R values into its R description (coma-separated) *)

let of_list vs =
   String.concat "," vs

(** Converts a list of strings into its R description (coma-separated and quoted) *)

let of_string_list ss =
   of_list (List.map of_string ss)

(** Converts a float value into its R description, dealing with special values *)

let of_float ?(islogscale=false) v =
   match classify_float v with 
   | FP_zero when islogscale -> "0.1" 
   | FP_normal | FP_subnormal | FP_zero -> sprintf "%f" v
   | FP_infinite | FP_nan -> "NA"

(** Defines a variable name for a series of data *)

let batch_name i = 
  sprintf "data%d" i

(** Defines a default vector of colors to use for plots *)

let colors_default = 
   [ "#000000"; "#FF0000"; "#0000FF"; "#00FF00"; "#AA00AA"; 
     "#000099"; "#CCFF99"; "#FF66FF"; "#FFFF00"; "#00FFFF"; "#CC0066"; "#CCCCFF"; "#C0C0C0"; "#0000CC"; "#FF66FF"; "#990000"; "#009900"; "#CC0000"; "#00CC00"; "#FFCCCC";]

   (* XList.init nbcols (fun i -> sprintf "%d" (i+1)) 
      sprintf "cols <- as.vector(c(%s))" (XList.to_string "," (fun x -> x) vals) *)

(** Declare 'cols' to be a list of nb colors given by RGB code *)

let colors_define color_codes nb = 
  if nb > List.length color_codes then Pbench.error (sprintf "Rtool.colors_define: too many different colors requested %d" nb);
  let codes = XList.take nb color_codes in
  sprintf "cols <- as.vector(c(%s))" (XList.to_string "," (fun x -> sprintf "'%s'" x) codes)

(** Defines a default vector of point characters to use for plots *)

let pchs_default =
  [ 4; 19; 17; 15; 25; 0; 2; 5; 6; 3; 1; 7; 8; 9; 10; 24; 23; 18 ]

(** Declare 'pchs' to be a list of colors given by RGB code *)

let pchs_define nbpchs =
  sprintf "pchs <- as.vector(c(%s))" (XList.to_string "," (fun x -> sprintf "%d" x) pchs_default)

(** Defines a title for a R chart *)

let chart_title s =
   sprintf "title(main='%s', col.main='black', font.main=1, cex.main=1)" s

(** Converts a legend into its R code *)

let string_of_legend_pos = Legend.(function
   | Bottom -> "bottom"
   | Bottom_left -> "bottomleft"
   | Bottom_right -> "bottomright"
   | Top -> "top"
   | Top_left -> "topleft"
   | Top_right -> "topright"
   | Left -> "left"
   | Right -> "right"
   | Center -> "center"
   | Outside_below -> Pbench.error "Outside_below not available for legend in this type of plot"
   | Nowhere -> Pbench.error "Nowhere not available for legend in this type of plot"
   )

(** Builds a R script that generates an image to store the content
    described by a given R script, provided the basename to be used
    for the name of the image file, and the extension, which is one
    of "pdf" or "eps", and provided dimensions *)

let wrap_image rscript dimensions basename extension =
   let (width,height) = dimensions in
   let format = match extension with
      | "pdf" -> "pdf"
      | "eps" -> "postscript"
      | _ -> failwith ("unsupported image format for R:" ^ extension)
      in
   let imgfilename = basename ^ "." ^ extension in
   let lines = List.concat [
      [ sprintf "%s('%s', height=%f, width=%f)" format imgfilename height width ];
      [ rscript ];
      [ "dev.off()" ] ] in
   String.concat "\n" lines

(** Execution of an R script, described by string "rscript",
    using a file called "rfilename" as temporary file. *)

let execute rscript rfilename =
   XFile.put_contents rfilename rscript;
   let cmd = sprintf "R --silent --file=%s > null" rfilename in
   Pbench.system cmd
   (* let succes = XSys.command_as_bool cmd in
   if not succes
      then Pbench.warning (sprintf "failure on executing R on file: %s\n" rfilename)*)
