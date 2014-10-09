open XBase


(** Possible values for legend position *)

type legend_pos = 
   | Bottom
   | Bottom_left 
   | Bottom_right
   | Top
   | Top_left
   | Top_right
   | Left
   | Right
   | Center
   | Outside_below
   (* todo: support additional outside *)

type arg = 
   | Legend_pos of legend_pos

type t = arg list

let get_legend_pos args = XOpt.get_default args (function Legend_pos x -> Some x (* | _ -> None *)) Top_right
