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
   | Outside_below    (* todo: support additional outside *)
   | Nowhere


type arg = 
   | Legend_pos of legend_pos

type t = arg list

let get_legend_pos args = XOpt.get_default args (function Legend_pos x -> Some x (* | _ -> None *)) Top_right



(************************************************************************)
(** Helper functions *)

let legend_pos_of_string = function
   | "bottom" -> Bottom 
   | "bottomleft" -> Bottom_left 
   | "bottomright" -> Bottom_right
   | "top" -> Top
   | "topleft" -> Top_left
   | "topright" -> Top_right
   | "left" -> Left
   | "right" -> Right
   | "center" -> Center 
   | "nowhere" -> Nowhere
   | _ -> Pbench.error "invalid name for legend position"
