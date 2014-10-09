open XBase

type arg = 
   | Label of string
   | Is_log of bool
   | Lower of float option
   | Upper of float option

type t = arg list

let get_label_opt args = XOpt.get_option args (function Label x -> Some x | _ -> None)
let get_label args = XOpt.get_default args (function Label x -> Some x | _ -> None) "" 
let get_is_log args = XOpt.get_default args (function Is_log x -> Some x | _ -> None) false
let get_lower args = XOpt.get_default args (function Lower x -> Some x | _ -> None) None
let get_upper args = XOpt.get_default args (function Upper x -> Some x | _ -> None) None
