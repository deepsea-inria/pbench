
(** Returns true if [str] ends with the substring [s] *)

let string_ends_with s str =
   let n = String.length s in
   let m = String.length str in
   if m < n
      then false
      else (s = String.sub str (m - n) n)

(** Returns true if [str] starts with the substring [s] *)

let string_starts_with s str =
   let n = String.length s in
   let m = String.length str in
   if m < n
      then false
      else (s = String.sub str 0 n)

(** Comparison function for strings *)

let cmp x y =
  if x < y then -1 else if x = y then 0 else 1

(** Add quotes around a string *)

let quote s =
   "\"" ^ s ^ "\""

(** Tests whether a character is a digit *)

let char_is_digit c =
     int_of_char '0' <= int_of_char c
  && int_of_char c <= int_of_char '9'
