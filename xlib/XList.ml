open XBase

(************************************************************************)
(** Projections *)

(** Return the last item of a non-empty list *)

let last l =
  List.nth l (List.length l - 1)

(** Return the first item and the tail of the list *)

let head_and_tail = function
   | [] -> raise Not_found
   | x::xs -> x, xs

(** Return the last item and the front of the list (recursive implementation) *)

let take_last l =
   let rec aux acc = function
      | [] -> raise Not_found
      | [x] -> x, (List.rev acc)
      | x::xs -> aux (x::acc) xs
   in
   aux [] l

(** Drop returns the sublist obtained by keeping only the n first items *)

let rec take n xs =
   match n, xs with
        0, xs -> []
      | n, [] -> failwith "XList.take: invalid argument"
      | n, x::xs -> x :: take (n-1) xs

(** Drop returns the sublist obtained by skipping the n first items *)

let rec drop n xs =
   match n, xs with
        0, xs -> xs
      | n, [] -> failwith "XList.drop: invalid argument"
      | n, x::xs -> drop (n-1) xs

(** Combined calls to take and drop (for efficiency) *)

let rec take_drop n l =
  if n = 0 then ([], l) else
  match l with
  | [] -> failwith "XList.take_drop: invalid argument"
  | x::l' -> let (h,t) = take_drop (n-1) l' in
             (x::h, t)

(** Given a list, returns the n-th item and the list of all other items;
    (current implementation is not tail rec) *)

let rec extract_nth n l =
   match l with
   | [] -> failwith "XList.extract invalid argument"
   | x::q ->
      if n = 0 then (x,q) else
      let (y,q') = extract_nth (n-1) q in
      (y,x::q')

(** Split just before the first item that satisfies a given predicate *)

let split_on_predicate f l =
  let rec aux acc = function
    | [] -> (List.rev acc, [])
    | (x::t) as l -> if f x then (List.rev acc, l) else aux (x::acc) t
    in
  aux [] l


(************************************************************************)
(** Non recursive operators *)

(** List reverseal, not recursive *)

let rev_not_rec l =
   let res = ref [] in
   let cur = ref l in
   begin try while true do
      match !cur with
      | [] -> raise XBase.Break
      | x::xs ->
         res := x::!res;
         cur := xs
   done with XBase.Break -> () end;
   !res

(** List append, not recursive *)

let append_not_rec l1 l2 =
   let res = ref l2 in
   let cur = ref (rev_not_rec l1) in
   begin try while true do
      match !cur with
      | [] -> raise XBase.Break
      | x::xs ->
         res := x::!res;
         cur := xs
   done with XBase.Break -> () end;
   !res

(** List filter, not recursive *)

let filter_not_rec p l =
  let rec find accu = function
    | [] -> rev_not_rec accu
    | x :: l -> if p x then find (x :: accu) l else find accu l
  in
  find [] l

(** List map, not recursive *)

let map_not_rec f l =
   rev_not_rec (List.rev_map f l)


(************************************************************************)
(** Iteration *)

(** Initialize a list of length n, passing the index as argument *)

let init n f =
   let rec aux acc i =
      if i = n then acc else aux (f i :: acc) (i+1)
      in
   rev_not_rec (aux [] 0)

(** Iteration with the index being passed in addition to the item *)

let iteri f l =
  let rec aux i = function
    | [] -> ()
    | h::t -> (f i h); (aux (i+1) t)
    in
  aux 0 l

(** Map with the index being passed in addition to the item *)

let mapi f l =
  let rec aux i = function
    | [] -> []
    | h::t -> (f i h)::(aux (i+1) t)
    in
  aux 0 l

(** Extract every n-th item from a list *)

let filter_every n l =
  if n <= 0 then failwith "XList.filter_every: invalid arg";
  let rec aux acc k = function
      | [] -> acc
      | x::l' ->
          if k = 0 then aux (x::acc) (n-1) l'
                   else aux acc (k-1) l'
     in
  rev_not_rec (aux [] (n-1) l)

(** Count the number of items satisfying a predicate *)

let count f l =
  let rec aux acc = function
    | [] -> acc
    | x::t -> aux (if f x then 1+acc else acc) t
    in
  aux 0 l

(** Combine concat and map *)

let concat_map f l =
  List.concat (map_not_rec f l)


(************************************************************************)
(** Set operations *)

(** Remove all occurences of x from a list *)

let remove x l1 =
   List.filter (fun y -> y <> x) l1

(** [substract l1 l2] removes all occurences of items from [l2], in [l1] *)

let substract l1 l2 =
   List.filter (fun x -> not (List.mem x l2)) l1

(** [inter l1 l2] returns the list of values common to [l1] and [l2] *)

let inter l1 l2 =
   List.filter (fun x -> List.mem x l2) l1

(** [is_included l1 l2] returns true if any item in [l1] also belongs to [l2] *)

let is_included l1 l2 =
   ~~ List.for_all l1 (fun x -> List.mem x l2)

(** [equiv l1 l2] returns true if l1 and l2 contains the same items up to reordering] *)

let equiv l1 l2 =
   is_included l1 l2 && is_included l2 l1

(** [remove_duplicate l] keeps a single copy of each item *)

let remove_duplicate l =
   let rec aux acc = function
      | [] -> List.rev acc
      | x::t -> if List.mem x acc then aux acc t else aux (x::acc) t
      in
   aux [] l


(************************************************************************)
(** Iterated properties *)

(** [same_items l] returns true if all the items of [l] are equal
    (w.r.t. structural equality) *)

let same_items l =
   match l with
   | [] -> true
   | y::xs -> List.for_all (fun x -> x = y) xs

(** [unique l] checks that all the values in [l] are identical and it returns
    this unique value *)

let unique l =
   match l with
   | [] -> failwith "Xlist.unique: called on an empty list"
   | x::t ->
      if not (same_items l) then failwith "XList.unique: items are not identical";
      x

(** [map_to_same f l] returns true if, for any item [x] in the list,
    [f x] always returns the same value (w.r.t. structural equality) *)

let map_to_same f l =
   match l with
   | [] -> true
   | x::xs ->
      let v = f x in
      List.for_all (fun y -> v = f y) xs


(************************************************************************)
(** Construction *)

  (** [imper_build body] calls the function [body] by providing it as argument
    a function [add] that may be used to add items to a list; The final
    return value is the content of this list. *)

let build item_creator =
   let r = ref [] in
   item_creator (XBase.add_to_list_ref r);
   rev_not_rec !r



(************************************************************************)
(** Sorting *)

(** Comparison functions for association lists *)

let cmp_fst (x1,y1) (x2,y2) =
   x1 - x2

let cmp_snd (x1,y1) (x2,y2) =
   y1 - y2

let cmp_fst_desc (x1,y1) (x2,y2) =
   x2 - x1

let cmp_snd_desc (x1,y1) (x2,y2) =
   y2 - y1


(** Return the result of sorting an association list according to the keys *)

let ksort cmp l =
   List.sort cmp_fst l

(** Insert an item into a sorted list *)

let sorted_insert cmp x l =
   let rec ins = function
      | [] -> [x]
      | y::q -> if cmp x y <= 0 then x::y::q else y::(ins q)
      in
   ins l

(** Place values into buckets, according to a bucketing function;
    takes a function of type ['a -> 'b], a list of type ['a list],
    and returns a [('b * ('a list)) list] *)

let bucket f l =
   let t = Hashtbl.create 8 in
   let get_bucket k =
      if Hashtbl.mem t k then begin
         Hashtbl.find t k
      end else begin
         Hashtbl.add t k [];
         []
      end in
   ~~ List.iter (List.rev l) (fun v ->
      let k = f v in
      let b = get_bucket k in
      Hashtbl.replace t k (v::b));
   Hashtbl.fold (fun k b acc -> (k,b)::acc) t []



(************************************************************************)
(** Shuffling *)

(* Shuffle a list, naively; in quadratic complexity, and not tail rec *)

let schuffle l =
   let rec aux q =
      let n = List.length q in
      if n = 0 then [] else begin
         let k = Random.int n in
         let (x,q') = extract_nth k q in
         x :: (aux q')
      end
      in
   aux l


(************************************************************************)
(** Association lists *)

(** Returns the list of keys *)

let keys l =
   List.map fst l

(** Returns "Some" of the value bound to a key, or "None" if not found *)

let assoc_option k l =
   try Some (List.assoc k l)
   with Not_found -> None

(** Returns the value bound to a key, or a default value *)

let assoc_default k l def =
   try List.assoc k l
   with Not_found -> def

(** Same as (k, v)::(List.remove_assoc k l), but preserves order of bindings;
    raise Not_found if argument does not exist *)

let assoc_replace k v l =
   let rec aux = function
      | [] -> raise Not_found
      | (k',v')::l' -> if k = k' then (k,v)::l' else (k',v')::(aux l')
      in
   aux l

(** Apply assoc_replace several times *)

let assoc_replaces kvs l =
   List.fold_left (fun acc (k,v) -> assoc_replace k v acc) l kvs

(** Like assoc_replace, but adds the binding to the front if it does not exist *)

let assoc_replace_or_add k v l =
   try assoc_replace k v l
   with Not_found -> (k,v)::l

(** Remove several keys from an association list *)

let remove_assocs ks l =
  List.fold_left (fun acc k -> List.remove_assoc k acc) l ks


(************************************************************************)
(** Conversion with options *)

(** Converts an option into a list of zero or one item *)

let of_option = function
   | None -> []
   | Some x -> [x]


(************************************************************************)
(** List printing *)

let to_string sep string_of_item l =
   String.concat sep (List.map string_of_item l)

let string_of_list = to_string (* alias *)

let rec fprintf sep print_item ppf = function
   | [] -> ()
   | [x] -> print_item ppf x
   | x::q ->
      print_item ppf x;
      Format.fprintf ppf "%s" sep;
      fprintf sep print_item ppf q


(************************************************************************)
(** List parsing *)

let parse_coma_separated_list parse_item s =
   let parts = Str.split (Str.regexp "[,]") s in
   List.map parse_item parts
