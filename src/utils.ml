let fmt s = Printf.sprintf s
let print fmtstr = Printf.ksprintf print_endline fmtstr
let tmp fmtstr = Printf.ksprintf print_endline fmtstr
let itmp fmtstr = Printf.ifprintf print_endline fmtstr
let failwithf f = Printf.ksprintf failwith f

type vector = {
  mutable x : float;
  mutable y : float;
}

let get_distance (pos1 : vector) (pos2 : vector) : float =
  let x = abs_float (pos1.x -. pos2.x) in
  let y = abs_float (pos1.y -. pos2.y) in
  sqrt ((x *. x) +. (y *. y))

type rect = {
  mutable pos : vector;
  mutable w : float;
  mutable h : float;
}

let rect_center_x (rect : rect) = rect.pos.x +. (rect.w /. 2.)
let rect_center_y (rect : rect) = rect.pos.y +. (rect.h /. 2.)
let get_rect_center (rect : rect) = { x = rect_center_x rect; y = rect_center_y rect }

let raylib_Rect_to_rect (r : Raylib.Rectangle.t) : rect =
  Raylib.Rectangle.{ w = width r; h = height r; pos = { x = x r; y = y r } }

let rect_to_Rect (r : rect) : Raylib.Rectangle.t = Raylib.Rectangle.create r.pos.x r.pos.y r.w r.h

let scale_rect scale rect =
  {
    pos = { x = rect.pos.x *. scale; y = rect.pos.y *. scale };
    w = rect.w *. scale;
    h = rect.h *. scale;
  }

let get_midpoint (r1 : rect) (r2 : rect) : vector =
  let c1 = get_rect_center r1 in
  let c2 = get_rect_center r2 in
  { x = (c1.x +. c2.x) /. 2.; y = (c1.y +. c2.y) /. 2. }

module Float = struct
  include Float

  let range (n : int) : float list = List.init n (fun x -> Int.to_float x)
  let bound (min : float) (n : float) (max : float) : float = Float.max min (Float.min n max)
end

module Int = struct
  include Int
  module Map = Map.Make (Int)

  let range (n : int) : int list = List.init n (fun x -> x)
  let bound (min : int) (n : int) (max : int) : int = Int.max min (Int.min n max)
end

module String = struct
  include String
  module Map = Map.Make (String)
  module Set = Set.Make (String)

  (* returns the strings before and after the first occurrence of char c:
     split_at_first "a.b.c.d" '.' => "a", "b.c.d"
  *)
  let split_at_first_opt c str : string option * string =
    match String.index_from_opt str 0 c with
    | None -> (None, str)
    | Some idx -> (Some (Str.string_before str idx), Str.string_after str (idx + 1))

  let split_at_first c str : string * string =
    match split_at_first_opt c str with
    | Some prefix, rest -> (prefix, rest)
    | None, _ ->
      failwith (Printf.sprintf "Utils.split_at_first ---- no separator '%c' in string '%s'" c str)

  let maybe_trim_before c str : string =
    match split_at_first_opt c str with
    | Some prefix, rest -> rest
    | None, _ -> str

  let join strs = String.concat ", " strs
  let join_lines strs = String.concat "\n" strs
  let to_int s = int_of_string s
end

module Array = struct
  include Array

  let sub = ArrayLabels.sub
end

module Random = struct
  include Random

  let float_between lower upper =
    if upper < lower then
      raise (Invalid_argument "float_between lower upper")
    else
      float (upper -. lower) +. lower

  let int_between lower upper =
    if upper < lower then
      raise (Invalid_argument "int_between lower upper")
    else
      int (upper - lower) + lower

  let in_rect_x (rect : rect) = float_between rect.pos.x (rect.pos.x +. rect.w)
  let in_rect_y (rect : rect) = float_between rect.pos.y (rect.pos.y +. rect.h)
end

module List = struct
  include List

  let replace_assoc (k : 'a) (v : 'b) (xs : ('a * 'b) list) : ('a * 'b) list =
    (k, v) :: List.remove_assoc k xs

  let find_idx x xs =
    let matches ((_i, x') : int * 'a) : bool = x = x' in
    match List.find_opt matches (List.mapi (fun i x -> (i, x)) xs) with
    | None -> failwith "find_idx: no matches"
    | Some (idx, _) -> idx

  let filter_somes xs =
    (* TODO seems like there is probably a better way to do List.filter_mapi, but this works *)
    xs |> List.filter Option.is_some |> List.map Option.get

  let only xs =
    if List.length xs = 1 then
      List.nth xs 0
    else
      failwithf "only: expected 1, got %d" (List.length xs)

  (* returns a random element from xs *)
  let sample xs = List.nth xs (Random.int (List.length xs))

  let uniq xs =
    let res = ref [] in
    let add_x x =
      if not (List.mem x !res) then
        res := x :: !res
    in
    List.iter add_x xs;
    !res

  let last xs =
    if List.length xs = 0 then
      failwith "List.last on empty list"
    else
      List.nth xs (List.length xs - 1)

  let to_array xs = Array.of_list xs
  let to_string_map xs = String.Map.of_list xs
  let to_int_map xs = Int.Map.of_list xs

  module Non_empty = struct
    type 'a t = 'a * 'a list

    let length ((_, rest) : 'a t) : int = List.length rest + 1
    let hd ((head, rest) : 'a t) = head
    let tl ((head, rest) : 'a t) = rest
    let mem (x : 'a) ((head, rest) : 'a t) = x = head || List.mem x rest

    let assoc_opt (k : 'a) (xs : ('a * 'b) t) =
      if k = fst (hd xs) then
        Some (snd (hd xs))
      else
        List.assoc_opt k (tl xs)

    let iter (f : 'a -> 'b) ((head, rest) : 'a t) : unit =
      f head;
      List.iter f rest

    let map (f : 'a -> 'b) ((head, rest) : 'a t) : 'b t = (f head, List.map f rest)
    let to_list ((head, rest) : 'a t) : 'a list = head :: rest
  end

  let to_ne_list (xs : 'a list) : 'a Non_empty.t =
    match List.nth_opt xs 0 with
    | None -> failwith "can't create non-empty list from empty list"
    | Some head -> (head, List.tl xs)
end

type 'a ne_list = 'a List.Non_empty.t

(* 2-d array *)
module Matrix = struct
  type 'a t = 'a array array

  let make (xs : 'a list) (w : int) : 'a t =
    if List.length xs mod w <> 0 then
      failwithf "Matrix.make - can't evenly split list of length %d into rows of width %d"
        (List.length xs) w
    else (
      let json_data = xs |> List.to_array in
      let row_count = Array.length json_data / w in
      let make_row row_idx = Array.sub json_data ~pos:(row_idx * w) ~len:w in
      Array.map make_row (Int.range row_count |> List.to_array))

  let sub (matrix : 'a t) x y w h : ('a t, string) Result.t =
    try
      let rows : 'a t = Array.sub matrix ~pos:y ~len:h in
      Result.ok (Array.map (fun row -> Array.sub row ~pos:x ~len:w) rows)
    with
    | e ->
      let msg =
        if Array.length matrix = 0 then
          "empty array"
        else if Array.length matrix.(0) = 0 then
          "empty rows"
        else if x < 0 then
          fmt "x too small (%d)" x
        else if x + w > Array.length matrix.(0) - 1 then
          fmt "x + w too large (%d + %d > %d)" x w (Array.length matrix.(0) - 1)
        else if y < 0 then
          fmt "y too small (%d < 0)" y
        else if y + h > Array.length matrix - 1 then
          fmt "y + h too large (%d + %d > %d)" y h (Array.length matrix - 1)
        else (
          print "Matrix.sub unknown error (boundary checks passed)";
          raise e)
      in
      Result.error (fmt "Matrix.sub: %s" msg)
end
