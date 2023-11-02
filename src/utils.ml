let fmt s = Printf.sprintf s
let print fmtstr = Printf.ksprintf print_endline fmtstr
let tmp fmtstr = Printf.ksprintf print_endline fmtstr
let itmp fmtstr = Printf.ifprintf print_endline fmtstr
let failwithf f = Printf.ksprintf failwith f
let join ?(sep = ", ") strs = String.concat sep strs

module Float = struct
  include Float

  let range (n : int) : float list = List.init n (fun x -> Int.to_float x)
  let bound (min : float) (n : float) (max : float) : float = Float.max min (Float.min n max)
end

module Int = struct
  include Int

  let range (n : int) : int list = List.init n (fun x -> x)
  let bound (min : int) (n : int) (max : int) : int = Int.max min (Int.min n max)
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
end

module String = struct
  include String

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
end
