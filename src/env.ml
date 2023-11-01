let (development, project_root) : bool * string =
  let is_dev = ref false in
  let rec find_project_root path =
    let dir = Filename.dirname path in
    match Filename.basename dir with
    | "hallowdale" -> dir
    | "_build" ->
      is_dev := true;
      find_project_root dir
    | _ -> find_project_root dir
  in
  let project_root = find_project_root Sys.executable_name in
  Sys.chdir project_root;
  (!is_dev, project_root)

let room_scale = 3
let tile_size = 12

let max_width, max_height =
  let tiles (x_tiles, y_tiles) =
    (* if this is not an exact multiple of tile_size, pixels get distorted when the camera isn't moving *)
    ( x_tiles * tile_size * room_scale |> Int.to_float,
      y_tiles * tile_size * room_scale |> Int.to_float )
  in
  tiles (44, 24)

let min_width, min_height = (max_width /. 2., max_height /. 2.)

let (window_w, window_h, window_scale, font_size) : float * float * float * int =
  let monitor_w, monitor_h =
    (* Raylib.set_config_flags [ Raylib.ConfigFlags.Window_resizable ]; *)
    (* need to run this before get_monitor_w/h *)
    Raylib.init_window 100 100 "hallowdale";
    let monitor = Raylib.get_current_monitor () in
    (Raylib.get_monitor_width monitor, Raylib.get_monitor_height monitor)
  in
  print_endline (Printf.sprintf "config monitor w/h: %d, %d" monitor_w monitor_h);
  let window_ratio =
    let w_ratio = (monitor_w |> Int.to_float) /. max_width in
    let h_ratio = (monitor_h |> Int.to_float) /. max_height in
    Float.min w_ratio h_ratio
  in
  let window_scale =
    if window_ratio < 0.5 then
      failwith "monitor too small :("
    else if window_ratio > 1. then
      1.
    else
      window_ratio
  in
  ( max_width *. window_scale,
    max_height *. window_scale,
    window_scale,
    24. *. window_scale |> Float.to_int )
