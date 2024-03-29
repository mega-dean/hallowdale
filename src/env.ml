open Utils

let verbose =
  if Array.length Sys.argv > 1 && Array.mem Sys.argv.(1) (Array.of_list [ "-v"; "--verbose" ]) then (
    print "verbose enabled";
    true)
  else
    false

let (development, project_root) : bool * string =
  let is_dev = ref false in
  let root_regex = ref "hallowdale-v[0-1]\\.[0-9]\\.[0-9]" in
  let rec find_project_root path : string =
    let dir = Filename.dirname path in
    match Filename.basename dir with
    | "/" ->
      failwith
        "could not find project root: make sure the executable is in a directory like \
         `hallowdale-v0.1.2`"
    | "_build" ->
      is_dev := true;
      root_regex := "hallowdale";
      find_project_root dir
    | basename -> (
      try
        let _ = Str.search_forward (Str.regexp !root_regex) basename 0 in
        dir
      with
      | Not_found -> find_project_root dir)
  in
  let project_root = find_project_root Sys.executable_name in
  Sys.chdir project_root;
  if verbose then
    print "env: %s\nroot: %s" (if !is_dev then "dev" else "prod") project_root;
  (!is_dev, project_root)

let room_scale = 3
let tile_size = 12
let w_tiles, h_tiles = (44, 24)

let max_width, max_height =
  let tiles (x_tiles, y_tiles) =
    (* if this is not an exact multiple of tile_size, pixels get distorted when the camera isn't moving *)
    ( x_tiles * tile_size * room_scale |> Int.to_float,
      y_tiles * tile_size * room_scale |> Int.to_float )
  in
  tiles (w_tiles, h_tiles)

let min_width, min_height = (max_width /. 2., max_height /. 2.)

let (window_w, window_h, window_scale, font_size) : float * float * float * float =
  let monitor_w, monitor_h =
    (* Raylib.set_config_flags [ Raylib.ConfigFlags.Window_resizable ]; *)
    (* need to run this before get_monitor_w/h *)
    Raylib.init_window 100 100 "hallowdale";
    (* TODO figure out why the Windows build is using raylib 0.5.1 instead of 1.2.1
       Raylib.toggle_borderless_windowed ();
    *)
    let monitor = Raylib.get_current_monitor () in
    (Raylib.get_monitor_width monitor, Raylib.get_monitor_height monitor)
  in
  let window_ratio, adjust_for_title_bar =
    let w_ratio = (monitor_w |> Int.to_float) /. max_width in
    let h_ratio = (monitor_h |> Int.to_float) /. max_height in
    if w_ratio < h_ratio then
      (w_ratio, false)
    else
      (h_ratio, true)
  in
  let window_scale =
    if development then
      List.last
        [
          0.5;
          (* 0.6; *)
          (* 0.65; *)
          0.7;
          (* 0.75; *)
          (* 0.8; *)
          (* 0.9; *)
          1.;
          (* 1.5; *)
        ]
    else if window_ratio < 0.5 then
      failwith "monitor too small :("
    else if adjust_for_title_bar then
      (* TODO this is approximate, and can be removed when toggle_borderless_windowed is fixed *)
      window_ratio *. 0.9
    else
      window_ratio
  in
  if verbose then
    print "window scale: %f" window_scale;
  (max_width *. window_scale, max_height *. window_scale, window_scale, 24. *. window_scale)
