[@@@ocaml.warning "-26-27-32"]

(* TODO move all the values in this file to config / atd *)

type scale_config = {
  ghost : float;
  health : float;
  soul : float;
  slash : float;
  room : float;
  font_size : int;
  paragraph_spacing : int;
}

let scale =
  (* if these aren't whole numbers, the pixels don't scale evenly *)
  let ghost = 2 in
  let health = 2 in
  let soul = 3 in
  let room = 3 in
  let font_size =
    (* TODO at sizes > 24, text starts to overlap with some of the ability outlines (eg focus-info) *)
    24
  in
  {
    ghost = ghost |> Int.to_float;
    health = health |> Int.to_float;
    soul = soul |> Int.to_float;
    slash = 0.8;
    room = room |> Int.to_float;
    font_size;
    paragraph_spacing = font_size * 2;
  }

type window_config = {
  width : int;
  height : int;
  center_x : float;
  center_y : float;
  fps : int;
}

let window : window_config =
  let width, height = (1280, 720) in
  let fps =
    (* 960 *)
    (* 480 *)
    (* 240 *)
    144
    (* 60 *)
  in
  if fps < 60 then
    (* TODO could make a separate utils.ml file so this can use printing fns, but this whole config
       file might go away as things are moved into json configs
    *)
    failwith
      (Printf.sprintf "got invalid fps %d, needs to be at least 60 to avoid dropping inputs" fps);
  { width; height; center_x = Float.of_int width /. 2.; center_y = Float.of_int height /. 2.; fps }

type ghost_config = {
  width : float;
  height : float;
  vx : float;
  wall_slide_vy : float;
  jump_vy : float;
  wall_jump_vy : float;
  dash_duration : int;
  dive_vy : float;
  max_vy : float;
  debug_v : float;
  small_debug_v : float;
}

let ghost : ghost_config =
  let width, height = (20., 34.) in
  let jump_vy = -1020. in
  let wall_jump_vy = jump_vy *. 0.8 in
  {
    width;
    height;
    max_vy = 1400.;
    dive_vy = 1600.;
    vx = 400.;
    jump_vy;
    wall_jump_vy;
    wall_slide_vy = 400.;
    dash_duration = 20;
    debug_v = 20.;
    small_debug_v = 2.;
  }

type action_config = {
  max_soul : int;
  soul_per_cast : int;
  soul_gained_per_nail : int;
  vengeful_spirit_vx : float;
  attack_duration : float;
  vengeful_spirit_duration : float;
}

let action : action_config =
  {
    max_soul = 99;
    soul_per_cast = 33;
    soul_gained_per_nail = 11;
    attack_duration = 0.07;
    vengeful_spirit_vx = 800.;
    vengeful_spirit_duration = 1.5;
  }

type physics_config = {
  mutable gravity : float;
  jump_damping : float;
  jump_fall_threshold : float;
}

let physics = { gravity = 1400.; jump_damping = 0.8; jump_fall_threshold = -80. }

type debug_keys = {
  mutable n : bool;
  mutable t : bool;
  mutable zero : bool;
  mutable eight : bool;
  mutable nine : bool;
}

let debugs = { n = false; t = false; zero = false; eight = false; nine = true }
