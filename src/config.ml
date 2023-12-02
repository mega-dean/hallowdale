open Types
open Env

(* TODO make window_scale mutable so window size can be changed while the game is running *)
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
  let ghost = 2. in
  let health = 2. in
  let soul = 3. in
  {
    ghost = ghost *. window_scale;
    health = health *. window_scale;
    soul = soul *. window_scale;
    slash = 0.8 *. window_scale;
    room = (room_scale |> Int.to_float) *. window_scale;
    font_size = Env.font_size;
    paragraph_spacing = Env.font_size * 2;
  }

type world_map_config = {
  scale : float;
  room_x_offset : float;
  room_y_offset : float;
}

let world_map : world_map_config =
  {
    scale = 45.8 *. window_scale;
    (* these are offset to account for the padding around the map *)
    room_x_offset = 479.;
    room_y_offset = 177.;
  }

type window_config = {
  scale : float;
  w : float;
  h : float;
  w_tiles : int;
  h_tiles : int;
  tile_size : float;
  dest_tile_size : float;
  center : vector;
  max_width : float;
  max_height : float;
  min_width : float;
  min_height : float;
  camera_motion : vector;
  fps : int;
}

let window : window_config =
  let fps =
    (* 960 *)
    (* 480 *)
    (* 240 *)
    144
    (* 60 *)
  in
  let fps_scale = (fps |> Int.to_float) /. 144. in
  if fps < 60 then
    failwith
      (Printf.sprintf "got invalid fps %d, needs to be at least 60 to avoid dropping inputs" fps);
  {
    scale = window_scale;
    w = window_w;
    h = window_h;
    w_tiles;
    h_tiles;
    tile_size = Env.tile_size |> Int.to_float;
    dest_tile_size = (Env.tile_size |> Int.to_float) *. scale.room;
    center = { x = window_w /. 2.; y = window_h /. 2. };
    max_width;
    max_height;
    min_width;
    min_height;
    fps;
    camera_motion = { x = 60. *. fps_scale; y = 12. *. fps_scale };
  }

type platform_config = {
  disappearable_touched_time : float;
  disappearable_invisible_time : float;
  rotatable_touched_time : float;
  rotatable_upside_down_time : float;
  rotatable_spikes_dy : float;
  rotatable_anim_duration : float;
}

let platform =
  {
    disappearable_touched_time = 0.9;
    disappearable_invisible_time = 1.4;
    rotatable_touched_time = 0.9;
    rotatable_upside_down_time = 2.;
    rotatable_spikes_dy =
      (* this is specific to the rotating c-heart spikes
         (would have to look this up from texture height to do it generically) *)
      70. *. window_scale;
    rotatable_anim_duration = 0.05;
  }

type ghost_config = {
  width : float;
  height : float;
  vx : float;
  wall_slide_vy : float;
  flap_vy : float;
  jump_vy : float;
  wall_jump_vy : float;
  dash_duration : int;
  dive_vy : float;
  c_dash_whoosh_scale : float;
  wraiths_scale : float;
  max_vy : float;
  debug_v : float;
  small_debug_v : float;
  recoil_speed : float;
  head_w : float;
  head_h : float;
  neck_x : float;
  neck_y : float;
  entity_neck_x : float;
  entity_neck_y : float;
}

let ghost : ghost_config =
  let width, height = (20., 34.) in
  let jump_vy = -1080. *. window_scale in
  let wall_jump_vy = jump_vy *. 0.8 in
  {
    width;
    height;
    max_vy = 1100. *. window_scale;
    dive_vy = 1600. *. window_scale;
    c_dash_whoosh_scale = 6. *. window_scale;
    wraiths_scale = 1.7 *. window_scale;
    vx = 400. *. window_scale;
    jump_vy;
    flap_vy = jump_vy *. 0.8;
    wall_jump_vy;
    wall_slide_vy = 400. *. window_scale;
    dash_duration = 20;
    debug_v = 20.;
    small_debug_v = 2.;
    (* this is currently used for both damage recoils and pogos *)
    recoil_speed = 800. *. window_scale;
    (* every ghost head image is 40px by 40px *)
    head_w = 40. *. scale.ghost;
    head_h = 40. *. scale.ghost;
    (* the neck on every head image is at (20, 30) *)
    neck_x = 20. *. scale.ghost;
    neck_y = 30. *. scale.ghost;
    (* the neck on the entity.dest is at (10, 18) *)
    entity_neck_x = 10. *. scale.ghost;
    entity_neck_y = 18. *. scale.ghost;
  }

type action_config = {
  max_soul : int;
  soul_per_cast : int;
  soul_gained_per_nail : int;
  attack_duration : float;
  vengeful_spirit_vx : float;
  vengeful_spirit_recoil : float;
  vengeful_spirit_duration : float;
}

let action : action_config =
  {
    max_soul = 99;
    soul_per_cast = 33;
    soul_gained_per_nail = 11;
    attack_duration = 0.07;
    vengeful_spirit_vx = 1000. *. window_scale;
    vengeful_spirit_recoil = 80. *. window_scale;
    vengeful_spirit_duration = 1.5;
  }

type physics_config = {
  mutable gravity : float;
  jump_damping : float;
  jump_fall_threshold : float;
}

let physics =
  {
    gravity = 1800. *. window_scale;
    jump_damping = 0.8;
    jump_fall_threshold = -80. *. window_scale;
  }

type text = {
  spacing : float;
  tall_margin_y_bottom : float;
  short_margin_y_bottom : float;
  outline_offset_y : float;
  outline_h : float;
  (* focus needs a separate config because there is text above the outline *)
  focus_outline_offset_y : float;
  focus_outline_bottom_offset_y : float;
  base_config : Interaction.text_config;
  menu_config : Interaction.text_config;
  dialogue_config : Interaction.text_config;
  floating_config : Interaction.text_config;
}

let get_text_margins menu_choice =
  let main_menu_margins = (50., 360.) in
  let pause_menu_margins = (250., 220.) in
  let margin_x, margin_y_top =
    match menu_choice with
    | PAUSE_MENU _
    | CHANGE_WEAPON_MENU _
    | CHANGE_GHOST_MENU _
    | SETTINGS_MENU _
    | CHANGE_AUDIO_SETTING _ ->
      pause_menu_margins
    | SELECT_GAME_MODE _
    | MAIN_MENU _
    | CONFIRM_DELETE_MENU _
    | SAVE_FILES_MENU _ ->
      main_menu_margins
  in
  (margin_x *. window_scale, margin_y_top *. window_scale)

let text =
  let base_config : Interaction.text_config =
    {
      margin_x = 50. *. window_scale;
      margin_y_top = 20. *. window_scale;
      margin_y_bottom = 20. *. window_scale;
      padding = { x = 50. *. window_scale; y = 50. *. window_scale };
      centered = false;
    }
  in
  let outline_offset_y = window.h /. 4. in

  {
    spacing = 2. *. window_scale;
    tall_margin_y_bottom = 50. *. window_scale;
    short_margin_y_bottom = 350. *. window_scale;
    outline_offset_y = window.h /. 4.;
    outline_h = 100. *. window.scale;
    focus_outline_offset_y = 50. *. window_scale;
    focus_outline_bottom_offset_y = outline_offset_y +. (200. *. window.scale);
    base_config;
    menu_config = base_config;
    dialogue_config =
      {
        margin_x = 250. *. window_scale;
        margin_y_top = 50. *. window_scale;
        margin_y_bottom = 450. *. window_scale;
        padding = { x = 30. *. window_scale; y = 30. *. window_scale };
        centered = false;
      };
    floating_config =
      {
        margin_x = 150.;
        margin_y_top = 50.;
        margin_y_bottom = window.h *. 0.75;
        padding = { x = 50.; y = 50. };
        centered = true;
      };
  }

let get_plain_text_config margin_y_bottom =
  {
    text.base_config with
    margin_x = 150. *. window_scale;
    margin_y_top = 50. *. window_scale;
    margin_y_bottom;
  }

let get_menu_text_config margin_x margin_y_top margin_y_bottom =
  { text.base_config with margin_x; margin_y_top; margin_y_bottom; centered = true }

type other = {
  kp_start_x : float;
  kp_start_y : float;
  ss_respawn_y_offset : float;
  ss_warp_y_offset : float;
  main_menu_y_offset : float;
  hud_padding : float;
  save_slots : int;
}

let other =
  {
    (* these are scaled by Game.create *)
    kp_start_x = 1800.;
    kp_start_y = 150.;
    ss_respawn_y_offset = 10. *. window_scale;
    ss_warp_y_offset = 80. *. window_scale;
    main_menu_y_offset = 50. *. window_scale;
    hud_padding = 8. *. window_scale;
    save_slots = 4;
  }

type debug_keys = {
  mutable n : bool;
  mutable t : bool;
  mutable zero : bool;
  mutable eight : bool;
  mutable nine : bool;
}

let debugs = { n = false; t = false; zero = false; eight = false; nine = true }
