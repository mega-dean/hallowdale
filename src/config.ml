open Utils
open Types
open Env

(* TODO make window_scale mutable so window size can be changed while the game is running *)
type scale_config = {
  ghost : float;
  enemy : float;
  health : float;
  soul : float;
  dream_nail : float;
  slash : float;
  focus : float;
  room : float;
  damage : float;
  npc : float;
  font_size : int;
  paragraph_spacing : int;
}

let scale =
  let ghost = 2. in
  let enemy =
    (* there is also a per-enemy "scale" in enemy configs *)
    2.2
  in
  let health = ghost in
  let soul = 3. in
  let npc = ghost in
  {
    ghost = ghost *. window_scale;
    enemy = enemy *. window_scale;
    health = health *. window_scale;
    soul = soul *. window_scale;
    dream_nail = window_scale;
    slash = 0.8 *. window_scale;
    focus = 3. *. window_scale;
    room = (room_scale |> Int.to_float) *. window_scale;
    damage = (room_scale |> Int.to_float) *. window_scale;
    npc = npc *. window_scale;
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
    disappearable_touched_time = 0.8;
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
  starting_max_soul : int;
  starting_max_health : int;
  width : float;
  height : float;
  vx : float;
  wall_slide_vy : float;
  flap_vy : float;
  jump_vy : float;
  wall_jump_vy : float;
  upslash_vy : float;
  hardfall_duration : float;
  dive_vy : float;
  c_dash_whoosh_scale : float;
  shade_dash_sparkles_scale : float;
  wraiths_scale : float;
  max_vy : float;
  debug_v : float;
  small_debug_v : float;
  recoil_speed : float;
  pogo_recoil_time : float;
  nail_recoil_time : float;
  damage_recoil_time : float;
  shine_size : float;
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
    starting_max_soul = 99;
    starting_max_health = 5;
    width;
    height;
    max_vy = 1100. *. window_scale;
    dive_vy = 2000. *. window_scale;
    hardfall_duration = 1.25;
    c_dash_whoosh_scale = 6. *. window_scale;
    shade_dash_sparkles_scale = 6. *. window_scale;
    wraiths_scale = 1.7 *. window_scale;
    vx = 400. *. window_scale;
    jump_vy;
    flap_vy = jump_vy *. 0.8;
    wall_jump_vy;
    wall_slide_vy = 400. *. window_scale;
    upslash_vy = 300. *. window_scale;
    debug_v = 20. *. window_scale;
    small_debug_v = 2. *. window_scale;
    pogo_recoil_time = 0.2;
    nail_recoil_time = 0.1;
    damage_recoil_time = 0.066666;
    (* this is currently used for both damage recoils and pogos *)
    recoil_speed = 800. *. window_scale;
    shine_size = 1200. *. window_scale;
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
  soul_per_cast : int;
  spell_twister_soul_per_cast : int;
  soul_gained_per_nail : int;
  attack_duration : float;
  vengeful_spirit_vx : float;
  shade_soul_vx : float;
  vengeful_spirit_recoil : float;
  vengeful_spirit_duration : float;
  quick_focus_speed : float;
  dream_wielder_speed : float;
}

let action : action_config =
  let vengeful_spirit_vx = 1200. *. window_scale in
  {
    soul_per_cast = 33;
    spell_twister_soul_per_cast = 24;
    soul_gained_per_nail = 11;
    attack_duration = 0.07;
    vengeful_spirit_vx;
    shade_soul_vx = vengeful_spirit_vx *. 1.125;
    vengeful_spirit_recoil = 80. *. window_scale;
    vengeful_spirit_duration = 1.5;
    quick_focus_speed = 0.5;
    dream_wielder_speed = 0.5;
  }

type physics_config = {
  gravity : float;
  jump_damping : float;
  jump_fall_threshold : float;
}

let physics =
  {
    gravity = 1800. *. window_scale;
    jump_damping = 0.8;
    jump_fall_threshold = -80. *. window_scale;
  }

type enemy = {
  death_recoil_vx : float;
  death_recoil_time : float;
  death_vy : float;
  multi_hit_damage_kinds : damage_kind list;
  multi_hit_cooldown : float;
}

let enemy =
  {
    death_recoil_vx = -100. *. window_scale;
    death_recoil_time = 0.2;
    death_vy = -500. *. window_scale;
    multi_hit_damage_kinds = [ VENGEFUL_SPIRIT; HOWLING_WRAITHS ];
    multi_hit_cooldown = 0.2;
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
  dialogue_config : Interaction.text_config;
  floating_config : Interaction.text_config;
  progress_config : Interaction.text_config;
  short_floating_duration : float;
  long_floating_duration : float;
}

let get_text_margins menu_choice =
  let main_menu_values = (500., 360., 0.) in
  let pause_menu_values = (250., 220., 300.) in
  let margin_x, margin_y_top, cursor_padding =
    match menu_choice with
    | PAUSE_MENU _
    | CHANGE_WEAPON_MENU _
    | CHANGE_GHOST_MENU _
    | SETTINGS_MENU _
    | CHANGE_AUDIO_SETTING _ ->
      pause_menu_values
    | SELECT_GAME_MODE _
    | MAIN_MENU _
    | CONFIRM_DELETE_MENU _
    | SAVE_FILES_MENU _ ->
      main_menu_values
  in
  (margin_x *. window_scale, margin_y_top *. window_scale, cursor_padding *. window_scale)

let text =
  let base_config : Interaction.text_config =
    {
      margin_x = 50. *. window_scale;
      margin_y_top = 20. *. window_scale;
      margin_y_bottom = 20. *. window_scale;
      padding = { x = 50. *. window_scale; y = 50. *. window_scale };
      cursor_padding = 0.;
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
    dialogue_config =
      {
        margin_x = 250. *. window_scale;
        margin_y_top = 50. *. window_scale;
        margin_y_bottom = 450. *. window_scale;
        padding = { x = 30. *. window_scale; y = 30. *. window_scale };
        cursor_padding = 0.;
        centered = false;
      };
    floating_config =
      {
        margin_x = 150. *. window_scale;
        margin_y_top = 50. *. window_scale;
        margin_y_bottom = window.h *. 0.75;
        padding = { x = 50. *. window_scale; y = 50. *. window_scale };
        cursor_padding = 0.;
        centered = true;
      };
    progress_config =
      {
        margin_x = 50. *. window_scale;
        margin_y_top = 50. *. window_scale;
        margin_y_bottom = 50. *. window_scale;
        padding = { x = 50. *. window_scale; y = 50. *. window_scale };
        cursor_padding = 0.;
        centered = true;
      };
    short_floating_duration = 2.;
    long_floating_duration = 3.;
  }

let get_plain_text_config margin_y_bottom =
  {
    text.base_config with
    margin_x = 150. *. window_scale;
    margin_y_top = 50. *. window_scale;
    margin_y_bottom;
  }

let get_menu_text_config menu_choice =
  let margin_x, margin_y_top, cursor_padding = get_text_margins menu_choice in
  let margin_y_bottom = margin_y_top in
  { text.base_config with margin_x; margin_y_top; margin_y_bottom; cursor_padding; centered = true }

type interactions = {
  duncan_initial_jump_vx : float;
  duncan_chair_jump_vx : float;
  troy_dive_jump_vx : float;
  troy_final_jump_vx : float;
  abed_shelves_jump_vx : float;
  britta_trash_can_jump_vx : float;
  shirley_island_camera_motion : float;
}

let interactions =
  {
    duncan_initial_jump_vx = -350. *. window_scale;
    duncan_chair_jump_vx = 150. *. window_scale;
    troy_dive_jump_vx = 900. *. window_scale;
    troy_final_jump_vx = 200. *. window_scale;
    abed_shelves_jump_vx = 300. *. window_scale;
    britta_trash_can_jump_vx = 300. *. window_scale;
    shirley_island_camera_motion = 3. *. window_scale;
  }

type other = {
  kp_start_x : float;
  kp_start_y : float;
  ss_respawn_y_offset : float;
  ss_warp_y_offset : float;
  main_menu_y_offset : float;
  hud_padding : float;
  progress_x_padding : float;
  progress_y_padding : float;
  save_slots : int;
  phantom_floor_h : float;
  raindrop_speed : float;
  raindrop_scale : float;
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
    progress_x_padding = 600. *. window_scale;
    progress_y_padding = 100. *. window_scale;
    save_slots = 4;
    phantom_floor_h = 20. *. window_scale;
    raindrop_speed = 3. *. window_scale;
    raindrop_scale = 4. *. window_scale;
  }

let lever_shape =
  make_shape
    [
      { x = 10. *. scale.room; y = 0. *. scale.room };
      { x = 5. *. scale.room; y = 8. *. scale.room };
      { x = 7. *. scale.room; y = 40. *. scale.room };
      { x = 12. *. scale.room; y = 40. *. scale.room };
      { x = 15. *. scale.room; y = 8. *. scale.room };
    ]

let random_fragment_vx ?(direction : direction option = None) () =
  let min, max =
    match direction with
    | None -> (-250., 250.)
    | Some LEFT -> (-500., 0.)
    | Some RIGHT -> (0., 500.)
    | Some d -> failwithf "random_fragment_vx invalid direction %s" (Show.direction d)
  in
  Random.float_between min max *. window_scale

let random_fragment_vy ?(direction : direction option = None) () =
  let min, max =
    match direction with
    | None ->
      (* most fragments should fly upward *)
      (-800., 200.)
    | Some UP -> (-500., 0.)
    | Some DOWN -> (0., 500.)
    | Some d -> failwithf "random_fragment_vy invalid direction %s" (Show.direction d)
  in
  Random.float_between min max *. window_scale

type debug_keys = {
  mutable n : bool;
  mutable t : bool;
  mutable zero : bool;
  mutable eight : bool;
  mutable nine : bool;
}

let debugs = { n = false; t = false; zero = false; eight = false; nine = true }
