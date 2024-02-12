open Utils
open Types

let empty_save_file () : Json_t.save_file =
  let kings_pass_drop = { x = Config.other.kp_start_x; y = Config.other.kp_start_y } in
  {
    (* this is only needed because these stub save files are created before game mode is chosen *)
    game_mode = Show.game_mode CLASSIC;
    ghost_id = "BRITTA";
    ghosts_in_party = [ "BRITTA" ];
    ghost_x = kings_pass_drop.x;
    ghost_y = kings_pass_drop.y;
    respawn_x = kings_pass_drop.x;
    respawn_y = kings_pass_drop.y;
    room_name = "forgotten_deans-pass";
    abilities =
      {
        (* movement *)
        crystal_heart = false;
        dream_nail = false;
        ismas_tear = false;
        mantis_claw = false;
        monarch_wings = false;
        mothwing_cloak = false;
        shade_cloak = false;
        (* spells *)
        vengeful_spirit = false;
        desolate_dive = false;
        howling_wraiths = false;
        shade_soul = false;
        descending_dark = false;
        abyss_shriek = false;
        quick_focus = false;
        soul_catcher_bonus = 0;
        dream_wielder = false;
        deep_focus = false;
        shaman_stone = false;
        spell_twister = false;
      };
    progress =
      {
        frame_idx = 0;
        keys_found = [];
        purple_pens_found = [];
        dreamer_items_found = 0;
        by_room = [];
        steel_sole = { dunks = 0; c_dashes = 0 };
        last_upgrade_claimed = 0;
      };
    max_health = Config.ghost.starting_max_health;
    max_soul = Config.ghost.starting_max_soul;
    weapons =
      [
        "Old Nail";
        (* "Devil's Drench XJ-11"; *)
        (* "Bush Scissors"; *)
        (* "Limpken Wrench"; *)
        (* "Orange Paintball Gun"; *)
        (* "Pickle Magnet"; *)
        (* "Sword of Duquesne"; *)
        (* "Switch"; *)
        (* "Quantum Spanner"; *)
      ];
    current_weapon = "Old Nail";
  }

let load_all_save_slots () : save_slot list =
  let load_file save_file_idx : save_slot =
    match File.maybe_read (File.save_file_path save_file_idx) with
    | None -> { file = empty_save_file (); new_game = true }
    | Some save_file -> { file = Json_j.save_file_of_string save_file; new_game = false }
  in
  List.map (fun i -> load_file (i + 1)) (Int.range Config.other.save_slots)
(* { slot_1 = load_file 1; slot_2 = load_file 2; slot_3 = load_file 3; slot_4 = load_file 4 } *)

let save ?(after_fn = ignore) (game : game) (state : state) =
  Room.save_progress_to_game game;
  game.room.respawn.target <-
    (match game.mode with
    | CLASSIC
    | DEMO ->
      clone_vector game.room.respawn.target
    | STEEL_SOLE ->
      {
        game.room.respawn.target with
        y = game.room.respawn.target.y -. Config.other.ss_respawn_y_offset;
      });
  let save_file : Json_t.save_file =
    {
      game_mode = Show.game_mode game.mode;
      ghost_id = Show.ghost_id game.player.ghost.id;
      ghosts_in_party =
        (* game.players only has the uncontrolled ghosts, but save_file.ghosts_in_party
           should include the current ghosts id
        *)
        [ game.player.ghost.id ] @ Player.ghost_ids_in_party game.party
        |> List.map Show.ghost_id
        |> List.uniq;
      ghost_x = game.player.ghost.entity.dest.pos.x /. Config.window.scale;
      ghost_y = game.player.ghost.entity.dest.pos.y /. Config.window.scale;
      respawn_x = game.room.respawn.target.x /. Config.window.scale;
      respawn_y = game.room.respawn.target.y /. Config.window.scale;
      room_name = Room.get_filename game.room;
      abilities = game.player.abilities;
      progress = clone_game_progress game.progress;
      weapons = game.player.weapons |> String.Map.to_list |> List.map fst;
      current_weapon = game.player.current_weapon.name;
      max_health = game.player.health.max;
      max_soul = game.player.soul.max;
    }
  in
  save_file.progress.frame_idx <- state.frame.idx;
  let contents = Json_j.string_of_save_file save_file |> Yojson.Safe.prettify in
  let written = File.write (File.save_file_path game.save_file_slot) contents in
  if written then (
    game.save_file <- save_file;
    after_fn state)
  else
    failwith "error when trying to save"

let initialize_steel_sole ~with_keys (save_file : Json_t.save_file) =
  {
    save_file with
    progress =
      {
        save_file.progress with
        keys_found =
          (if with_keys then
             [
               "Dean's Closet Key";
               "Dean's Brand";
               "Song 127";
               "Laybourne's Breathprint";
               "Kentucky Fried Chicken Eleven Herbs and Space Experience Pass";
             ]
           else
             []);
        by_room =
          [
            ( "forgotten_a",
              {
                removed_tile_idxs = [];
                removed_platform_ids = [];
                (* add this so the warp from teacher's lounge to cpu wing is always available
                   - TODO this causes forgotten_a to always show up on the world map for steel sole
                *)
                finished_interactions = [ "cutscene:arrive-at-shirley-island" ];
                revealed_shadow_layers = [];
              } );
          ];
      };
    ghosts_in_party = List.map Show.ghost_id [ ABED; ANNIE; BRITTA; JEFF; TROY ];
    abilities =
      {
        crystal_heart = true;
        dream_nail = true;
        ismas_tear = true;
        mantis_claw = true;
        monarch_wings = true;
        mothwing_cloak = true;
        vengeful_spirit = true;
        howling_wraiths = true;
        desolate_dive = true;
        (* not enabling shade_cloak now because it isn't used for any obstacles and it looks bad *)
        shade_cloak = false;
        shade_soul = false;
        descending_dark = false;
        abyss_shriek = false;
        quick_focus = false;
        soul_catcher_bonus = 0;
        dream_wielder = false;
        deep_focus = false;
        shaman_stone = false;
        spell_twister = false;
      };
  }

let start ?(is_new_game = true) (state : state) (game : game) (save_file : Json_t.save_file) =
  if is_new_game then (
    state.screen_fade <-
      Some
        {
          target_alpha = 255;
          timer = None;
          show_ghost = false;
        };
    let interaction_name =
      match game.mode with
      | STEEL_SOLE -> "ss-opening-poem"
      | CLASSIC
      | DEMO ->
        "opening-poem"
    in
    let trigger : trigger = make_stub_trigger INFO "cutscene" interaction_name in
    Player.maybe_begin_interaction state game trigger)
  else (
    state.frame.idx <- save_file.progress.frame_idx;
    try
      let timeout = Unix.getenv "HALLOWDALE_TIMEOUT" |> String.to_int in
      state.frame.timeout <- save_file.progress.frame_idx + timeout
    with
    | Not_found -> ());
  Audio.stop_music state.menu_music.t;
  state.game_context <- IN_PROGRESS game

let create
    (state : state)
    (mode : game_mode)
    (save_file : Json_t.save_file)
    (global : global_cache)
    (area_musics : area_music list)
    (world : world)
    (save_file_slot : int) : game =
  let start_pos =
    { x = save_file.ghost_x *. Config.window.scale; y = save_file.ghost_y *. Config.window.scale }
  in
  let ghosts_file : ghosts_file = Player.read_config () in
  let use_json_config ghost_id pose_name =
    Sprite.build_texture_from_path
      { asset_dir = GHOSTS; character_name = Show.ghost_id ghost_id; pose_name }
  in

  let make_head_textures ghost_id : ghost_head_textures =
    {
      look_down = use_json_config ghost_id "look-down";
      look_up = use_json_config ghost_id "look-up";
      idle = use_json_config ghost_id "idle";
      read = use_json_config ghost_id "read";
      take_damage = use_json_config ghost_id "take-damage";
      walk = use_json_config ghost_id "walk";
      wall_slide = use_json_config ghost_id "wall-slide";
    }
  in

  let idle_texture = global.textures.ghost_bodies.idle in
  let shared_ghost_textures = Player.load_shared_textures ghosts_file.shared_textures in
  let make_ghost (party_ghost : party_ghost) : player =
    Player.init party_ghost.ghost.id idle_texture party_ghost.ghost.head_textures
      ghosts_file.actions (clone_vector start_pos) save_file global.weapons shared_ghost_textures
  in

  let party : party_ghost list =
    let make_party_ghost id : party_ghost =
      let in_party = List.mem (Show.ghost_id id) save_file.ghosts_in_party in
      let config = make_head_textures id in
      Player.init_party id config idle_texture ~start_pos:{ x = 1000.; y = 1000. } in_party
    in
    List.map make_party_ghost [ BRITTA; ABED; TROY; ANNIE; JEFF ]
  in

  let _, room_id = Tiled.parse_room_filename "Game.init" save_file.room_name in
  let room_location = List.assoc room_id world in
  let exits = Tiled.JsonRoom.get_exits room_location in
  let room =
    Room.init
      {
        file_name = save_file.room_name;
        progress_by_room = save_file.progress.by_room |> List.to_string_map;
        exits;
        enemy_configs = global.enemy_configs;
        npc_configs = global.npc_configs;
        pickup_indicator_texture = global.textures.pickup_indicator;
        lever_texture = global.textures.door_lever;
        raindrop_texture = global.textures.raindrop;
        respawn_pos =
          {
            x = save_file.respawn_x *. Config.window.scale;
            y = save_file.respawn_y *. Config.window.scale;
          };
        platforms = global.textures.platforms;
      }
  in

  Room.reset_tile_groups room;

  let music = Audio.get_area_music room.area.id area_musics in

  let current_ghost_id = Player.parse_name save_file.ghost_id in
  let party_ghost = Option.get (Player.find_party_ghost current_ghost_id party) in
  let player = make_ghost party_ghost in

  Entity.unfreeze player.ghost.entity;
  Player.equip_weapon player save_file.current_weapon;

  {
    mode;
    player;
    party;
    room;
    room_changed_last_frame = false;
    in_boss_fight = false;
    reflection_x = None;
    music;
    interaction =
      {
        steps = [];
        text = None;
        speaker_name = None;
        corner_text = None;
        floating_text = None;
        use_dashes_in_archives = None;
      };
    progress = clone_game_progress save_file.progress;
    save_file;
    save_file_slot;
  }

let init state mode (save_file : Json_t.save_file) save_file_idx =
  let game = create state mode save_file state.global state.area_musics state.world save_file_idx in
  (* update the camera when a file is loaded so the ghost doesn't start too far offscreen *)
  let _ = Camera.tick game state in
  state.camera.update_instantly <- true;
  game

let load state (save_file : Json_t.save_file) save_file_idx =
  let mode =
    match save_file.game_mode with
    | "Classic" -> CLASSIC
    | "Steel Sole" -> STEEL_SOLE
    | "Demo" -> DEMO
    | _ -> failwithf "invalid game_mode in save_file %d" save_file_idx
  in
  init state mode save_file save_file_idx

let create_new state mode (save_file : Json_t.save_file) save_file_idx =
  init state mode save_file save_file_idx
