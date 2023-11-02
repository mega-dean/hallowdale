open Utils
open Types

let save_file_path idx = File.make_root_path [ "saves"; fmt "%d.json" idx ]

let empty_save_file () : Json_t.save_file =
  let kings_pass_drop = { x = Config.other.kp_start_x; y = Config.other.kp_start_y } in
  let b =
    (* newgame *)
    false
    (* newgameplus *)
    (* true *)
  in
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
        mothwing_cloak = b;
        mantis_claw = b;
        crystal_heart = b;
        monarch_wings = b;
        shade_cloak = b;
        ismas_tear = b;
        (* spells *)
        vengeful_spirit = b;
        desolate_dive = b;
        howling_wraiths = b;
      };
    progress =
      {
        by_room = [];
        steel_sole = { purple_pens_found = []; dunks = 0; c_dashes = 0; frame_idx = 0 };
      };
    weapons =
      [
        "Old Nail";
        (* "Devil's Drench XJ-11"; *)
        (* "Bush Scissors"; *)
        (* "Limpken Wrench"; *)
        (* "Orange Paintball Gun"; *)
        (* "Pickle Magnet"; *)
        (* "Quantum Spanner"; *)
      ];
    current_weapon = "Old Nail";
  }

let load_all_save_slots () : save_slots =
  let load_file save_file_idx : Json_t.save_file * bool =
    match File.maybe_read (save_file_path save_file_idx) with
    | None -> (empty_save_file (), true)
    | Some save_file -> (Json_j.save_file_of_string save_file, false)
  in
  { slot_1 = load_file 1; slot_2 = load_file 2; slot_3 = load_file 3; slot_4 = load_file 4 }

let save ?(after_fn = ignore) (game : game) (state : state) =
  Room.save_progress game;
  game.room.respawn_pos <-
    (match game.mode with
    | CLASSIC
    | DEMO ->
      clone_vector game.player.ghost.entity.dest.pos
    | STEEL_SOLE ->
      {
        game.player.ghost.entity.dest.pos with
        y = game.player.ghost.entity.dest.pos.y -. Config.other.ss_respawn_y_offset;
      });
  let save_file : Json_t.save_file =
    let ghosts' = game.party in
    {
      game_mode = Show.game_mode game.mode;
      ghost_id = Show.ghost_id game.player.ghost.id;
      ghosts_in_party =
        (* game.players only has the uncontrolled ghosts, but save_file.ghosts_in_party
           should include the current ghosts id
        *)
        [ game.player.ghost.id ] @ Player.ghost_ids_in_party ghosts'
        |> List.map Show.ghost_id
        |> List.uniq;
      ghost_x = game.player.ghost.entity.dest.pos.x /. Config.window.scale;
      ghost_y = game.player.ghost.entity.dest.pos.y /. Config.window.scale;
      respawn_x = game.room.respawn_pos.x /. Config.window.scale;
      respawn_y = game.room.respawn_pos.y /. Config.window.scale;
      room_name = Tiled.Room.get_filename game.room;
      abilities = game.player.abilities;
      progress =
        {
          game.progress with
          steel_sole = { game.progress.steel_sole with frame_idx = state.frame.idx };
        };
      (* { by_room = game.progress.by_room; steel_sole = game.progress.steel_sole }; *)
      weapons = List.map fst game.player.weapons;
      current_weapon = game.player.current_weapon.name;
    }
  in
  let save_file_path = File.make_root_path [ "saves"; fmt "%d.json" game.save_file_slot ] in
  let contents = Json_j.string_of_save_file save_file |> Yojson.Safe.prettify in
  let written = File.write save_file_path contents in
  if written then
    after_fn state
  else
    failwith "error when trying to save"

let initialize_steel_sole (save_file : Json_t.save_file) =
  save_file.ghosts_in_party <- List.map Show.ghost_id [ ABED; ANNIE; BRITTA; JEFF; TROY ];

  (* TODO not enabling shade_cloak now because it isn't used for any obstacles and it looks bad *)
  save_file.abilities.crystal_heart <- true;
  save_file.abilities.ismas_tear <- true;
  save_file.abilities.mantis_claw <- true;
  save_file.abilities.monarch_wings <- true;
  save_file.abilities.mothwing_cloak <- true;

  (* no dive because it can cause a soft-lock in water/acid (because it waits for current_floor
     to know when it is done diving)
  *)
  save_file.abilities.vengeful_spirit <- true;
  save_file.abilities.howling_wraiths <- true;
  ()

let start ?(is_new_game = true) (state : state) (game : game) (save_file : Json_t.save_file) =
  if is_new_game then (
    Entity.freeze game.player.ghost.entity;
    state.screen_fade <- Some 255;
    let trigger : trigger = make_stub_trigger INFO "cutscene" "opening-poem" in
    Player.maybe_begin_interaction state game trigger)
  else
    state.frame.idx <- save_file.progress.steel_sole.frame_idx;
  Raylib.stop_music_stream state.menu_music.t;
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
    (* PERF try using build_texture_from_image *)
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

  let idle_texture = global.textures.ghost_bodies.idle.texture' in
  let shared_ghost_textures = Player.load_shared_textures ghosts_file.shared_textures in
  let make_ghost (party_ghost : party_ghost) : player =
    Player.init party_ghost.ghost.id idle_texture party_ghost.ghost.head_textures
      ghosts_file.actions (clone_vector start_pos) save_file global.weapons shared_ghost_textures
  in

  let party : party_ghost list =
    let make_party_ghost id : party_ghost =
      let in_party = List.mem (Show.ghost_id id) save_file.ghosts_in_party in
      let config = make_head_textures id in
      Player.init_party id config idle_texture { x = 1000.; y = 1000. } in_party
    in
    List.map make_party_ghost [ BRITTA; ABED; TROY; ANNIE; JEFF ]
  in

  let _, room_id = Tiled.parse_room_filename "Game.init" save_file.room_name in
  let room_location = List.assoc room_id world in
  let exits = Tiled.Room.get_exits room_location in
  let room : room =
    Room.init
      {
        file_name = save_file.room_name;
        progress_by_room = save_file.progress.by_room;
        exits;
        enemy_configs = global.enemy_configs;
        npc_configs = global.npc_configs;
        pickup_indicator_texture = global.textures.pickup_indicator;
        lever_texture = global.textures.door_lever;
        respawn_pos =
          {
            x = save_file.respawn_x *. Config.window.scale;
            y = save_file.respawn_y *. Config.window.scale;
          };
        platforms = global.textures.platforms;
      }
  in
  Tiled.Room.reset_tile_groups room;

  let music = List.find (fun am -> List.mem room.area.id am.areas) area_musics in

  let current_ghost_id = Player.parse_name save_file.ghost_id in
  let party_ghost = Option.get (Player.find_party_ghost current_ghost_id party) in
  let player = make_ghost party_ghost in

  player.ghost.entity.update_pos <- true;
  Player.equip_weapon player save_file.current_weapon;

  {
    mode;
    player;
    party;
    room;
    music;
    interaction =
      { steps = []; text = None; speaker_name = None; corner_text = None; floating_text = None };
    progress = { by_room = save_file.progress.by_room; steel_sole = save_file.progress.steel_sole };
    save_file_slot;
    debug_paused = false;
    debug_safe_ss = false;
  }

let init state ?(mode = None) (save_file : Json_t.save_file) save_file_idx =
  let mode' =
    match mode with
    | Some m -> m
    | None -> (
      match save_file.game_mode with
      | "Classic" -> CLASSIC
      | "Steel Sole" -> STEEL_SOLE
      | "Demo" -> DEMO
      | _ -> failwithf "invalid game_mode in save_file %d" save_file_idx)
  in
  let game =
    create state mode' save_file state.global state.area_musics state.world save_file_idx
  in
  state.camera.update_instantly <- true;
  state.camera.raylib <-
    (* update the camera when a file is loaded so the ghost doesn't start too far offscreen
       TODO can maybe improve this, since it can still be off if the camera is bounded
    *)
    Tiled.create_camera_at
      (Raylib.Vector2.create game.player.ghost.entity.dest.pos.x game.player.ghost.entity.dest.pos.y)
      0. Config.window.center.x Config.window.center.y;
  game
