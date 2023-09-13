open Types

[@@@ocaml.warning "-26-27-32"]

let save_file_path idx = fmt "saves/%d.json" idx

let new_game () : Json_t.save_file =
  let kings_pass_drop = { x = 1800.; y = 550. } in
  let b =
    (* newgame *)
    (* false *)
    (* newgameplus *)
    true
  in
  {
    ghost_id = "BRITTA";
    ghosts_in_party = [ "BRITTA" ];
    ghost_x = kings_pass_drop.x;
    ghost_y = kings_pass_drop.y;
    (* there aren't any hazards in king's pass for new games, so respawn_pos doesn't matter *)
    respawn_x = 0.;
    respawn_y = 0.;
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
    progress = [];
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
    match File.maybe_read (fmt "../%s" (save_file_path save_file_idx)) with
    | None -> (new_game (), true)
    | Some save_file -> (Json_j.save_file_of_string save_file, false)
  in
  { slot_1 = load_file 1; slot_2 = load_file 2; slot_3 = load_file 3; slot_4 = load_file 4 }

let init
    (save_file : Json_t.save_file)
    (global : global_cache)
    (area_musics : area_music list)
    (world : world)
    (save_file_slot : int) : game =
  let start_pos = { x = save_file.ghost_x; y = save_file.ghost_y } in

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

  let party : (ghost_id * party_ghost) list =
    let make_party_ghost id : ghost_id * party_ghost =
      let in_party = List.mem (Show.ghost_id id) save_file.ghosts_in_party in
      let config = make_head_textures id in
      (id, Player.init_party id config idle_texture { x = 1000.; y = 1000. } in_party)
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
        progress = save_file.progress;
        exits;
        enemy_configs = global.enemy_configs;
        npc_configs = global.npc_configs;
        pickup_indicator_texture = global.textures.pickup_indicator;
        lever_texture = global.textures.door_lever;
        respawn_pos = { x = save_file.respawn_x; y = save_file.respawn_y };
        platforms = global.textures.platforms;
      }
  in
  room.layers <- Tiled.Room.get_layer_tile_groups room room.progress.removed_idxs_by_layer;

  let music = List.find (fun am -> List.mem room.area.id am.areas) area_musics in

  let current_ghost_id = Player.parse_name save_file.ghost_id in
  let party_ghost = List.assoc current_ghost_id party in
  let player = make_ghost party_ghost in

  player.ghost.entity.update_pos <- true;
  Player.equip_weapon player save_file.current_weapon;

  {
    player;
    party;
    room;
    music;
    interaction =
      {
        steps = [];
        text = None;
        speaker_name = None;
        black_rects = [];
        corner_text = None;
        floating_text = None;
      };
    progress = save_file.progress;
    save_file_slot;
    debug_paused = false;
  }
