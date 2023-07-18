open Types

[@@@ocaml.warning "-26-27-32"]

let save_file_path idx = fmt "saves/%d.json" idx

let new_game () : Json_t.save_file =
  let kings_pass_drop = { x = 1800.; y = 100. } in
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
        mothwing_cloak = false;
        mantis_claw = false;
        crystal_heart = false;
        monarch_wings = true;
        shade_cloak = false;
        ismas_tear = false;
        (* spells *)
        vengeful_spirit = false;
        desolate_dive = false;
        howling_wraiths = false;
      };
    progress = [];
    weapons =
      [
        "Old Nail";
        (* "Devil's Drench XJ-11"; *)
        (* "Bush Scissors"; *)
        (* "Limpken Wrench"; *)
        "Orange Paintball Gun";
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
    (world : world)
    (save_file_slot : int) : game =
  let start_pos = { x = save_file.ghost_x; y = save_file.ghost_y } in

  let ghosts_file : ghosts_file = Ghost.read_config () in
  let use_json_config configs pose_name =
    let config =
      match List.find_opt (fun (tc : texture_config) -> tc.pose_name = pose_name) configs with
      | None ->
        let ghost_name = (List.nth configs 0).character_name in
        failwithf "could not find pose config in ghosts/config.json for '%s' for ghost %s" pose_name
          ghost_name
      | Some v -> v
    in
    Sprite.build_texture_from_config config
  in

  let britta_configs = List.assoc BRITTA ghosts_file.textures in
  let jeff_configs = List.assoc JEFF ghosts_file.textures in
  let abed_configs = List.assoc ABED ghosts_file.textures in
  let troy_configs = List.assoc TROY ghosts_file.textures in
  let annie_configs = List.assoc ANNIE ghosts_file.textures in

  let britta_ghost_textures : ghost_textures =
    {
      cast = use_json_config britta_configs "cast";
      crawl = use_json_config britta_configs "crawl";
      dash = use_json_config britta_configs "dash";
      dive = use_json_config britta_configs "dive";
      fall = use_json_config britta_configs "fall";
      flap = use_json_config britta_configs "flap";
      focus = use_json_config britta_configs "focus";
      idle = use_json_config britta_configs "idle";
      jump = use_json_config britta_configs "jump";
      nail = use_json_config britta_configs "nail";
      read = use_json_config britta_configs "read";
      take_damage = use_json_config britta_configs "take-damage";
      walk = use_json_config britta_configs "walk";
      wall_slide = use_json_config britta_configs "wall-slide";
    }
  in

  let shared_ghost_textures = Ghost.load_shared_textures ghosts_file.shared_textures in
  let make_ghost id (party_ghost : party_ghost) : ghost =
    Ghost.init id party_ghost.textures ghosts_file.actions (clone_vector start_pos) save_file
      global.weapons shared_ghost_textures
  in

  let ghosts' : (ghost_id * party_ghost) list =
    let make_party_ghost id config : ghost_id * party_ghost =
      let in_party = List.mem (Show.ghost_id id) save_file.ghosts_in_party in
      (id, Ghost.init_party id config { x = 1000.; y = 1000. } in_party)
    in
    [
      make_party_ghost BRITTA britta_ghost_textures;
      make_party_ghost ABED
        {
          britta_ghost_textures with
          dash = use_json_config abed_configs "dash";
          fall = use_json_config abed_configs "fall";
          idle = use_json_config abed_configs "idle";
          jump = use_json_config abed_configs "jump";
          walk = use_json_config abed_configs "walk";
        };
      make_party_ghost TROY
        {
          britta_ghost_textures with
          dash = use_json_config troy_configs "dash";
          dive = use_json_config troy_configs "dive";
          fall = use_json_config troy_configs "fall";
          idle = use_json_config troy_configs "idle";
          jump = use_json_config troy_configs "jump";
          walk = use_json_config troy_configs "walk";
        };
      make_party_ghost ANNIE
        {
          britta_ghost_textures with
          idle = use_json_config annie_configs "idle";
          nail = use_json_config annie_configs "nail";
          walk = use_json_config annie_configs "walk";
        };
      make_party_ghost JEFF
        {
          britta_ghost_textures with
          crawl = use_json_config jeff_configs "crawl";
          fall = use_json_config jeff_configs "fall";
          idle = use_json_config jeff_configs "idle";
          jump = use_json_config jeff_configs "jump";
          nail = use_json_config jeff_configs "nail";
          walk = use_json_config jeff_configs "walk";
        };
    ]
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
      }
  in
  room.layers <- Tiled.Room.get_layer_tile_groups room room.progress.removed_idxs_by_layer;

  let current_ghost_id = Ghost.parse_name save_file.ghost_id in
  let party_ghost = List.assoc current_ghost_id ghosts' in
  let ghost = make_ghost current_ghost_id party_ghost in
  ghost.entity.update_pos <- true;
  Ghost.equip_weapon ghost save_file.current_weapon;

  {
    ghost;
    ghosts';
    room;
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
