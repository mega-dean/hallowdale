open Types

[@@@ocaml.warning "-26-27-32"]

let save_file_path idx = fmt "saves/%d.json" idx

let new_game () : Json_t.save_file =
  let kings_pass_drop = { x = 1800.; y = 100. } in
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
        (* mothwing_cloak = true;
         * mantis_claw = true;
         * crystal_heart = true;
         * monarch_wings = true;
         * shade_cloak = true;
         * ismas_tear = true; *)

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
    (area_musics : area_music list)
    (world : world)
    (save_file_slot : int) : game =
  let start_pos = { x = save_file.ghost_x; y = save_file.ghost_y } in

  let ghosts_file : ghosts_file = Ghost.read_config () in
  let use_json_config configs pose_name =
    let config =
      match List.find_opt (fun (tc : texture_config) -> tc.path.pose_name = pose_name) configs with
      | None ->
        let ghost_name = (List.nth configs 0).path.character_name in
        failwithf "could not find pose config in ghosts/config.json for '%s' for ghost %s" pose_name
          ghost_name
      | Some v -> v
    in
    Sprite.build_texture_from_config config
  in

  let britta_head_configs = List.assoc BRITTA ghosts_file.head_textures_by_ghost in
  let jeff_head_configs = List.assoc JEFF ghosts_file.head_textures_by_ghost in
  let abed_head_configs = List.assoc ABED ghosts_file.head_textures_by_ghost in
  let troy_head_configs = List.assoc TROY ghosts_file.head_textures_by_ghost in
  let annie_head_configs = List.assoc ANNIE ghosts_file.head_textures_by_ghost in

  let load_head_texture name =
    let configs =
      (* TODO add other heads *)
      britta_head_configs
    in
    use_json_config configs name
  in

  let britta_head_textures : ghost_head_textures =
    {
      cast = load_head_texture "cast";
      look_down = load_head_texture "look-down";
      look_up = load_head_texture "look-up";
      idle = load_head_texture "idle";
      read = load_head_texture "read";
      take_damage = load_head_texture "take-damage";
      walk = load_head_texture "walk";
      wall_slide = load_head_texture "wall-slide";
    }
  in

  let idle_texture = global.textures.ghost_bodies.idle.texture' in
  let shared_ghost_textures = Ghost.load_shared_textures ghosts_file.shared_textures in
  let make_ghost (party_ghost : party_ghost) : ghost =
    (* TODO add other heads *)
    Ghost.init party_ghost.ghost'.id idle_texture party_ghost.ghost'.head_textures
      ghosts_file.actions (clone_vector start_pos) save_file global.weapons shared_ghost_textures
  in

  let ghosts' : (ghost_id * party_ghost) list =
    let make_party_ghost id config : ghost_id * party_ghost =
      let in_party = List.mem (Show.ghost_id id) save_file.ghosts_in_party in
      (id, Ghost.init_party id config idle_texture { x = 1000.; y = 1000. } in_party)
    in
    [
      make_party_ghost BRITTA britta_head_textures;
      make_party_ghost ABED britta_head_textures;
      make_party_ghost TROY britta_head_textures;
      make_party_ghost ANNIE britta_head_textures;
      make_party_ghost JEFF britta_head_textures;
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
        platforms = global.textures.platforms;
      }
  in
  room.layers <- Tiled.Room.get_layer_tile_groups room room.progress.removed_idxs_by_layer;

  let music = List.find (fun am -> List.mem room.area.id am.areas) area_musics in

  let current_ghost_id = Ghost.parse_name save_file.ghost_id in
  let party_ghost = List.assoc current_ghost_id ghosts' in
  let ghost = make_ghost party_ghost in

  ghost.ghost'.entity.update_pos <- true;
  Ghost.equip_weapon ghost save_file.current_weapon;

  {
    ghost;
    ghosts';
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
