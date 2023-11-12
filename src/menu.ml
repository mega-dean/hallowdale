open Utils
open Types

let main_menu () : menu =
  { choices = [ MAIN_MENU START_GAME; MAIN_MENU QUIT ]; current_choice_idx = 0 }

let select_game_mode_menu save_file save_file_idx : menu =
  {
    choices =
      [
        (* TODO enable Classic mode *)
        (* SELECT_GAME_MODE (USE_MODE (CLASSIC, save_file, save_file_idx)); *)
        SELECT_GAME_MODE (USE_MODE (DEMO, save_file, save_file_idx));
        SELECT_GAME_MODE (USE_MODE (STEEL_SOLE, save_file, save_file_idx));
        SELECT_GAME_MODE BACK;
      ];
    current_choice_idx = 0;
  }

let pause_menu ghost_count : menu =
  {
    choices =
      [
        Some (PAUSE_MENU CONTINUE);
        (if ghost_count > 1 then Some (PAUSE_MENU CHANGE_GHOST) else None);
        Some (PAUSE_MENU CHANGE_WEAPON);
        Some (PAUSE_MENU SETTINGS);
        Some (PAUSE_MENU QUIT_TO_MAIN_MENU);
      ]
      |> List.filter_somes;
    current_choice_idx = 0;
  }

let settings_menu () : menu =
  {
    choices = [ SETTINGS_MENU MUSIC; SETTINGS_MENU SOUND_EFFECTS; SETTINGS_MENU BACK ];
    current_choice_idx = 0;
  }

let music_menu () : menu =
  {
    choices =
      [
        CHANGE_AUDIO_SETTING (MUSIC, INCREASE);
        CHANGE_AUDIO_SETTING (MUSIC, DECREASE);
        CHANGE_AUDIO_SETTING (MUSIC, BACK);
      ];
    current_choice_idx = 0;
  }

let sound_effects_menu () : menu =
  {
    choices =
      [
        CHANGE_AUDIO_SETTING (SOUND_EFFECTS, INCREASE);
        CHANGE_AUDIO_SETTING (SOUND_EFFECTS, DECREASE);
        CHANGE_AUDIO_SETTING (SOUND_EFFECTS, BACK);
      ];
    current_choice_idx = 0;
  }

let save_files_menu () : menu =
  {
    choices =
      [
        SAVE_FILES SLOT_1; SAVE_FILES SLOT_2; SAVE_FILES SLOT_3; SAVE_FILES SLOT_4; SAVE_FILES BACK;
      ];
    current_choice_idx = 0;
  }

let change_ghost_menu (ghosts : party_ghost list) : menu =
  let ghost_choices =
    List.filter_map
      (fun party_ghost ->
        if party_ghost.in_party then
          Some (CHANGE_GHOST_MENU (USE_GHOST party_ghost.ghost.id))
        else
          None)
      ghosts
  in
  { choices = ghost_choices @ [ CHANGE_GHOST_MENU BACK ]; current_choice_idx = 0 }

let change_weapon_menu (weapon_names : string list) : menu =
  let weapon_choices = List.map (fun name -> CHANGE_WEAPON_MENU (EQUIP_WEAPON name)) weapon_names in
  { choices = weapon_choices @ [ CHANGE_WEAPON_MENU BACK ]; current_choice_idx = 0 }

let update_menu_choice (menu : menu) state =
  if state.frame_inputs.down.pressed then (
    Audio.play_sound state "click";
    menu.current_choice_idx <- Int.min (menu.current_choice_idx + 1) (List.length menu.choices - 1));
  if state.frame_inputs.up.pressed then (
    Audio.play_sound state "click";
    menu.current_choice_idx <- Int.max 0 (menu.current_choice_idx - 1))

let update_pause_menu (game : game) (state : state) : state =
  if state.frame_inputs.pause.pressed then (
    match state.pause_menu with
    | None ->
      Audio.play_sound state "menu-expand";
      state.pause_menu <-
        Some (MENU (pause_menu (List.length (Player.ghost_ids_in_party game.party))))
    | Some _ ->
      Audio.play_sound state "menu-close";
      state.pause_menu <- None)
  else if state.frame_inputs.open_map.pressed then (
    match state.pause_menu with
    | None ->
      Audio.play_sound state "menu-expand";
      let on_map x offset = (x /. Config.world_map.scale) +. offset in
      let room_pos_on_map x y =
        let x', y' =
          (on_map x Config.world_map.room_x_offset, on_map y Config.world_map.room_y_offset)
        in

        { x = x' *. Config.window.scale; y = y' *. Config.window.scale }
      in
      let black_rects =
        let rooms_found = List.map fst game.progress.by_room in
        let maybe_block_room (room_id, room_location) =
          if game.room.id = room_id || List.mem room_location.filename rooms_found then
            None
          else
            Some
              {
                pos = room_pos_on_map room_location.global_x room_location.global_y;
                w = ((room_location.w /. Config.world_map.scale) +. 2.) *. Config.window.scale;
                h = ((room_location.h /. Config.world_map.scale) +. 2.) *. Config.window.scale;
              }
        in
        List.filter_map maybe_block_room state.world
      in
      let ghost_pos =
        let room_pos =
          let room_location = List.assoc game.room.id state.world in
          room_pos_on_map room_location.global_x room_location.global_y
        in
        let entity_pos = game.player.ghost.entity.dest.pos in
        {
          x = on_map (entity_pos.x *. Config.window.scale) room_pos.x;
          y = on_map (entity_pos.y *. Config.window.scale) room_pos.y;
        }
      in
      state.pause_menu <- Some (WORLD_MAP { black_rects; ghost_pos })
    | Some _ ->
      Audio.play_sound state "menu-close";
      state.pause_menu <- None);

  (match state.pause_menu with
  | None -> ()
  | Some (WORLD_MAP rects) ->
    (* the map is closed by unpausing, so there's nothing to do here *)
    ()
  | Some (MENU menu) ->
    update_menu_choice menu state;

    if state.frame_inputs.jump.pressed then (
      (* TODO this plays the confirm sound even when selecting "Back" *)
      Audio.play_sound state "confirm";
      match List.nth menu.choices menu.current_choice_idx with
      | PAUSE_MENU CONTINUE ->
        state.pause_menu <- None;
        game.interaction.text <- None
      | PAUSE_MENU CHANGE_WEAPON ->
        state.pause_menu <- Some (MENU (change_weapon_menu (List.map fst game.player.weapons)))
      | PAUSE_MENU CHANGE_GHOST -> state.pause_menu <- Some (MENU (change_ghost_menu game.party))
      | PAUSE_MENU QUIT_TO_MAIN_MENU ->
        Audio.reset_music game.music.music;
        Audio.reset_music state.menu_music;
        (* TODO unload textures *)
        state.pause_menu <- None;
        Game.save game state ~after_fn:(fun state ->
            state.game_context <- MAIN_MENU (main_menu (), Game.load_all_save_slots ()))
      | CHANGE_WEAPON_MENU (EQUIP_WEAPON weapon_name) -> Player.equip_weapon game.player weapon_name
      | CHANGE_GHOST_MENU (USE_GHOST ghost_id) ->
        if game.player.ghost.id <> ghost_id then
          Player.swap_current_ghost state game ghost_id
      | SETTINGS_MENU BACK
      | CHANGE_GHOST_MENU BACK
      | CHANGE_WEAPON_MENU BACK ->
        state.pause_menu <-
          Some (MENU (pause_menu (List.length (Player.ghost_ids_in_party game.party))))
      | PAUSE_MENU SETTINGS -> state.pause_menu <- Some (MENU (settings_menu ()))
      | SETTINGS_MENU MUSIC -> state.pause_menu <- Some (MENU (music_menu ()))
      | SETTINGS_MENU SOUND_EFFECTS -> state.pause_menu <- Some (MENU (sound_effects_menu ()))
      | CHANGE_AUDIO_SETTING (MUSIC, INCREASE) -> Audio.increase_music_volume state game
      | CHANGE_AUDIO_SETTING (MUSIC, DECREASE) -> Audio.decrease_music_volume state game
      | CHANGE_AUDIO_SETTING (SOUND_EFFECTS, INCREASE) -> Audio.increase_sound_effects_volume state
      | CHANGE_AUDIO_SETTING (SOUND_EFFECTS, DECREASE) -> Audio.decrease_sound_effects_volume state
      | CHANGE_AUDIO_SETTING (_, BACK) -> state.pause_menu <- Some (MENU (settings_menu ()))
      | c -> failwithf "unhandled menu choice: %s" (Show.menu_choice (Some game) c)));
  state

let update_main_menu (menu : menu) (save_slots : save_slots) (state : state) : state =
  update_menu_choice menu state;

  let load_file save_file_idx =
    let (save_file, is_new_game) : Json_t.save_file * bool =
      match save_file_idx with
      | 1 -> save_slots.slot_1
      | 2 -> save_slots.slot_2
      | 3 -> save_slots.slot_3
      | 4 -> save_slots.slot_4
      | _ -> failwithf "bad save file idx: %d" save_file_idx
    in
    if is_new_game then
      state.game_context <- MAIN_MENU (select_game_mode_menu save_file save_file_idx, save_slots)
    else (
      let game = Game.load state save_file save_file_idx in
      (match game.mode with
      | CLASSIC
      | DEMO ->
        ()
      | STEEL_SOLE ->
        (* this prevents an immediate damage respawn from landing on the bench *)
        Player.add_phantom_floor game game.player.ghost.entity.dest.pos);
      Game.start ~is_new_game state game save_file)
  in

  let start_new_game game_mode save_file save_file_idx =
    let game = Game.create_new state game_mode save_file save_file_idx in
    Game.start state game save_file
  in

  if state.frame_inputs.jump.pressed then (
    Audio.play_sound state "confirm";
    match List.nth menu.choices menu.current_choice_idx with
    | MAIN_MENU START_GAME -> state.game_context <- SAVE_FILES (save_files_menu (), save_slots)
    | MAIN_MENU QUIT ->
      print "exiting";
      exit 0
    | SAVE_FILES SLOT_1 -> load_file 1
    | SAVE_FILES SLOT_2 -> load_file 2
    | SAVE_FILES SLOT_3 -> load_file 3
    | SAVE_FILES SLOT_4 -> load_file 4
    | SAVE_FILES BACK -> state.game_context <- MAIN_MENU (main_menu (), save_slots)
    | SELECT_GAME_MODE (USE_MODE (mode, save_file, save_file_idx)) ->
      let save_file' =
        match mode with
        | CLASSIC -> save_file
        | DEMO
        | STEEL_SOLE ->
          Game.initialize_steel_sole save_file
      in
      start_new_game mode save_file' save_file_idx
    | SELECT_GAME_MODE BACK -> state.game_context <- SAVE_FILES (save_files_menu (), save_slots)
    | _ -> failwith "update_main_menu - needs MAIN_MENU menu.choices");
  state
