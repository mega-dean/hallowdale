open Utils
open Types

let make_menu choices : menu = { choices; current_choice_idx = 0 }
let main_menu () : menu = make_menu [ MAIN_MENU START_GAME; MAIN_MENU QUIT ]

let select_game_mode_menu save_file save_file_idx : menu =
  make_menu
    [
      SELECT_GAME_MODE (USE_MODE (CLASSIC, save_file, save_file_idx));
      SELECT_GAME_MODE (USE_MODE (DEMO, save_file, save_file_idx));
      SELECT_GAME_MODE (USE_MODE (STEEL_SOLE, save_file, save_file_idx));
      SELECT_GAME_MODE BACK;
    ]

let pause_menu ghost_count : menu =
  make_menu
    ([
       Some (PAUSE_MENU CONTINUE);
       (if ghost_count > 1 then Some (PAUSE_MENU CHANGE_GHOST) else None);
       Some (PAUSE_MENU CHANGE_WEAPON);
       Some (PAUSE_MENU SETTINGS);
       Some (PAUSE_MENU SAVE);
       Some (PAUSE_MENU QUIT_TO_MAIN_MENU);
     ]
    |> List.filter_somes)

let settings_menu () : menu =
  make_menu [ SETTINGS_MENU MUSIC; SETTINGS_MENU SOUND_EFFECTS; SETTINGS_MENU BACK ]

let music_menu () : menu =
  make_menu
    [
      CHANGE_AUDIO_SETTING (MUSIC, INCREASE);
      CHANGE_AUDIO_SETTING (MUSIC, DECREASE);
      CHANGE_AUDIO_SETTING (MUSIC, BACK);
    ]

let sound_effects_menu () : menu =
  make_menu
    [
      CHANGE_AUDIO_SETTING (SOUND_EFFECTS, INCREASE);
      CHANGE_AUDIO_SETTING (SOUND_EFFECTS, DECREASE);
      CHANGE_AUDIO_SETTING (SOUND_EFFECTS, BACK);
    ]

let get_save_file_choices save_slots : menu_choice list =
  let saved_games = List.filter (fun save -> not save.new_game) save_slots in
  let delete_save_files =
    let maybe_delete_save_file idx =
      let slot = List.nth save_slots idx in
      if slot.new_game then
        None
      else
        Some (SAVE_FILES_MENU (DELETE_SAVE_FILE (idx + 1)))
    in
    List.map maybe_delete_save_file (Int.range Config.other.save_slots) |> List.filter_somes
  in
  List.map (fun i -> SAVE_FILES_MENU (START_SLOT (i + 1))) (Int.range Config.other.save_slots)
  @ delete_save_files
  @ [ SAVE_FILES_MENU BACK ]

let save_files_menu save_slots : menu = make_menu (get_save_file_choices save_slots)

let confirm_delete_menu idx =
  make_menu [ CONFIRM_DELETE_MENU (CONFIRM_DELETE idx); CONFIRM_DELETE_MENU CANCEL ]

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
  make_menu (ghost_choices @ [ CHANGE_GHOST_MENU BACK ])

let change_weapon_menu (weapon_names : string list) : menu =
  let weapon_choices = List.map (fun name -> CHANGE_WEAPON_MENU (EQUIP_WEAPON name)) weapon_names in
  make_menu (weapon_choices @ [ CHANGE_WEAPON_MENU BACK ])

let update_menu_choice (menu : menu) state =
  if state.frame_inputs.down.pressed then (
    Audio.play_sound state "click";
    menu.current_choice_idx <- Int.min (menu.current_choice_idx + 1) (List.length menu.choices - 1));
  if state.frame_inputs.up.pressed then (
    Audio.play_sound state "click";
    menu.current_choice_idx <- Int.max 0 (menu.current_choice_idx - 1))

let update_pause_menu (game : game) (state : state) : state =
  let handle_pause_menu choice =
    match choice with
    | CONTINUE ->
      state.pause_menu <- None;
      game.interaction.text <- None
    | CHANGE_WEAPON ->
      state.pause_menu <-
        Some (MENU (change_weapon_menu (game.player.weapons |> String.Map.to_list |> List.map fst)))
    | CHANGE_GHOST -> state.pause_menu <- Some (MENU (change_ghost_menu game.party))
    | SETTINGS -> state.pause_menu <- Some (MENU (settings_menu ()))
    | SAVE -> Game.save game state
    | QUIT_TO_MAIN_MENU ->
      Audio.reset_music game.music.music;
      Audio.reset_music state.menu_music;
      (* TODO unload textures *)
      state.pause_menu <- None;
      Game.save game state ~after_fn:(fun state ->
          state.game_context <- MAIN_MENU (main_menu (), Game.load_all_save_slots ()))
  in

  let go_back () =
    state.pause_menu <-
      Some (MENU (pause_menu (List.length (Player.ghost_ids_in_party game.party))))
  in

  let handle_change_weapon_menu choice =
    match choice with
    | EQUIP_WEAPON weapon_name -> Player.equip_weapon game.player weapon_name
    | BACK -> go_back ()
  in

  let handle_change_ghost_menu choice =
    match choice with
    | USE_GHOST ghost_id ->
      if game.player.ghost.id <> ghost_id then
        Player.swap_current_ghost state game ghost_id
    | BACK -> go_back ()
  in

  let handle_settings_menu choice =
    match choice with
    | MUSIC -> state.pause_menu <- Some (MENU (music_menu ()))
    | SOUND_EFFECTS -> state.pause_menu <- Some (MENU (sound_effects_menu ()))
    | BACK -> go_back ()
  in

  let handle_audio_setting_menu (settings_choice, change_choice) =
    match (settings_choice, change_choice) with
    | MUSIC, INCREASE -> Audio.increase_music_volume state game
    | MUSIC, DECREASE -> Audio.decrease_music_volume state game
    | SOUND_EFFECTS, INCREASE -> Audio.increase_sound_effects_volume state
    | SOUND_EFFECTS, DECREASE -> Audio.decrease_sound_effects_volume state
    | _, BACK -> state.pause_menu <- Some (MENU (settings_menu ()))
    | BACK, _ -> failwith "invalid change audio menu"
  in

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
    (match game.player.ghost.hardfall_timer with
    | None -> ()
    | Some time -> game.player.ghost.hardfall_timer <- Some { at = time.at +. state.frame.dt });

    update_menu_choice menu state;

    if state.frame_inputs.jump.pressed then (
      (* TODO this plays the confirm sound even when selecting "Back" *)
      Audio.play_sound state "confirm";
      let menu_choice = List.nth menu.choices menu.current_choice_idx in
      match menu_choice with
      | PAUSE_MENU choice -> handle_pause_menu choice
      | CHANGE_WEAPON_MENU choice -> handle_change_weapon_menu choice
      | CHANGE_GHOST_MENU choice -> handle_change_ghost_menu choice
      | SETTINGS_MENU choice -> handle_settings_menu choice
      | CHANGE_AUDIO_SETTING choice -> handle_audio_setting_menu choice
      | MAIN_MENU _
      | SELECT_GAME_MODE _
      | SAVE_FILES_MENU _
      | CONFIRM_DELETE_MENU _ ->
        failwithf "invalid pause menu: %s" (Show.menu_choice (Some game) menu_choice)));
  state

let update_main_menu (menu : menu) (save_slots : save_slot list) (state : state) : state =
  update_menu_choice menu state;

  let load_file save_file_idx =
    let save_slot : save_slot =
      match List.nth_opt save_slots (save_file_idx - 1) with
      | None -> failwithf "bad save file idx: %d" save_file_idx
      | Some slot -> slot
    in
    if save_slot.new_game then
      state.game_context <-
        MAIN_MENU (select_game_mode_menu save_slot.file save_file_idx, save_slots)
    else (
      let game = Game.load state save_slot.file save_file_idx in
      (match game.mode with
      | CLASSIC
      | DEMO ->
        ()
      | STEEL_SOLE ->
        (* this prevents an immediate damage respawn from landing on the bench *)
        Player.add_phantom_floor game game.player.ghost.entity.dest.pos);
      (* tick camera to ensure it is entirely inside room before trying to render *)
      let state' = Camera.tick game state in
      Game.start ~is_new_game:save_slot.new_game state' game save_slot.file)
  in

  let start_new_game game_mode save_file save_file_idx =
    let game = Game.create_new state game_mode save_file save_file_idx in
    Game.start state game save_file
  in

  let show_save_files_menu () =
    state.game_context <- SAVE_FILES (save_files_menu save_slots, save_slots)
  in

  let handle_main_menu choice =
    match choice with
    | START_GAME -> show_save_files_menu ()
    | QUIT ->
      print "exiting";
      exit 0
  in

  let handle_game_mode_menu choice =
    match choice with
    | USE_MODE (mode, save_file, save_file_idx) ->
      let save_file' =
        match mode with
        | CLASSIC -> save_file
        | DEMO -> Game.initialize_steel_sole ~with_keys:false save_file
        | STEEL_SOLE -> Game.initialize_steel_sole ~with_keys:true save_file
      in
      start_new_game mode save_file' save_file_idx
    | BACK -> show_save_files_menu ()
  in

  let handle_save_files_menu choice =
    match choice with
    | START_SLOT n -> load_file n
    | DELETE_SAVE_FILE n -> state.game_context <- MAIN_MENU (confirm_delete_menu n, save_slots)
    | BACK -> state.game_context <- MAIN_MENU (main_menu (), save_slots)
  in

  let handle_delete_file_menu choice =
    match choice with
    | CONFIRM_DELETE n ->
      File.delete_save n;
      let reloaded_save_slots = Game.load_all_save_slots () in
      state.game_context <- SAVE_FILES (save_files_menu reloaded_save_slots, reloaded_save_slots)
    | CANCEL -> show_save_files_menu ()
  in

  if state.frame_inputs.jump.pressed then (
    Audio.play_sound state "confirm";
    match List.nth menu.choices menu.current_choice_idx with
    | MAIN_MENU choice -> handle_main_menu choice
    | SELECT_GAME_MODE choice -> handle_game_mode_menu choice
    | SAVE_FILES_MENU choice -> handle_save_files_menu choice
    | CONFIRM_DELETE_MENU choice -> handle_delete_file_menu choice
    | PAUSE_MENU _
    | CHANGE_WEAPON_MENU _
    | CHANGE_GHOST_MENU _
    | SETTINGS_MENU _
    | CHANGE_AUDIO_SETTING _ ->
      failwith "update_main_menu - needs MAIN_MENU menu.choices");
  state
