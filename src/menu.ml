open Types

let main_menu () : menu =
  { choices = [ MAIN_MENU START_GAME; MAIN_MENU QUIT ]; current_choice_idx = 0 }

let select_game_mode_menu save_file save_file_idx : menu =
  {
    choices =
      [
        (* TODO enable Classic mode *)
        (* SELECT_GAME_MODE (USE_MODE (CLASSIC, save_file, save_file_idx)); *)
        SELECT_GAME_MODE (USE_MODE (STEEL_SOLE, save_file, save_file_idx));
        SELECT_GAME_MODE (USE_MODE (DEMO, save_file, save_file_idx));
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
      |> Utils.filter_somes;
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
        CHANGE_SETTING (MUSIC, INCREASE);
        CHANGE_SETTING (MUSIC, DECREASE);
        CHANGE_SETTING (MUSIC, BACK);
      ];
    current_choice_idx = 0;
  }

let sound_effects_menu () : menu =
  {
    choices =
      [
        CHANGE_SETTING (SOUND_EFFECTS, INCREASE);
        CHANGE_SETTING (SOUND_EFFECTS, DECREASE);
        CHANGE_SETTING (SOUND_EFFECTS, BACK);
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
    play_sound state "click";
    menu.current_choice_idx <- Int.min (menu.current_choice_idx + 1) (List.length menu.choices - 1));
  if state.frame_inputs.up.pressed then (
    play_sound state "click";
    menu.current_choice_idx <- Int.max 0 (menu.current_choice_idx - 1))

let update_pause_menu (game : game) (state : state) : state =
  if state.frame_inputs.pause.pressed then (
    match state.pause_menu with
    | None ->
      play_sound state "menu-expand";
      state.pause_menu <- Some (pause_menu (List.length (Player.ghost_ids_in_party game.party)))
    | Some _ ->
      play_sound state "menu-close";
      state.pause_menu <- None);

  (match state.pause_menu with
  | None -> ()
  | Some menu ->
    update_menu_choice menu state;
    let get_new_volume increase v =
      Utils.bound 0.
        (if increase then
           v +. 0.1
        else
          v -. 0.1)
        2.
    in
    let change_music_volume increase =
      let new_volume = get_new_volume increase state.settings.music_volume in
      state.settings.music_volume <- new_volume;
      Raylib.set_music_volume game.music.music.t new_volume
    in

    let change_sound_effects_volume increase =
      let new_volume = get_new_volume increase state.settings.sound_effects_volume in
      state.settings.sound_effects_volume <- new_volume
    in

    if state.frame_inputs.jump.pressed then (
      (* TODO this plays the confirm sound even when selecting "Back" *)
      play_sound state "confirm";
      match List.nth menu.choices menu.current_choice_idx with
      | PAUSE_MENU CONTINUE ->
        state.pause_menu <- None;
        game.interaction.text <- None
      | PAUSE_MENU CHANGE_WEAPON ->
        state.pause_menu <- Some (change_weapon_menu (List.map fst game.player.weapons))
      | PAUSE_MENU CHANGE_GHOST -> state.pause_menu <- Some (change_ghost_menu game.party)
      | PAUSE_MENU QUIT_TO_MAIN_MENU ->
        Raylib.seek_music_stream game.music.music.t 0.;
        Raylib.seek_music_stream state.menu_music.t 0.;
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
        state.pause_menu <- Some (pause_menu (List.length (Player.ghost_ids_in_party game.party)))
      | PAUSE_MENU SETTINGS -> state.pause_menu <- Some (settings_menu ())
      | SETTINGS_MENU MUSIC -> state.pause_menu <- Some (music_menu ())
      | SETTINGS_MENU SOUND_EFFECTS -> state.pause_menu <- Some (sound_effects_menu ())
      | CHANGE_SETTING (MUSIC, INCREASE) -> change_music_volume true
      | CHANGE_SETTING (MUSIC, DECREASE) -> change_music_volume false
      | CHANGE_SETTING (SOUND_EFFECTS, INCREASE) -> change_sound_effects_volume true
      | CHANGE_SETTING (SOUND_EFFECTS, DECREASE) -> change_sound_effects_volume false
      | CHANGE_SETTING (_, BACK) -> state.pause_menu <- Some (settings_menu ())
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
      let game = Game.init state save_file save_file_idx in
      (match game.mode with
      | CLASSIC
      | DEMO ->
        ()
      | STEEL_SOLE ->
        (* this prevents an immediate damage respawn from landing on the bench *)
        Player.add_phantom_floor game game.player.ghost.entity.dest.pos);
      Game.start ~is_new_game state game save_file)
  in

  let initialize_and_start_game game_mode save_file save_file_idx =
    let game = Game.init state ~mode:(Some game_mode) save_file save_file_idx in
    Game.start state game save_file
  in

  if state.frame_inputs.jump.pressed then (
    play_sound state "confirm";
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
      (match mode with
      | DEMO
      | CLASSIC
      | STEEL_SOLE ->
        Game.initialize_steel_sole save_file);
      initialize_and_start_game mode save_file save_file_idx
    | SELECT_GAME_MODE BACK -> state.game_context <- SAVE_FILES (save_files_menu (), save_slots)
    | _ -> failwith "update_main_menu - needs MAIN_MENU menu.choices");
  state
