open Types

[@@@ocaml.warning "-26-27-32"]

let main_menu () : menu =
  { choices = [ MAIN_MENU START_GAME; MAIN_MENU QUIT ]; current_choice_idx = 0 }

let select_game_mode_menu save_file save_file_idx : menu =
  {
    choices =
      [
        SELECT_GAME_MODE (USE_MODE (CLASSIC, save_file, save_file_idx));
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

let save_game ?(after_fn = ignore) (game : game) (state : state) =
  Room.save_progress game;
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
        |> Utils.uniq;
      ghost_x = game.player.ghost.entity.dest.pos.x;
      ghost_y = game.player.ghost.entity.dest.pos.y;
      respawn_x = game.room.respawn_pos.x;
      respawn_y = game.room.respawn_pos.y;
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
  (* FIXME path *)
  let save_file_path = fmt "../saves/%d.json" game.save_file_slot in
  let contents = Json_j.string_of_save_file save_file |> Yojson.Safe.prettify in
  let written = File.write save_file_path contents in
  if written then
    after_fn state
  else
    failwith "error when trying to save"

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
        save_game game state ~after_fn:(fun state ->
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

let initialize_camera_and_game state ?(mode = None) (save_file : Json_t.save_file) save_file_idx =
  let mode' =
    match mode with
    | Some m -> m
    | None -> (
      match save_file.game_mode with
      | "Classic" -> CLASSIC
      | "Steel Sole" -> STEEL_SOLE
      | _ -> failwithf "invalid game_mode in save_file %d" save_file_idx)
  in
  let game = Game.init mode' save_file state.global state.area_musics state.world save_file_idx in
  state.camera.update_instantly <- true;
  state.camera.raylib <-
    (* update the camera when a file is loaded so the ghost doesn't start too far offscreen
       TODO can maybe improve this, since it can still be off if the camera is bounded
    *)
    Tiled.create_camera_at
      (Raylib.Vector2.create game.player.ghost.entity.dest.pos.x game.player.ghost.entity.dest.pos.y)
      0.;
  game

(* CLEANUP seems like this should be in game.ml instead of menu.ml *)
let initialize_ss_game (save_file : Json_t.save_file) =
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

let start_game ?(is_new_game = true) (state : state) (game : game) (save_file : Json_t.save_file) =
  if is_new_game then (
    Entity.freeze game.player.ghost.entity;
    state.screen_fade <- Some 255;
    let trigger : trigger = make_stub_trigger INFO "cutscene" "opening-poem" in
    Player.maybe_begin_interaction state game trigger)
  else
    state.frame.idx <- save_file.progress.steel_sole.frame_idx;
  Raylib.stop_music_stream state.menu_music.t;
  state.game_context <- IN_PROGRESS game

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
      let game = initialize_camera_and_game state save_file save_file_idx in
      (match game.mode with
      | CLASSIC -> ()
      | STEEL_SOLE -> Player.add_phantom_floor game game.player.ghost.entity.dest.pos);
      start_game ~is_new_game state game save_file)
  in

  let initialize_and_start_game game_mode save_file save_file_idx =
    let game = initialize_camera_and_game state ~mode:(Some game_mode) save_file save_file_idx in
    start_game state game save_file
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
    | SELECT_GAME_MODE (USE_MODE (CLASSIC, save_file, save_file_idx)) ->
      initialize_and_start_game CLASSIC save_file save_file_idx
    | SELECT_GAME_MODE (USE_MODE (STEEL_SOLE, save_file, save_file_idx)) ->
      initialize_ss_game save_file;
      initialize_and_start_game STEEL_SOLE save_file save_file_idx
    | SELECT_GAME_MODE BACK -> state.game_context <- SAVE_FILES (save_files_menu (), save_slots)
    | _ -> failwith "update_main_menu - needs MAIN_MENU menu.choices");
  state
