open Types

[@@@ocaml.warning "-26-27-32"]

let main_menu () : menu = { choices = [ MAIN_MENU START_GAME; MAIN_MENU QUIT ]; current_choice_idx = 0 }

let pause_menu ghost_count : menu =
  {
    choices =
      [
        Some (PAUSE_MENU CONTINUE);
        (if ghost_count > 1 then Some (PAUSE_MENU CHANGE_GHOST) else None);
        Some (PAUSE_MENU CHANGE_WEAPON);
        Some (PAUSE_MENU QUIT_TO_MAIN_MENU);
      ]
      |> Utils.filter_somes;
    current_choice_idx = 0;
  }

let save_files_menu () : menu =
  {
    choices = [ SAVE_FILES SLOT_1; SAVE_FILES SLOT_2; SAVE_FILES SLOT_3; SAVE_FILES SLOT_4; SAVE_FILES BACK ];
    current_choice_idx = 0;
  }

let change_ghost_menu (ghosts : (ghost_id * ghost) list) : menu =
  let ghost_choices =
    List.filter_map (fun (id, ghost) -> if ghost.in_party then Some (CHANGE_GHOST_MENU (USE_GHOST id)) else None) ghosts
  in
  { choices = ghost_choices @ [ CHANGE_GHOST_MENU BACK ]; current_choice_idx = 0 }

let change_weapon_menu (weapon_names : string list) : menu =
  let weapon_choices = List.map (fun name -> CHANGE_WEAPON_MENU (EQUIP_WEAPON name)) weapon_names in
  { choices = weapon_choices @ [ CHANGE_WEAPON_MENU BACK ]; current_choice_idx = 0 }

let update_menu_choice (menu : menu) frame_inputs =
  if frame_inputs.down.pressed then
    menu.current_choice_idx <- Int.min (menu.current_choice_idx + 1) (List.length menu.choices - 1);
  if frame_inputs.up.pressed then
    menu.current_choice_idx <- Int.max 0 (menu.current_choice_idx - 1)

let update_pause_menu (game : game) (state : state) : state =
  if state.frame_inputs.pause.pressed then (
    match state.pause_menu with
    | None -> state.pause_menu <- Some (pause_menu (List.length (Ghost.available_ghost_ids game.ghosts)))
    | Some _ -> state.pause_menu <- None);

  (match state.pause_menu with
  | None -> ()
  | Some menu ->
    update_menu_choice menu state.frame_inputs;

    if state.frame_inputs.jump.pressed then (
      match List.nth menu.choices menu.current_choice_idx with
      | PAUSE_MENU CONTINUE ->
        state.pause_menu <- None;
        game.interaction.text <- None
      | PAUSE_MENU CHANGE_WEAPON -> state.pause_menu <- Some (change_weapon_menu (List.map fst game.ghost.weapons))
      | PAUSE_MENU CHANGE_GHOST ->
        state.pause_menu <- Some (change_ghost_menu ([ (game.ghost.id, game.ghost) ] @ game.ghosts))
      | PAUSE_MENU QUIT_TO_MAIN_MENU ->
        state.pause_menu <- None;
        (* TODO unload textures *)
        Room.save_progress game;
        let save_file : Json_t.save_file =
          {
            ghost_id = Show.ghost_id game.ghost.id;
            ghosts_in_party =
              (* game.ghosts only has the uncontrolled ghosts, but save_file.ghosts_in_party
                 should include the current ghost's id *)
              [ game.ghost.id ] @ Ghost.available_ghost_ids game.ghosts |> List.map Show.ghost_id;
            ghost_x = game.ghost.entity.dest.pos.x;
            ghost_y = game.ghost.entity.dest.pos.y;
            room_name = Tiled.Room.get_uuid' game.room.area.id game.room.id;
            abilities = game.ghost.abilities;
            progress = game.progress;
            weapons = List.map fst game.ghost.weapons;
            current_weapon = game.ghost.current_weapon.name;
          }
        in
        let save_file_path = fmt "../saves/%d.json" game.save_file_slot in
        let contents = Json_j.string_of_save_file save_file in
        let written = File.write save_file_path contents in
        if written then
          state.game_context <- MAIN_MENU (main_menu (), Game.load_all_save_slots ())
        else
          failwith "error when trying to save"
      | CHANGE_WEAPON_MENU (EQUIP_WEAPON weapon_name) -> Ghost.equip_weapon game.ghost weapon_name
      | CHANGE_GHOST_MENU (USE_GHOST ghost_id) ->
        if game.ghost.id <> ghost_id then
          Ghost.swap_current_ghost state game ghost_id
      | CHANGE_GHOST_MENU BACK
      | CHANGE_WEAPON_MENU BACK ->
        state.pause_menu <- Some (pause_menu (List.length (Ghost.available_ghost_ids game.ghosts)))
      | c -> failwithf "unhandled menu choice: %s" (Show.menu_choice (Some game) c)));
  state

let update_main_menu (menu : menu) (save_slots : save_slots) (state : state) : state =
  update_menu_choice menu state.frame_inputs;

  let load_file save_file_idx =
    let (save_file, is_new_game) : Json_t.save_file * bool =
      match save_file_idx with
      | 1 -> save_slots.slot_1
      | 2 -> save_slots.slot_2
      | 3 -> save_slots.slot_3
      | 4 -> save_slots.slot_4
      | _ -> failwithf "bad save file idx: %d" save_file_idx
    in
    let game = Game.init save_file state.global state.world save_file_idx in
    state.camera.raylib <-
      (* update the camera when a file is loaded so the ghost doesn't start too far offscreen
         TODO can maybe improve this, since it can still be off if the camera is bounded
      *)
      Tiled.create_camera_at (Raylib.Vector2.create game.ghost.entity.dest.pos.x game.ghost.entity.dest.pos.y) 0.;
    if is_new_game then (
      Entity.freeze game.ghost.entity;
      state.screen_fade <- Some 255;
      Ghost.maybe_begin_interaction state game "info_opening-poem");
    state.game_context <- IN_PROGRESS game
  in

  if state.frame_inputs.jump.pressed then (
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
    | _ -> failwith "update_main_menu - needs MAIN_MENU menu.choices");
  state
