open Types
open Controls

[@@@ocaml.warning "-26-27-32"]

(* this function initializes state and sets loaded_state to MAIN_MENU *)
let init () : state =
  let world = Tiled.init_world "Deepnest_East" in
  let parse_texture_configs parse_name coll =
    let parse (name, config) = (parse_name "state.init" name, config) in
    List.map parse coll
  in

  let enemies_file : Json_t.enemies_file = File.read_config "enemies" Json_j.enemies_file_of_string in
  let enemy_configs = parse_texture_configs Enemy.parse_name enemies_file.enemies in
  let shared_enemy_configs = enemies_file.shared_textures in

  let npcs_file = File.read_config "npcs" Json_j.npcs_file_of_string in
  let npc_configs = parse_texture_configs Npc.parse_name npcs_file.npcs in

  let build_shared_npc_texture name =
    let config =
      match List.assoc_opt name npcs_file.shared_textures with
      | Some c -> c
      | None -> failwithf "missing config for '%s' in config/npcs.json" name
    in
    Sprite.build_texture_from_config
      {
        asset_dir = NPCS;
        character_name = "shared";
        pose_name = name;
        count = config.count;
        duration = { seconds = config.duration };
        x_offset = 0.;
        y_offset = 0.;
      }
  in

  let door_lever = build_shared_npc_texture "door-lever" in
  let pickup_indicator = build_shared_npc_texture "pickup-indicator" in
  let main_menu = build_shared_npc_texture "main-menu" in

  let ability_outlines =
    Sprite.build_texture_from_config
      {
        asset_dir = GHOSTS;
        character_name = "shared";
        pose_name = "ability-outlines";
        count = 1;
        duration = { seconds = 0. };
        x_offset = 0.;
        y_offset = 0.;
      }
  in

  let damage =
    let json_config = List.assoc "damage" shared_enemy_configs in
    Sprite.build_texture_from_config ~particle:true
      {
        asset_dir = ENEMIES;
        character_name = "shared";
        pose_name = "damage";
        count = json_config.count;
        duration = { seconds = json_config.duration };
        x_offset = 0.;
        y_offset = 0.;
      }
  in

  let global =
    {
      lore = File.read_config "lore" Json_j.lore_file_of_string;
      weapons = File.read_config "weapons" Json_j.weapons_file_of_string;
      enemy_configs;
      npc_configs;
      textures = { ability_outlines; damage; pickup_indicator; main_menu; door_lever };
    }
  in

  let camera_target = Raylib.Vector2.create 0. 0. in
  let camera = Tiled.create_camera_at camera_target 0. in

  print "initialized state\n=================\n";
  {
    game_context = MAIN_MENU (Menu.main_menu (), Game.load_all_save_slots ());
    pause_menu = None;
    world;
    camera = { raylib = camera; subject = GHOST; shake = 0. };
    screen_fade = None;
    frame = { idx = 0; dt = 0.; time = 0. };
    frame_inputs =
      {
        up = { pressed = false; down = false; released = false; down_since = None };
        down = { pressed = false; down = false; released = false; down_since = None };
        left = { pressed = false; down = false; released = false; down_since = None };
        right = { pressed = false; down = false; released = false; down_since = None };
        cast = { pressed = false; down = false; released = false; down_since = None };
        d_nail = { pressed = false; down = false; released = false; down_since = None };
        dash = { pressed = false; down = false; released = false; down_since = None };
        focus = { pressed = false; down = false; released = false; down_since = None };
        jump = { pressed = false; down = false; released = false; down_since = None };
        nail = { pressed = false; down = false; released = false; down_since = None };
        pause = { pressed = false; down = false; released = false; down_since = None };
      };
    debug = { enabled = false; rects = [] };
    global;
  }

let update_camera (game : game) (state : state) =
  let trigger_config : (string * rect) option = Ghost.find_trigger_collision game.ghost game.room.triggers.camera in
  let subject =
    match state.camera.subject with
    | GHOST ->
      let e = game.ghost.entity in
      let offset = (* TODO move to config *) 50. in
      if e.sprite.facing_right then
        { e.dest.pos with x = e.dest.pos.x +. offset }
      else
        { e.dest.pos with x = e.dest.pos.x -. offset }
    | FIXED pos -> pos
  in
  let new_camera =
    let current = Raylib.Camera2D.target state.camera.raylib in
    let camera_state =
      {
        current_pos = { x = Raylib.Vector2.x current; y = Raylib.Vector2.y current };
        subject;
        room_bounds = game.room.camera_bounds;
      }
    in
    let bounded_target () =
      let subject = camera_state.subject in
      let target_x, target_y =
        match trigger_config with
        | None -> (subject.x, subject.y)
        | Some (config, rect) ->
          let x_config, y_config = Utils.separate config '-' in
          let x_bound =
            let left = rect.pos.x +. Config.window.center_x in
            let right = rect.pos.x +. rect.w -. Config.window.center_x in
            match x_config with
            | "gx" -> subject.x
            | ">x" -> Utils.bound left subject.x camera_state.room_bounds.max.x
            | "<x" -> Utils.bound camera_state.room_bounds.min.x subject.x right
            | "x" -> Utils.bound left subject.x right
            | _ -> failwithf "invalid x_bound %s" x_config
          in
          let y_bound =
            let top = rect.pos.y +. Config.window.center_y in
            let bottom = rect.pos.y +. rect.h -. Config.window.center_y in
            match y_config with
            | "gy" -> subject.y
            | ">y" -> Utils.bound top subject.y camera_state.room_bounds.max.y
            | "<y" -> Utils.bound camera_state.room_bounds.min.y subject.y bottom
            | "y" -> Utils.bound top subject.y bottom
            | _ -> failwithf "invalid y_bound %s" y_config
          in
          (x_bound, y_bound)
      in
      let current_x, current_y = (camera_state.current_pos.x, camera_state.current_pos.y) in
      let between amount a b =
        (* TODO use motion to calculate *)
        (* amount = 1.0 returns the midpoint, larger amount is closer to `a` *)
        ((amount *. a) +. b) /. (amount +. 1.)
      in
      let diff_x, diff_y = (abs_float (current_x -. target_x), abs_float (current_y -. target_y)) in
      let smooth_x =
        if diff_x > 1. then
          between (Config.window.fps * 25 / 60 |> Int.to_float) current_x target_x
        else
          target_x
      in
      let smooth_y =
        if diff_y > 1. then
          between (Config.window.fps * 5 / 60 |> Int.to_float) current_y target_y
        else
          target_y
      in
      Raylib.Vector2.create
        (Utils.bound camera_state.room_bounds.min.x smooth_x camera_state.room_bounds.max.x)
        (Utils.bound camera_state.room_bounds.min.y smooth_y camera_state.room_bounds.max.y)
    in
    let target = bounded_target () in
    Tiled.create_camera_at target state.camera.shake
  in
  state.camera.raylib <- new_camera;
  if state.camera.shake > 0. then
    state.camera.shake <- state.camera.shake -. state.frame.dt;
  state

let update_fragments (game : game) (state : state) =
  let update_fragment (f : entity) = Entity.update_pos game.room f state.frame.dt in
  let all_spawned_fragments = List.map (fun l -> l.spawned_fragments) game.room.layers |> List.flatten in
  List.iter update_fragment all_spawned_fragments;
  state

(* return value is "keep spawned" *)
let update_projectile p (frame_info : frame_info) : bool =
  let despawn_projectile =
    match p.despawn with
    | X_BOUNDS (min_x, max_x) ->
      p.entity.dest.pos.x < min_x -. Config.window.center_x || p.entity.dest.pos.x > max_x +. Config.window.center_x
    | TIME_LEFT d -> frame_info.time -. p.spawned.at > d.seconds
  in
  if despawn_projectile then
    false
  else (
    Entity.apply_v frame_info.dt p.entity;
    Sprite.advance_animation frame_info.time p.entity.sprite.texture p.entity.sprite;
    true)

let update_enemies (game : game) (state : state) =
  let behavior_params : enemy_behavior_params =
    { ghost_pos = game.ghost.entity.dest.pos; room_bounds = game.room.camera_bounds; time = state.frame.time }
  in
  let update_enemy ((_, enemy) : enemy_id * enemy) =
    let unremoved_projectiles = ref [] in
    let update_projectile' (projectile : projectile) =
      let keep_spawned = update_projectile projectile state.frame in
      if keep_spawned then (
        unremoved_projectiles := projectile :: !unremoved_projectiles;
        if Ghost.is_vulnerable state game then (
          match Collision.with_entity game.ghost.entity projectile.entity.dest with
          | None -> ()
          | Some c ->
            (* TODO add collision shape to enemy projectiles *)
            if Collision.between_entities game.ghost.entity projectile.entity then
              Ghost.start_action state game (TAKE_DAMAGE c.direction)))
    in
    List.iter update_projectile' enemy.spawned_projectiles;
    enemy.spawned_projectiles <- !unremoved_projectiles;
    if enemy.entity.update_pos then
      Entity.update_pos game.room enemy.entity state.frame.dt;
    let interacting () = List.length game.interaction.steps > 0 in
    if (not (interacting ())) && enemy.status.choose_behavior then
      enemy.choose_behavior ~self:enemy behavior_params;
    Sprite.advance_animation state.frame.time enemy.entity.sprite.texture enemy.entity.sprite;
    let advance_or_despawn (sprite : sprite) = Sprite.advance_or_despawn state.frame.time sprite.texture sprite in
    enemy.damage_sprites <- List.filter_map advance_or_despawn enemy.damage_sprites;
    (match Ghost.get_spell_sprite game.ghost with
    | None -> ()
    | Some (sprite, action) -> (
      (* TODO use Collision.between_shapes *)
      match Collision.with_entity enemy.entity sprite.dest with
      | None -> ()
      | Some c ->
        let damage_kind =
          if game.ghost.current.is_diving then
            DESOLATE_DIVE
          else if not (Ghost.past_cooldown game.ghost.history.dive_cooldown state.frame.time) then
            DESOLATE_DIVE_SHOCKWAVE
          else
            HOWLING_WRAITHS
        in
        ignore (Enemy.maybe_take_damage state enemy action damage_kind (Ghost.get_damage game.ghost damage_kind) c.rect)
      ));

    if Enemy.is_dead enemy then (
      match enemy.on_killed.interaction_name with
      | None -> ()
      | Some name ->
        if enemy.on_killed.multiple_enemies then (
          let all_bosses_dead =
            let living_bosses =
              List.filter (fun (enemy_id, e) -> enemy_id = enemy.id && not (Enemy.is_dead e)) game.room.enemies
            in
            List.length living_bosses = 0
          in
          if all_bosses_dead then
            Ghost.maybe_begin_interaction state game name)
        else
          Ghost.maybe_begin_interaction state game name)
  in
  List.iter update_enemy game.room.enemies;
  state

let update_npcs (game : game) (state : state) =
  let update_npc (npc : npc) =
    Entity.update_pos game.room npc.entity state.frame.dt;
    Sprite.advance_animation state.frame.time npc.entity.sprite.texture npc.entity.sprite
  in

  let update_pickup_indicators (sprite : sprite) = Sprite.advance_animation state.frame.time sprite.texture sprite in

  let update_ghost (_id, ghost) =
    Sprite.advance_animation state.frame.time ghost.entity.sprite.texture ghost.entity.sprite;
    if ghost.entity.update_pos then (
      let debug =
        if state.debug.enabled then
          Some "npc update_ghost"
        else
          None
      in

      Entity.update_pos ~debug game.room ghost.entity state.frame.dt;
      Ghost.maybe_unset_current_floor ghost)
  in

  List.iter update_ghost game.ghosts;
  List.iter update_npc game.room.npcs;
  (* pickup indicators aren't really "npcs" *)
  List.iter update_pickup_indicators game.room.pickup_indicators;
  state

let update_spawned_vengeful_spirits (game : game) (state : state) =
  let damage_enemies vs_start_time (f : sprite) =
    let maybe_damage_enemy ((_enemy_id, enemy) : enemy_id * enemy) =
      if enemy.status.check_damage_collisions then (
        match Collision.with_entity enemy.entity f.dest with
        | None -> ()
        | Some c ->
          ignore
            (Enemy.maybe_take_damage state enemy vs_start_time VENGEFUL_SPIRIT
               (Ghost.get_damage game.ghost VENGEFUL_SPIRIT)
               c.rect))
    in
    List.iter maybe_damage_enemy game.room.enemies
  in
  let update_vengeful_spirit (projectile : projectile) =
    let keep_spawned = update_projectile projectile state.frame in
    if keep_spawned then
      damage_enemies projectile.spawned projectile.entity.sprite
    else
      game.ghost.spawned_vengeful_spirits <- List.filter (fun p -> p <> projectile) game.ghost.spawned_vengeful_spirits
  in

  List.iter update_vengeful_spirit game.ghost.spawned_vengeful_spirits;
  state

let update_frame_inputs (state : state) : state =
  let update_frame_input key (input : frame_input) =
    input.released <- key_released key;
    input.down <- key_down key;
    input.pressed <- key_pressed key;
    if input.pressed then
      input.down_since <- Some { at = state.frame.time }
    else if input.down then
      input.down_since <- input.down_since
    else
      input.down_since <- None
  in
  update_frame_input (ARROW UP) state.frame_inputs.up;
  update_frame_input (ARROW DOWN) state.frame_inputs.down;
  update_frame_input (ARROW LEFT) state.frame_inputs.left;
  update_frame_input (ARROW RIGHT) state.frame_inputs.right;
  update_frame_input CAST state.frame_inputs.cast;
  update_frame_input DASH state.frame_inputs.dash;
  update_frame_input D_NAIL state.frame_inputs.d_nail;
  update_frame_input FOCUS state.frame_inputs.focus;
  update_frame_input JUMP state.frame_inputs.jump;
  update_frame_input NAIL state.frame_inputs.nail;
  update_frame_input PAUSE state.frame_inputs.pause;
  state

let update_menu_choice (menu : menu) frame_inputs =
  if frame_inputs.down.pressed then
    menu.current_choice_idx <- Int.min (menu.current_choice_idx + 1) (List.length menu.choices - 1);
  if frame_inputs.up.pressed then
    menu.current_choice_idx <- Int.max 0 (menu.current_choice_idx - 1)

let tick (state : state) =
  (* TODO-4 add sound effects and music *)
  if Controls.key_pressed DEBUG_4 then
    if state.debug.enabled then (
      state.debug.enabled <- false;
      print " disabled debug at %d\n\\----------------------/\n" state.frame.idx)
    else (
      state.debug.enabled <- true;
      print "\n/---------------------\\\n enabled debug at %d" state.frame.idx);

  state.debug.rects <- [];
  match state.game_context with
  | SAVE_FILES (menu, save_slots)
  | MAIN_MENU (menu, save_slots) ->
    state |> update_frame_inputs |> Menu.update_main_menu menu save_slots
  | IN_PROGRESS game -> (
    let state' = state |> update_frame_inputs |> Menu.update_pause_menu game in
    match state'.pause_menu with
    | Some menu -> state'
    | None ->
      let st' =
        state'
        |> Ghost.update game
        |> update_spawned_vengeful_spirits game
        |> update_enemies game
        |> update_npcs game
        |> update_fragments game
        |> update_camera game
      in
      st')
