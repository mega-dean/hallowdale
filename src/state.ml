open Types
open Controls

[@@@ocaml.warning "-26-27-32"]

(* this function initializes state and sets game_context to MAIN_MENU *)
let init () : state =
  let world = Tiled.init_world "Deepnest_East" in
  let parse_texture_configs parse_name coll =
    let parse (name, config) = (parse_name "state.init" name, config) in
    List.map parse coll
  in

  let enemies_file : Json_t.enemies_file =
    File.read_config "enemies" Json_j.enemies_file_of_string
  in
  let enemy_configs = parse_texture_configs Enemy.parse_name enemies_file.enemies in
  let shared_enemy_configs = enemies_file.shared_textures in

  let npcs_file = File.read_config "npcs" Json_j.npcs_file_of_string in
  let npc_configs = parse_texture_configs Npc.parse_name npcs_file.npcs in

  let build_shared_npc_texture ?(once = false) name =
    let config =
      match List.assoc_opt name npcs_file.shared_textures with
      | Some c -> c
      | None -> failwithf "missing config for '%s' in config/npcs.json" name
    in
    Sprite.build_texture_from_config ~once
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

  let door_lever_struck = build_shared_npc_texture ~once:true "door-lever-struck" in
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

  let platforms =
    let texture-config = 
    let load_platform_texture name =
      ( name,
        Sprite.build_texture_from_config
          {
            asset_dir = TILED;
            character_name = "platforms";
            pose_name = name;
            count = 1;
            duration = { seconds = 0. };
            x_offset = 0.;
            y_offset = 0.;
          } )
    in
    let path =
      Sprite.get_path 
      (* CLEANUP maybe just expose Sprite.get_path *)fmt "../assets/%s/%s/" (Show.asset_dir TILED) 
    in

    let check_file filename =
      let name, extension = Utils.split_at_first '.' filename in
      if extension = "png" then
        Some name
      else
        None
    in
    let texture_names = List.filter_map check_file (File.ls path) in
    List.map load_platform_texture texture_names
  in

  let global =
    {
      lore = File.read_config "lore" Json_j.lore_file_of_string;
      weapons = File.read_config "weapons" Json_j.weapons_file_of_string;
      enemy_configs;
      npc_configs;
      textures =
        {
          ability_outlines;
          damage;
          pickup_indicator;
          main_menu;
          door_lever;
          door_lever_struck;
          platforms;
        };
    }
  in

  let camera_target = Raylib.Vector2.create 0. 0. in
  let camera = Tiled.create_camera_at camera_target 0. in

  print "initialized state\n=================\n";
  {
    game_context = MAIN_MENU (Menu.main_menu (), Game.load_all_save_slots ());
    pause_menu = None;
    should_save = false;
    world;
    camera =
      {
        raylib = camera;
        subject = GHOST;
        shake = 0.;
        update_instantly = false;
        motion = SMOOTH (Config.window.camera_motion_x, Config.window.camera_motion_y);
      };
    screen_fade = None;
    frame = { idx = 0; dt = 0.; time = 0. };
    frame_inputs =
      {
        up = { pressed = false; down = false; released = false; down_since = None };
        down = { pressed = false; down = false; released = false; down_since = None };
        left = { pressed = false; down = false; released = false; down_since = None };
        right = { pressed = false; down = false; released = false; down_since = None };
        cast = { pressed = false; down = false; released = false; down_since = None };
        c_dash = { pressed = false; down = false; released = false; down_since = None };
        dream_nail = { pressed = false; down = false; released = false; down_since = None };
        dash = { pressed = false; down = false; released = false; down_since = None };
        focus = { pressed = false; down = false; released = false; down_since = None };
        jump = { pressed = false; down = false; released = false; down_since = None };
        nail = { pressed = false; down = false; released = false; down_since = None };
        pause = { pressed = false; down = false; released = false; down_since = None };
        interact = { pressed = false; down = false; released = false; down_since = None };
      };
    debug = { enabled = false; show_frame_inputs = true; rects = [] };
    global;
  }

let update_camera (game : game) (state : state) =
  let trigger_config : trigger option =
    Ghost.find_trigger_collision' game.ghost game.room.triggers.camera
  in
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
        | Some trigger ->
          let x_config, y_config =
            match trigger.kind with
            | CAMERA (x, y) -> (x, y)
            | _ -> failwith "camera trigger needs CAMERA kind"
          in
          let x_bound =
            let left = trigger.dest.pos.x +. Config.window.center_x in
            let right = trigger.dest.pos.x +. trigger.dest.w -. Config.window.center_x in
            match x_config with
            | "gx" -> subject.x
            | ">x" -> Utils.bound left subject.x camera_state.room_bounds.max.x
            | "<x" -> Utils.bound camera_state.room_bounds.min.x subject.x right
            | "x" -> Utils.bound left subject.x right
            | _ -> failwithf "invalid x_bound %s" x_config
          in
          let y_bound =
            let top = trigger.dest.pos.y +. Config.window.center_y in
            let bottom = trigger.dest.pos.y +. trigger.dest.h -. Config.window.center_y in
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
        (* amount = 1.0 returns the midpoint, larger amount is closer to `a` *)
        ((amount *. a) +. b) /. (amount +. 1.)
      in
      let diff_x, diff_y = (abs_float (current_x -. target_x), abs_float (current_y -. target_y)) in
      let create_bounded_camera x y =
        Raylib.Vector2.create
          (Utils.bound camera_state.room_bounds.min.x x camera_state.room_bounds.max.x)
          (Utils.bound camera_state.room_bounds.min.y y camera_state.room_bounds.max.y)
      in
      if state.camera.update_instantly then (
        state.camera.update_instantly <- false;
        create_bounded_camera target_x target_y)
      else (
        let smooth_x, smooth_y =
          match state.camera.motion with
          | LINEAR speed ->
            let between' speed current target =
              let eps = current -. target in
              if abs_float eps < speed then
                target
              else if eps < speed then
                current +. speed
              else if eps > speed then
                current -. speed
              else
                target
            in
            (between' speed current_x target_x, between' speed current_y target_y)
          | SMOOTH (x_speed, y_speed) ->
            let smooth_x =
              if diff_x > 1. then
                between x_speed current_x target_x
              else
                target_x
            in
            let smooth_y =
              if diff_y > 1. then
                between y_speed current_y target_y
              else
                target_y
            in
            (smooth_x, smooth_y)
        in

        create_bounded_camera smooth_x smooth_y)
    in
    let target = bounded_target () in
    Tiled.create_camera_at target state.camera.shake
  in
  state.camera.raylib <- new_camera;
  if state.camera.shake > 0. then
    state.camera.shake <- state.camera.shake -. state.frame.dt;
  state

(* return value is "keep spawned" *)
let update_projectile p (frame_info : frame_info) : bool =
  let despawn_projectile =
    match p.despawn with
    | X_BOUNDS (min_x, max_x) ->
      p.entity.dest.pos.x < min_x -. Config.window.center_x
      || p.entity.dest.pos.x > max_x +. Config.window.center_x
    | TIME_LEFT d -> frame_info.time -. p.spawned.at > d.seconds
  in
  if despawn_projectile then
    false
  else (
    Entity.apply_v frame_info.dt p.entity;
    Sprite.advance_animation frame_info.time p.entity.sprite.texture p.entity.sprite;
    true)

(* this is for inanimate objects like jug fragments or door levers *)
let update_environment (game : game) (state : state) =
  let loose_projectiles = ref [] in
  let update_fragment (f : entity) = Entity.update_pos game.room f state.frame.dt in
  let all_spawned_fragments =
    List.map (fun l -> l.spawned_fragments) game.room.layers |> List.flatten
  in
  let update_lever (lever_sprite, _, trigger) =
    match Sprite.advance_or_despawn state.frame.time lever_sprite.texture lever_sprite with
    | None -> lever_sprite.texture <- state.global.textures.door_lever
    | Some lever_sprite -> ()
  in
  let update_projectile' projectile =
    let keep_spawned = update_projectile projectile state.frame in
    if keep_spawned then
      loose_projectiles := projectile :: !loose_projectiles
  in
  List.iter update_fragment all_spawned_fragments;
  List.iter update_lever game.room.triggers.levers;
  List.iter update_projectile' game.room.loose_projectiles;
  game.room.loose_projectiles <- !loose_projectiles;
  state

(* this despawns corner_text and floating_text, not the normal interaction text *)
let update_interaction_text (game : game) (state : state) =
  let maybe_unset text unset =
    match text with
    | None -> ()
    | Some (_, end_time) ->
      if state.frame.time > end_time.at then
        unset ()
  in
  maybe_unset game.interaction.floating_text (fun () -> game.interaction.floating_text <- None);
  maybe_unset game.interaction.corner_text (fun () -> game.interaction.corner_text <- None);
  state

(* TODO some enemy behavior gets really messed up when an interaction is started
   (eg. frogs when reading lore)
*)
let update_enemies (game : game) (state : state) =
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
              Ghost.start_action state game (TAKE_DAMAGE (projectile.damage, c.direction))))
    in
    List.iter update_projectile' enemy.spawned_projectiles;
    enemy.spawned_projectiles <- !unremoved_projectiles;
    enemy.floor_collision_this_frame <-
      Entity.update_enemy_pos
        ~gravity_multiplier':
          (Some
             (if Enemy.is_dead enemy then
                enemy.json.death_gravity_multiplier
             else
               enemy.entity.config.gravity_multiplier))
        game.room enemy.entity state.frame.dt;
    let interacting () = List.length game.interaction.steps > 0 in
    if (not (interacting ())) && enemy.status.choose_behavior then
      Enemy.choose_behavior enemy state game;
    Sprite.advance_animation state.frame.time enemy.entity.sprite.texture enemy.entity.sprite;
    let advance_or_despawn (sprite : sprite) =
      Sprite.advance_or_despawn state.frame.time sprite.texture sprite
    in
    enemy.damage_sprites <- List.filter_map advance_or_despawn enemy.damage_sprites;
    (match Ghost.get_spell_sprite game.ghost with
    | None -> ()
    | Some (sprite, action_started) -> (
      (* TODO use Collision.between_shapes *)
      match Collision.with_entity enemy.entity sprite.dest with
      | None -> ()
      | Some collision ->
        let damage_kind =
          if game.ghost.current.is_diving then
            DESOLATE_DIVE
          else if not (Ghost.past_cooldown game.ghost.history.dive_cooldown state.frame.time) then
            DESOLATE_DIVE_SHOCKWAVE
          else
            HOWLING_WRAITHS
        in
        ignore
          (Enemy.maybe_take_damage state enemy action_started damage_kind
             (Ghost.get_damage game.ghost damage_kind)
             collision)));

    let maybe_begin_interaction' () =
      let trigger = make_stub_trigger BOSS_KILLED "boss-killed" (Show.enemy_id enemy.id) in
      Ghost.maybe_begin_interaction state game trigger
    in
    if Enemy.is_dead enemy then (
      match enemy.on_killed.interaction_name with
      | None -> ()
      | Some name ->
        if enemy.on_killed.multiple_enemies then (
          let all_bosses_dead =
            let living_bosses =
              List.filter
                (fun (enemy_id, e) -> enemy_id = enemy.id && not (Enemy.is_dead e))
                game.room.enemies
            in
            List.length living_bosses = 0
          in
          if all_bosses_dead then
            maybe_begin_interaction' ())
        else
          maybe_begin_interaction' ())
  in
  List.iter update_enemy game.room.enemies;
  state

let update_npcs (game : game) (state : state) =
  let update_npc (npc : npc) =
    Entity.update_pos game.room npc.entity state.frame.dt;
    Sprite.advance_animation state.frame.time npc.entity.sprite.texture npc.entity.sprite
  in

  let update_pickup_indicators (sprite : sprite) =
    Sprite.advance_animation state.frame.time sprite.texture sprite
  in

  let update_ghost ((_id, ghost) : ghost_id * party_ghost) =
    Sprite.advance_animation state.frame.time ghost.entity.sprite.texture ghost.entity.sprite;
    if ghost.entity.update_pos then (
      Entity.update_pos game.room ghost.entity state.frame.dt;
      Entity.maybe_unset_current_floor ghost.entity game.room)
  in

  List.iter update_ghost game.ghosts';
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
        | Some collision ->
          ignore
            (Enemy.maybe_take_damage state enemy vs_start_time VENGEFUL_SPIRIT
               (Ghost.get_damage game.ghost VENGEFUL_SPIRIT)
               collision))
    in
    List.iter maybe_damage_enemy game.room.enemies
  in
  let update_vengeful_spirit (projectile : projectile) =
    let keep_spawned = update_projectile projectile state.frame in
    if keep_spawned then
      damage_enemies projectile.spawned projectile.entity.sprite
    else
      game.ghost.spawned_vengeful_spirits <-
        List.filter (fun p -> p <> projectile) game.ghost.spawned_vengeful_spirits
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
  update_frame_input C_DASH state.frame_inputs.c_dash;
  update_frame_input DASH state.frame_inputs.dash;
  update_frame_input D_NAIL state.frame_inputs.dream_nail;
  update_frame_input FOCUS state.frame_inputs.focus;
  update_frame_input JUMP state.frame_inputs.jump;
  update_frame_input NAIL state.frame_inputs.nail;
  update_frame_input PAUSE state.frame_inputs.pause;
  update_frame_input INTERACT state.frame_inputs.interact;
  state

let update_menu_choice (menu : menu) frame_inputs =
  if frame_inputs.down.pressed then
    menu.current_choice_idx <- Int.min (menu.current_choice_idx + 1) (List.length menu.choices - 1);
  if frame_inputs.up.pressed then
    menu.current_choice_idx <- Int.max 0 (menu.current_choice_idx - 1)

let tick (state : state) =
  (* TODO-3 add sound effects and music *)
  if not (holding_shift ()) then (
    if Controls.key_pressed DEBUG_3 then
      if state.debug.show_frame_inputs then (
        state.debug.show_frame_inputs <- false;
        print " disabled show_frame_inputs at %d\n\\----------------------/\n" state.frame.idx)
      else (
        state.debug.show_frame_inputs <- true;
        print "\n/---------------------\\\n show_frame_inputs debug at %d" state.frame.idx);
    if Controls.key_pressed DEBUG_4 then
      if state.debug.enabled then (
        state.debug.enabled <- false;
        print " disabled debug at %d\n\\----------------------/\n" state.frame.idx)
      else (
        state.debug.enabled <- true;
        print "\n/---------------------\\\n enabled debug at %d" state.frame.idx));

  state.debug.rects <- [];
  match state.game_context with
  | SAVE_FILES (menu, save_slots)
  | MAIN_MENU (menu, save_slots) ->
    state |> update_frame_inputs |> Menu.update_main_menu menu save_slots
  | IN_PROGRESS game ->
    if game.debug_paused then
      if key_pressed DEBUG_2 then
        state
        |> update_frame_inputs
        |> Menu.update_pause_menu game
        |> Ghost.handle_debug_keys game
        |> Ghost.update game
        |> update_spawned_vengeful_spirits game
        |> update_enemies game
        |> update_npcs game
        |> update_environment game
        |> update_camera game
      else
        state |> update_frame_inputs |> Menu.update_pause_menu game |> Ghost.handle_debug_keys game
    else (
      let state' = state |> update_frame_inputs |> Menu.update_pause_menu game in
      match state'.pause_menu with
      | Some menu -> state'
      | None ->
        let show_triggers ?(color = Raylib.Color.blue) triggers =
          state.debug.rects <- List.map (fun r -> (color, r.dest)) triggers @ state.debug.rects
        in
        let show_respawn_triggers ?(color = Raylib.Color.blue) triggers =
          state.debug.rects <- List.map (fun (_, r) -> (color, r.dest)) triggers @ state.debug.rects
        in

        show_triggers game.room.triggers.lore;
        show_triggers game.room.triggers.cutscene;
        show_triggers game.room.triggers.d_nail;
        show_triggers ~color:Raylib.Color.red game.room.triggers.item_pickups;

        state.debug.rects <-
          List.map
            (fun (s : sprite) ->
              tmp "setting debug rect: %s" (Show.rect s.dest);
              (Raylib.Color.orange, s.dest))
            game.room.platforms
          @ state.debug.rects;

        (* show_respawn_triggers ~color:(Raylib.Color.red) game.room.triggers.respawn; *)
        if state.should_save then (
          Menu.save_game game state ignore;
          state.should_save <- false);
        state'
        |> Ghost.handle_debug_keys game
        |> Ghost.update game
        |> update_spawned_vengeful_spirits game
        |> update_enemies game
        |> update_npcs game
        |> update_environment game
        |> update_interaction_text game
        |> update_camera game)
