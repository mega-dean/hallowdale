open Types
open Controls

(* this function initializes state and sets game_context to MAIN_MENU *)
let init window_w window_h window_scale : state =
  let print_line s =
    let left_line = String.make 10 '=' in
    let right_line = String.make (90 - String.length s) '=' in
    print "%s %s %s" left_line s right_line
  in

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

  let ghosts_file = File.read_config "ghosts" Json_j.ghosts_file_of_string in
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
        path = { asset_dir = NPCS; character_name = "shared"; pose_name = name };
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
  let skybox = build_shared_npc_texture "skybox" in
  let world_map =
    (* TODO make a world-map-small.png that doesn't look bad at small window sizes *)
    Sprite.build_static_texture "world-map"
  in
  let ability_outlines = Sprite.build_static_texture ~asset_dir:GHOSTS "ability-outlines" in

  let ghost_bodies : ghost_body_textures =
    let load_ghost_body_texture name : ghost_body_texture =
      let config =
        let key =
          match name with
          | "nail-up"
          | "nail-down" ->
            "nail"
          | _ -> name
        in
        match List.assoc_opt key ghosts_file.body_textures with
        | Some c -> c
        | None ->
          if key = name then
            failwithf "missing config for '%s' in config/ghosts.json" key
          else
            failwithf "missing config for '%s' (from name '%s') in config/ghosts.json" key name
      in
      {
        texture' =
          Sprite.build_texture_from_config
            {
              path = { asset_dir = GHOSTS; character_name = "body"; pose_name = name };
              count = config.count;
              duration = { seconds = config.duration };
              x_offset = config.x_offset;
              y_offset = config.y_offset;
            };
        render_offset =
          { x = config.x_offset *. Config.scale.ghost; y = config.y_offset *. Config.scale.ghost };
      }
    in
    {
      cast = load_ghost_body_texture "cast";
      crawl = load_ghost_body_texture "crawl";
      dash = load_ghost_body_texture "dash";
      dive = load_ghost_body_texture "dive";
      fall = load_ghost_body_texture "fall";
      flap = load_ghost_body_texture "flap";
      focus = load_ghost_body_texture "focus";
      idle = load_ghost_body_texture "idle";
      jump = load_ghost_body_texture "jump";
      nail = load_ghost_body_texture "nail";
      read = load_ghost_body_texture "read";
      take_damage = load_ghost_body_texture "take-damage";
      walk = load_ghost_body_texture "walk";
      wall_slide = load_ghost_body_texture "wall-slide";
    }
  in

  let damage =
    let json_config = List.assoc "damage" shared_enemy_configs in
    Sprite.build_texture_from_config ~particle:true
      {
        path = { asset_dir = ENEMIES; character_name = "shared"; pose_name = "damage" };
        count = json_config.count;
        duration = { seconds = json_config.duration };
        x_offset = 0.;
        y_offset = 0.;
      }
  in

  print_line "loading platforms...";
  let platforms =
    let load_platform_texture name =
      ( name,
        Sprite.build_texture_from_config
          {
            path = { asset_dir = TILED; character_name = "platforms"; pose_name = name };
            count = 1;
            duration = { seconds = 0. };
            x_offset = 0.;
            y_offset = 0.;
          } )
    in
    let path =
      (* duplicated in Sprite.get_path *)
      File.make_assets_path [ Show.asset_dir TILED; "platforms" ]
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
  let rotating_platform =
    Sprite.build_texture_from_config ~once:true
      {
        path = { asset_dir = TILED; character_name = "platforms"; pose_name = "rotating" };
        count = 5;
        duration = { seconds = Config.platform.rotatable_anim_duration };
        x_offset = 0.;
        y_offset = 0.;
      }
  in
  print_line "done loading platforms";

  let load_sound name =
    let path = File.make_assets_path [ "audio"; "sound-effects"; fmt "%s.ogg" name ] in
    (name, Raylib.load_sound path)
  in

  let sounds =
    List.map load_sound
      [
        "punch";
        "nail-swing";
        "sword_hit";
        "nail-hit-metal";
        "glass-break";
        "break";
        "spray";
        "alarmswitch";
        "cancel";
        "click";
        "confirm";
        "menu-close";
        "menu-expand";
        "dash";
      ]
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
          rotating_platform;
          ghost_bodies;
          skybox;
          world_map;
        };
      sounds;
    }
  in

  let camera_target = Raylib.Vector2.create 0. 0. in
  let camera =
    Tiled.create_camera_at camera_target 0. Config.window.center.x Config.window.center.y
  in

  print_line "done_initializing_state";

  (* TODO this should get saved to the save file instead of resetting every time *)
  let settings = { music_volume = 0.5; sound_effects_volume = 0.5 } in

  let load_music name ?(intro = 0.) ?(loop = Float.max_float) areas : area_music =
    let music =
      Raylib.load_music_stream (File.make_assets_path [ "audio"; "music"; fmt "%s.ogg" name ])
    in
    Raylib.set_music_volume music settings.music_volume;
    (* TODO this probably isn't a good way to do this
       - this is starting all ~8 area musics, and then only advancing the one for the current area
       - should probably be using stop_music_stream
    *)
    Raylib.play_music_stream music;
    { areas; music = { name; t = music; loop_start = { at = intro }; loop_end = { at = loop } } }
  in

  let area_musics : area_music list =
    [
      (* TODO these loop times are not very precise *)
      load_music "as-i-lay-me-down" ~intro:72.128 ~loop:171.947
        [ FORGOTTEN_CLASSROOMS; INFECTED_CLASSROOMS ];
      load_music "daybreak" [ AC_REPAIR_ANNEX ];
      load_music "ending" [ COMPUTER_WING ];
      load_music "greendale" [ LIBRARY; MEOW_MEOW_BEENZ ];
      load_music "kiss-from-a-rose" ~intro:22.255 ~loop:73.545 [ CITY_OF_CHAIRS; OUTLANDS ];
      load_music "mash-theme" ~loop:75.451 [ BASEMENT ];
      load_music "somewhere-out-there" ~intro:35.306 ~loop:93.553 [ TRAMPOLINEPATH ];
    ]
  in

  {
    menu_music = (load_music "opening" []).music;
    area_musics;
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
        motion = SMOOTH (Config.window.camera_motion.x, Config.window.camera_motion.y);
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
        open_map = { pressed = false; down = false; released = false; down_since = None };
        pause = { pressed = false; down = false; released = false; down_since = None };
        interact = { pressed = false; down = false; released = false; down_since = None };
      };
    debug = { enabled = false; show_frame_inputs = true; rects = [] };
    global;
    settings;
  }

let update_camera (game : game) (state : state) =
  let trigger_config : trigger option =
    Player.find_trigger_collision game.player game.room.triggers.camera
  in
  let subject =
    match state.camera.subject with
    | GHOST ->
      let e = game.player.ghost.entity in
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
            let left = trigger.dest.pos.x +. Config.window.center.x in
            let right = trigger.dest.pos.x +. trigger.dest.w -. Config.window.center.x in
            match x_config with
            | "gx" -> subject.x
            | ">x" -> Utils.bound left subject.x camera_state.room_bounds.max.x
            | "<x" -> Utils.bound camera_state.room_bounds.min.x subject.x right
            | "x" -> Utils.bound left subject.x right
            | _ -> failwithf "invalid x_bound %s" x_config
          in
          let y_bound =
            let top = trigger.dest.pos.y +. Config.window.center.y in
            let bottom = trigger.dest.pos.y +. trigger.dest.h -. Config.window.center.y in
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
        (* TODO adjust camera motion for fps *)
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
    Tiled.create_camera_at (bounded_target ()) state.camera.shake Config.window.center.x
      Config.window.center.y
  in
  state.camera.raylib <- new_camera;
  if state.camera.shake > 0. then
    state.camera.shake <- state.camera.shake -. state.frame.dt;
  state

(* return value is "keep spawned" *)
let update_projectile p (state : state) : bool =
  let despawn_projectile =
    match p.despawn with
    | X_BOUNDS (min_x, max_x) ->
      p.entity.dest.pos.x < min_x -. Config.window.center.x
      || p.entity.dest.pos.x > max_x +. Config.window.center.x
    | TIME_LEFT d -> state.frame.time -. p.spawned.at > d.seconds
  in
  if despawn_projectile then
    false
  else (
    Entity.apply_v state.frame.dt p.entity;
    Sprite.advance_animation state.frame.time p.entity.sprite.texture p.entity.sprite;
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
    let keep_spawned = update_projectile projectile state in
    if keep_spawned then
      loose_projectiles := projectile :: !loose_projectiles
  in
  let initiate_platform_reactions (platform : platform) =
    match platform.kind with
    | Some (TEMPORARY VISIBLE) ->
      platform.kind <- Some (TEMPORARY (TOUCHED Config.platform.disappearable_touched_time))
    | Some (DISAPPEARABLE VISIBLE) ->
      platform.kind <- Some (DISAPPEARABLE (TOUCHED Config.platform.disappearable_touched_time))
    | Some (ROTATABLE UPRIGHT) ->
      platform.kind <- Some (ROTATABLE (TOUCHED Config.platform.rotatable_touched_time))
    | _ -> ()
  in
  let finish_platform_reactions (platform : platform) =
    let shake_platform () =
      let amount = 4. in
      if state.frame.idx mod 2 = 0 then
        platform.sprite.dest.pos.x <- platform.sprite.dest.pos.x +. amount
      else
        platform.sprite.dest.pos.x <- platform.sprite.dest.pos.x -. amount
    in

    let decrement_time ~continue ~change f =
      let new_f = f -. state.frame.dt in
      if new_f > 0. then
        continue new_f
      else
        change new_f
    in

    (* TODO this respawns the floor when the room is exited *)
    let handle_temporary_platform (state' : disappearable_state) =
      match state' with
      | VISIBLE -> ()
      | TOUCHED f ->
        decrement_time
          ~continue:(fun new_f ->
            shake_platform ();
            platform.kind <- Some (TEMPORARY (TOUCHED new_f)))
          ~change:(fun new_f ->
            game.player.ghost.entity.current_floor <- None;
            platform.sprite.dest.pos.x <- -.platform.sprite.dest.pos.x;
            platform.kind <-
              Some (TEMPORARY (INVISIBLE Config.platform.disappearable_invisible_time)))
          f
      | INVISIBLE f -> ()
    in

    let handle_disappearable_platform (state' : disappearable_state) =
      match state' with
      | VISIBLE -> ()
      | TOUCHED f ->
        decrement_time
          ~continue:(fun new_f ->
            shake_platform ();
            platform.kind <- Some (DISAPPEARABLE (TOUCHED new_f)))
          ~change:(fun new_f ->
            game.player.ghost.entity.current_floor <- None;
            platform.sprite.dest.pos.x <- -.platform.sprite.dest.pos.x;
            platform.kind <-
              Some (DISAPPEARABLE (INVISIBLE Config.platform.disappearable_invisible_time)))
          f
      | INVISIBLE f ->
        decrement_time
          ~continue:(fun new_f -> platform.kind <- Some (DISAPPEARABLE (INVISIBLE new_f)))
          ~change:(fun new_f ->
            platform.sprite.dest.pos.x <- platform.original_x;
            platform.kind <- Some (DISAPPEARABLE VISIBLE))
          f
    in

    let handle_rotatable_platform (state' : rotatable_state) =
      match state' with
      | UPRIGHT -> ()
      | TOUCHED f ->
        decrement_time
          ~continue:(fun new_f ->
            shake_platform ();
            platform.kind <- Some (ROTATABLE (TOUCHED new_f)))
          ~change:(fun new_f -> Platform.start_rotating platform game state.global.textures)
          f
      | ROTATING_NOW -> (
        match
          Sprite.advance_or_despawn state.frame.time platform.sprite.texture platform.sprite
        with
        | None -> Platform.finish_rotating platform game state.global.textures
        | Some s -> ())
      | UPSIDE_DOWN f ->
        decrement_time
          ~continue:(fun new_f -> platform.kind <- Some (ROTATABLE (UPSIDE_DOWN new_f)))
          ~change:(fun new_f -> Platform.reset_rotation platform game state.global.textures)
          f
    in
    match platform.kind with
    | None -> ()
    | Some (TEMPORARY state') -> handle_temporary_platform state'
    | Some (DISAPPEARABLE state') -> handle_disappearable_platform state'
    | Some (ROTATABLE state') -> handle_rotatable_platform state'
  in
  List.iter initiate_platform_reactions game.player.ghost.entity.current_platforms;
  List.iter finish_platform_reactions game.room.platforms;
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
    | Some (tt : Interaction.transient_text) -> (
      match tt.visible with
      | PAUSE_MENU -> (
        match state.pause_menu with
        | Some _ -> ()
        | None -> unset ())
      | TIME end_time ->
        if state.frame.time > end_time.at then
          unset ())
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
      let keep_spawned = update_projectile projectile state in
      if keep_spawned then (
        unremoved_projectiles := projectile :: !unremoved_projectiles;
        if Player.is_vulnerable state game then (
          match Collision.with_entity game.player.ghost.entity projectile.entity.dest with
          | None -> ()
          | Some c ->
            (* TODO add collision shape to enemy projectiles *)
            if Collision.between_entities game.player.ghost.entity projectile.entity then
              Player.start_action state game (TAKE_DAMAGE (projectile.damage, c.direction))))
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
    (match Player.get_spell_sprite game.player with
    | None -> ()
    | Some (sprite, action_started) -> (
      (* TODO use Collision.between_shapes *)
      match Collision.with_entity enemy.entity sprite.dest with
      | None -> ()
      | Some collision ->
        let damage_kind =
          if game.player.current.is_diving then
            DESOLATE_DIVE
          else if not (Player.past_cooldown game.player.history.dive_cooldown state.frame.time) then
            DESOLATE_DIVE_SHOCKWAVE
          else
            HOWLING_WRAITHS
        in
        ignore
          (Enemy.maybe_take_damage state enemy action_started damage_kind
             (Player.get_damage game.player damage_kind)
             collision)));

    let maybe_begin_interaction' () =
      let trigger = make_stub_trigger BOSS_KILLED "boss-killed" (Show.enemy_id enemy.id) in
      Player.maybe_begin_interaction state game trigger
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

  let update_ghost (party_ghost : party_ghost) =
    let ghost = party_ghost.ghost in
    Sprite.advance_animation state.frame.time ghost.entity.sprite.texture ghost.entity.sprite;
    if ghost.entity.update_pos then (
      Entity.update_pos game.room ghost.entity state.frame.dt;
      Entity.maybe_unset_current_floor ghost.entity game.room)
  in

  List.iter update_ghost game.party;
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
               (Player.get_damage game.player VENGEFUL_SPIRIT)
               collision))
    in
    List.iter maybe_damage_enemy game.room.enemies
  in
  let update_vengeful_spirit (projectile : projectile) =
    let keep_spawned = update_projectile projectile state in
    if keep_spawned then
      damage_enemies projectile.spawned projectile.entity.sprite
    else
      game.player.spawned_vengeful_spirits <-
        List.filter (fun p -> p <> projectile) game.player.spawned_vengeful_spirits
  in

  List.iter update_vengeful_spirit game.player.spawned_vengeful_spirits;
  state

let update_frame_inputs (state : state) : state =
  let update_frame_input ?(direction = false) key (input : frame_input) =
    input.released <- key_released ~direction key;
    input.down <- key_down ~direction key;
    input.pressed <- key_pressed ~direction key;
    if input.pressed then
      input.down_since <- Some { at = state.frame.time }
    else if not input.down then
      input.down_since <- None
  in
  update_frame_input ~direction:true (ARROW UP) state.frame_inputs.up;
  update_frame_input ~direction:true (ARROW DOWN) state.frame_inputs.down;
  update_frame_input ~direction:true (ARROW LEFT) state.frame_inputs.left;
  update_frame_input ~direction:true (ARROW RIGHT) state.frame_inputs.right;
  update_frame_input CAST state.frame_inputs.cast;
  update_frame_input C_DASH state.frame_inputs.c_dash;
  update_frame_input DASH state.frame_inputs.dash;
  update_frame_input D_NAIL state.frame_inputs.dream_nail;
  update_frame_input FOCUS state.frame_inputs.focus;
  update_frame_input JUMP state.frame_inputs.jump;
  update_frame_input NAIL state.frame_inputs.nail;
  update_frame_input OPEN_MAP state.frame_inputs.open_map;
  update_frame_input PAUSE state.frame_inputs.pause;
  update_frame_input INTERACT state.frame_inputs.interact;
  state

let add_debug_rects state rects = state.debug.rects <- rects @ state.debug.rects

let tick (state : state) =
  if not (holding_shift ()) then (
    if Controls.key_pressed DEBUG_3 then
      if state.debug.show_frame_inputs then (
        state.debug.show_frame_inputs <- false;
        print " disabled show_frame_inputs at %d\n\\----------------------/\n" state.frame.idx)
      else (
        state.debug.show_frame_inputs <- true;
        print "\n/----------------------\\\n show_frame_inputs debug at %d" state.frame.idx);
    if Controls.key_pressed DEBUG_4 then
      if state.debug.enabled then (
        state.debug.enabled <- false;
        print " disabled debug at %d\n\\----------------------/\n" state.frame.idx)
      else (
        state.debug.enabled <- true;
        print "\n/----------------------\\\n enabled debug at %d" state.frame.idx));
  state.debug.rects <- [];
  match state.game_context with
  | SAVE_FILES (menu, save_slots)
  | MAIN_MENU (menu, save_slots) ->
    if Raylib.get_music_time_played state.menu_music.t > state.menu_music.loop_end.at then
      Raylib.seek_music_stream state.menu_music.t
        (Utils.bound 0.1 state.menu_music.loop_start.at Float.max_float);
    Raylib.update_music_stream state.menu_music.t;
    state |> update_frame_inputs |> Menu.update_main_menu menu save_slots
  | IN_PROGRESS game ->
    (* TODO the music stutters at room transitions
       - maybe need to check difference in state.frame.time and seek forward
    *)
    let music = game.music.music in
    if Raylib.get_music_time_played music.t > music.loop_end.at then
      Raylib.seek_music_stream music.t (Utils.bound 0.1 music.loop_start.at Float.max_float);

    Raylib.update_music_stream music.t;

    if game.debug_paused then
      if key_pressed DEBUG_2 then
        state
        |> update_frame_inputs
        |> Menu.update_pause_menu game
        |> Player.handle_debug_keys game
        |> Player.update game
        |> update_spawned_vengeful_spirits game
        |> update_enemies game
        |> update_npcs game
        |> update_environment game
        |> update_camera game
      else
        state |> update_frame_inputs |> Menu.update_pause_menu game |> Player.handle_debug_keys game
    else (
      let state' = state |> update_frame_inputs |> Menu.update_pause_menu game in
      match state'.pause_menu with
      | Some menu ->
        (match game.mode with
        | CLASSIC
        | DEMO ->
          ()
        | STEEL_SOLE ->
          let time, current_time =
            let current_time_frames = state.frame.idx in
            let get_time frames =
              let ms' =
                (* TODO this won't be consistent if a game is started with one fps, and loaded with a different one
                   - need to convert and save hours/minutes/seconds/ms to save file, then use current fps to count up
                *)
                fmt "%.3f"
                  ((frames mod Config.window.fps |> Int.to_float)
                  /. (Config.window.fps |> Int.to_float))
              in
              let ms = String.sub ms' 1 (String.length ms' - 1) in
              let seconds' = frames / Config.window.fps in
              let seconds = seconds' mod 60 in
              let minutes' = seconds' / 60 in
              let minutes = minutes' mod 60 in
              let hours' = minutes' / 60 in
              let hours = hours' mod 60 in
              fmt "%02d:%02d:%02d%s" hours minutes seconds ms
            in
            if List.length game.progress.steel_sole.purple_pens_found = 0 then
              ("", get_time current_time_frames)
            else (
              let frames' =
                (* take the first because newest entries are pushed to the front *)
                List.hd game.progress.steel_sole.purple_pens_found |> fst
              in
              (fmt " in %s" (get_time frames'), get_time current_time_frames))
          in
          let pen_lore =
            (* TODO this is a pretty naive way to check for pen lore, maybe the json file should
               be an object instead of a list
            *)
            List.filter (fun (k, v) -> Str.string_match (Str.regexp "[1-6]") k 0) state.global.lore
          in
          let total_purple_pen_count = List.length pen_lore in
          game.interaction.corner_text <-
            Some
              {
                content =
                  fmt "%d / %d purple pens found%s, %d dunks, %d c-dashes --- %s"
                    (List.length game.progress.steel_sole.purple_pens_found)
                    total_purple_pen_count time game.progress.steel_sole.dunks
                    game.progress.steel_sole.c_dashes current_time;
                visible = PAUSE_MENU;
              });
        state'
      | None ->
        if state.debug.enabled then (
          let show_triggers ?(color = Raylib.Color.blue) triggers =
            add_debug_rects state (List.map (fun r -> (color, r.dest)) triggers)
          in
          let show_respawn_triggers ?(color = Raylib.Color.blue) triggers =
            add_debug_rects state (List.map (fun (_, r) -> (color, r.dest)) triggers)
          in

          show_triggers game.room.triggers.lore;
          show_triggers game.room.triggers.cutscene;
          show_triggers game.room.triggers.d_nail;
          show_triggers ~color:Raylib.Color.red game.room.triggers.item_pickups;

          (* show_respawn_triggers ~color:(Raylib.Color.red) game.room.triggers.respawn; *)
          add_debug_rects state
            (List.map
               (fun (p : platform) -> (Raylib.Color.orange, p.sprite.dest))
               game.room.platforms);

          add_debug_rects state
            (List.map (fun (rect, _) -> (Raylib.Color.orange, rect)) game.room.conveyor_belts);

          add_debug_rects state
            (List.map (fun (r : rect) -> (Raylib.Color.orange, r)) game.room.floors);

          add_debug_rects state
            (List.map
               (fun (r : rect) -> (Raylib.Color.red, r))
               (game.room.spikes @ game.room.hazards @ List.map snd game.room.platform_spikes)));

        if state.should_save then (
          Game.save game state;
          state.should_save <- false);

        state'
        |> Player.handle_debug_keys game
        |> Player.update game
        |> update_spawned_vengeful_spirits game
        |> update_enemies game
        |> update_npcs game
        |> update_environment game
        |> update_interaction_text game
        |> update_camera game)
