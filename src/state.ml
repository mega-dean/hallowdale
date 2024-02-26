open Utils
open Types

(* this function initializes state and sets game_context to MAIN_MENU *)
let init () : state =
  let print_with_line s =
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
  let raindrop = build_shared_npc_texture "raindrop" in
  let world_map =
    (* TODO make a world-map-small.png that doesn't look bad at small window sizes *)
    Sprite.build_static_texture "world-map"
  in
  let ability_outlines = Sprite.build_static_texture ~asset_dir:GHOSTS "ability-outlines" in

  let ghost_bodies : ghost_body_textures =
    let load_ghost_body_texture name : texture =
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
      (* offsets_scale isn't needed here because offsets are already set to the thing they are
         supposed to be
      *)
      Sprite.build_texture_from_config
        {
          path = { asset_dir = GHOSTS; character_name = "body"; pose_name = name };
          count = config.count;
          duration = { seconds = config.duration };
          (* entity_neck_x/y is already scaled by scale.ghost in config.ml *)
          x_offset = (config.x_offset *. Config.scale.ghost) -. Config.ghost.entity_neck_x;
          y_offset = (config.y_offset *. Config.scale.ghost) -. Config.ghost.entity_neck_y;
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

  let platforms : (string * texture) list =
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
      let name, extension = String.split_at_first '.' filename in
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

  let sounds =
    List.map Audio.load_sound
      [
        "high-punch";
        "punch";
        "nail-swing";
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

  let (lore, honda_quotes) : string String.Map.t * string list =
    let lore_file = File.read_config "lore" Json_j.lore_file_of_string in
    let honda_lines, lore_lines = List.partition (fun (k, _v) -> k = "honda") lore_file in
    (lore_lines |> List.to_string_map, List.map snd honda_lines)
  in

  let global =
    {
      lore;
      weapons = File.read_config "weapons" Json_j.weapons_file_of_string |> List.to_string_map;
      honda_quotes;
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
          platforms = platforms |> List.to_string_map;
          rotating_platform;
          ghost_bodies;
          skybox;
          raindrop;
          world_map;
        };
      sounds = sounds |> List.to_string_map;
    }
  in

  let camera_target = Raylib.Vector2.create 0. 0. in
  let camera =
    Tiled.create_camera_at camera_target 0. Config.window.center.x Config.window.center.y
  in

  print_with_line "done_initializing_state";

  (* TODO this should get saved to the save file instead of resetting every time *)
  let settings = { music_volume = 0.5; sound_effects_volume = 0.5 } in

  let area_musics : area_music list =
    [
      (* TODO these loop times are not very precise *)
      Audio.load_music "as-i-lay-me-down" ~intro:72.128 ~loop:171.947
        [ FORGOTTEN_CLASSROOMS; INFECTED_CLASSROOMS ]
        settings.music_volume;
      Audio.load_music "daybreak" [ AC_REPAIR_ANNEX ] settings.music_volume;
      Audio.load_music "greendale" [ LIBRARY; MEOW_MEOW_BEENZ; VENTWAYS ] settings.music_volume;
      Audio.load_music "kiss-from-a-rose" ~intro:22.255 ~loop:73.545 [ CITY_OF_CHAIRS; OUTLANDS ]
        settings.music_volume;
      Audio.load_music "mash-theme" ~loop:75.451 [ BASEMENT; COMPUTER_WING ] settings.music_volume;
      Audio.load_music "somewhere-out-there" ~intro:35.306 ~loop:93.553 [ TRAMPOLINEPATH ]
        settings.music_volume;
    ]
  in

  {
    menu_music = (Audio.load_music "opening" [] settings.music_volume).music;
    area_musics;
    context = MAIN_MENU (Menu.main_menu (), Game.load_all_save_slots ());
    pause_menu = None;
    save_pos = None;
    world;
    controls = Controls.load ();
    rebinding_action = None;
    camera =
      {
        raylib = camera;
        subject = GHOST;
        shake = 0.;
        update_instantly = false;
        motion = SMOOTH (Config.window.camera_motion.x, Config.window.camera_motion.y);
      };
    ignore_camera_triggers = false;
    screen_fade = None;
    frame = { idx = 0; dt = 0.; time = 0.; timeout = Int.max_int };
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
    debug =
      { enabled = false; show_frame_inputs = true; rects = []; paused = false; safe_ss = false };
    global;
    settings;
  }

(* return value is "keep spawned" *)
let update_projectile (projectile : projectile) (game : game) (state : state) : bool =
  (match projectile.update_v with
  | None -> ()
  | Some WAVY ->
    projectile.entity.v <-
      {
        x = projectile.entity.v.x;
        y = projectile.entity.v.y +. cos (state.frame.time -. projectile.spawned.at);
      }
  | Some (HOMING dv) ->
    projectile.entity.v <-
      {
        x =
          (if rect_center_x projectile.entity.dest < rect_center_x game.player.ghost.entity.dest
           then
             projectile.entity.v.x +. dv
           else
             projectile.entity.v.x -. dv);
        y =
          (* don't take rect_center_y of ghost so it aims the projectile at the ghost's head *)
          (if rect_center_y projectile.entity.dest < game.player.ghost.entity.dest.pos.y then
             projectile.entity.v.y +. dv
           else
             projectile.entity.v.y -. dv);
      });
  let collisions =
    match projectile.orbiting with
    | None ->
      Entity.update_pos ~_debug:true ~apply_floor_collisions:projectile.collide_with_floors
        game.room state.frame.dt projectile.entity
    | Some (offset, clockwise, enemy) ->
      let new_pos =
        let w, h = get_scaled_texture_size 1. projectile.entity.sprite.texture in
        (* TODO radius is slightly different based on window_scale *)
        let radius = Enemy.get_attr enemy "orbit_radius" in
        let speed =
          if enemy.health.current < enemy.health.max / 2 then
            Enemy.get_attr enemy "orbit_speed_wounded"
          else
            Enemy.get_attr enemy "orbit_speed_healthy"
        in
        let mult = if clockwise then -1. else 1. in
        {
          x =
            rect_center_x enemy.entity.dest
            +. (radius *. sin (mult *. (offset +. (state.frame.time *. speed))))
            -. w;
          y =
            rect_center_y enemy.entity.dest
            +. (radius *. cos (mult *. (offset +. (state.frame.time *. speed))))
            -. h;
        }
      in
      projectile.entity.dest.pos <- new_pos;
      []
  in
  let despawn_projectile =
    match projectile.despawn with
    (* these adjust by Config.window.center.x/y as a buffer so the projectiles don't
       despawn on-screen *)
    | BOSS_AREA_Y (min_y, max_y) ->
      let y = rect_center_y projectile.entity.dest in
      y < min_y -. Config.window.center.y || y > max_y +. Config.window.center.y
    | BOSS_AREA_X (min_x, max_x) ->
      let x = rect_center_x projectile.entity.dest in
      x < min_x -. Config.window.center.x || x > max_x +. Config.window.center.x
    | TIME_LEFT d -> state.frame.time -. projectile.spawned.at > d.seconds
    | DETONATE (d, new_projectiles) ->
      let despawn = state.frame.time -. projectile.spawned.at > d.seconds in
      if despawn then
        List.iter (fun (p : projectile) -> Entity.unfreeze p.entity) new_projectiles;
      despawn
    | UNTIL_FLOOR_COLLISION -> List.length collisions > 0
    | UNTIL_ENEMY_DEATH -> false
  in
  if despawn_projectile then
    false
  else (
    Sprite.advance_animation state.frame.time projectile.entity.sprite;
    true)

(* this is for inanimate objects like jug fragments or door levers *)
let update_environment (game : game) (state : state) =
  let loose_projectiles = ref [] in
  let update_fragment (entity : entity) = Entity.update_pos_ game.room state.frame.dt entity in
  let all_spawned_fragments =
    List.map (fun l -> l.spawned_fragments) game.room.layers |> List.flatten
  in
  let update_lever lever =
    match Sprite.advance_or_despawn state.frame.time lever.sprite with
    | None -> lever.sprite.texture <- state.global.textures.door_lever
    | Some _lever_sprite -> ()
  in
  let update_projectile' projectile =
    let keep_spawned = update_projectile projectile game state in
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
    | Some (LOCKED_DOOR (key, _state)) ->
      let unlocked =
        if key = "monkey-gas" then
          game.player.abilities.howling_wraiths
        else
          List.mem key game.progress.keys_found
      in
      if unlocked then
        (* after unlocking a door, shake and permanently disappear like a TEMPORARY platform *)
        platform.kind <-
          Some (LOCKED_DOOR (key, TOUCHED Config.platform.disappearable_touched_time))
      else
        game.interaction.floating_text <-
          Some
            {
              content = "This door is locked.";
              visible = Interaction.make_UNTIL Config.text.short_floating_duration state.frame.time;
              scale = 1.;
            }
    | None
    | Some (TEMPORARY _)
    | Some (DISAPPEARABLE _)
    | Some (ROTATABLE _) ->
      ()
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

    let handle_permanently_removable_platform ?(key : string option) (state' : disappearable_state)
        =
      let locked_door =
        match key with
        | None -> false
        | Some _k -> true
      in
      let new_platform_kind s =
        if locked_door then
          Some (LOCKED_DOOR (Option.get key, s))
        else
          Some (TEMPORARY s)
      in
      match state' with
      | VISIBLE -> ()
      | TOUCHED f ->
        decrement_time
          ~continue:(fun new_f ->
            shake_platform ();
            platform.kind <- new_platform_kind (TOUCHED new_f))
          ~change:(fun _new_f ->
            game.room.progress.removed_platform_ids <-
              platform.id :: game.room.progress.removed_platform_ids;
            if not locked_door then
              Entity.unset_removed_floor game.player.ghost.entity platform.sprite.dest;
            platform.sprite.dest.pos.x <- -.platform.sprite.dest.pos.x;
            platform.kind <-
              new_platform_kind (INVISIBLE Config.platform.disappearable_invisible_time))
          f
      | INVISIBLE _f -> ()
    in

    let handle_temporary_platform (state' : disappearable_state) =
      handle_permanently_removable_platform state'
    in

    let handle_locked_door_platform ~key (state' : disappearable_state) =
      handle_permanently_removable_platform ~key state'
    in

    let handle_disappearable_platform (state' : disappearable_state) =
      match state' with
      | VISIBLE -> ()
      | TOUCHED f ->
        decrement_time
          ~continue:(fun new_f ->
            shake_platform ();
            platform.kind <- Some (DISAPPEARABLE (TOUCHED new_f)))
          ~change:(fun _new_f ->
            Entity.unset_removed_floor game.player.ghost.entity platform.sprite.dest;
            platform.sprite.dest.pos.x <- -.platform.sprite.dest.pos.x;
            platform.kind <-
              Some (DISAPPEARABLE (INVISIBLE Config.platform.disappearable_invisible_time)))
          f
      | INVISIBLE f ->
        decrement_time
          ~continue:(fun new_f -> platform.kind <- Some (DISAPPEARABLE (INVISIBLE new_f)))
          ~change:(fun _new_f ->
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
          ~change:(fun _new_f -> Platform.start_rotating platform state.global.textures)
          f
      | ROTATING_NOW -> (
        match Sprite.advance_or_despawn state.frame.time platform.sprite with
        | None -> Platform.finish_rotating platform game state.global.textures
        | Some _s -> ())
      | UPSIDE_DOWN f ->
        decrement_time
          ~continue:(fun new_f -> platform.kind <- Some (ROTATABLE (UPSIDE_DOWN new_f)))
          ~change:(fun _new_f -> Platform.reset_rotation platform game state.global.textures)
          f
    in
    match platform.kind with
    | None -> ()
    | Some (TEMPORARY state') -> handle_temporary_platform state'
    | Some (DISAPPEARABLE state') -> handle_disappearable_platform state'
    | Some (ROTATABLE state') -> handle_rotatable_platform state'
    | Some (LOCKED_DOOR (key, state')) -> handle_locked_door_platform ~key state'
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
    | Some (text : Interaction.non_blocking_text) -> (
      match text.visible with
      | UNTIL_UNSET -> ()
      | PAUSE_MENU_OPEN -> (
        match state.pause_menu with
        | Some _ -> ()
        | None -> unset ())
      | UNTIL (_duration, end_time) ->
        if state.frame.time > end_time.at then
          unset ())
  in
  maybe_unset game.interaction.floating_text (fun () -> game.interaction.floating_text <- None);
  maybe_unset game.interaction.corner_text (fun () -> game.interaction.corner_text <- None);
  state

let update_enemies (game : game) (state : state) =
  let interacting = List.length game.interaction.steps > 0 in
  let update_enemy (enemy : enemy) =
    let unremoved_projectiles = ref [] in
    let update_projectile' (projectile : projectile) =
      let keep_spawned = update_projectile projectile game state in
      if keep_spawned then (
        unremoved_projectiles := projectile :: !unremoved_projectiles;
        if Player.is_vulnerable state game then (
          match Collision.with_entity game.player.ghost.entity projectile.entity.dest with
          | None -> ()
          | Some c ->
            (* TODO add collision shape to enemy projectiles *)
            if Collision.between_entities game.player.ghost.entity projectile.entity then
              Player.start_action state game (TAKE_DAMAGE (projectile.damage, c.collided_from))))
    in
    List.iter update_projectile' enemy.projectiles;
    enemy.projectiles <- !unremoved_projectiles;
    enemy.floor_collisions_this_frame <-
      Entity.update_pos
        ~gravity_multiplier_override:
          (if Enemy.is_dead enemy then
             Some enemy.json.death_gravity_multiplier
           else
             None)
        game.room state.frame.dt enemy.entity;
    Entity.maybe_unset_current_floor enemy.entity game.room;
    if enemy.status.active && not interacting then
      Enemy.choose_behavior enemy state game;
    Sprite.advance_animation state.frame.time enemy.entity.sprite;
    enemy.damage_sprites <-
      List.filter_map (Sprite.advance_or_despawn state.frame.time) enemy.damage_sprites;
    if Enemy.is_alive enemy then (
      match Player.get_spell_sprite game.player with
      | None -> ()
      | Some (sprite, action_started) -> (
        match Collision.between_sprite_and_enemy sprite enemy with
        | None -> ()
        | Some collision ->
          let damage_kind, collision_direction =
            if game.player.current.is_diving then
              (DESOLATE_DIVE, DOWN)
            else if not (Player.past_cooldown game.player.history.dive_cooldown state.frame.time)
            then
              (DESOLATE_DIVE_SHOCKWAVE, UP)
            else
              (HOWLING_WRAITHS, UP)
          in
          ignore
            (Enemy.maybe_take_damage ~collision_direction state enemy action_started damage_kind
               (Player.get_damage game.player damage_kind)
               collision)));

    let begin_boss_interaction () =
      let trigger = make_stub_trigger BOSS_KILLED "boss-killed" (Show.enemy_id enemy.id) in
      Player.maybe_begin_interaction state game trigger
    in
    if Enemy.is_dead enemy then (
      if enemy.on_killed.start_boss_killed_interaction then (
        if enemy.on_killed.multiple_enemies then (
          let all_bosses_dead =
            let living_bosses =
              List.filter
                (fun (e : enemy) -> e.id = enemy.id && not (Enemy.is_dead e))
                game.room.enemies
            in
            List.length living_bosses = 0
          in
          if all_bosses_dead then
            begin_boss_interaction ())
        else
          begin_boss_interaction ()))
  in
  List.iter
    (fun (enemy : enemy) ->
      match enemy.kind with
      | ENEMY ->
        (* prevent enemies from moving during interactions *)
        if not interacting then
          update_enemy enemy
      | BOSS
      | MULTI_BOSS ->
        (* bosses can move during interactions, but they don't call choose_behavior *)
        update_enemy enemy)
    game.room.enemies;
  state

let update_npcs (game : game) (state : state) =
  let update_npc (npc : npc) =
    Entity.update_pos_ game.room state.frame.dt npc.entity;
    Sprite.advance_animation state.frame.time npc.entity.sprite
  in

  let update_ghost (party_ghost : party_ghost) =
    let ghost = party_ghost.ghost in
    Sprite.advance_animation state.frame.time ghost.entity.sprite;
    if not ghost.entity.frozen then (
      Entity.update_pos_ game.room state.frame.dt ghost.entity;
      Entity.maybe_unset_current_floor ghost.entity game.room)
  in

  List.iter update_ghost game.party;
  List.iter update_npc game.room.npcs;
  state

let reset_frame game state =
  game.reflection_x <- None;
  state

let update_spawned_vengeful_spirits (game : game) (state : state) =
  let damage_enemies vs_start_time (projectile : projectile) =
    let maybe_damage_enemy (enemy : enemy) =
      if enemy.status.check_damage_collisions then (
        match Collision.between_sprite_and_enemy projectile.entity.sprite enemy with
        | None -> ()
        | Some collision ->
          let collision_direction = if projectile.entity.v.x > 0. then RIGHT else LEFT in
          ignore
            (Enemy.maybe_take_damage ~collision_direction state enemy vs_start_time VENGEFUL_SPIRIT
               projectile.damage collision))
    in
    List.iter maybe_damage_enemy game.room.enemies
  in
  let update_vengeful_spirit (projectile : projectile) =
    let keep_spawned = update_projectile projectile game state in
    if keep_spawned then
      damage_enemies projectile.spawned projectile
    else
      game.player.spawned_vengeful_spirits <-
        List.filter (fun p -> p <> projectile) game.player.spawned_vengeful_spirits
  in

  List.iter update_vengeful_spirit game.player.spawned_vengeful_spirits;
  state

let update_frame_inputs (state : state) : state =
  let update_frame_input ?(direction = false) key (input : frame_input) =
    input.released <- Controls.key_released state.controls ~direction key;
    input.pressed <-
      (if direction && input.down then
         false
       else
         Controls.key_pressed state.controls ~direction key);
    input.down <- Controls.key_down state.controls ~direction key;
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

let maybe_save_game game state =
  (match state.save_pos with
  | None -> ()
  | Some pos ->
    let original_pos = game.player.ghost.entity.dest.pos in
    game.player.ghost.entity.dest.pos <- pos;
    Game.save game state;
    game.player.ghost.entity.dest.pos <- original_pos;
    state.save_pos <- None);
  state

let add_debug_rects state rects = state.debug.rects <- rects @ state.debug.rects

let tick (state : state) =
  if Env.development && not (Controls.holding_shift ()) then (
    if Controls.key_pressed state.controls DEBUG_3 then
      if state.debug.show_frame_inputs then (
        state.debug.show_frame_inputs <- false;
        print " disabled show_frame_inputs at %d\n\\----------------------/\n" state.frame.idx)
      else (
        state.debug.show_frame_inputs <- true;
        print "\n/----------------------\\\n show_frame_inputs debug at %d" state.frame.idx);
    if Controls.key_pressed state.controls DEBUG_4 then
      if state.debug.enabled then (
        state.debug.enabled <- false;
        print " disabled debug at %d\n\\----------------------/\n" state.frame.idx)
      else (
        state.debug.enabled <- true;
        print "\n/----------------------\\\n enabled debug at %d" state.frame.idx));
  state.debug.rects <- [];
  match state.context with
  | SAVE_FILES (menu, save_slots)
  | MAIN_MENU (menu, save_slots) ->
    Audio.play_menu_music state;
    state |> update_frame_inputs |> Menu.update_main_menu menu save_slots
  | RETURN_TO_MAIN_MENU _game ->
    state.context <- MAIN_MENU (Menu.main_menu (), Game.load_all_save_slots ());
    state
  | RELOAD_LAST_SAVED_GAME game ->
    let saved_game_before_dying = Game.load state game.save_file game.save_file_slot in
    state.context <- IN_PROGRESS saved_game_before_dying;
    state
  | IN_PROGRESS game -> (
    (* TODO the music stutters at room transitions
       - maybe need to check difference in state.frame.time and seek forward
    *)
    let find_existing_binding (type a) (value : a) (action_map : a Game_action.Map.t) :
        game_action option =
      let res = ref None in
      let find (action : game_action) value' =
        if value = value' then
          res := Some action
      in
      Game_action.Map.iter find action_map;
      !res
    in
    let update_bindings action input new_bindings replaced_bindings show_control save_bindings =
      new_bindings := (Controls.show_game_action action, show_control input) :: !new_bindings;
      let replace (k, v) = replaced_bindings := List.replace_assoc k v !replaced_bindings in
      List.iter replace !new_bindings;
      let contents =
        Json_j.string_of_keybinds_file !replaced_bindings
        |> String.split_on_char '('
        |> String.join_lines_with '('
      in
      save_bindings contents
    in
    match state.rebinding_action with
    | Some (control_type, action) ->
      (match control_type with
      | KEY -> (
        match Raylib.get_key_pressed () with
        | Raylib.Key.Null -> ()
        | key ->
          let replaced_bindings =
            ref (File.read_config "key_overrides" Json_j.keybinds_file_of_string)
          in
          let new_bindings = ref [] in
          (match find_existing_binding key state.controls.keyboard with
          | None -> ()
          | Some binding ->
            let old_key = Game_action.Map.find action state.controls.keyboard in
            new_bindings :=
              (Controls.show_game_action binding, Controls.show_key old_key) :: !new_bindings;
            state.controls.keyboard <-
              Game_action.Map.update binding (fun _ -> Some old_key) state.controls.keyboard);
          state.controls.keyboard <-
            Game_action.Map.update action (fun _ -> Some key) state.controls.keyboard;
          update_bindings action key new_bindings replaced_bindings Controls.show_key
            File.save_keyboard_bindings;
          state.rebinding_action <- None)
      | BUTTON -> (
        match Controls.get_pressed_button () with
        | None -> ()
        | Some button ->
          let replaced_bindings =
            ref (File.read_config "button_overrides" Json_j.keybinds_file_of_string)
          in
          let new_bindings = ref [] in
          (match find_existing_binding button state.controls.gamepad with
          | None -> ()
          | Some binding ->
            let old_button = Game_action.Map.find action state.controls.gamepad in
            new_bindings :=
              (Controls.show_game_action binding, Controls.show_button old_button) :: !new_bindings;
            state.controls.gamepad <-
              Game_action.Map.update binding (fun _ -> Some old_button) state.controls.gamepad);
          state.controls.gamepad <-
            Game_action.Map.update action (fun _ -> Some button) state.controls.gamepad;
          update_bindings action button new_bindings replaced_bindings Controls.show_button
            File.save_gamepad_bindings;
          state.rebinding_action <- None));
      state
    | None ->
      Audio.play_game_music game;

      if state.debug.paused then (
        game.player.ghost.hardfall_time <- None;
        if Controls.key_pressed state.controls DEBUG_2 then
          state
          |> update_frame_inputs
          |> Menu.update_pause_menu game
          |> Player.handle_debug_keys game
          |> Player.tick game
          |> update_spawned_vengeful_spirits game
          |> update_enemies game
          |> update_npcs game
          |> update_environment game
          |> Camera.tick game
        else
          state
          |> update_frame_inputs
          |> Menu.update_pause_menu game
          |> Player.handle_debug_keys game)
      else (
        let state' = state |> update_frame_inputs |> Menu.update_pause_menu game in
        match state'.pause_menu with
        | Some _menu ->
          (match game.mode with
          | CLASSIC -> ()
          | DEMO
          | STEEL_SOLE ->
            let time, current_time =
              if List.length game.progress.purple_pens_found = 0 then
                ("", Progress.get_total_game_time state.frame.idx)
              else (
                let frames_at_last_pen =
                  (* take the first because newest entries are pushed to the front *)
                  List.hd game.progress.purple_pens_found |> fst
                in
                ( fmt " in %s" (Progress.get_total_game_time frames_at_last_pen),
                  Progress.get_total_game_time state.frame.idx ))
            in
            let pen_progress = Progress.get_row state game "Purple Pens" in
            game.interaction.corner_text <-
              Some
                {
                  content =
                    fmt "%d / %d purple pens found%s, %d dunks, %d c-dashes --- %s"
                      pen_progress.found pen_progress.total time game.progress.steel_sole.dunks
                      game.progress.steel_sole.c_dashes current_time;
                  visible = PAUSE_MENU_OPEN;
                  scale = 1.;
                });
          state'
        | None ->
          if state.debug.enabled && Env.development then (
            let show_triggers ?(color = Raylib.Color.blue) (triggers : trigger list) =
              add_debug_rects state (List.map (fun (r : trigger) -> (color, r.dest)) triggers)
            in
            let _show_respawn_triggers ?(color = Raylib.Color.blue) triggers =
              add_debug_rects state (List.map (fun (_, r) -> (color, r.dest)) triggers)
            in

            show_triggers game.room.triggers.lore;
            show_triggers game.room.triggers.cutscene;
            show_triggers game.room.triggers.d_nail;

            (* show_respawn_triggers ~color:(Raylib.Color.red) game.room.triggers.respawn; *)
            add_debug_rects state
              (List.map
                 (fun (p : platform) -> (Raylib.Color.orange, p.sprite.dest))
                 game.room.platforms);

            add_debug_rects state
              [
                ( (if game.room.respawn.in_trigger_now then
                     Raylib.Color.red
                   else
                     Raylib.Color.green),
                  { pos = game.room.respawn.target; w = 100.; h = 100. } );
              ];

            add_debug_rects state
              (List.map (fun (rect, _) -> (Raylib.Color.orange, rect)) game.room.conveyor_belts);

            add_debug_rects state
              (List.map (fun (r : rect) -> (Raylib.Color.orange, r)) game.room.floors);

            add_debug_rects state
              (List.map
                 (fun (r : rect) -> (Raylib.Color.red, r))
                 (game.room.spikes
                 @ game.room.hazards
                 @ (game.room.platform_spikes |> String.Map.to_list |> List.map snd))));

          (* when transitioning into a large room, state.frame.dt can be a lot larger than (1/fps),
             so this skips position updates to prevent the ghost from falling through floors
          *)
          if game.room_changed_last_frame then (
            game.room_changed_last_frame <- false;
            state')
          else
            state'
            |> reset_frame game
            |> Player.handle_debug_keys game
            |> Player.tick game
            |> update_spawned_vengeful_spirits game
            |> update_enemies game
            |> update_npcs game
            |> update_environment game
            |> update_interaction_text game
            |> Camera.tick game
            |> maybe_save_game game))
