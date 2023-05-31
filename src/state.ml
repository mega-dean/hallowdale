open Types
open Controls

[@@@ocaml.warning "-26-27-32"]

let npcs_config_path = "config/npcs.json"
let enemies_config_path = "config/enemies.json"
let ghost_config_path = "config/ghosts.json"
let save_file_path idx = fmt "saves/%d.json" idx

(* let read_config path parse_fn json_file_of_string =
 *   let configs = Tiled.read_whole_file (fmt "../%s" path) |> json_file_of_string in
 *   let parse_name (id_str, props) = (parse_fn path id_str, props) in
 *   List.map parse_name configs *)

let read_npcs_config () : Json_t.npcs_file =
  Tiled.read_whole_file (fmt "../%s" npcs_config_path) |> Json_j.npcs_file_of_string

let read_enemies_config () : Json_t.enemies_file =
  Tiled.read_whole_file (fmt "../%s" enemies_config_path) |> Json_j.enemies_file_of_string

type ghosts_file = {
  textures : (ghost_id * texture_config list) list;
  actions : (string * ghost_action_config) list;
  shared_textures : (string * texture_config) list;
}

let read_ghost_configs () : ghosts_file =
  let ghost_file : Json_t.ghosts_file =
    Tiled.read_whole_file (fmt "../%s" ghost_config_path) |> Json_j.ghosts_file_of_string
  in
  let parse_texture_config character_name ((pose_name, ghost_pose) : string * Json_t.texture_config) : texture_config =
    {
      asset_dir = GHOSTS;
      character_name;
      pose_name;
      count = ghost_pose.count;
      duration = { seconds = ghost_pose.duration };
      x_offset = ghost_pose.x_offset |> Int.to_float;
      y_offset = ghost_pose.y_offset |> Int.to_float;
    }
  in
  let parse_ghost_texture ((ghost_id_str, ghost_poses) : string * (string * Json_t.texture_config) list) :
      ghost_id * texture_config list =
    let ghost_id = Ghost.parse_name ghost_id_str in
    let texture_configs : texture_config list = List.map (parse_texture_config ghost_id_str) ghost_poses in
    (ghost_id, texture_configs)
  in
  let textures = List.map parse_ghost_texture ghost_file.individual_textures in
  let shared_textures : (string * texture_config) list =
    let parse_texture_config' (s, tc) : string * texture_config = (s, parse_texture_config "shared" (s, tc)) in
    List.map parse_texture_config' ghost_file.shared_textures
  in
  let parse_ghost_action ((name, json_ghost_action) : string * Json_t.ghost_action) =
    ( name,
      {
        duration = { seconds = json_ghost_action.duration };
        cooldown = { seconds = json_ghost_action.cooldown };
        input_buffer = { seconds = json_ghost_action.input_buffer };
      } )
  in
  let actions : (string * ghost_action_config) list = List.map parse_ghost_action ghost_file.actions in
  { actions; textures; shared_textures }

(* CLEANUP maybe want a new game.ml file *)
let load_game (save_file : Json_t.save_file) (global : global_cache) (world : world) (save_file_slot : int) : game =
  let start_pos = { x = save_file.ghost_x; y = save_file.ghost_y } in

  let ghosts_file = read_ghost_configs () in
  let use_json_config configs pose_name =
    let config =
      match List.find_opt (fun (tc : texture_config) -> tc.pose_name = pose_name) configs with
      | None ->
        let ghost_name = (List.nth configs 0).character_name in
        failwithf "could not find pose config in ghosts/config.json for '%s' for ghost %s" pose_name ghost_name
      | Some v -> v
    in
    Sprite.build_texture_from_config config
  in

  let britta_configs = List.assoc BRITTA ghosts_file.textures in
  let jeff_configs = List.assoc JEFF ghosts_file.textures in
  let abed_configs = List.assoc ABED ghosts_file.textures in
  let troy_configs = List.assoc TROY ghosts_file.textures in
  let annie_configs = List.assoc ANNIE ghosts_file.textures in

  let britta_ghost_textures : ghost_textures =
    {
      cast = use_json_config britta_configs "cast";
      crawl = use_json_config britta_configs "crawl";
      dash = use_json_config britta_configs "dash";
      dive = use_json_config britta_configs "dive";
      fall = use_json_config britta_configs "fall";
      flap = use_json_config britta_configs "flap";
      focus = use_json_config britta_configs "focus";
      idle = use_json_config britta_configs "idle";
      jump = use_json_config britta_configs "jump";
      nail = use_json_config britta_configs "nail";
      read = use_json_config britta_configs "read";
      take_damage = use_json_config britta_configs "take-damage";
      walk = use_json_config britta_configs "walk";
      wall_slide = use_json_config britta_configs "wall-slide";
    }
  in

  let shared_ghost_textures = Ghost.load_shared_textures ghosts_file.shared_textures in
  let load_weapon name = (name, List.assoc name global.weapons) in
  let max_health =
    let finished_interaction_names =
      match List.assoc_opt save_file.room_name save_file.progress with
      | None -> []
      | Some progress -> progress.finished_interactions
    in
    tmp "got finished_interaction_names: %s" (finished_interaction_names |> join);
    let health_increases =
      List.filter (String.starts_with ~prefix:"health_") finished_interaction_names |> List.length
    in
    tmp "got health_increases: %d" health_increases;
    let base_health = 5 in
    base_health + health_increases
  in
  let make_ghost id in_party textures : ghost =
    tmp " =========================== making ghost %s with max health %d" (Show.ghost_id id) max_health;
    Ghost.init id in_party textures.idle ghosts_file.actions (clone_vector start_pos) save_file.abilities
      (List.map load_weapon save_file.weapons)
      textures shared_ghost_textures max_health
  in

  let britta = make_ghost BRITTA true britta_ghost_textures in
  britta.entity.update_pos <- true;

  let ghosts =
    tmp "making ghosts with in_party: %s" (save_file.ghosts_in_party |> join);
    (* TODO could check config keys to decide when to fall back to britta so poses can be added without recompiling *)
    let make_uncontrolled_ghost name config =
      let in_party = List.mem (Show.ghost_id name) save_file.ghosts_in_party in
      make_ghost name in_party config
    in
    [
      ( ABED,
        make_uncontrolled_ghost ABED
          {
            britta_ghost_textures with
            dash = use_json_config abed_configs "dash";
            fall = use_json_config abed_configs "fall";
            idle = use_json_config abed_configs "idle";
            jump = use_json_config abed_configs "jump";
            walk = use_json_config abed_configs "walk";
          } );
      ( TROY,
        make_uncontrolled_ghost TROY
          {
            britta_ghost_textures with
            dash = use_json_config troy_configs "dash";
            dive = use_json_config troy_configs "dive";
            fall = use_json_config troy_configs "fall";
            idle = use_json_config troy_configs "idle";
            jump = use_json_config troy_configs "jump";
            walk = use_json_config troy_configs "walk";
          } );
      ( ANNIE,
        make_uncontrolled_ghost ANNIE
          {
            britta_ghost_textures with
            idle = use_json_config annie_configs "idle";
            nail = use_json_config annie_configs "nail";
            walk = use_json_config annie_configs "walk";
          } );
      ( JEFF,
        make_uncontrolled_ghost JEFF
          {
            britta_ghost_textures with
            crawl = use_json_config jeff_configs "crawl";
            fall = use_json_config jeff_configs "fall";
            idle = use_json_config jeff_configs "idle";
            jump = use_json_config jeff_configs "jump";
            nail = use_json_config jeff_configs "nail";
            walk = use_json_config jeff_configs "walk";
          } );
    ]
  in

  List.iter
    (fun ((_, g) : ghost_id * ghost) ->
      Entity.freeze g.entity;
      g.entity.dest.pos.x <- -1. *. g.entity.dest.pos.x;
      g.entity.dest.pos.y <- -1. *. g.entity.dest.pos.y)
    ghosts;

  let _, room_id = Tiled.parse_room_filename "State.load_room" save_file.room_name in
  let room_location = List.assoc room_id world in
  let exits = Tiled.Room.get_exits room_location in
  let parse_texture_configs parse_name coll =
    let parse (name, config) = (parse_name "state.init" name, config) in
    List.map parse coll
  in

  let room : room =
    Room.init
      {
        file_name = save_file.room_name;
        progress = save_file.progress;
        exits;
        enemy_configs = global.enemy_configs;
        npc_configs = global.npc_configs;
        pickup_indicator_texture = global.textures.pickup_indicator;
      }
  in
  room.layers <- Tiled.Room.get_layer_tile_groups room room.progress.removed_idxs_by_layer;
  let ghost =
    let ghost_id = Ghost.parse_name save_file.ghost_id in
    match List.assoc_opt ghost_id ghosts with
    | None -> britta
    | Some ghost ->
      if ghost.in_party then (
        (* CLEANUP shouldn't need to do this
           - this is only here because Ghost.init hides every ghost except britta
        *)
        Entity.unhide ghost.entity;
        ghost)
      else
        failwithf "invalid save file: ghost %s is not in party" (Show.ghost_id ghost.id)
  in
  {
    ghost;
    ghosts;
    room;
    interaction = { steps = []; text = None; speaker_name = None; name = None };
    progress = save_file.progress;
    save_file_slot;
  }

let load_all_save_slots () =
  (* CLEANUP duplicated *)
  let load_file save_file_idx : Json_t.save_file =
    match Tiled.try_read_file (fmt "../%s" (save_file_path save_file_idx)) with
    | None ->
      {
        ghost_id = "BRITTA";
        ghosts_in_party = (* CLEANUP should only be britta *) [ "BRITTA"; "TROY" ];
        ghost_x = 1500.;
        ghost_y = 100.;
        room_name = "forgotten_deans-pass";
        abilities =
          {
            mothwing_cloak = false;
            mantis_claw = false;
            crystal_heart = false;
            monarch_wings = false;
            vengeful_spirit = false;
            desolate_dive = false;
            howling_wraiths = false;
          };
        progress = [];
        weapons = [ "Old Nail" ];
        current_weapon = "Old Nail";
      }
    | Some save_file ->
      let s = Json_j.save_file_of_string save_file in
      itmp "loading save file with current_weapon: %s" s.current_weapon;
      s
  in
  { slot_1 = load_file 1; slot_2 = load_file 2; slot_3 = load_file 3; slot_4 = load_file 4 }

(* this function initializes state and sets loaded_state to MAIN_MENU *)
let init () : state =
  (* CLEANUP remove *)
  let _room_name, _room_id, _start_x, _start_y =
    let xs =
      [
        (* starting drop *)
        ("forgotten_deans-pass", FC_DEANS_PASS, 1500., 100.);
        (* bottom of start drop *)
        ("forgotten_deans-pass", FC_DEANS_PASS, 1500., 3600.);
        (* big door *)
        ("forgotten_deans-pass", FC_DEANS_PASS, 7000., 600.);
        (* duncan fight *)
        ("infected_teachers-lounge", IC_TEACHERS_LOUNGE, 800., 2200.);
        (* past duncan fight *)
        (* ("infected_teachers-lounge", IC_TEACHERS_LOUNGE, true, false, false, 1000., 2200.); *)

        (* by the breakable wall *)
        (* ("infected_a", IC_A, true, true, true, 2200., 1800.); *)
        (* locker boys fight *)
        (* ("infected_b", IC_B, true, true, false, 1700., 900.); *)
        (* after locker boys fight *)
        (* ("infected_b", IC_B, true, true, false, 3200., 900.); *)
        (* by cafeteria exit *)
        (* ("infected_b", IC_B, true, true, false, 600., 1900.); *)

        (* cafeteria floor *)
        (* ("forgotten_cafeteria", FC_CAFETERIA, true, true, false, 1400., 2600.); *)
        (* stairwell *)
        (* ("forgotten_stairwell", FC_STAIRWELL, true, true, false, 400., 200.); *)

        (* bush scissors *)
        (* ("trampoline_f", TP_F, true, true, false, 400., 800.); *)
        (* acb note *)
        (* ("forgotten_b", FC_B, true, true, true, 400., 600.); *)
      ]
    in
    List.nth xs (List.length xs - 1)
  in

  let world = Tiled.init_world "Deepnest_East" in
  let parse_texture_configs parse_name coll =
    let parse (name, config) = (parse_name "state.init" name, config) in
    List.map parse coll
  in

  let enemies_file : Json_t.enemies_file = read_enemies_config () in
  let enemy_configs = parse_texture_configs Enemy.parse_name enemies_file.enemies in
  let shared_enemy_configs = enemies_file.shared_textures in

  let npcs_file = read_npcs_config () in
  let npc_configs = parse_texture_configs Npc.parse_name npcs_file.npcs in

  let pickup_indicator =
    let config = List.assoc "pickup-indicator" npcs_file.shared_textures in
    Sprite.build_texture_from_config
      {
        asset_dir = NPCS;
        character_name = "shared";
        pose_name = "pickup-indicator";
        count = config.count;
        duration = { seconds = config.duration };
        x_offset = 0.;
        y_offset = 0.;
      }
  in

  let weapon_configs = Tiled.load_weapons_config () in

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
      lore = Tiled.load_lore_config ();
      weapons = weapon_configs;
      enemy_configs;
      npc_configs;
      textures = { ability_outlines; damage; pickup_indicator };
    }
  in

  let camera_target = Raylib.Vector2.create 0. 0. in
  let camera = Tiled.create_camera_at camera_target 0. in

  print "initialized state\n=================\n";
  {
    loaded_state = MAIN_MENU (main_menu (), load_all_save_slots ());
    pause_menu = None;
    world;
    camera = { raylib = camera; subject = GHOST; shake = 0. };
    screen_faded = false;
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
        if Ghost.is_vulnerable (state, game) then (
          match Collision.with_entity game.ghost.entity projectile.entity.dest with
          | None -> ()
          | Some c ->
            (* TODO add collision shape to enemy projectiles *)
            if Collision.between_entities game.ghost.entity projectile.entity then
              Ghost.start_action (state, game) (TAKE_DAMAGE c.direction)))
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
            Ghost.maybe_begin_interaction (state, game) name)
        else
          Ghost.maybe_begin_interaction (state, game) name)
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
  update_frame_input UP state.frame_inputs.up;
  update_frame_input DOWN state.frame_inputs.down;
  update_frame_input LEFT state.frame_inputs.left;
  update_frame_input RIGHT state.frame_inputs.right;
  update_frame_input CAST state.frame_inputs.cast;
  update_frame_input DASH state.frame_inputs.dash;
  update_frame_input D_NAIL state.frame_inputs.d_nail;
  update_frame_input FOCUS state.frame_inputs.focus;
  update_frame_input JUMP state.frame_inputs.jump;
  update_frame_input NAIL state.frame_inputs.nail;
  update_frame_input PAUSE state.frame_inputs.pause;
  state

(* CLEANUP move this somewhere *)
let write_file (filename : string) (contents : string) : bool =
  let filename' = convert_path filename in
  let ch = open_out filename' in
  let s = Printf.fprintf ch "%s\n" contents in
  close_out ch;
  true

let update_menu_choice (menu : menu) frame_inputs =
  if frame_inputs.down.pressed then
    menu.current_choice_idx <- Int.min (menu.current_choice_idx + 1) (List.length menu.choices - 1);
  if frame_inputs.up.pressed then
    menu.current_choice_idx <- Int.max 0 (menu.current_choice_idx - 1)

(* CLEANUP move these to new file menu.ml *)
let update_pause_menu (game : game) (state : state) : state =
  if state.frame_inputs.pause.pressed then (
    match state.pause_menu with
    | None -> state.pause_menu <- Some (pause_menu ())
    | Some _ -> state.pause_menu <- None);

  (match state.pause_menu with
  | None -> ()
  | Some menu ->
    update_menu_choice menu state.frame_inputs;

    if state.frame_inputs.d_nail.pressed then (
      match List.nth menu.choices menu.current_choice_idx with
      | PAUSE_MENU' CONTINUE ->
        state.pause_menu <- None;
        game.interaction.text <- None
      | PAUSE_MENU' CHANGE_WEAPON ->
        tmp "opening CHANGE_WEAPON";
        state.pause_menu <- Some (change_weapon_menu (List.map fst game.ghost.weapons))
      | PAUSE_MENU' QUIT_TO_MAIN_MENU ->
        state.pause_menu <- None;
        (* TODO unload textures *)
        let save_file : Json_t.save_file =
          itmp "saving file with finished interactions: %s" (game.room.progress.finished_interactions |> join);
          itmp "saving file with %d removed idxs" (List.length game.room.progress.removed_idxs_by_layer);
          let ghosts_in_party = Ghost.available_ghost_ids game.ghosts |> List.map Show.ghost_id in
          itmp "saving ghosts_in_party: %s" (ghosts_in_party |> join);
          itmp "saving ghost at: %s" (Show.vector game.ghost.entity.dest.pos);

          (* CLEANUP duplicated *)
          let room_uuid = Tiled.Room.get_uuid game.room in
          game.progress <- Utils.assoc_replace room_uuid game.room.progress game.progress;

          tmp "got progress keys: %s" (List.map fst game.progress |> join);

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
        tmp "saving contents in %d:\n%s" game.save_file_slot contents;
        let written = write_file save_file_path contents in
        if written then
          state.loaded_state <- MAIN_MENU (main_menu (), load_all_save_slots ())
        else
          failwith "error when trying to save"
      | CHANGE_WEAPON_MENU (EQUIP_WEAPON weapon_name) ->
        tmp "equipping weapon %s" weapon_name;
        Ghost.equip_weapon game.ghost weapon_name
      | CHANGE_WEAPON_MENU BACK -> state.pause_menu <- Some (pause_menu ())
      | c -> tmp "menu choice: %s" (Show.menu_choice c)));
  state

let update_main_menu (menu : menu) (save_slots : save_slots) (state : state) : state =
  update_menu_choice menu state.frame_inputs;

  let load_file save_file_idx =
    let save_file : Json_t.save_file =
      (* CLEANUP this seems dumb *)
      match save_file_idx with
      | 1 -> save_slots.slot_1
      | 2 -> save_slots.slot_2
      | 3 -> save_slots.slot_3
      | 4 -> save_slots.slot_4
      | _ -> failwith "bad save file idx"
    in
    let game =
      itmp "load_game with slot %d" save_file_idx;
      load_game save_file state.global state.world save_file_idx
    in
    (* CLEANUP move this somewhere better - probably in load_game *)
    Ghost.equip_weapon game.ghost save_file.current_weapon;
    state.camera.raylib <-
      (* update the camera when a file is loaded so the ghost doesn't start too far offscreen
         CLEANUP can maybe improve this, since it can still be off if the camera is bounded
      *)
      Tiled.create_camera_at (Raylib.Vector2.create game.ghost.entity.dest.pos.x game.ghost.entity.dest.pos.y) 0.;
    state.loaded_state <- IN_PROGRESS game
  in

  if state.frame_inputs.d_nail.pressed then (
    match List.nth menu.choices menu.current_choice_idx with
    | MAIN_MENU START_GAME ->
      tmp "open save_files menu";
      state.loaded_state <- SAVE_FILES (save_files_menu (), save_slots)
    | MAIN_MENU QUIT ->
      tmp "exiting";
      exit 0
    | SAVE_FILES SLOT_1 -> load_file 1
    | SAVE_FILES SLOT_2 -> load_file 2
    | SAVE_FILES SLOT_3 -> load_file 3
    | SAVE_FILES SLOT_4 -> load_file 4
    | SAVE_FILES BACK -> state.loaded_state <- MAIN_MENU (main_menu (), load_all_save_slots ())
    | _ -> failwith "update_main_menu - needs MAIN_MENU menu.choices");
  state

let tick (state : state) =
  (* TODO-4 add sound effects and music *)
  if Controls.key_pressed DEBUG_4 then
    if state.debug.enabled then (
      state.debug.enabled <- false;
      print "disabled debug at %d\n\\------------------\n" state.frame.idx)
    else (
      state.debug.enabled <- true;
      print "\n/-----------------\nenabled debug at %d" state.frame.idx);

  match state.loaded_state with
  | SAVE_FILES (menu, save_slots)
  | MAIN_MENU (menu, save_slots) ->
    state |> update_frame_inputs |> update_main_menu menu save_slots
  | IN_PROGRESS game -> (
    tmp "max ghost health: %d" game.ghost.health.max;
    let state' = state |> update_frame_inputs |> update_pause_menu game in
    match state'.pause_menu with
    | Some menu -> state'
    | None ->
      state'
      |> Ghost.update game
      |> update_spawned_vengeful_spirits game
      |> update_enemies game
      |> update_npcs game
      |> update_fragments game
      |> update_camera game)
