open Types
open Controls

[@@@ocaml.warning "-26-27-32"]

let npcs_config_path = "config/npcs.json"
let enemies_config_path = "config/enemies.json"
let ghost_config_path = "config/ghosts.json"

let read_config path parse_fn json_file_of_string =
  let configs = Tiled.read_whole_file (fmt "../%s" path) |> json_file_of_string in
  let parse_name (id_str, props) = (parse_fn path id_str, props) in
  List.map parse_name configs

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

let init () : state =
  (* TODO handle this in a config file (probably will be similar to save files) *)
  let room_name, room_id, vengeful_spirit, mothwing_cloak, mantis_claw, start_x, start_y =
    let xs =
      [
        (* starting drop *)
        ("forgotten_deans-pass", FC_DEANS_PASS, false, false, false, 1500., 100.);
        (* bottom of start drop *)
        ("forgotten_deans-pass", FC_DEANS_PASS, true, true, true, 1500., 3600.);
        (* big door *)
        ("forgotten_deans-pass", FC_DEANS_PASS, false, false, false, 7000., 600.);
        (* duncan fight *)
        (* ("infected_teachers-lounge", IC_TEACHERS_LOUNGE, true, false, false, 800., 2200.); *)
        (* past duncan fight *)
        (* ("infected_teachers-lounge", IC_TEACHERS_LOUNGE, true, false, false, 1000., 2200.); *)

        (* by the breakable wall *)
        ("infected_a", IC_A, true, true, true, 2200., 1800.);
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
        ("trampoline_f", TP_F, true, true, false, 400., 800.);
        (* acb note *)
        (* ("forgotten_b", FC_B, true, true, true, 400., 600.); *)
      ]
    in
    List.nth xs (List.length xs - 1)
  in

  let start_pos = { x = start_x; y = start_y } in

  let world = Tiled.init_world "Deepnest_East" in
  let empty_progress = { rooms = []; global = [] } in
  let room_location = List.assoc room_id world in
  let exits = Tiled.Room.get_exits room_location in
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

  let room =
    Room.init
      {
        file_name = room_name;
        progress = empty_progress;
        exits;
        enemy_configs;
        npc_configs;
        pickup_indicator_texture = pickup_indicator;
      }
  in

  room.layers <- Tiled.Room.get_layer_tile_groups room [];
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
  let weapon_configs = Tiled.load_weapons_config () in

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
  let make_ghost id in_party textures : ghost =
    Ghost.init id in_party textures.idle ghosts_file.actions (clone_vector start_pos)
      {
        mothwing_cloak;
        mantis_claw;
        crystal_heart = false;
        monarch_wings = false;
        vengeful_spirit;
        desolate_dive = false;
        howling_wraiths = false;
      }
      [ ("Old Nail", List.assoc "Old Nail" weapon_configs) ]
      textures shared_ghost_textures
  in

  let ghosts =
    (* TODO could check config keys to decide when to fall back to britta so poses can be added without recompiling *)
    let make_uncontrolled_ghost name config = make_ghost name false config in
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

  let britta = make_ghost BRITTA true britta_ghost_textures in
  britta.entity.update_pos <- true;

  let camera_target = Raylib.Vector2.create britta.entity.dest.pos.x britta.entity.dest.pos.y in
  let camera = Tiled.create_camera_at camera_target 0. in

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

  print "initialized state\n=================\n";
  {
    ghost = britta;
    ghosts;
    room;
    world;
    camera;
    camera_subject = GHOST;
    screen_faded = false;
    shake = 0.;
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
      };
    progress = empty_progress;
    interaction = { steps = []; text = None; speaker_name = None; name = None };
    debug = { enabled = false; rects = [] };
    global;
  }

let update_camera state : state =
  let trigger_config : (string * rect) option = Ghost.find_trigger_collision state.ghost state.room.triggers.camera in
  let subject =
    match state.camera_subject with
    | GHOST ->
      let e = state.ghost.entity in
      let offset = (* TODO move to config *) 50. in
      if e.sprite.facing_right then
        { e.dest.pos with x = e.dest.pos.x +. offset }
      else
        { e.dest.pos with x = e.dest.pos.x -. offset }
    | FIXED pos -> pos
  in
  let new_camera =
    let current = Raylib.Camera2D.target state.camera in
    let camera_state =
      {
        current_pos = { x = Raylib.Vector2.x current; y = Raylib.Vector2.y current };
        subject;
        room_bounds = state.room.camera_bounds;
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
    Tiled.create_camera_at target state.shake
  in
  state.camera <- new_camera;
  if state.shake > 0. then
    state.shake <- state.shake -. state.frame.dt;
  state

let update_fragments state =
  let update_fragment (f : entity) = Entity.update_pos state f in
  let all_spawned_fragments = List.map (fun l -> l.spawned_fragments) state.room.layers |> List.flatten in
  List.iter update_fragment all_spawned_fragments;
  state

let update_projectile p (frame_info : frame_info) : bool =
  let despawn_projectile =
    match p.despawn with
    | X_BOUNDS (min_x, max_x) ->
      p.entity.dest.pos.x < min_x -. Config.window.center_x || p.entity.dest.pos.x > max_x +. Config.window.center_x
    | TIME_LEFT d -> frame_info.time -. p.spawned.at > d.seconds
  in
  if despawn_projectile then
    true
  else (
    Entity.apply_v frame_info.dt p.entity;
    Sprite.advance_animation frame_info.time p.entity.sprite.texture p.entity.sprite;
    false)

let update_enemies state =
  let behavior_params : enemy_behavior_params =
    { ghost_pos = state.ghost.entity.dest.pos; room_bounds = state.room.camera_bounds; time = state.frame.time }
  in
  let ghost_vulnerable = Ghost.past_cooldown state state.ghost.actions.take_damage in
  let update_enemy ((_, enemy) : enemy_id * enemy) =
    let unremoved_projectiles = ref [] in
    let update_projectile (projectile : projectile) =
      let despawned = update_projectile projectile state.frame in
      if despawned then
        ()
      else (
        unremoved_projectiles := projectile :: !unremoved_projectiles;
        if ghost_vulnerable then (
          match collision_between state.ghost.entity projectile.entity.dest with
          | None -> ()
          | Some c -> Ghost.start_action state (TAKING_DAMAGE c.direction)))
    in
    List.iter update_projectile enemy.spawned_projectiles;
    enemy.spawned_projectiles <- !unremoved_projectiles;
    if enemy.entity.update_pos then
      Entity.update_pos state enemy.entity;
    let interacting () = List.length state.interaction.steps > 0 in
    if (not (interacting ())) && enemy.status.choose_behavior then
      enemy.choose_behavior ~self:enemy behavior_params;
    Sprite.advance_animation state.frame.time enemy.entity.sprite.texture enemy.entity.sprite;
    let advance_or_despawn (sprite : sprite) = Sprite.advance_or_despawn state.frame.time sprite.texture sprite in
    enemy.damage_sprites <- List.filter_map advance_or_despawn enemy.damage_sprites;
    if Enemy.is_dead enemy then (
      match enemy.on_killed.interaction_name with
      | None -> ()
      | Some name ->
        if enemy.on_killed.multiple_enemies then (
          let all_bosses_dead =
            let living_bosses =
              List.filter (fun (enemy_id, e) -> enemy_id = enemy.id && not (Enemy.is_dead e)) state.room.enemies
            in
            List.length living_bosses = 0
          in
          if all_bosses_dead then
            Ghost.maybe_begin_interaction state name)
        else
          Ghost.maybe_begin_interaction state name)
  in
  List.iter update_enemy state.room.enemies;
  state

let update_npcs state =
  let update_npc (npc : npc) =
    Entity.update_pos state npc.entity;
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

      Entity.update_pos ~debug state ghost.entity;
      Ghost.maybe_unset_current_floor ghost)
  in

  List.iter update_ghost state.ghosts;
  List.iter update_npc state.room.npcs;
  (* pickup indicators aren't really "npcs" *)
  List.iter update_pickup_indicators state.room.pickup_indicators;
  state

let get_vs_damage state =
  (* TODO check ghost.abilities.shade_soul *)
  15

let update_spawned_vengeful_spirits state =
  let damage_enemies vs_start_frame (f : sprite) =
    let maybe_damage_enemy ((_enemy_id, enemy) : enemy_id * enemy) =
      if enemy.status.check_damage_collisions then (
        match collision_between enemy.entity f.dest with
        | None -> ()
        | Some c ->
          if vs_start_frame > Enemy.took_damage_at enemy VENGEFUL_SPIRIT then
            Enemy.take_damage state enemy VENGEFUL_SPIRIT (get_vs_damage state) c.rect)
    in
    List.iter maybe_damage_enemy state.room.enemies
  in
  let update_vengeful_spirit (projectile : projectile) =
    let despawned = update_projectile projectile state.frame in
    if despawned then
      state.ghost.spawned_vengeful_spirits <-
        List.filter (fun p -> p <> projectile) state.ghost.spawned_vengeful_spirits
    else
      damage_enemies projectile.spawned projectile.entity.sprite
  in

  List.iter update_vengeful_spirit state.ghost.spawned_vengeful_spirits;
  state

let update_frame_inputs state : state =
  let input_cache = state.frame_inputs in
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
  update_frame_input UP input_cache.up;
  update_frame_input DOWN input_cache.down;
  update_frame_input LEFT input_cache.left;
  update_frame_input RIGHT input_cache.right;
  update_frame_input CAST input_cache.cast;
  update_frame_input DASH input_cache.dash;
  update_frame_input D_NAIL input_cache.d_nail;
  update_frame_input FOCUS input_cache.focus;
  update_frame_input JUMP input_cache.jump;
  update_frame_input NAIL input_cache.nail;
  state

let tick state =
  (* TODO-6 add menus: main/startup, pause, switch ghosts, switch weapons *)
  (* TODO-4 add sound effects and music *)
  state
  |> update_frame_inputs
  |> Ghost.update
  |> update_spawned_vengeful_spirits
  |> update_enemies
  |> update_npcs
  |> update_fragments
  |> update_camera
