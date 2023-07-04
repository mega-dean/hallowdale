open Types

[@@@ocaml.warning "-26-27-32"]

let is_dead (enemy : enemy) : bool = enemy.health.current <= 0
let is_alive (enemy : enemy) : bool = not (is_dead enemy)

let parse_name context name : enemy_id =
  match name with
  | "DUNCAN" -> DUNCAN
  | "LOCKER_BOY" -> LOCKER_BOY
  | "PENGUIN" -> PENGUIN
  | _ -> failwithf "Enemy.parse_name: found unrecognized enemy name '%s' in %s" name context

let last_performed_action (e : enemy) : string * float =
  let action_performed ((action, _time) : enemy_action * time) : bool =
    (* this assumes new actions are pushed to the front of the list *)
    match action with
    | PERFORMED _ -> true
    | _ -> false
  in
  match List.find_opt action_performed e.history with
  | Some (PERFORMED action, performed) -> (action, performed.at)
  | _ -> failwith "unreachable"

let action_started_at (e : enemy) (action_name : string) : time =
  match List.assoc_opt (PERFORMED action_name) e.history with
  | None -> Zero.time ()
  | Some time -> time

let last_damage (enemy : enemy) : time =
  let damage = ref 0. in
  let check_history ((action, time) : enemy_action * time) =
    match action with
    | TOOK_DAMAGE damage_kind -> (
      match damage_kind with
      | DREAM_NAIL ->
        (* TODO would be nice to have the enemies flash when they are hit with dream nail *)
        ()
      | _ ->
        if time.at > !damage then
          damage := time.at)
    | _ -> ()
  in
  List.iter check_history enemy.history;
  { at = !damage }

let load_pose (texture_config : texture_config) : string * texture =
  (texture_config.pose_name, Sprite.build_texture_from_config texture_config)

let set_pose (enemy : enemy) (pose_name : string) : unit =
  match List.assoc_opt pose_name enemy.textures with
  | None ->
    failwithf "could not find pose '%s' configured in enemies.json for enemy %s" pose_name
      (Show.enemy enemy)
  | Some texture -> Entity.update_sprite_texture enemy.entity texture

let get_prop ?(default = None) key props : float =
  match (List.assoc_opt key props, default) with
  | None, None -> failwithf "could not find enemy prop '%s' for enemy" key
  | None, Some d -> d
  | Some v, _ -> v

let set_prop (e : enemy) key new_val = e.props <- Utils.replace_assoc key new_val e.props

let get_json_prop (e : enemy) key : float =
  match List.assoc_opt key e.json.props with
  | None ->
    failwithf "could not find json prop '%s' in enemies.json for enemy %s" key (Show.enemy_id e.id)
  | Some v -> v

let spawn_projectile
    (e : enemy)
    ?(scale = 1.)
    ?(projectile_texture_name = "projectile")
    ?(pogoable = false)
    spawn_direction
    despawn
    spawn_time : projectile =
  let projectile_texture = List.assoc projectile_texture_name e.textures in
  let src = get_src projectile_texture in
  let w, h = (src.w *. Config.scale.room *. scale, src.h *. Config.scale.room *. scale) in
  let vx = get_json_prop e "projectile_vx" in
  let vx', dest_x =
    match spawn_direction with
    | LEFT -> (-1. *. vx, e.entity.dest.pos.x -. w)
    | RIGHT -> (vx, e.entity.dest.pos.x +. e.entity.dest.w)
    | _ -> failwith "spawn_projectile - bad spawn_direction"
  in
  let dest = { pos = { x = dest_x; y = e.entity.dest.pos.y -. (h /. 2.) }; w; h } in
  let entity =
    Entity.create
      (fmt "%s projectile ---------------------- " (Show.enemy_name e))
      ~scale:(Config.scale.room *. scale) ~v:{ x = vx'; y = 0. } ~facing_right:(vx' > 0.)
      ~collision:(Some DEST) projectile_texture dest
  in

  { entity; despawn; spawned = { at = spawn_time }; pogoable }

let set_action (enemy : enemy) ?(current_duration_opt = None) pose_name' current_time current_props
    =
  let get_prop' prop_name = get_prop prop_name current_props in
  let starting_action, current_duration =
    match current_duration_opt with
    | None -> (true, -999.)
    | Some duration -> (false, duration)
  in
  let pose_name =
    (* this is only used for changing properties during an interaction, so maybe this could just
       strip the prefix "interaction-" *)
    ref pose_name'
  in
  (match pose_name' with
  (* these poses can apply to any enemy *)
  | "walking" ->
    let walk_vx = get_json_prop enemy "walk_vx" in
    if enemy.entity.sprite.facing_right then
      enemy.entity.v.x <- walk_vx
    else
      enemy.entity.v.x <- -1. *. walk_vx
  | _ -> (
    (* these poses are all enemy-specific *)
    match enemy.id with
    | DUNCAN -> (
      match pose_name' with
      | "interaction-jumping" ->
        pose_name := "jumping";
        if starting_action then (
          enemy.entity.v.y <- get_json_prop enemy "small_jump_vy";
          enemy.entity.current_floor <- None)
      | "landed" ->
        pose_name := "idle";
        let range = get_json_prop enemy "projectile_range" in
        let projectile_duration =
          X_BOUNDS (enemy.entity.dest.pos.x -. range, enemy.entity.dest.pos.x +. range)
        in
        enemy.entity.v.x <- 0.;
        enemy.entity.sprite.facing_right <-
          (* TODO might help to use a prefix like is_ or bool_ for these props that are used as booleans *)
          get_prop' "facing_right" = 1.;
        enemy.spawned_projectiles <-
          [
            spawn_projectile enemy LEFT projectile_duration current_time;
            spawn_projectile enemy RIGHT projectile_duration current_time;
          ]
          @ enemy.spawned_projectiles
      | "jumping" ->
        enemy.entity.v.x <- get_prop' "random_jump_vx";
        enemy.entity.v.y <- get_json_prop enemy "jump_vy";
        enemy.entity.current_floor <- None
      | _ -> failwithf "unknown pose_name for DUNCAN %s" pose_name')
    | LOCKER_BOY -> (
      match pose_name' with
      | "vanish" ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.
      | "dash" ->
        if starting_action then (
          let x =
            if get_prop' "random_direction_right" = 1. then (
              enemy.entity.sprite.facing_right <- true;
              enemy.entity.v.x <- get_json_prop enemy "dash_vx";
              get_json_prop enemy "wall_perch_left_x" +. 120.)
            else (
              enemy.entity.sprite.facing_right <- false;
              enemy.entity.v.x <- -1. *. get_json_prop enemy "dash_vx";
              get_json_prop enemy "wall_perch_right_x" -. 120.)
          in
          Entity.unhide enemy.entity;
          Entity.unfreeze enemy.entity;
          (* enemy.entity.current_floor <- None; *)
          enemy.entity.dest.pos.x <- x;
          enemy.entity.dest.pos.y <- 1340.)
        else (
          let max_dx = Config.window.width - 300 |> Int.to_float in
          let check_right () =
            if enemy.entity.dest.pos.x > get_json_prop enemy "wall_perch_left_x" +. max_dx then
              set_prop enemy "should_vanish" 1.
          in
          let check_left () =
            if enemy.entity.dest.pos.x < get_json_prop enemy "wall_perch_right_x" -. max_dx then
              set_prop enemy "should_vanish" 1.
          in
          if enemy.entity.sprite.facing_right then check_right () else check_left ())
      | "dive" ->
        if starting_action then (
          let x = get_prop' "random_dive_x" in
          Entity.unhide enemy.entity;
          Entity.unfreeze enemy.entity;
          enemy.entity.v.y <- get_json_prop enemy "dive_vy";
          enemy.entity.dest.pos.x <- x;
          enemy.entity.dest.pos.y <- get_json_prop enemy "dive_y")
        else if enemy.entity.dest.pos.y > 1300. then
          set_prop enemy "should_vanish" 1.
      | "wall-perch" ->
        if starting_action then (
          let x, projectile_direction =
            if get_prop' "random_direction_right" = 1. then (
              enemy.entity.sprite.facing_right <- true;
              (get_json_prop enemy "wall_perch_left_x", RIGHT))
            else (
              enemy.entity.sprite.facing_right <- false;
              (get_json_prop enemy "wall_perch_right_x", LEFT))
          in
          let y = get_prop' "random_wall_perch_y" in
          Entity.unhide enemy.entity;
          Entity.unfreeze enemy.entity;

          enemy.entity.dest.pos.x <- x;
          enemy.entity.dest.pos.y <- y;
          let projectile_duration =
            X_BOUNDS
              ( get_json_prop enemy "wall_perch_left_x" -. 200.,
                get_json_prop enemy "wall_perch_right_x" +. 200. )
          in
          enemy.spawned_projectiles <-
            [
              spawn_projectile enemy ~scale:0.5 ~pogoable:true projectile_direction
                projectile_duration current_time;
            ])
        else if current_duration > 2. then
          set_prop enemy "should_vanish" 1.
      | _ -> ())
    | _ -> ()));
  set_pose enemy !pose_name

let start_action (enemy : enemy) (pose_name : string) (current_time : float) current_props =
  set_action enemy pose_name current_time current_props

let continue_action
    (enemy : enemy)
    (pose_name : string)
    current_duration_opt
    current_time
    current_props =
  set_action enemy ~current_duration_opt pose_name current_time current_props

let log_action (enemy : enemy) (action_name : string) (current : float) =
  enemy.history <- Utils.replace_assoc (PERFORMED action_name) { at = current } enemy.history

let start_and_log_action (enemy : enemy) (action_name : string) (current : float) current_props =
  log_action enemy action_name current;
  start_action enemy action_name current current_props

let took_damage_at (enemy : enemy) (damage_kind : damage_kind) =
  match List.assoc_opt (TOOK_DAMAGE damage_kind : enemy_action) enemy.history with
  | None -> Zero.time ()
  | Some time -> time

let maybe_take_damage
    (state : state)
    (enemy : enemy)
    (ghost_action_started : time)
    (damage_kind : damage_kind)
    (damage : int)
    (dest : rect) : bool =
  let kill_enemy () =
    (match enemy.kind with
    | ENEMY ->
      enemy.entity.y_recoil <- Some { speed = 100.; time_left = { seconds = 1. }; reset_v = true };
      enemy.entity.x_recoil <-
        Some
          {
            speed = (if Random.bool () then -100. else 100.);
            time_left = { seconds = 1. };
            reset_v = true;
          }
    | _ -> ());
    (match enemy.id with
    | LOCKER_BOY -> Entity.hide enemy.entity
    | _ -> ());
    (* TODO start_and_log_action "die"; *)
    enemy.spawned_projectiles <- [];
    enemy.status.choose_behavior <- false;
    enemy.status.check_damage_collisions <- false
  in

  if ghost_action_started > took_damage_at enemy damage_kind then (
    enemy.history <-
      Utils.replace_assoc
        (TOOK_DAMAGE damage_kind : enemy_action)
        { at = state.frame.time } enemy.history;
    enemy.health.current <- enemy.health.current - damage;
    let damage_texture = state.global.textures.damage in
    let texture_w, texture_h = get_scaled_texture_size damage_texture in
    enemy.damage_sprites <-
      Sprite.spawn_particle
        (fmt "damage %s" (Show.enemy_name enemy))
        damage_texture
        { pos = { x = dest.pos.x; y = dest.pos.y }; w = texture_w; h = texture_h }
        state.frame.time
      :: enemy.damage_sprites;
    if is_dead enemy then
      kill_enemy ();
    true)
  else
    false

let time_ago (e : enemy) (action_name : string) (clock : time) : duration =
  let performed : time = action_started_at e action_name in
  { seconds = clock.at -. performed.at }

(* object rects in Tiled define the position of the enemy, and enemies.json defines w/h *)
let create_from_rects
    (enemy_rects : (enemy_id * rect) list)
    finished_interactions
    (enemy_configs : (enemy_id * Json_t.enemy_config) list) : enemy list =
  (* TODO this is loading the same textures multiple times for eg. locker-boys *)
  let build id kind enemy_name (enemy_config : Json_t.enemy_config) entity_dest on_killed : enemy =
    (* TODO-4 remove .choose_behavior from enemy and just match .kind every frame *)
    let choose_behavior =
      match id with
      | LOCKER_BOY ->
        fun ~(self : enemy) (params : enemy_behavior_params) : unit ->
          let vanished = action_started_at self "vanish" in
          let unvanished = action_started_at self "unvanish" in
          let vanish_duration = animation_loop_duration (List.assoc "vanish" self.textures) in
          let still_vanishing = params.time -. vanished.at < vanish_duration in
          let should_unvanish () = (not still_vanishing) && vanished > unvanished in
          let should_vanish () = get_prop ~default:(Some 0.) "should_vanish" self.props = 1. in
          let unvanish () =
            self.entity.v <- Zero.vector ();
            match Random.int 3 with
            | 0 ->
              start_and_log_action self "wall-perch" params.time
                [
                  ("random_direction_right", if Random.bool () then 1. else 0.);
                  ("random_wall_perch_y", get_json_prop self "dive_y" +. 200. +. Random.float 500.);
                ]
            | 1 ->
              let left = get_json_prop self "wall_perch_left_x" in
              let right = get_json_prop self "wall_perch_right_x" in
              start_and_log_action self "dive" params.time
                [ ("random_dive_x", left +. Random.float (right -. left)) ]
            | 2 ->
              start_and_log_action self "dash" params.time
                [ ("random_direction_right", if Random.bool () then 1. else 0.) ]
            | _ -> failwith "unreachable LOCKER_BOY behavior"
          in
          if still_vanishing then
            ()
          else if should_unvanish () then (
            log_action self "unvanish" params.time;
            unvanish ())
          else if should_vanish () then (
            set_prop self "should_vanish" 0.;
            start_and_log_action self "vanish" params.time [])
          else (
            let action, action_duration = last_performed_action self in
            continue_action self action (Some (params.time -. action_duration)) params.time []);
          ()
      | DUNCAN ->
        let update_duncan ~(self : enemy) (params : enemy_behavior_params) : unit =
          match self.entity.current_floor with
          | None -> ()
          | Some _floor ->
            let jumped = action_started_at self "jumping" in
            let landed = action_started_at self "landed" in
            if jumped.at > landed.at then
              start_and_log_action self "landed" params.time
                [ ("facing_right", if params.ghost_pos.x > self.entity.dest.pos.x then 1. else 0.) ]
            else if params.time -. landed.at > List.assoc "jump_wait_time" self.json.props then (
              let room_center_x = (params.room_bounds.min.x +. params.room_bounds.max.x) /. 2. in
              let jump_vx =
                if room_center_x > self.entity.dest.pos.x then
                  Random.float 500.
                else
                  -1. *. Random.float 500.
              in
              start_and_log_action self "jumping" params.time [ ("random_jump_vx", jump_vx) ])
        in
        update_duncan
      | PENGUIN ->
        fun ~(self : enemy) (params : enemy_behavior_params) : unit ->
          ignore self;
          ignore params;
          ()
    in

    let texture_configs : texture_config list =
      List.map (Entity.to_texture_config ENEMIES enemy_name) enemy_config.texture_configs
    in
    let entity, textures =
      Entity.create_from_textures ~collision:(Some DEST) texture_configs entity_dest
    in

    let json =
      match List.assoc_opt id enemy_configs with
      | None -> failwithf "could not find enemy json config for %s" (Show.enemy_id id)
      | Some j -> j
    in
    let status =
      let b = Entity.is_on_screen entity in
      { choose_behavior = b; check_damage_collisions = b }
    in
    {
      id;
      status;
      kind;
      entity;
      health = { current = enemy_config.health; max = enemy_config.health };
      textures;
      choose_behavior;
      history = [];
      props = [];
      spawned_projectiles = [];
      damage_sprites = [];
      on_killed;
      json;
    }
  in
  let build_enemy_from_rect ((enemy_id, dest) : enemy_id * rect) : enemy option =
    let enemy_name = Show.enemy_id enemy_id in
    let enemy_config : Json_t.enemy_config =
      match List.assoc_opt enemy_id enemy_configs with
      | None -> failwithf "missing config in enemies.json for %s" enemy_name
      | Some config -> config
    in
    let cutscene_name, multiple_enemies, enemy_kind =
      (* boss_kind could probably be configured as an optional variant in atd, but using a string with default value is easier *)
      match enemy_config.kind with
      | "enemy" -> ("", false, ENEMY)
      | "boss" -> (fmt "boss-killed_%s" enemy_name, false, BOSS)
      | "multi-boss" -> (fmt "boss-killed_%s" enemy_name, true, MULTI_BOSS)
      | _ -> failwithf "%s bad boss_kind '%s'" enemy_name enemy_config.kind
    in
    let on_killed =
      {
        interaction_name = (if cutscene_name = "" then None else Some cutscene_name);
        multiple_enemies;
      }
    in
    if List.mem cutscene_name finished_interactions then
      None
    else (
      let w, h =
        ( (enemy_config.w |> Int.to_float) *. Config.scale.ghost,
          (enemy_config.h |> Int.to_float) *. Config.scale.ghost )
      in
      Some (build enemy_id enemy_kind enemy_name enemy_config { dest with w; h } on_killed))
  in

  List.filter_map build_enemy_from_rect enemy_rects
