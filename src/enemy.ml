open Utils
open Types

let is_dead (enemy : enemy) : bool = enemy.health.current <= 0
let is_alive (enemy : enemy) : bool = not (is_dead enemy)

let parse_name context name : enemy_id =
  match name with
  | "DUNCAN" -> DUNCAN
  | "LOCKER_BOY" -> LOCKER_BOY
  | "PENGUIN" -> PENGUIN
  | "FISH" -> FISH
  | "FROG" -> FROG
  | "ELECTRICITY" -> ELECTRICITY
  | "WIRED_ELECTRICITY" -> WIRED_ELECTRICITY
  | _ -> failwithf "Enemy.parse_name: found unrecognized enemy name '%s' in %s" name context

let action_started_at (enemy : enemy) (action_name : string) : time =
  match List.assoc_opt (PERFORMED action_name) enemy.history with
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
  (texture_config.path.pose_name, Sprite.build_texture_from_config texture_config)

let set_pose (enemy : enemy) (pose_name' : string) : unit =
  let pose_name =
    if String.starts_with ~prefix:"interaction-" pose_name' then
      Str.string_after pose_name' (String.length "interaction-")
    else
      pose_name'
  in
  match StringMap.find_opt pose_name enemy.textures with
  | None ->
    failwithf "could not find pose '%s' configured in enemies.json for enemy %s" pose_name
      (Show.enemy enemy)
  | Some texture -> Entity.update_sprite_texture enemy.entity texture

(* this can't take an enemy arg like the other functions because it uses current_props
   for "single-frame"/"temporary" props
*)
let get_prop ?(default = None) key props : float =
  match (List.assoc_opt key props, default) with
  | None, None -> failwithf "could not find enemy prop '%s' for enemy" key
  | None, Some d -> d
  | Some v, _ -> v

let get_bool_prop (enemy : enemy) prop : bool = get_prop ~default:(Some 0.) prop enemy.props = 1.
let set_prop (enemy : enemy) key new_val = enemy.props <- List.replace_assoc key new_val enemy.props

let get_json_prop (enemy : enemy) key : float =
  match StringMap.find_opt key enemy.json_props with
  | None ->
    failwithf "could not find json prop '%s' in enemies.json for enemy %s" key
      (Show.enemy_id enemy.id)
  | Some v -> v

(* this only supports projectiles moving horizontally *)
let spawn_projectile
    ?(scale = 1.)
    ?(projectile_texture_name = "projectile")
    ?(pogoable = false)
    ?(projectile_vx_opt = None)
    ?(damage = 1)
    ~(x_alignment : x_alignment)
    ~(direction : direction)
    (enemy : enemy)
    (despawn : projectile_duration)
    spawn_time : projectile =
  let projectile_texture =
    match StringMap.find_opt projectile_texture_name enemy.textures with
    | Some t -> t
    | None ->
      failwithf "could not find projectile '%s' for %s" projectile_texture_name
        (Show.enemy_name enemy)
  in
  let src = get_src projectile_texture in
  let w, h = (src.w *. Config.scale.room *. scale, src.h *. Config.scale.room *. scale) in
  let vx =
    match projectile_vx_opt with
    | None -> get_json_prop enemy "projectile_vx"
    | Some vx' -> vx'
  in
  let spawn_pos = Entity.get_child_pos enemy.entity (ALIGNED (x_alignment, CENTER)) w h in
  let vx' =
    match direction with
    | LEFT -> -1. *. vx
    | RIGHT -> vx
    | _ -> failwith "spawn_projectile direction must be horizontal"
  in
  let dest = { pos = spawn_pos; w; h } in
  let entity =
    Entity.create
      (fmt "%s projectile" (Show.enemy_name enemy))
      ~scale:(Config.scale.room *. scale) ~v:{ x = vx'; y = 0. } ~facing_right:(vx' > 0.)
      ~collision:(Some DEST) projectile_texture dest
  in
  { entity; despawn; spawned = { at = spawn_time }; pogoable; damage }

(* this is for logging arbitrary actions that don't have to be a renderable pose *)
let log_action (enemy : enemy) (action_name : string) (current : float) =
  enemy.history <- List.replace_assoc (PERFORMED action_name) { at = current } enemy.history

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
    (collision : collision) : bool =
  let kill_enemy () =
    (* TODO start_and_log_action "die"; *)
    enemy.entity.v.x <- 0.;
    enemy.spawned_projectiles <- [];
    enemy.status.choose_behavior <- false;
    enemy.status.check_damage_collisions <- false;
    match enemy.id with
    | LOCKER_BOY -> Entity.hide enemy.entity
    | FROG ->
      let v =
        let v' = get_json_prop enemy "death_recoil_v" in
        match collision.direction with
        | UP -> { x = 0.; y = -1. *. v' }
        | DOWN -> { x = 0.; y = v' }
        | RIGHT -> { x = v'; y = 0. }
        | LEFT -> { x = -1. *. v'; y = 0. }
      in
      enemy.entity.v <- v;
      (* this is just undoing the changes above, because the enemy isn't quite dead yet *)
      enemy.status.choose_behavior <- true;
      set_prop enemy "death_recoil" 1.
    | PENGUIN
    | DUNCAN
    | FISH
    | WIRED_ELECTRICITY
    | ELECTRICITY ->
      ()
  in

  if ghost_action_started > took_damage_at enemy damage_kind then (
    enemy.history <-
      List.replace_assoc
        (TOOK_DAMAGE damage_kind : enemy_action)
        { at = state.frame.time } enemy.history;
    enemy.health.current <- enemy.health.current - damage;
    let damage_texture = state.global.textures.damage in
    let texture_w, texture_h = get_scaled_texture_size Config.scale.room damage_texture in
    enemy.damage_sprites <-
      Sprite.spawn_particle
        (fmt "damage %s" (Show.enemy_name enemy))
        damage_texture
        {
          pos = { x = collision.rect.pos.x; y = collision.rect.pos.y };
          w = texture_w;
          h = texture_h;
        }
        state.frame.time
      :: enemy.damage_sprites;
    if is_dead enemy then
      kill_enemy ();
    true)
  else
    false

module type MEnemy = sig
  type action

  (* FIXME this only uses state for state.frame.time, so maybe just pass in that float
     - except for Frog, which updates state.camera.shake for explosions
  *)
  val choose_behavior : enemy -> state -> game -> unit
end

module Duncan : MEnemy = struct
  type action =
    | JUMP
    | LANDED
    | (* this is only used during cutscenes *)
      (* FIXME maybe add SCAVENGE ?
         - currently don't need it because it is only used in cutscenes, as a string
      *)
      WALK

  (* FIXME see if this would work (instead of string keys) *)
  (* type history = (action * float) list *)

  let set_action
      ?(interaction = false)
      (starting_action : bool)
      (enemy : enemy)
      (action : action)
      current_time
      current_props =
    let get_current_prop prop_name = get_prop prop_name current_props in
    let pose_name =
      match action with
      | WALK ->
        let walk_vx = get_json_prop enemy "walk_vx" in
        if enemy.entity.sprite.facing_right then
          enemy.entity.v.x <- walk_vx
        else
          enemy.entity.v.x <- -1. *. walk_vx;
        "walking"
      | LANDED ->
        let range = get_json_prop enemy "projectile_range" in
        let projectile_duration =
          X_BOUNDS (enemy.entity.dest.pos.x -. range, enemy.entity.dest.pos.x +. range)
        in
        enemy.entity.v.x <- 0.;
        enemy.entity.sprite.facing_right <-
          (* TODO might help to use a prefix like is_ or bool_ for these props that are used as booleans *)
          get_current_prop "facing_right" = 1.;
        enemy.spawned_projectiles <-
          [
            spawn_projectile enemy ~x_alignment:LEFT ~direction:RIGHT projectile_duration
              current_time;
            spawn_projectile enemy ~x_alignment:RIGHT ~direction:LEFT projectile_duration
              current_time;
          ]
          @ enemy.spawned_projectiles;
        "idle"
      | JUMP ->
        if interaction then
          if starting_action then (
            enemy.entity.v.y <- get_json_prop enemy "small_jump_vy";
            enemy.entity.current_floor <- None);
        enemy.entity.v.x <- get_current_prop "random_jump_vx";
        enemy.entity.v.y <- get_json_prop enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jumping"
    in
    set_pose enemy pose_name

  let start_and_log_action' enemy action current_time props : unit =
    let action_name =
      match action with
      | JUMP -> "jumping"
      | LANDED -> "landed"
      | WALK -> "walking"
    in
    log_action enemy action_name current_time;
    set_action true enemy action current_time props

  let choose_behavior (enemy : enemy) state game =
    match enemy.entity.current_floor with
    | None -> ()
    | Some _floor ->
      if false then
        start_and_log_action' enemy WALK state.frame.time
          [
            ( "facing_right",
              if game.player.ghost.entity.dest.pos.x > enemy.entity.dest.pos.x then 1. else 0. );
          ]
      else (
        let jumped = action_started_at enemy "jumping" in
        let landed = action_started_at enemy "landed" in
        if jumped.at > landed.at then
          start_and_log_action' enemy LANDED state.frame.time
            [
              ( "facing_right",
                if game.player.ghost.entity.dest.pos.x > enemy.entity.dest.pos.x then 1. else 0. );
            ]
        else if state.frame.time -. landed.at > List.assoc "jump_wait_time" enemy.json.props then (
          let room_center_x =
            (game.room.camera_bounds.min.x +. game.room.camera_bounds.max.x) /. 2.
          in
          let jump_vx =
            if room_center_x > enemy.entity.dest.pos.x then
              Random.float 500.
            else
              -1. *. Random.float 500.
          in
          start_and_log_action' enemy JUMP state.frame.time [ ("random_jump_vx", jump_vx) ]))
end

module LockerBoy = struct
  type action =
    | VANISH
    | UNVANISH
    | WALL_PERCH
    | DASH
    | DIVE

  let start_action (enemy : enemy) (action : action) current_time current_props =
    let get_current_prop prop_name = get_prop prop_name current_props in
    match action with
    | UNVANISH -> failwith "FIXME"
    | VANISH ->
      enemy.entity.v.x <- 0.;
      enemy.entity.v.y <- 0.
    | DASH ->
      let x =
        if get_current_prop "random_direction_right" = 1. then (
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
      enemy.entity.dest.pos.y <- 1340.
    | DIVE ->
      let x = get_current_prop "random_dive_x" in
      Entity.unhide enemy.entity;
      Entity.unfreeze enemy.entity;
      enemy.entity.v.y <- get_json_prop enemy "dive_vy";
      enemy.entity.dest.pos.x <- x;
      enemy.entity.dest.pos.y <- get_json_prop enemy "dive_y"
    | WALL_PERCH ->
      let (x, direction, x_alignment) : float * direction * x_alignment =
        if get_current_prop "random_direction_right" = 1. then (
          enemy.entity.sprite.facing_right <- true;
          (get_json_prop enemy "wall_perch_left_x", RIGHT, RIGHT))
        else (
          enemy.entity.sprite.facing_right <- false;
          (get_json_prop enemy "wall_perch_right_x", LEFT, LEFT))
      in
      let y = get_current_prop "random_wall_perch_y" in
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
          spawn_projectile enemy ~scale:0.5 ~pogoable:true ~x_alignment ~direction
            projectile_duration current_time;
        ]

  (* CLEANUP label args *)
  let continue_action (enemy : enemy) (action : action) current_time current_props current_duration
      =
    match action with
    | VANISH
    | UNVANISH ->
      start_action enemy action current_time current_props
    | DASH ->
      let max_dx = Config.window.max_width -. 300. in
      let check_right () =
        if enemy.entity.dest.pos.x > get_json_prop enemy "wall_perch_left_x" +. max_dx then
          set_prop enemy "should_vanish" 1.
      in
      let check_left () =
        if enemy.entity.dest.pos.x < get_json_prop enemy "wall_perch_right_x" -. max_dx then
          set_prop enemy "should_vanish" 1.
      in
      if enemy.entity.sprite.facing_right then check_right () else check_left ()
    | WALL_PERCH ->
      if current_duration > 2. then
        set_prop enemy "should_vanish" 1.
    | DIVE ->
      (* CLEANUP this should check for floor collision instead of pos.y *)
      if enemy.entity.dest.pos.y > 1300. then
        set_prop enemy "should_vanish" 1.

  let action_to_string action =
    match action with
    | VANISH -> "vanish"
    | UNVANISH -> "unvanish"
    | WALL_PERCH -> "wall-perch"
    | DASH -> "dash"
    | DIVE -> "dive"

  let string_to_action action_name =
    match action_name with
    | "vanish" -> VANISH
    | "unvanish" -> UNVANISH
    | "wall-perch" -> WALL_PERCH
    | "dash" -> DASH
    | "dive" -> DIVE
    | _ -> failwithf "unknown action: %s" action_name

  (* CLEANUP duplicated *)
  let start_and_log_action' enemy action current_time props : unit =
    log_action enemy (action |> action_to_string) current_time;
    start_action enemy action current_time props

  let last_performed_action (enemy : enemy) : (action * float) option =
    let action_performed ((action, _time) : enemy_action * time) : bool =
      (* this assumes new actions are pushed to the front of the list *)
      match action with
      | PERFORMED _ -> true
      | _ -> false
    in
    match List.find_opt action_performed enemy.history with
    | Some (PERFORMED action_name, performed) -> Some (action_name |> string_to_action, performed.at)
    | _ -> None

  (* TODO update locker boy configs for new room location *)
  let choose_behavior (enemy : enemy) state game =
    (* CLEANUP duplicated *)
    let still_doing (action_name : string) (duration : float) : bool =
      (* TODO maybe look up duration by name by adding json props like `action-name_duration` *)
      let started = action_started_at enemy action_name in
      state.frame.time -. started.at < duration
    in
    (* TODO this isn't working after resizing the screen - definitely need to update
       config values, maybe some code too *)
    let vanished = action_started_at enemy "vanish" in
    let unvanished = action_started_at enemy "unvanish" in
    let vanish_duration = animation_loop_duration (StringMap.find "vanish" enemy.textures) in
    let still_vanishing = still_doing "vanish" vanish_duration in
    let should_unvanish () = (not still_vanishing) && vanished > unvanished in
    let should_vanish () = get_bool_prop enemy "should_vanish" in
    let unvanish () =
      enemy.entity.v <- Zero.vector ();
      match Random.int 3 with
      | 0 ->
        start_and_log_action' enemy WALL_PERCH state.frame.time
          [
            ("random_direction_right", if Random.bool () then 1. else 0.);
            ("random_wall_perch_y", get_json_prop enemy "dive_y" +. 200. +. Random.float 500.);
          ]
      | 1 ->
        let left = get_json_prop enemy "wall_perch_left_x" in
        let right = get_json_prop enemy "wall_perch_right_x" in
        start_and_log_action' enemy DIVE state.frame.time
          [ ("random_dive_x", left +. Random.float (right -. left)) ]
      | 2 ->
        start_and_log_action' enemy DASH state.frame.time
          [ ("random_direction_right", if Random.bool () then 1. else 0.) ]
      | _ -> failwith "unreachable LOCKER_BOY behavior"
    in
    if still_vanishing then
      ()
    else if should_unvanish () then (
      log_action enemy "unvanish" state.frame.time;
      unvanish ())
    else if should_vanish () then (
      set_prop enemy "should_vanish" 0.;
      start_and_log_action' enemy VANISH state.frame.time [])
    else ((* TODO probably will need this for a lot of enemies *)
      match last_performed_action enemy with
      | None -> ()
      | Some (action_name, action_duration) ->
        let action =
          match action_name with
          (* FIXME  *)
          | _ -> DASH
        in
        continue_action enemy action state.frame.time [] (state.frame.time -. action_duration));
    ()
end

module Frog = struct
  type action =
    | HOMING
    | ASCEND
    | EXPLODE

  let set_action (enemy : enemy) (action : action) (room : room) current_time current_props =
    (* TODO move hardcoded numbers to json config *)
    let pose_name =
      match action with
      | HOMING ->
        let min_v, max_v =
          let v' = get_json_prop enemy "homing_v" in
          (-1. *. v', v')
        in
        let larger v = Float.bound min_v (v +. 5. +. Random.float 10.) max_v in
        let smaller v = Float.bound min_v (v -. (5. +. Random.float 10.)) max_v in
        let ghost_x = get_prop "ghost_x" current_props in
        let ghost_y = get_prop "ghost_y" current_props in
        let vx =
          if ghost_x > enemy.entity.dest.pos.x then
            larger enemy.entity.v.x
          else
            smaller enemy.entity.v.x
        in
        let vy =
          if ghost_y > enemy.entity.dest.pos.y then
            larger enemy.entity.v.y
          else
            smaller enemy.entity.v.y
        in
        enemy.entity.v.x <- vx;
        enemy.entity.v.y <- vy;
        "homing"
      | ASCEND ->
        enemy.entity.v.y <- -500. +. Random.float 150.;
        "idle"
      | EXPLODE ->
        let projectile =
          let explosion_scale = 4. in
          let projectile_duration = TIME_LEFT { seconds = 1. } in
          spawn_projectile enemy ~projectile_texture_name:"explosion" ~scale:explosion_scale
            ~pogoable:true ~projectile_vx_opt:(Some 0.) ~x_alignment:CENTER ~direction:RIGHT
            ~damage:2 projectile_duration current_time
        in
        (* this will only catch collisions on the first frame of the
           explosion, so a frog can move into an explosion without dying
        *)
        let check_frog_collision ((_, target_enemy) : enemy_id * enemy) =
          if target_enemy.id = FROG && target_enemy <> enemy then
            if Collision.between_entities projectile.entity target_enemy.entity then (
              let vx =
                let v' = get_json_prop enemy "death_recoil_v" in
                let explosion_center = (Entity.get_center projectile.entity).x in
                let target_center = (Entity.get_center target_enemy.entity).x in
                if explosion_center > target_center then
                  -1. *. v'
                else
                  v'
              in
              (* this only recoils the frog horizontally *)
              target_enemy.entity.v.x <- vx;
              target_enemy.health.current <- 0;
              set_prop target_enemy "death_recoil" 1.)
        in
        List.iter check_frog_collision room.enemies;
        room.loose_projectiles <- projectile :: room.loose_projectiles;
        Entity.hide enemy.entity;
        "idle"
    in
    set_pose enemy pose_name

  let choose_behavior enemy state game =
    (* CLEANUP duplicated *)
    let ghost_pos = game.player.ghost.entity.dest.pos in
    let still_doing (action_name : string) (duration : float) : bool =
      (* TODO maybe look up duration by name by adding json props like `action-name_duration` *)
      let started = action_started_at enemy action_name in
      state.frame.time -. started.at < duration
    in
    (* TODO these could be moved to enemies.json *)
    (* TODO this is breaking by the frog getting stuck in "ascending"
       - happens when starting a "Read" interaction in the same room
    *)
    let dunk_vy = 40. in
    let dunk_duration = 2. in
    let cooldown_duration = 0.5 in
    let initial_x = get_prop "initial_x" enemy.props in
    let initial_y = get_prop "initial_y" enemy.props in
    (* most enemies set choose_behavior <- false on death, but not FROG *)
    if is_dead enemy then (
      let any_liquid_collisions () =
        let collisions =
          Entity.get_water_collisions game.room enemy.entity
          @ Entity.get_acid_collisions game.room enemy.entity
        in
        List.length collisions > 0
      in
      let dunk () =
        enemy.entity.v <- { x = 0.; y = dunk_vy };
        log_action enemy "dunked" state.frame.time;
        set_prop enemy "dunked" 1.;
        set_pose enemy "struck"
      in

      if get_bool_prop enemy "dunked" then
        if still_doing "dunked" dunk_duration then
          set_pose enemy "struck"
        else
          Entity.hide enemy.entity
      else if get_bool_prop enemy "homing" then (
        set_action enemy HOMING game.room state.frame.time
          [ ("ghost_x", ghost_pos.x); ("ghost_y", ghost_pos.y) ];
        let should_explode =
          enemy.floor_collision_this_frame
          || Collision.between_entities enemy.entity game.player.ghost.entity
          (* TODO maybe check slash collisions too *)
        in
        if should_explode then (
          state.camera.shake <- 1.;
          set_action enemy EXPLODE game.room state.frame.time [])
        else if any_liquid_collisions () then
          dunk ())
      else if get_bool_prop enemy "struck_cooldown" then (
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        let cooldown_started = action_started_at enemy "struck_cooldown" in
        if not (still_doing "struck_cooldown" cooldown_duration) then
          set_prop enemy "homing" 1.)
      else if get_bool_prop enemy "death_recoil" then
        if enemy.floor_collision_this_frame then (
          log_action enemy "struck_cooldown" state.frame.time;
          set_prop enemy "struck_cooldown" 1.)
        else if any_liquid_collisions () then (* TODO maybe have a different texture for "dunked" *)
          dunk ()
        else
          set_pose enemy "struck")
    else (
      if enemy.entity.dest.pos.y > initial_y +. 100. then (
        (* TODO duplicated - maybe add turn_towards_origin fn if this is shared often enough *)
        let rand_x = Random.float 25. in
        let new_vx =
          if initial_x > enemy.entity.dest.pos.x then (
            enemy.entity.sprite.facing_right <- true;
            rand_x)
          else (
            enemy.entity.sprite.facing_right <- false;
            -1. *. rand_x)
        in
        enemy.entity.v.x <- new_vx;
        set_action enemy ASCEND game.room state.frame.time []);
      if enemy.entity.v.y > 0. then (
        enemy.entity.v.y <- Float.bound 0. enemy.entity.v.y 120.;
        set_pose enemy "idle-descending")
      else
        set_pose enemy "idle")
end

module Electricity = struct
  type action = SHOCK

  let set_action (enemy : enemy) (action : action) current_time =
    let pose_name =
      match action with
      | SHOCK ->
        let action_name = "shock" in
        (* ELECTRICITY should always have the "idle" pose - starting this action just adds the shock child sprite *)
        let shock_config = List.assoc action_name enemy.json.texture_configs in
        let projectile_duration =
          TIME_LEFT { seconds = (shock_config.count |> Int.to_float) *. shock_config.duration }
        in
        enemy.spawned_projectiles <-
          [
            spawn_projectile enemy ~projectile_texture_name:action_name ~projectile_vx_opt:(Some 0.)
              ~scale:1.1 ~pogoable:true ~x_alignment:CENTER ~direction:RIGHT projectile_duration
              current_time;
          ];
        log_action enemy action_name current_time;
        "idle"
    in
    set_pose enemy pose_name

  let choose_behavior enemy state game =
    let last_shock = action_started_at enemy "shock" in
    let shock_duration =
      (* needs to be greater than shock config (count * duration) *)
      get_json_prop enemy "shock_duration"
    in
    if last_shock.at < state.frame.time -. shock_duration then
      set_action enemy SHOCK state.frame.time
end

module Fish = struct
  let choose_behavior enemy state game =
    let last_direction_change = action_started_at enemy "change_direction" in
    let direction_change_dt = get_prop ~default:(Some 1.) "direction_change_dt" enemy.props in
    if last_direction_change.at < state.frame.time -. direction_change_dt then (
      let direction_change_max_dt = get_json_prop enemy "direction_change_max_dt" in
      set_prop enemy "direction_change_dt" (Random.float direction_change_max_dt);
      let initial_x = get_prop "initial_x" enemy.props in
      let initial_y = get_prop "initial_y" enemy.props in
      let max_v = get_json_prop enemy "max_v" in
      let rand_x, rand_y = (Random.float max_v, Random.float max_v) in
      let new_vx =
        if initial_x > enemy.entity.dest.pos.x then (
          enemy.entity.sprite.facing_right <- true;
          rand_x)
        else (
          enemy.entity.sprite.facing_right <- false;
          -1. *. rand_x)
      in
      let new_vy =
        if initial_y > enemy.entity.dest.pos.y then
          rand_y
        else
          -1. *. rand_y
      in
      enemy.entity.v.x <- new_vx;
      enemy.entity.v.y <- new_vy;
      log_action enemy "change_direction" state.frame.time)
end

let choose_behavior (enemy : enemy) (state : state) (game : game) =
  let ghost_pos = game.player.ghost.entity.dest.pos in
  match enemy.id with
  | LOCKER_BOY -> LockerBoy.choose_behavior enemy state game
  | DUNCAN -> Duncan.choose_behavior enemy state game
  | FISH -> Fish.choose_behavior enemy state game
  | FROG -> Frog.choose_behavior enemy state game
  | WIRED_ELECTRICITY -> (* TODO *) ()
  | ELECTRICITY -> Electricity.choose_behavior enemy state game
  | PENGUIN -> (* TODO  *) ()

(* object rects in Tiled define the position of the enemy, and enemies.json defines w/h *)
let create_from_rects
    (enemy_rects : (enemy_id * rect) list)
    finished_interactions
    (enemy_configs : (enemy_id * Json_t.enemy_config) list) : enemy list =
  let texture_cache : (enemy_id * (string * texture) list) list ref = ref [] in
  let build id kind enemy_name (enemy_config : Json_t.enemy_config) entity_dest on_killed : enemy =
    let texture_configs : texture_config list =
      List.map (Entity.to_texture_config ENEMIES enemy_name) enemy_config.texture_configs
    in
    let entity, textures =
      match List.assoc_opt id !texture_cache with
      | None ->
        Entity.create_from_texture_configs ~collision:(Some DEST)
          ~gravity_multiplier:enemy_config.gravity_multiplier texture_configs entity_dest
      | Some textures ->
        Entity.create_from_textures ~collision:(Some DEST)
          ~gravity_multiplier:enemy_config.gravity_multiplier texture_configs textures entity_dest
    in

    texture_cache := List.replace_assoc id textures !texture_cache;

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
      damage = json.damage;
      health = { current = enemy_config.health; max = enemy_config.health };
      textures = textures |> List.to_string_map;
      history = [];
      props = [ ("initial_x", entity.dest.pos.x); ("initial_y", entity.dest.pos.y) ];
      floor_collision_this_frame = false;
      spawned_projectiles = [];
      damage_sprites = [];
      on_killed;
      json_props = json.props |> List.to_string_map;
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
      | "boss" -> (fmt "boss-killed:%s" enemy_name, false, BOSS)
      | "multi-boss" -> (fmt "boss-killed:%s" enemy_name, true, MULTI_BOSS)
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
