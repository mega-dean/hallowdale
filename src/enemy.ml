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
  match EnemyActionMap.find_opt (PERFORMED action_name) enemy.history with
  | None -> Zero.time ()
  | Some time -> time

let last_damage (enemy : enemy) : time =
  let damage = ref 0. in
  let check_history (action : enemy_action) time =
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
  EnemyActionMap.iter check_history enemy.history;
  { at = !damage }

let load_pose (texture_config : texture_config) : string * texture =
  (texture_config.path.pose_name, Sprite.build_texture_from_config texture_config)

let set_pose (enemy : enemy) (pose_name : string) : unit =
  match StringMap.find_opt pose_name enemy.textures with
  | None ->
    failwithf "could not find pose '%s' configured in enemies.json for enemy %s" pose_name
      (Show.enemy enemy)
  | Some texture -> Entity.update_sprite_texture enemy.entity texture

(* this can't take an enemy arg like the other functions because it uses current_props
   for "single-frame"/"temporary" props
*)
let get_prop ?(default = None) key props : float =
  match (StringMap.find_opt key props, default) with
  | None, None -> failwithf "could not find enemy prop '%s' for enemy" key
  | None, Some d -> d
  | Some v, _ -> v

let get_bool_prop (enemy : enemy) prop : bool = get_prop ~default:(Some 0.) prop enemy.props = 1.

let set_prop (enemy : enemy) key new_val =
  enemy.props <- StringMap.update key (fun _ -> Some new_val) enemy.props

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

let took_damage_at (enemy : enemy) (damage_kind : damage_kind) =
  match EnemyActionMap.find_opt (TOOK_DAMAGE damage_kind : enemy_action) enemy.history with
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
      EnemyActionMap.update (TOOK_DAMAGE damage_kind)
        (fun _ -> Some { at = state.frame.time })
        enemy.history;
    enemy.health.current <- enemy.health.current - damage;
    let damage_texture = state.global.textures.damage in
    let texture_w, texture_h = get_scaled_texture_size Config.scale.room damage_texture in
    let new_damage_sprite =
      Sprite.spawn_particle
        (fmt "damage %s" (Show.enemy_name enemy))
        damage_texture
        {
          pos = { x = collision.rect.pos.x; y = collision.rect.pos.y };
          w = texture_w;
          h = texture_h;
        }
        state.frame.time
    in
    enemy.damage_sprites <- new_damage_sprite :: enemy.damage_sprites;
    if is_dead enemy then
      kill_enemy ();
    true)
  else
    false

module type Enemy_actions = sig
  type t

  val to_string : t -> string
  val from_string : string -> t
  val set : enemy -> ?frame_props:float StringMap.t -> t -> current_time:float -> unit
end

module type Loggable = sig
  include Enemy_actions

  val log : enemy -> string -> float -> unit
  val start_and_log : enemy -> t -> float -> (string * float) list -> unit
  val still_doing : enemy -> string -> duration:float -> frame_time:float -> bool
end

module Make_loggable (Actions : Enemy_actions) : Loggable with type t = Actions.t = struct
  include Actions

  let log (enemy : enemy) action_name value =
    enemy.history <-
      EnemyActionMap.update (PERFORMED action_name) (fun _ -> Some { at = value }) enemy.history

  let start_and_log enemy (action : Actions.t) current_time props : unit =
    let action_name = action |> Actions.to_string in
    enemy.last_performed <- Some (action_name, { at = current_time });
    log enemy action_name current_time;
    Actions.set enemy action ~current_time ~frame_props:(props |> List.to_string_map)

  (* CLEANUP maybe look up duration by name by adding json props like `action-name_duration` *)
  let still_doing (enemy : enemy) (action_name : string) ~duration ~frame_time : bool =
    let started = action_started_at enemy action_name in
    frame_time -. started.at < duration
end

module type M = sig
  module Action : Loggable

  type args

  val get_args : state -> game -> args
  val choose_behavior : enemy -> args -> unit
end

module Duncan_actions = struct
  type t =
    | JUMP
    | LANDED
    | WALK

  let to_string (action : t) : string =
    match action with
    | JUMP -> "jumping"
    | LANDED -> "landed"
    | WALK -> "walk"

  let from_string (s : string) : t =
    match s with
    | "jumping" -> JUMP
    | "landed" -> LANDED
    | "walking" -> WALK
    | _ -> failwithf "Duncan_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = StringMap.empty) (action : t) ~current_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
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
          get_frame_prop "facing_right" = 1.;
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
        enemy.entity.v.x <- get_frame_prop ~default:(Some enemy.entity.v.x) "random_jump_vx";
        enemy.entity.v.y <- get_json_prop enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jumping"
    in
    set_pose enemy pose_name
end

let get_boss_area boss_name game =
  match game.room.boss_area with
  | None -> failwithf "missing boss-area for %s" boss_name
  | Some rect -> rect

module Duncan : M = struct
  module Action = Make_loggable (Duncan_actions)

  type args = {
    frame_time : float;
    camera_bounds : bounds;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      camera_bounds = game.room.camera_bounds;
      boss_area = get_boss_area "DUNCAN" game;
      ghost_pos = game.player.ghost.entity.dest.pos;
    }

  let choose_behavior (enemy : enemy) args =
    match enemy.entity.current_floor with
    | None -> ()
    | Some _floor ->
      let jumped = action_started_at enemy "jumping" in
      let landed = action_started_at enemy "landed" in
      let airtime =
        (* this is incorrect for the first jump (off the vending machine), but it's close enough *)
        landed.at -. jumped.at
      in
      if jumped.at > landed.at then
        Action.start_and_log enemy LANDED args.frame_time
          [ ("facing_right", if args.ghost_pos.x > enemy.entity.dest.pos.x then 1. else 0.) ]
      else if args.frame_time -. landed.at > get_json_prop enemy "jump_wait_time" then (
        let target_x = Random.float args.boss_area.w +. args.boss_area.pos.x in
        let jump_vx =
          let dx = target_x -. enemy.entity.dest.pos.x in
          dx /. airtime
        in
        Action.start_and_log enemy JUMP args.frame_time [ ("random_jump_vx", jump_vx) ])
end

module Locker_boy_actions = struct
  type t =
    | VANISH
    | WALL_PERCH
    | DASH
    | DIVE

  let to_string (action : t) : string =
    match action with
    | VANISH -> "vanish"
    | WALL_PERCH -> "wall-perch"
    | DASH -> "dash"
    | DIVE -> "dive"

  let from_string (s : string) : t =
    match s with
    | "vanish" -> VANISH
    | "wall-perch" -> WALL_PERCH
    | "dash" -> DASH
    | "dive" -> DIVE
    | _ -> failwithf "Locker_boy_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = StringMap.empty) (action : t) ~current_time =
    let get_frame_prop prop_name = get_prop prop_name frame_props in
    let pose_name : string =
      match action with
      | VANISH ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "vanish"
      | DASH ->
        let x =
          if get_frame_prop "random_direction_right" = 1. then (
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
        enemy.entity.dest.pos.y <- 1340.;
        "dash"
      | DIVE ->
        let x = get_frame_prop "random_dive_x" in
        Entity.unhide enemy.entity;
        Entity.unfreeze enemy.entity;
        enemy.entity.v.y <- get_json_prop enemy "dive_vy";
        enemy.entity.dest.pos.x <- x;
        enemy.entity.dest.pos.y <- get_json_prop enemy "dive_y";
        "dive"
      | WALL_PERCH ->
        let (x, direction, x_alignment) : float * direction * x_alignment =
          if get_frame_prop "random_direction_right" = 1. then (
            enemy.entity.sprite.facing_right <- true;
            (get_json_prop enemy "wall_perch_left_x", RIGHT, RIGHT))
          else (
            enemy.entity.sprite.facing_right <- false;
            (get_json_prop enemy "wall_perch_right_x", LEFT, LEFT))
        in
        let y = get_frame_prop "random_wall_perch_y" in
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
          ];
        "wall-perch"
    in
    set_pose enemy pose_name
end

module Locker_boy : M = struct
  module Action = Make_loggable (Locker_boy_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
  }

  let get_args state game : args =
    { frame_time = state.frame.time; boss_area = get_boss_area "LOCKER_BOYS" game }

  let continue_action (enemy : enemy) (action : Action.t) current_time current_duration =
    match action with
    | VANISH -> Action.set enemy action ~current_time
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
      enemy.entity.v.y <- 0.;
      if current_duration > 2. then
        set_prop enemy "should_vanish" 1.
    | DIVE ->
      (* TODO this vanishes immediately, probably should be a delay *)
      if Option.is_some enemy.entity.current_floor then
        set_prop enemy "should_vanish" 1.

  let choose_behavior (enemy : enemy) args =
    let vanished = action_started_at enemy "vanish" in
    let unvanished = action_started_at enemy "unvanish" in
    let vanish_duration = animation_loop_duration (StringMap.find "vanish" enemy.textures) in
    let still_vanishing =
      Action.still_doing enemy "vanish" ~duration:vanish_duration ~frame_time:args.frame_time
    in
    let should_unvanish () = (not still_vanishing) && vanished.at > unvanished.at in
    let should_vanish () = get_bool_prop enemy "should_vanish" in
    let unvanish () =
      (* these state updates happen here because "unvanish" isn't its own action that can be
         started (it just chooses the next action to start)
      *)
      enemy.entity.v <- Zero.vector ();
      enemy.entity.current_floor <- None;
      match List.sample [ `WALL_PERCH; `DIVE; `DASH ] with
      | `WALL_PERCH ->
        Action.start_and_log enemy WALL_PERCH args.frame_time
          [
            ("random_direction_right", if Random.bool () then 1. else 0.);
            ("random_wall_perch_y", get_json_prop enemy "dive_y" +. 200. +. Random.float 500.);
          ]
      | `DIVE ->
        let left = get_json_prop enemy "wall_perch_left_x" in
        let right = get_json_prop enemy "wall_perch_right_x" in
        Action.start_and_log enemy DIVE args.frame_time
          [ ("random_dive_x", left +. Random.float (right -. left)) ]
      | `DASH ->
        Action.start_and_log enemy DASH args.frame_time
          [ ("random_direction_right", if Random.bool () then 1. else 0.) ]
    in
    if still_vanishing then
      ()
    else if should_unvanish () then (
      Action.log enemy "unvanish" args.frame_time;
      unvanish ())
    else if should_vanish () then (
      set_prop enemy "should_vanish" 0.;
      Action.start_and_log enemy VANISH args.frame_time [])
    else ((* TODO probably will need this for a lot of enemies *)
      match enemy.last_performed with
      | None -> ()
      | Some (action_name, action_time) ->
        continue_action enemy (Action.from_string action_name) args.frame_time
          (args.frame_time -. action_time.at))
end

module Frog_actions = struct
  type t =
    | HOMING
    | ASCEND
    | DUNKED

  let to_string (action : t) : string =
    match action with
    | HOMING -> "homing"
    | ASCEND -> "ascend"
    | DUNKED -> "dunked"

  let from_string (s : string) : t =
    match s with
    | "homing" -> HOMING
    | "ascend" -> ASCEND
    | "dunked" -> DUNKED
    | _ -> failwithf "Frog_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = StringMap.empty) (action : t) ~current_time =
    (* TODO move hardcoded numbers to json config *)
    let pose_name =
      match action with
      | HOMING ->
        let min_v, max_v =
          let v' = get_json_prop enemy "homing_v" in
          (-1. *. v', v')
        in
        let dv = get_prop "random_homing_dv" frame_props in
        let larger v = Float.bound min_v (v +. dv) max_v in
        let smaller v = Float.bound min_v (v -. dv) max_v in
        let ghost_x = get_prop "ghost_x" frame_props in
        let ghost_y = get_prop "ghost_y" frame_props in
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
        enemy.entity.v.y <- -500. +. get_prop "random_dvy" frame_props;
        "idle"
      | DUNKED ->
        (* CLEANUP move to config *)
        let dunk_vy = 40. in
        enemy.entity.v <- { x = 0.; y = dunk_vy };
        set_prop enemy "dunked" 1.;
        "struck"
    in
    set_pose enemy pose_name
end

module Frog : M = struct
  module Action = Make_loggable (Frog_actions)

  type args = {
    frame_time : float;
    state : state;
    room : room;
    ghost_entity : entity;
  }

  let get_args state (game : game) : args =
    {
      frame_time = state.frame.time;
      state;
      room = game.room;
      ghost_entity = game.player.ghost.entity;
    }

  let choose_behavior (enemy : enemy) args =
    (* CLEANUP move to enemies.json *)
    (* TODO this is breaking by the frog getting stuck in "ascending"
       - happens when starting a "Read" interaction in the same room
    *)
    let dunk_duration = 2. in
    let cooldown_duration = 0.5 in
    let initial_x = get_prop "initial_x" enemy.props in
    let initial_y = get_prop "initial_y" enemy.props in
    (* most enemies set choose_behavior <- false on death, but not FROG *)
    if is_dead enemy then (
      let any_liquid_collisions () =
        let collisions =
          Entity.get_water_collisions args.room enemy.entity
          @ Entity.get_acid_collisions args.room enemy.entity
        in
        List.length collisions > 0
      in
      if get_bool_prop enemy "dunked" then
        if Action.still_doing enemy "dunked" ~duration:dunk_duration ~frame_time:args.frame_time
        then
          set_pose enemy "struck"
        else
          Entity.hide enemy.entity
      else if get_bool_prop enemy "homing" then (
        Action.start_and_log enemy HOMING args.frame_time
          [
            ("ghost_x", args.ghost_entity.dest.pos.x);
            ("ghost_y", args.ghost_entity.dest.pos.y);
            ("random_homing_dv", 5. +. Random.float 10.);
          ];
        let should_explode =
          enemy.floor_collision_this_frame
          || Collision.between_entities enemy.entity args.ghost_entity
          (* TODO maybe check slash collisions too *)
        in
        if should_explode then (
          args.state.camera.shake <- 1.;
          let projectile =
            let explosion_scale = 4. in
            let projectile_duration = TIME_LEFT { seconds = 1. } in
            spawn_projectile enemy ~projectile_texture_name:"explosion" ~scale:explosion_scale
              ~pogoable:true ~projectile_vx_opt:(Some 0.) ~x_alignment:CENTER ~direction:RIGHT
              ~damage:2 projectile_duration args.frame_time
          in
          (* this will only catch collisions on the first frame of the
             explosion, so a frog can move into an explosion without dying
          *)
          let check_frog_collision (target_enemy : enemy) =
            if target_enemy.id = FROG && target_enemy <> enemy then
              if Collision.between_entities projectile.entity target_enemy.entity then (
                let vx =
                  let v' = get_json_prop enemy "death_recoil_v" in
                  let explosion_center = (Entity.get_center projectile.entity).x in
                  let target_center = (Entity.get_center target_enemy.entity).x in
                  if explosion_center > target_center then
                    -.v'
                  else
                    v'
                in
                (* this only recoils the frog horizontally *)
                target_enemy.entity.v.x <- vx;
                target_enemy.health.current <- 0;
                set_prop target_enemy "death_recoil" 1.)
          in
          List.iter check_frog_collision args.room.enemies;
          args.room.loose_projectiles <- projectile :: args.room.loose_projectiles;
          Entity.hide enemy.entity)
        else if any_liquid_collisions () then
          Action.start_and_log enemy DUNKED args.frame_time [])
      else if get_bool_prop enemy "struck_cooldown" then (
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        if
          not
            (Action.still_doing enemy "struck_cooldown" ~duration:cooldown_duration
               ~frame_time:args.frame_time)
        then
          set_prop enemy "homing" 1.)
      else if get_bool_prop enemy "death_recoil" then
        if enemy.floor_collision_this_frame then (
          Action.log enemy "struck_cooldown" args.frame_time;
          set_prop enemy "struck_cooldown" 1.)
        else if any_liquid_collisions () then (* TODO maybe have a different texture for "dunked" *)
          Action.start_and_log enemy DUNKED args.frame_time []
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
        Action.start_and_log enemy ASCEND args.frame_time [ ("random_dvy", Random.float 150.) ]);
      if enemy.entity.v.y > 0. then (
        enemy.entity.v.y <- Float.bound 0. enemy.entity.v.y 120.;
        set_pose enemy "idle-descending")
      else
        set_pose enemy "idle")
end

module Electricity_actions = struct
  type t = SHOCK

  let to_string (action : t) : string =
    match action with
    | SHOCK -> "shock"

  let from_string (s : string) : t =
    match s with
    | "shock" -> SHOCK
    | _ -> failwithf "Electricity_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = StringMap.empty) (action : t) ~current_time =
    let pose_name =
      match action with
      | SHOCK ->
        let action_name = action |> to_string in
        (* ELECTRICITY should always have the "idle" pose - starting this action just adds the shock child sprite *)
        let shock_config = List.assoc action_name enemy.json.texture_configs in
        let projectile_duration =
          TIME_LEFT { seconds = (shock_config.count |> Int.to_float) *. shock_config.duration }
        in
        enemy.spawned_projectiles <-
          [
            spawn_projectile enemy ~projectile_texture_name:action_name ~projectile_vx_opt:(Some 0.)
              ~scale:1.1 ~pogoable:false ~x_alignment:CENTER ~direction:RIGHT projectile_duration
              current_time;
          ];
        "idle"
    in
    set_pose enemy pose_name
end

module Electricity : M = struct
  module Action = Make_loggable (Electricity_actions)

  type args = { frame_time : float }

  let get_args state game : args = { frame_time = state.frame.time }

  let choose_behavior enemy args =
    let last_shock = action_started_at enemy "shock" in
    let shock_duration =
      (* needs to be greater than shock config (count * duration) *)
      get_json_prop enemy "shock_duration"
    in
    if last_shock.at < args.frame_time -. shock_duration then
      Action.start_and_log enemy SHOCK args.frame_time []
end

module Fish_actions = struct
  type t = MOVE

  let to_string (action : t) : string =
    match action with
    | MOVE -> "move"

  let from_string (s : string) : t =
    match s with
    | "move" -> MOVE
    | _ -> failwithf "Electricity_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = StringMap.empty) (action : t) ~current_time =
    let pose_name =
      match action with
      | MOVE ->
        set_prop enemy "direction_change_dt" (get_prop "random_direction_change_dt" frame_props);
        let initial_x = get_prop "initial_x" enemy.props in
        let initial_y = get_prop "initial_y" enemy.props in
        let rand_x = get_prop "random_x" frame_props in
        let rand_y = get_prop "random_y" frame_props in
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
        "idle"
    in
    set_pose enemy pose_name
end

module Fish : M = struct
  module Action = Make_loggable (Fish_actions)

  type args = { frame_time : float }

  let get_args state game : args = { frame_time = state.frame.time }

  let choose_behavior enemy args =
    let last_direction_change = action_started_at enemy "move" in
    let direction_change_max_dt = get_json_prop enemy "direction_change_max_dt" in
    let direction_change_dt = get_prop ~default:(Some 1.) "direction_change_dt" enemy.props in
    if last_direction_change.at < args.frame_time -. direction_change_dt then (
      let max_v = get_json_prop enemy "max_v" in
      Action.start_and_log enemy MOVE args.frame_time
        [
          ("random_direction_change_dt", Random.float direction_change_max_dt);
          ("random_x", Random.float max_v);
          ("random_y", Random.float max_v);
        ])
end

let get_module (id : enemy_id) : (module M) =
  match id with
  | DUNCAN -> (module Duncan)
  | LOCKER_BOY -> (module Locker_boy)
  | FROG -> (module Frog)
  | ELECTRICITY -> (module Electricity)
  | FISH -> (module Fish)
  (* TODO *)
  (* | PENGUIN *)
  (* | WIRED_ELECTRICITY *)
  | _ -> failwithf "enemy %s not implemented yet" (Show.enemy_id id)

let choose_behavior (enemy : enemy) (state : state) (game : game) =
  let ghost_pos = game.player.ghost.entity.dest.pos in
  let (module M : M) = get_module enemy.id in
  M.choose_behavior enemy (M.get_args state game)

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

    let status =
      let b = Entity.is_on_screen entity in
      { choose_behavior = b; check_damage_collisions = b }
    in
    let json =
      match List.assoc_opt id enemy_configs with
      | None -> failwithf "could not find enemy json config for %s" (Show.enemy_id id)
      | Some j -> j
    in
    let json_props =
      let scaled =
        json.props |> List.map (fun (key, value) -> (key, value *. Config.window.scale))
      in
      scaled @ json.unscaled_props |> List.to_string_map
    in
    {
      id;
      status;
      kind;
      entity;
      damage = json.damage;
      health = { current = enemy_config.health; max = enemy_config.health };
      textures = textures |> List.to_string_map;
      history = EnemyActionMap.empty;
      last_performed = None;
      props =
        [ ("initial_x", entity.dest.pos.x); ("initial_y", entity.dest.pos.y) ] |> List.to_string_map;
      floor_collision_this_frame = false;
      spawned_projectiles = [];
      damage_sprites = [];
      on_killed;
      json_props;
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
