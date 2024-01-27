open Utils
open Types

let is_dead (enemy : enemy) : bool = enemy.health.current <= 0
let is_alive (enemy : enemy) : bool = not (is_dead enemy)

let parse_name context name : enemy_id =
  match name with
  | "BIRD" -> BIRD
  | "BAT" -> BAT
  | "DUNCAN" -> DUNCAN
  | "ELECTRICITY" -> ELECTRICITY
  | "FISH" -> FISH
  | "HIPPIE" -> HIPPIE
  | "FLYING_HIPPIE" -> FLYING_HIPPIE
  | "FLYING_HIPPIE_2" -> FLYING_HIPPIE_2
  | "FROG" -> FROG
  | "LOCKER_BOY" -> LOCKER_BOY
  | "MANICORN" -> MANICORN
  | "MANICORN_2" -> MANICORN_2
  | "MANICORN_3" -> MANICORN_3
  | "PENGUIN" -> PENGUIN
  | "WIRED_ELECTRICITY" -> WIRED_ELECTRICITY
  | _ -> failwithf "Enemy.parse_name: found unrecognized enemy name '%s' in %s" name context

let action_started_at (enemy : enemy) (action_name : string) : time =
  match Enemy_action.Map.find_opt (PERFORMED action_name) enemy.history with
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
  Enemy_action.Map.iter check_history enemy.history;
  { at = !damage }

let set_pose (enemy : enemy) (pose_name : string) : unit =
  match String.Map.find_opt pose_name enemy.textures with
  | None ->
    failwithf "could not find pose '%s' configured in enemies.json for enemy %s" pose_name
      (Show.enemy enemy)
  | Some texture ->
    Entity.update_sprite_texture
      ~scale:(enemy.json.scale *. Config.scale.enemy)
      enemy.entity texture

(* this can't take an enemy arg like the other functions because it uses current_props
   for "single-frame"/"temporary" props
*)
let get_prop ?(default = None) key props : float =
  match (String.Map.find_opt key props, default) with
  | None, None -> failwithf "could not find enemy prop '%s' for enemy" key
  | None, Some d -> d
  | Some v, _ -> v

let get_bool_prop (enemy : enemy) prop : bool =
  get_prop ~default:(Some 0.) prop enemy.status.props = 1.

let set_prop (enemy : enemy) key new_val =
  enemy.status.props <- String.Map.update key (fun _ -> Some new_val) enemy.status.props

let get_attr (enemy : enemy) key : float =
  match String.Map.find_opt key enemy.attrs with
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
    ?(gravity_multiplier = 0.)
    ?(collide_with_floors = false)
    ?(y_alignment = CENTER)
    ~(x_alignment : x_alignment)
    ~(direction : direction)
    (enemy : enemy)
    (despawn : projectile_duration)
    spawn_time : projectile =
  let projectile_texture =
    match String.Map.find_opt projectile_texture_name enemy.textures with
    | Some t ->
      Sprite.reset_texture t;
      t
    | None ->
      failwithf "could not find projectile '%s' for %s" projectile_texture_name
        (Show.enemy_name enemy)
  in
  let src = get_src projectile_texture in
  let w, h = (src.w *. Config.scale.enemy *. scale, src.h *. Config.scale.enemy *. scale) in
  let vx =
    match projectile_vx_opt with
    | None -> get_attr enemy "projectile_vx"
    | Some vx' -> vx'
  in
  let spawn_pos = Entity.get_child_pos enemy.entity (x_alignment, y_alignment) w h in
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
      ~scale:(scale *. Config.scale.enemy) ~v:{ x = vx'; y = 0. } ~facing_right:(vx' > 0.)
      ~gravity_multiplier ~collision:(Some DEST) projectile_texture dest
  in
  { entity; despawn; spawned = { at = spawn_time }; pogoable; damage; collide_with_floors }

let took_damage_at (enemy : enemy) (damage_kind : damage_kind) =
  match Enemy_action.Map.find_opt (TOOK_DAMAGE damage_kind : enemy_action) enemy.history with
  | None -> Zero.time ()
  | Some time -> time

let maybe_take_damage
    ?(collision_direction : direction option)
    (state : state)
    (enemy : enemy)
    (ghost_action_started : time)
    (damage_kind : damage_kind)
    (damage : int)
    (collision : collision) : bool =
  let kill_enemy () =
    enemy.spawned_projectiles <- [];
    enemy.status.active <- false;
    enemy.status.check_damage_collisions <- false;
    match enemy.id with
    | LOCKER_BOY -> Entity.hide enemy.entity
    | FROG ->
      let v =
        let v' = get_attr enemy "death_recoil_v" in
        match collision.collided_from with
        | UP -> { x = 0.; y = -1. *. v' }
        | DOWN -> { x = 0.; y = v' }
        | RIGHT -> { x = v'; y = 0. }
        | LEFT -> { x = -1. *. v'; y = 0. }
      in
      enemy.entity.v <- v;
      (* this is just undoing the changes above, because the enemy isn't quite dead yet *)
      enemy.status.active <- true;
      set_prop enemy "death_recoil" 1.
    | DUNCAN
    | WIRED_ELECTRICITY
    | ELECTRICITY ->
      ()
    | PENGUIN
    | HIPPIE
    | FLYING_HIPPIE
    | FLYING_HIPPIE_2
    | BIRD
    | BAT
    | MANICORN
    | MANICORN_2
    | MANICORN_3
    | FISH ->
      set_pose enemy "dead";
      enemy.entity.v.x <- 0.;
      enemy.entity.x_recoil <-
        Some
          {
            speed = Config.enemy.death_recoil_vx;
            time_left = { seconds = Config.enemy.death_recoil_time };
            reset_v = true;
          };
      enemy.entity.v.y <- Config.enemy.death_vy
  in

  let hit =
    if List.mem damage_kind Config.enemy.multi_hit_damage_kinds then
      state.frame.time > (took_damage_at enemy damage_kind).at +. Config.enemy.multi_hit_cooldown
    else
      ghost_action_started > took_damage_at enemy damage_kind
  in
  if hit then (
    enemy.history <-
      Enemy_action.Map.update (TOOK_DAMAGE damage_kind)
        (fun _ -> Some { at = state.frame.time })
        enemy.history;
    enemy.health.current <- enemy.health.current - damage;
    let damage_texture = state.global.textures.damage in
    let texture_w, texture_h = get_scaled_texture_size Config.scale.damage damage_texture in
    let new_damage_sprite =
      let pos = align (CENTER, CENTER) enemy.entity.dest texture_w texture_h in
      Sprite.spawn_particle
        (fmt "damage %s" (Show.enemy_name enemy))
        damage_texture
        { pos; w = texture_w; h = texture_h }
        state.frame.time
    in
    enemy.damage_sprites <- new_damage_sprite :: enemy.damage_sprites;
    if is_dead enemy then
      kill_enemy ()
    else (
      match enemy.kind with
      | ENEMY ->
        let direction =
          match collision_direction with
          | Some d -> d
          | None -> collision.collided_from
        in
        Entity.recoil enemy.entity direction
      | BOSS
      | MULTI_BOSS ->
        ());
    true)
  else
    false

let get_boss_area boss_name game =
  match game.room.boss_area with
  | None -> failwithf "missing boss-area for %s" boss_name
  | Some rect -> rect

let chase_to enemy (target : vector) =
  let dv = get_attr enemy "chase_dv" in
  let new_vx =
    if target.x > enemy.entity.dest.pos.x then
      enemy.entity.v.x +. dv
    else
      enemy.entity.v.x -. dv
  in
  let new_vy =
    if target.y > enemy.entity.dest.pos.y then
      enemy.entity.v.y +. dv
    else
      enemy.entity.v.y -. dv
  in
  let max_v = get_attr enemy "max_chase_v" in
  (Float.bound (-.max_v) new_vx max_v, Float.bound (-.max_v) new_vy max_v)

(* turns when reaching the edge of a floor, or when bumping into a wall *)
let should_turn (enemy : enemy) frame_time =
  let turned_recently =
    (* this prevents the enemy from turning every frame when landing on an edge, but it sometimes
       causes the enemy to walk over the edge *)
    frame_time -. (action_started_at enemy "turn").at < 0.5
  in
  if turned_recently then
    false
  else (
    let at_edge floor =
      (* TODO this doesn't work for floor seams *)
      enemy.entity.dest.pos.x < floor.pos.x
      || enemy.entity.dest.pos.x +. enemy.entity.dest.w > floor.pos.x +. floor.w
    in
    match enemy.floor_collisions_this_frame with
    | [] -> (
      match enemy.entity.current_floor with
      | Some (floor, _) when at_edge floor -> true
      | _ -> false)
    | [ collision ] ->
      if collision.collided_from = UP then
        false
      else
        true
    | multiple_collisions -> true)

let face_ghost (enemy : enemy) ~ghost_pos =
  enemy.entity.sprite.facing_right <- ghost_pos.x > rect_center_x enemy.entity.dest

let still_airborne enemy ~frame_time ~action_time =
  (* check frame_time - action_time so the action doesn't cancel on the first frame *)
  frame_time -. action_time.at < 0.1 || List.length enemy.floor_collisions_this_frame = 0

module type Enemy_actions = sig
  type t

  val to_string : t -> string
  val from_string : string -> t
  val set : enemy -> ?frame_props:float String.Map.t -> t -> frame_time:float -> unit
end

module type Loggable = sig
  include Enemy_actions

  val log : enemy -> string -> float -> unit
  val start_and_log : enemy -> frame_time:float -> ?frame_props:(string * float) list -> t -> unit
  val still_doing : enemy -> string -> duration:float -> frame_time:float -> bool

  val handle_charging :
    enemy ->
    charge_action:t ->
    action:t ->
    ghost_pos:vector ->
    action_time:time ->
    frame_time:float ->
    unit

  val maybe_continue :
    enemy -> continue_action:t -> stop_action:t -> frame_time:float -> bool -> unit

  val maybe_aggro :
    enemy -> ghost_pos:vector -> frame_time:float -> t option -> (unit -> unit) -> unit

  val handle_drifting : enemy -> t -> ghost_pos:vector -> frame_time:float -> unit
end

module Make_loggable (Actions : Enemy_actions) : Loggable with type t = Actions.t = struct
  include Actions

  let log (enemy : enemy) action_name value =
    enemy.history <-
      Enemy_action.Map.update (PERFORMED action_name) (fun _ -> Some { at = value }) enemy.history

  let start_and_log (enemy : enemy) ~frame_time ?(frame_props = []) (action : Actions.t) : unit =
    let action_name = action |> Actions.to_string in
    enemy.last_performed <- Some (action_name, { at = frame_time });
    log enemy action_name frame_time;
    Actions.set enemy action ~frame_time ~frame_props:(frame_props |> List.to_string_map)

  let still_doing (enemy : enemy) (action_name : string) ~duration ~frame_time : bool =
    let started = action_started_at enemy action_name in
    frame_time -. started.at < duration

  let handle_charging (enemy : enemy) ~charge_action ~action ~ghost_pos ~action_time ~frame_time =
    face_ghost enemy ~ghost_pos;
    if frame_time -. action_time.at < get_attr enemy "charge_duration" then
      Actions.set enemy charge_action ~frame_time
    else
      start_and_log enemy action ~frame_time

  let maybe_continue (enemy : enemy) ~continue_action ~stop_action ~frame_time still_doing_action =
    if still_doing_action then
      set enemy continue_action ~frame_time
    else
      start_and_log enemy stop_action ~frame_time

  (* required attrs: "aggro_distance" *)
  let maybe_aggro (enemy : enemy) ~ghost_pos ~frame_time idle_action aggro =
    let distance = get_distance enemy.entity.dest.pos ghost_pos in
    if enemy.health.current < enemy.health.max || distance < get_attr enemy "aggro_distance" then
      aggro ()
    else (
      match idle_action with
      | Some action -> start_and_log enemy action ~frame_time
      | None -> ())

  (* required attrs: "direction_change_max_dt", "aggro_distance", "max_v"
     required props: "direction_change_dt"
     required actions: "change_direction"
  *)
  let handle_drifting enemy change_direction_action ~ghost_pos ~frame_time =
    maybe_aggro enemy ~ghost_pos ~frame_time None (fun () -> set_prop enemy "is_chasing" 1.);
    let last_direction_change = action_started_at enemy "change_direction" in
    let direction_change_max_dt = get_attr enemy "direction_change_max_dt" in
    let direction_change_dt =
      get_prop ~default:(Some 1.) "direction_change_dt" enemy.status.props
    in
    if last_direction_change.at < frame_time -. direction_change_dt then (
      set_prop enemy "direction_change_dt" (Random.float direction_change_max_dt);
      let max_v = get_attr enemy "max_v" in
      start_and_log enemy change_direction_action ~frame_time
        ~frame_props:[ ("random_vx", Random.float max_v); ("random_vy", Random.float max_v) ])
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

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let pose_name =
      match action with
      | WALK ->
        let walk_vx = get_attr enemy "walk_vx" in
        if enemy.entity.sprite.facing_right then
          enemy.entity.v.x <- walk_vx
        else
          enemy.entity.v.x <- -1. *. walk_vx;
        "walking"
      | LANDED ->
        let range = get_attr enemy "projectile_range" in
        let projectile_duration =
          X_BOUNDS (enemy.entity.dest.pos.x -. range, enemy.entity.dest.pos.x +. range)
        in
        enemy.entity.v.x <- 0.;
        enemy.entity.sprite.facing_right <-
          (* TODO might help to use a prefix like is_ or bool_ for these props that are used as booleans *)
          get_frame_prop "facing_right" = 1.;
        let spawn x_alignment direction =
          spawn_projectile enemy ~x_alignment ~y_alignment:BOTTOM_INSIDE ~direction
            projectile_duration frame_time
        in
        enemy.spawned_projectiles <-
          [ spawn LEFT_INSIDE RIGHT; spawn RIGHT_INSIDE LEFT ] @ enemy.spawned_projectiles;
        "idle"
      | JUMP ->
        enemy.entity.v.x <- get_frame_prop ~default:(Some enemy.entity.v.x) "random_jump_vx";
        enemy.entity.v.y <- get_attr enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jumping"
    in
    set_pose enemy pose_name
end

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
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
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
        Action.start_and_log enemy LANDED ~frame_time
          ~frame_props:
            [ ("facing_right", if args.ghost_pos.x > enemy.entity.dest.pos.x then 1. else 0.) ]
      else if frame_time -. landed.at > get_attr enemy "jump_wait_time" then (
        let target_x = Random.in_rect_x args.boss_area in
        let jump_vx =
          let dx = target_x -. enemy.entity.dest.pos.x in
          dx /. airtime
        in
        Action.start_and_log enemy JUMP ~frame_time ~frame_props:[ ("random_jump_vx", jump_vx) ])
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

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop prop_name = get_prop prop_name frame_props in
    let pose_name : string =
      match action with
      | VANISH ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "vanish"
      | DASH ->
        let x =
          if get_frame_prop "random_facing_right" = 1. then (
            enemy.entity.sprite.facing_right <- true;
            enemy.entity.v.x <- get_attr enemy "dash_vx";
            get_frame_prop "boss_area_left")
          else (
            enemy.entity.sprite.facing_right <- false;
            enemy.entity.v.x <- -1. *. get_attr enemy "dash_vx";
            get_frame_prop "boss_area_right")
        in
        Entity.unhide enemy.entity;
        Entity.unfreeze enemy.entity;
        enemy.entity.dest.pos.x <- x;
        enemy.entity.dest.pos.y <- get_frame_prop "boss_area_bottom";
        "dash"
      | DIVE ->
        let x = get_frame_prop "random_dive_x" in
        Entity.unhide enemy.entity;
        Entity.unfreeze enemy.entity;
        enemy.entity.v.y <- get_attr enemy "dive_vy";
        enemy.entity.dest.pos.x <- x;
        enemy.entity.dest.pos.y <- get_attr enemy "dive_y";
        "dive"
      | WALL_PERCH ->
        let (x, direction, x_alignment) : float * direction * x_alignment =
          if get_frame_prop "random_facing_right" = 1. then (
            enemy.entity.sprite.facing_right <- true;
            (get_frame_prop "boss_area_left", RIGHT, RIGHT_INSIDE))
          else (
            enemy.entity.sprite.facing_right <- false;
            (get_frame_prop "boss_area_right", LEFT, LEFT_INSIDE))
        in
        let y = get_frame_prop "random_wall_perch_y" in
        Entity.unhide enemy.entity;
        Entity.unfreeze enemy.entity;
        enemy.entity.dest.pos.x <- x;
        enemy.entity.dest.pos.y <- y;
        let projectile_duration =
          X_BOUNDS
            (get_frame_prop "boss_area_left" -. 100., get_frame_prop "boss_area_right" +. 100.)
        in
        enemy.spawned_projectiles <-
          [
            spawn_projectile enemy ~scale:0.5 ~pogoable:true ~x_alignment ~direction
              projectile_duration frame_time;
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
    { frame_time = state.frame.time; boss_area = get_boss_area "LOCKER_BOY" game }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let continue_action (action : Action.t) (performed : time) =
      let left_boundary = args.boss_area.pos.x in
      let right_boundary = args.boss_area.pos.x +. args.boss_area.w in
      match action with
      | VANISH -> Action.set enemy action ~frame_time
      | DASH ->
        let check_right () =
          if enemy.entity.dest.pos.x > right_boundary then
            set_prop enemy "should_vanish" 1.
        in
        let check_left () =
          if enemy.entity.dest.pos.x < left_boundary then
            set_prop enemy "should_vanish" 1.
        in
        if enemy.entity.sprite.facing_right then check_right () else check_left ()
      | WALL_PERCH ->
        let current_duration = args.frame_time -. performed.at in
        enemy.entity.v.y <- 0.;
        if current_duration > 2. then
          set_prop enemy "should_vanish" 1.
      | DIVE ->
        (* TODO this vanishes immediately, probably should be a delay *)
        if Option.is_some enemy.entity.current_floor then
          set_prop enemy "should_vanish" 1.
    in
    let vanished = action_started_at enemy "vanish" in
    let unvanished = action_started_at enemy "unvanish" in
    let vanish_duration = animation_loop_duration (String.Map.find "vanish" enemy.textures) in
    let still_vanishing = Action.still_doing enemy "vanish" ~duration:vanish_duration ~frame_time in
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
        Action.start_and_log enemy WALL_PERCH ~frame_time
          ~frame_props:
            [
              ("random_facing_right", if Random.bool () then 1. else 0.);
              ("random_wall_perch_y", Random.in_rect_y args.boss_area);
              ("boss_area_left", args.boss_area.pos.x);
              ("boss_area_right", args.boss_area.pos.x +. args.boss_area.w);
            ]
      | `DIVE ->
        Action.start_and_log enemy DIVE ~frame_time
          ~frame_props:[ ("random_dive_x", Random.in_rect_x args.boss_area) ]
      | `DASH ->
        Action.start_and_log enemy DASH ~frame_time
          ~frame_props:
            [
              ("random_facing_right", if Random.bool () then 1. else 0.);
              ("boss_area_left", args.boss_area.pos.x);
              ("boss_area_right", args.boss_area.pos.x +. args.boss_area.w);
              ("boss_area_bottom", args.boss_area.pos.y +. args.boss_area.h);
            ]
    in
    if still_vanishing then
      ()
    else if should_unvanish () then (
      Action.log enemy "unvanish" args.frame_time;
      unvanish ())
    else if should_vanish () then (
      set_prop enemy "should_vanish" 0.;
      Action.start_and_log enemy VANISH ~frame_time)
    else (
      match enemy.last_performed with
      | None -> ()
      | Some (action_name, action_time) ->
        continue_action (Action.from_string action_name) action_time)
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

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop s = get_prop s frame_props in
    let pose_name =
      match action with
      | HOMING ->
        let min_v, max_v =
          let v' = get_attr enemy "homing_v" in
          (-1. *. v', v')
        in
        let dv = get_frame_prop "random_homing_dv" in
        let larger v = Float.bound min_v (v +. dv) max_v in
        let smaller v = Float.bound min_v (v -. dv) max_v in
        let ghost_x = get_frame_prop "ghost_x" in
        let ghost_y = get_frame_prop "ghost_y" in
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
        enemy.entity.v.y <- get_frame_prop "random_dvy";
        "idle"
      | DUNKED ->
        enemy.entity.v <- { x = 0.; y = get_attr enemy "dunk_vy" };
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
    let frame_time = args.frame_time in
    let dunk_duration = get_attr enemy "dunk_duration" in
    let cooldown_duration = get_attr enemy "cooldown_duration" in
    (* most enemies set `choose_behavior <- false` on death, but not FROG *)
    if is_dead enemy then (
      let any_liquid_collisions () =
        let collisions =
          Entity.get_water_collisions args.room enemy.entity
          @ Entity.get_acid_collisions args.room enemy.entity
        in
        List.length collisions > 0
      in
      if get_bool_prop enemy "dunked" then
        if Action.still_doing enemy "dunked" ~duration:dunk_duration ~frame_time then
          set_pose enemy "struck"
        else
          Entity.hide enemy.entity
      else if get_bool_prop enemy "homing" then (
        Action.start_and_log enemy HOMING ~frame_time
          ~frame_props:
            [
              ("ghost_x", args.ghost_entity.dest.pos.x);
              ("ghost_y", args.ghost_entity.dest.pos.y);
              ("random_homing_dv", Random.float_between 5. 15.);
            ];
        let should_explode =
          List.length enemy.floor_collisions_this_frame > 0
          || Collision.between_entities enemy.entity args.ghost_entity
          (* TODO maybe check slash collisions too *)
        in
        if should_explode then (
          args.state.camera.shake <- 1.;
          let projectile =
            let explosion_scale = 4. in
            let projectile_duration = TIME_LEFT { seconds = 0.8 } in
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
                  let v' = get_attr enemy "death_recoil_v" in
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
          Action.start_and_log enemy DUNKED ~frame_time)
      else if get_bool_prop enemy "struck_cooldown" then (
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        if not (Action.still_doing enemy "struck_cooldown" ~duration:cooldown_duration ~frame_time)
        then
          set_prop enemy "homing" 1.)
      else if get_bool_prop enemy "death_recoil" then
        if List.length enemy.floor_collisions_this_frame > 0 then (
          Action.log enemy "struck_cooldown" args.frame_time;
          set_prop enemy "struck_cooldown" 1.)
        else if any_liquid_collisions () then (* TODO maybe have a different texture for "dunked" *)
          Action.start_and_log enemy DUNKED ~frame_time
        else
          set_pose enemy "struck")
    else (
      if enemy.entity.dest.pos.y > enemy.initial_pos.y then (
        (* TODO duplicated - maybe add turn_towards_origin fn if this is shared often enough *)
        let rand_vx = Random.float (get_attr enemy "max_rand_vx") in
        let new_vx =
          if enemy.initial_pos.x > enemy.entity.dest.pos.x then (
            enemy.entity.sprite.facing_right <- true;
            rand_vx)
          else (
            enemy.entity.sprite.facing_right <- false;
            -.rand_vx)
        in
        enemy.entity.v.x <- new_vx;
        Action.start_and_log enemy ASCEND ~frame_time
          ~frame_props:
            [
              ( "random_dvy",
                Random.float_between
                  (-.get_attr enemy "max_ascend_vy")
                  (-.get_attr enemy "min_ascend_vy") );
            ]);
      if enemy.entity.v.y > 0. then (
        enemy.entity.v.y <- Float.bound 0. enemy.entity.v.y (get_attr enemy "float_vy");
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

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let pose_name =
      match action with
      | SHOCK ->
        let action_name = action |> to_string in
        let shock_config = List.assoc action_name enemy.json.texture_configs in
        let projectile_duration =
          TIME_LEFT { seconds = (shock_config.count |> Int.to_float) *. shock_config.duration }
        in
        enemy.spawned_projectiles <-
          [
            spawn_projectile enemy ~projectile_texture_name:action_name ~projectile_vx_opt:(Some 0.)
              ~scale:1.6 ~pogoable:false ~x_alignment:CENTER ~direction:RIGHT projectile_duration
              frame_time;
          ];
        (* ELECTRICITY should always have the "idle" pose - starting this action just adds
           the shock child sprite *)
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
      get_attr enemy "shock_duration"
    in
    if last_shock.at < args.frame_time -. shock_duration then
      Action.start_and_log enemy SHOCK ~frame_time:args.frame_time
end

module Fish_actions = struct
  type t = CHANGE_DIRECTION

  let to_string (action : t) : string =
    match action with
    | CHANGE_DIRECTION -> "change_direction"

  let from_string (s : string) : t =
    match s with
    | "change_direction" -> CHANGE_DIRECTION
    | _ -> failwithf "Fish_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let pose_name =
      match action with
      | CHANGE_DIRECTION ->
        let rand_vx = get_prop "random_vx" frame_props in
        let rand_vy = get_prop "random_vy" frame_props in
        let new_vx =
          if enemy.initial_pos.x > enemy.entity.dest.pos.x then (
            enemy.entity.sprite.facing_right <- true;
            rand_vx)
          else (
            enemy.entity.sprite.facing_right <- false;
            -1. *. rand_vx)
        in
        let new_vy =
          if enemy.initial_pos.y > enemy.entity.dest.pos.y then
            rand_vy
          else
            -1. *. rand_vy
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
    let last_direction_change = action_started_at enemy "change_direction" in
    let direction_change_max_dt = get_attr enemy "direction_change_max_dt" in
    let direction_change_dt =
      get_prop ~default:(Some 1.) "direction_change_dt" enemy.status.props
    in
    if last_direction_change.at < args.frame_time -. direction_change_dt then (
      set_prop enemy "direction_change_dt" (Random.float direction_change_max_dt);
      let max_v = get_attr enemy "max_v" in
      Action.start_and_log enemy CHANGE_DIRECTION ~frame_time:args.frame_time
        ~frame_props:[ ("random_vx", Random.float max_v); ("random_vy", Random.float max_v) ])
end

module Penguin_actions = struct
  type t =
    | WALK
    | TURN

  let to_string (action : t) : string =
    match action with
    | WALK -> "walk"
    | TURN -> "turn"

  let from_string (s : string) : t =
    match s with
    | "walk" -> WALK
    | "turn" -> TURN
    | _ -> failwithf "Penguin_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let move_enemy () =
      let walk_vx = get_attr enemy "walk_vx" in
      if enemy.entity.sprite.facing_right then
        enemy.entity.v.x <- walk_vx
      else
        enemy.entity.v.x <- -1. *. walk_vx
    in
    let pose_name =
      match action with
      | WALK ->
        move_enemy ();
        (* PENGUIN doesn't have a stationary idle state, so this pose is really like "walking" *)
        "idle"
      | TURN ->
        enemy.entity.sprite.facing_right <- not enemy.entity.sprite.facing_right;
        move_enemy ();
        "idle"
    in
    set_pose enemy pose_name
end

module Penguin : M = struct
  module Action = Make_loggable (Penguin_actions)

  type args = {
    frame_time : float;
    room : room;
  }

  let get_args state (game : game) : args = { frame_time = state.frame.time; room = game.room }

  let choose_behavior (enemy : enemy) args =
    let (action : Action.t) = if should_turn enemy args.frame_time then TURN else WALK in
    Action.start_and_log enemy action ~frame_time:args.frame_time
end

module Flying_hippie_actions = struct
  type t =
    | CHANGE_DIRECTION
    | CHASE
    | SHOOT_PROJECTILE

  let to_string (action : t) : string =
    match action with
    | CHANGE_DIRECTION -> "change_direction"
    | CHASE -> "chase"
    | SHOOT_PROJECTILE -> "shoot_projectile"

  let from_string (s : string) : t =
    match s with
    | "change_direction" -> CHANGE_DIRECTION
    | "chase" -> CHASE
    | _ -> failwithf "Flying_hippie_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let pose_name =
      match action with
      | CHANGE_DIRECTION ->
        let rand_vx = get_prop "random_vx" frame_props in
        let rand_vy = get_prop "random_vy" frame_props in
        let new_vx =
          if enemy.initial_pos.x > enemy.entity.dest.pos.x then (
            enemy.entity.sprite.facing_right <- true;
            rand_vx)
          else (
            enemy.entity.sprite.facing_right <- false;
            -1. *. rand_vx)
        in
        let new_vy =
          if enemy.initial_pos.y > enemy.entity.dest.pos.y then
            rand_vy
          else
            -1. *. rand_vy
        in
        enemy.entity.v.x <- new_vx;
        enemy.entity.v.y <- new_vy;
        "idle"
      | CHASE ->
        let new_vx = get_prop "new_vx" frame_props in
        let new_vy = get_prop "new_vy" frame_props in
        let new_facing_right = get_prop "new_facing_right" frame_props in
        enemy.entity.v.x <- new_vx;
        enemy.entity.v.y <- new_vy;
        enemy.entity.sprite.facing_right <- new_facing_right = 1.;
        "chasing"
      | SHOOT_PROJECTILE ->
        let projectile_duration = UNTIL_FLOOR_COLLISION in
        let direction : direction =
          if get_prop "projectile_direction_right" frame_props = 1. then
            RIGHT
          else
            LEFT
        in
        let projectile scale gravity =
          spawn_projectile enemy ~scale ~x_alignment:CENTER ~direction
            ~gravity_multiplier:(get_prop "gravity_multiplier" frame_props *. gravity)
            ~collide_with_floors:true projectile_duration frame_time
        in
        let new_projectiles =
          match enemy.level with
          | 1 -> [ projectile 0.7 1. ]
          | 2 -> [ projectile 1. 1.5; projectile 1. 1.; projectile 1. 0.5 ]
          | _ -> failwithf "invalid FLYING_HIPPIE level %d" enemy.level
        in
        enemy.spawned_projectiles <- new_projectiles @ enemy.spawned_projectiles;
        "chasing"
    in
    set_pose enemy pose_name
end

module Flying_hippie : M = struct
  module Action = Make_loggable (Flying_hippie_actions)

  type args = {
    frame_time : float;
    ghost_pos : vector;
  }

  let get_args state (game : game) : args =
    { frame_time = state.frame.time; ghost_pos = get_rect_center game.player.ghost.entity.dest }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let handle_chasing () =
      let offset = get_attr enemy "target_offset" in
      let target_pos_x, new_facing_right =
        if args.ghost_pos.x > enemy.entity.dest.pos.x then
          (args.ghost_pos.x -. offset, 1.)
        else
          (args.ghost_pos.x +. offset, 0.)
      in
      let target_pos_y = args.ghost_pos.y -. offset in
      let new_vx, new_vy = chase_to enemy { x = target_pos_x; y = target_pos_y } in
      let shot = action_started_at enemy "shoot_projectile" in
      let cooldown = get_attr enemy "projectile_cooldown" in
      if args.frame_time > shot.at +. cooldown then
        Action.start_and_log enemy SHOOT_PROJECTILE ~frame_time
          ~frame_props:
            [
              ("projectile_direction_right", new_facing_right);
              ("gravity_multiplier", Random.float_between 0.02 0.25);
            ]
      else
        Action.set enemy CHASE ~frame_time
          ~frame_props:
            ([ ("new_vx", new_vx); ("new_vy", new_vy); ("new_facing_right", new_facing_right) ]
            |> List.to_string_map)
    in
    if get_bool_prop enemy "is_chasing" then
      handle_chasing ()
    else
      Action.handle_drifting enemy CHANGE_DIRECTION ~ghost_pos:args.ghost_pos ~frame_time
end

module Bird_actions = struct
  type t =
    | CHANGE_DIRECTION
    | CHASE

  let to_string (action : t) : string =
    match action with
    | CHANGE_DIRECTION -> "change_direction"
    | CHASE -> "chase"

  let from_string (s : string) : t =
    match s with
    | "change_direction" -> CHANGE_DIRECTION
    | "chase" -> CHASE
    | _ -> failwithf "Bird_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let pose_name =
      match action with
      | CHANGE_DIRECTION ->
        let rand_vx = get_prop "random_vx" frame_props in
        let rand_vy = get_prop "random_vy" frame_props in
        let new_vx =
          if enemy.initial_pos.x > enemy.entity.dest.pos.x then (
            enemy.entity.sprite.facing_right <- true;
            rand_vx)
          else (
            enemy.entity.sprite.facing_right <- false;
            -1. *. rand_vx)
        in
        let new_vy =
          if enemy.initial_pos.y > enemy.entity.dest.pos.y then
            rand_vy
          else
            -1. *. rand_vy
        in
        enemy.entity.v.x <- new_vx;
        enemy.entity.v.y <- new_vy;
        "idle"
      | CHASE ->
        let new_vx = get_prop "new_vx" frame_props in
        let new_vy = get_prop "new_vy" frame_props in
        enemy.entity.v.x <- new_vx;
        enemy.entity.v.y <- new_vy;
        enemy.entity.sprite.facing_right <- new_vx > 0.;
        "chasing"
    in
    set_pose enemy pose_name
end

module Bird : M = struct
  module Action = Make_loggable (Bird_actions)

  type args = {
    frame_time : float;
    ghost_pos : vector;
  }

  let get_args state (game : game) : args =
    { frame_time = state.frame.time; ghost_pos = get_rect_center game.player.ghost.entity.dest }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let handle_chasing () =
      let new_vx, new_vy = chase_to enemy args.ghost_pos in
      Action.set enemy CHASE ~frame_time
        ~frame_props:([ ("new_vx", new_vx); ("new_vy", new_vy) ] |> List.to_string_map)
    in
    if get_bool_prop enemy "is_chasing" then
      handle_chasing ()
    else
      Action.handle_drifting enemy CHANGE_DIRECTION ~ghost_pos:args.ghost_pos ~frame_time
end

module Bat_actions = struct
  type t =
    | MOVE
    | CHANGE_DIRECTION

  let to_string (action : t) : string =
    match action with
    | MOVE -> "move"
    | CHANGE_DIRECTION -> "change_direction"

  let from_string (s : string) : t =
    match s with
    | "move" -> MOVE
    | "change_direction" -> CHANGE_DIRECTION
    | _ -> failwithf "Bird_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    (match action with
    | MOVE -> ()
    | CHANGE_DIRECTION ->
      enemy.entity.v.x <- get_prop "random_vx" frame_props;
      enemy.entity.v.y <- get_prop "random_vy" frame_props);
    set_pose enemy "idle"
end

module Bat : M = struct
  module Action = Make_loggable (Bat_actions)

  type args = { frame_time : float }

  let get_args state (game : game) : args = { frame_time = state.frame.time }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    (* check if v.x/y = 0. to change direction after recoiling from nail hit *)
    if
      enemy.entity.v.x = 0.
      || enemy.entity.v.y = 0.
      || List.length enemy.floor_collisions_this_frame > 0
    then (
      let v = Random.float_between (get_attr enemy "min_v") (get_attr enemy "max_v") in
      let angle = Random.float 360. in
      let vx = sin angle *. v in
      let vy = cos angle *. v in
      Action.set enemy CHANGE_DIRECTION ~frame_time
        ~frame_props:([ ("random_vx", vx); ("random_vy", vy) ] |> List.to_string_map))
    else
      Action.set enemy MOVE ~frame_time
end

module Manicorn_actions = struct
  type t =
    | STANDING
    | WALK
    | CHARGE_PUNCH
    | PUNCH
    | CHARGE_KICK
    | KICK
    | CHARGE_DASH
    | DASH

  let to_string (action : t) : string =
    match action with
    | STANDING -> "standing"
    | WALK -> "walking"
    | CHARGE_PUNCH -> "charge-punch"
    | PUNCH -> "punch"
    | CHARGE_KICK -> "charge-kick"
    | KICK -> "kick"
    | CHARGE_DASH -> "charge-dash"
    | DASH -> "dash"

  let from_string (s : string) : t =
    match s with
    | "standing" -> STANDING
    | "walking" -> WALK
    | "charge-punch" -> CHARGE_PUNCH
    | "punch" -> PUNCH
    | "charge-kick" -> CHARGE_KICK
    | "kick" -> KICK
    | "charge-dash" -> CHARGE_DASH
    | "dash" -> DASH
    | _ -> failwithf "Manicorn_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let pose_name =
      let forward attr =
        let vx = get_attr enemy attr in
        if enemy.entity.sprite.facing_right then vx else -.vx
      in
      match action with
      | STANDING ->
        enemy.entity.v.x <- 0.;
        "idle"
      | WALK ->
        enemy.entity.v.x <- forward "walk_vx";
        "walking"
      | CHARGE_PUNCH ->
        enemy.entity.v.x <- 0.;
        "charge-punch"
      | PUNCH ->
        enemy.entity.v.x <- forward "punch_vx";
        "punch"
      | CHARGE_KICK ->
        enemy.entity.v.x <- 0.;
        "charge-kick"
      | KICK ->
        enemy.entity.v.x <- forward "kick_vx";
        if Option.is_some enemy.entity.current_floor then
          enemy.entity.v.y <- get_attr enemy "jump_vy";
        "kick"
      | CHARGE_DASH ->
        enemy.entity.v.x <- 0.;
        "charge-dash"
      | DASH ->
        enemy.entity.v.x <- forward "dash_vx";
        "dash"
    in
    set_pose enemy pose_name
end

module Manicorn : M = struct
  module Action = Make_loggable (Manicorn_actions)

  type args = {
    frame_time : float;
    ghost_pos : vector;
  }

  let get_args state (game : game) : args =
    { frame_time = state.frame.time; ghost_pos = get_rect_center game.player.ghost.entity.dest }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in
    let maybe_start_action () =
      Action.maybe_aggro enemy ~ghost_pos ~frame_time (Some STANDING) (fun () ->
          match List.sample [ `PUNCH; `KICK; `DASH ] with
          | `PUNCH -> Action.start_and_log enemy CHARGE_PUNCH ~frame_time
          | `KICK -> Action.start_and_log enemy CHARGE_KICK ~frame_time
          | `DASH -> Action.start_and_log enemy CHARGE_DASH ~frame_time)
    in
    let handle_charge action_time charge_action action =
      Action.handle_charging enemy ~ghost_pos ~action_time ~charge_action ~action ~frame_time
    in
    match enemy.last_performed with
    | Some ("charge-punch", action_time) -> handle_charge action_time CHARGE_PUNCH PUNCH
    | Some ("punch", action_time) ->
      let punch_duration = animation_loop_duration (String.Map.find "punch" enemy.textures) in
      let still_punching = Action.still_doing enemy "punch" ~duration:punch_duration ~frame_time in
      Action.maybe_continue enemy ~continue_action:PUNCH ~stop_action:WALK ~frame_time
        still_punching
    | Some ("charge-kick", action_time) -> handle_charge action_time CHARGE_KICK KICK
    | Some ("kick", action_time) ->
      let still_kicking = still_airborne enemy ~frame_time ~action_time in
      Action.maybe_continue enemy ~continue_action:KICK ~stop_action:WALK ~frame_time still_kicking
    | Some ("charge-dash", action_time) -> handle_charge action_time CHARGE_DASH DASH
    | Some ("dash", action_time) ->
      let still_dashing =
        Action.still_doing enemy "dash" ~duration:(get_attr enemy "dash_duration") ~frame_time
      in
      Action.maybe_continue enemy ~continue_action:DASH ~stop_action:WALK ~frame_time still_dashing
    | Some ("walking", action_time) ->
      if args.frame_time -. action_time.at > get_attr enemy "walk_duration" then
        maybe_start_action ()
      else (
        face_ghost enemy ~ghost_pos;
        Action.set enemy WALK ~frame_time)
    | Some (_, _)
    | None ->
      maybe_start_action ()
end

module Hippie_actions = struct
  type t =
    | CHARGE_BITE
    | BITE
    | TURN
    | WALK

  let to_string (action : t) : string =
    match action with
    | CHARGE_BITE -> "charge-bite"
    | BITE -> "bite"
    | TURN -> "turn"
    | WALK -> "walk"

  let from_string (s : string) : t =
    match s with
    | "charge-bite" -> CHARGE_BITE
    | "bite" -> BITE
    | "turn" -> TURN
    | "walk" -> WALK
    | _ -> failwithf "Hippie_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let move_forward vx =
      enemy.entity.v.x <- (if enemy.entity.sprite.facing_right then vx else -.vx)
    in
    let pose_name =
      match action with
      | CHARGE_BITE ->
        enemy.entity.v.x <- 0.;
        "charge-bite"
      | BITE ->
        move_forward (get_attr enemy "bite_vx");
        if Option.is_some enemy.entity.current_floor then
          enemy.entity.v.y <- get_attr enemy "bite_vy";
        "bite"
      | TURN ->
        enemy.entity.sprite.facing_right <- not enemy.entity.sprite.facing_right;
        move_forward (get_attr enemy "walk_vx");
        "idle"
      | WALK ->
        move_forward (get_attr enemy "walk_vx");
        "walking"
    in
    set_pose enemy pose_name
end

module Hippie : M = struct
  module Action = Make_loggable (Hippie_actions)

  type args = {
    frame_time : float;
    ghost_pos : vector;
  }

  let get_args state (game : game) : args =
    { frame_time = state.frame.time; ghost_pos = get_rect_center game.player.ghost.entity.dest }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in
    match enemy.last_performed with
    | Some ("charge-bite", action_time) ->
      Action.handle_charging enemy ~ghost_pos ~action_time ~charge_action:CHARGE_BITE ~action:BITE
        ~frame_time
    | Some ("bite", action_time) ->
      let still_biting = still_airborne enemy ~frame_time ~action_time in
      Action.maybe_continue enemy ~continue_action:BITE ~stop_action:WALK ~frame_time still_biting
    | _ ->
      Action.maybe_aggro enemy ~ghost_pos ~frame_time
        (Some (if should_turn enemy args.frame_time then TURN else WALK))
        (fun () -> Action.start_and_log enemy CHARGE_BITE ~frame_time)
end

let get_module (id : enemy_id) : (module M) =
  match id with
  | DUNCAN -> (module Duncan)
  | LOCKER_BOY -> (module Locker_boy)
  | FROG -> (module Frog)
  | ELECTRICITY -> (module Electricity)
  | FISH -> (module Fish)
  | PENGUIN -> (module Penguin)
  | HIPPIE -> (module Hippie)
  | FLYING_HIPPIE
  | FLYING_HIPPIE_2 ->
    (module Flying_hippie)
  | BIRD -> (module Bird)
  | BAT -> (module Bat)
  | MANICORN
  | MANICORN_2
  | MANICORN_3 ->
    (module Manicorn)
  | WIRED_ELECTRICITY -> failwithf "enemy %s not implemented yet" (Show.enemy_id id)

let choose_behavior (enemy : enemy) (state : state) (game : game) =
  let (module M : M) = get_module enemy.id in
  M.choose_behavior enemy (M.get_args state game)

(* object rects in Tiled define the position of the enemy, and enemies.json defines w/h *)
let create_from_rects
    (enemy_rects : (enemy_id * rect) list)
    finished_interactions
    (enemy_configs : (enemy_id * Json_t.enemy_config) list) : enemy list =
  let texture_cache : (enemy_id * (string * texture) ne_list) list ref = ref [] in
  let build id kind enemy_asset_dir_name (enemy_config : Json_t.enemy_config) entity_dest on_killed
      : enemy =
    let level =
      match id with
      | FLYING_HIPPIE_2 -> 2
      | MANICORN_2 -> 2
      | MANICORN_3 -> 3
      | MANICORN
      | FISH
      | FROG
      | ELECTRICITY
      | PENGUIN
      | WIRED_ELECTRICITY
      | HIPPIE
      | FLYING_HIPPIE
      | BIRD
      | BAT
      | DUNCAN
      | LOCKER_BOY ->
        1
    in
    let texture_configs : texture_config ne_list =
      List.map (Entity.to_texture_config ENEMIES enemy_asset_dir_name) enemy_config.texture_configs
      |> List.to_ne_list
    in
    let entity, textures =
      match List.assoc_opt id !texture_cache with
      | None ->
        Entity.create_from_texture_configs ~collision:(Some DEST)
          ~gravity_multiplier:enemy_config.gravity_multiplier
          (Entity.scale_texture_configs (Config.scale.enemy *. enemy_config.scale) texture_configs)
          entity_dest
      | Some textures ->
        Entity.create_from_textures ~collision:(Some DEST)
          ~gravity_multiplier:enemy_config.gravity_multiplier texture_configs textures entity_dest
    in

    texture_cache := List.replace_assoc id textures !texture_cache;

    let status =
      let b = Entity.is_on_screen entity in
      { active = b; check_damage_collisions = b; props = String.Map.empty }
    in
    let json =
      match List.assoc_opt id enemy_configs with
      | None -> failwithf "could not find enemy json config for %s" (Show.enemy_id id)
      | Some j -> j
    in
    let attrs =
      let scaled =
        json.attrs |> List.map (fun (key, value) -> (key, value *. Config.window.scale))
      in
      scaled @ json.unscaled_attrs |> List.to_string_map
    in
    {
      id;
      status;
      kind;
      level;
      entity;
      damage = json.damage;
      health = { current = enemy_config.health; max = enemy_config.health };
      textures = textures |> List.Non_empty.to_list |> List.to_string_map;
      history = Enemy_action.Map.empty;
      last_performed = None;
      initial_pos = clone_vector entity.dest.pos;
      floor_collisions_this_frame = [];
      spawned_projectiles = [];
      damage_sprites = [];
      on_killed;
      attrs;
      json;
    }
  in
  let build_enemy_from_rect ((enemy_id, dest) : enemy_id * rect) : enemy option =
    let enemy_name = Show.enemy_id enemy_id in
    let enemy_asset_dir_name =
      match enemy_id with
      | FLYING_HIPPIE_2 -> "FLYING_HIPPIE"
      | MANICORN_2
      | MANICORN_3 ->
        "MANICORN"
      | MANICORN
      | HIPPIE
      | FLYING_HIPPIE
      | FISH
      | FROG
      | ELECTRICITY
      | PENGUIN
      | WIRED_ELECTRICITY
      | BIRD
      | BAT
      | DUNCAN
      | LOCKER_BOY ->
        enemy_name
    in
    let enemy_config : Json_t.enemy_config =
      match List.assoc_opt enemy_id enemy_configs with
      | None -> failwithf "missing config in enemies.json for %s" enemy_name
      | Some config -> config
    in
    let cutscene_name, multiple_enemies, enemy_kind =
      (* boss_kind could probably be configured as an optional variant in atd, but using a
         string with default value is easier *)
      match enemy_config.kind with
      | "enemy" -> ("", false, ENEMY)
      | "boss" -> (fmt "boss-killed:%s" enemy_name, false, BOSS)
      | "multi-boss" -> (fmt "boss-killed:%s" enemy_name, true, MULTI_BOSS)
      | _ -> failwithf "%s bad enemy_config.kind '%s'" enemy_name enemy_config.kind
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
        ( (enemy_config.w |> Int.to_float) *. Config.scale.enemy *. enemy_config.scale,
          (enemy_config.h |> Int.to_float) *. Config.scale.enemy *. enemy_config.scale )
      in
      let pos = align_to_bottom dest w h in
      let enemy =
        build enemy_id enemy_kind enemy_asset_dir_name enemy_config { pos; w; h } on_killed
      in
      (* without this, the enemy is unscaled for a single frame after room loads *)
      set_pose enemy "idle";
      Some enemy)
  in
  List.filter_map build_enemy_from_rect enemy_rects
