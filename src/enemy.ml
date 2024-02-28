open Utils
open Types

let is_dead (enemy : enemy) : bool = enemy.health.current <= 0
let is_alive (enemy : enemy) : bool = not (is_dead enemy)
let is_wounded (enemy : enemy) : bool = enemy.health.current < enemy.health.max / 2

let get_wounded_attr (enemy : enemy) attr : string =
  if is_wounded enemy then
    fmt "wounded_%s" attr
  else
    fmt "healthy_%s" attr

let parse_name context name : enemy_id =
  match name with
  (* enemies *)
  | "BAT" -> BAT
  | "BIRD" -> BIRD
  | "ELECTRICITY" -> ELECTRICITY
  | "FISH" -> FISH
  | "FLYING_HIPPIE" -> FLYING_HIPPIE
  | "FLYING_HIPPIE_2" -> FLYING_HIPPIE_2
  | "FROG" -> FROG
  | "FROG_BOMB" -> FROG_BOMB
  | "HIPPIE" -> HIPPIE
  | "HOPPING_HIPPIE" -> HOPPING_HIPPIE
  | "HUMBUG" -> HUMBUG
  | "MANICORN" -> MANICORN
  | "MANICORN_2" -> MANICORN_2
  | "MANICORN_3" -> MANICORN_3
  | "PENGUIN" -> PENGUIN
  (* bosses *)
  | "DUNCAN" -> DUNCAN
  | "LOCKER_BOY" -> LOCKER_BOY
  | "JOSHUA" -> JOSHUA
  | "VICE_DEAN_LAYBOURNE" -> VICE_DEAN_LAYBOURNE
  | "LUIS_GUZMAN" -> LUIS_GUZMAN
  | "BORCHERT" -> BORCHERT
  | "DEAN" -> DEAN
  | "BUDDY" -> BUDDY
  | "LAVA_BRITTA" -> LAVA_BRITTA
  | "LAVA_BRITTA_2" -> LAVA_BRITTA_2
  | "HICKEY" -> HICKEY
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
    failwithf "could not find pose '%s' configured in enemies.json for %s" pose_name
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

let get_projectile_texture ?(reset_texture = true) (enemy : enemy) name =
  match String.Map.find_opt name enemy.textures with
  | Some t ->
    if reset_texture then
      Sprite.reset_animation t
    else
      t
  | None -> failwithf "could not find projectile '%s' for %s" name (Show.enemy_name enemy)

type projectile_pos =
  | ABSOLUTE of vector
  | RELATIVE of relative_position

let spawn_projectile
    ?(reset_texture = true)
    ?(scale = 1.)
    ?(projectile_texture_name = "projectile")
    ?(pogoable = false)
    ?(damage = 1)
    ?(gravity_multiplier = 0.)
    ?(collide_with_floors = false)
    ?(draw_on_top = false)
    ?(orbiting = None)
    ?(update_v = None)
    ~(projectile_pos : projectile_pos)
    ~(v : vector)
    (enemy : enemy)
    (despawn : projectile_despawn)
    spawn_time : projectile =
  if Option.is_some orbiting && collide_with_floors then
    failwith "orbiting projectiles cannot collide with floors";
  let projectile_texture = get_projectile_texture ~reset_texture enemy projectile_texture_name in
  let src = get_src projectile_texture in
  let w, h = (src.w *. Config.scale.enemy *. scale, src.h *. Config.scale.enemy *. scale) in
  let spawn_pos =
    match projectile_pos with
    | ABSOLUTE pos -> pos
    | RELATIVE (x_alignment, y_alignment) ->
      Entity.get_child_pos enemy.entity (x_alignment, y_alignment) w h
  in
  let dest = { pos = spawn_pos; w; h } in
  let entity =
    Entity.create projectile_texture_name ~scale:(scale *. Config.scale.enemy) ~v
      ~facing_right:(v.x > 0.) ~gravity_multiplier ~collision:(Some DEST) projectile_texture dest
  in
  {
    entity;
    despawn;
    spawned = { at = spawn_time };
    update_v;
    pogoable;
    damage;
    collide_with_floors;
    draw_on_top;
    orbiting;
  }

let took_damage_at (enemy : enemy) (damage_kind : damage_kind) =
  match Enemy_action.Map.find_opt (TOOK_DAMAGE damage_kind : enemy_action) enemy.history with
  | None -> Zero.time ()
  | Some time -> time

let make_frog_explosion enemy frame_time =
  let explosion_scale = 4. in
  let projectile_duration = TIME_LEFT { seconds = 0.8 } in
  spawn_projectile enemy ~projectile_texture_name:"explosion" ~scale:explosion_scale ~pogoable:true
    ~v:(Zero.vector ())
    ~projectile_pos:(RELATIVE (CENTER, CENTER))
    ~damage:2 projectile_duration frame_time

let maybe_take_damage
    ?(collision_direction : direction option)
    (state : state)
    (game : game)
    (enemy : enemy)
    (ghost_action_started : time)
    (damage_kind : damage_kind)
    (damage : int)
    (collision : collision) : bool =
  let kill_enemy () =
    enemy.projectiles <- [];
    enemy.status.active <- false;
    enemy.status.check_damage_collisions <- false;
    match enemy.id with
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
    | FROG_BOMB ->
      state.camera.shake <- 1.;
      let projectile = make_frog_explosion enemy state.frame.time in
      let new_explosions = ref [ projectile ] in
      let check_frog_collision (target_enemy : enemy) =
        if target_enemy.id = FROG_BOMB && target_enemy <> enemy then
          if Collision.between_entities projectile.entity target_enemy.entity then (
            new_explosions := make_frog_explosion target_enemy state.frame.time :: !new_explosions;
            Entity.hide target_enemy.entity)
      in
      List.iter check_frog_collision game.room.enemies;
      game.room.loose_projectiles <- !new_explosions @ game.room.loose_projectiles;
      Entity.hide enemy.entity
    | DUNCAN
    | LAVA_BRITTA
    | LAVA_BRITTA_2
    | HICKEY
    | ELECTRICITY ->
      ()
    | BORCHERT
    | LUIS_GUZMAN
    | LOCKER_BOY
    | VICE_DEAN_LAYBOURNE
    | BUDDY
    | JOSHUA
    | DEAN
    | PENGUIN
    | HIPPIE
    | HOPPING_HIPPIE
    | HUMBUG
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
    else if enemy.json.can_recoil then (
      let direction =
        match collision_direction with
        | Some d -> d
        | None -> collision.collided_from
      in
      Entity.recoil ~reset_v:false enemy.entity direction);
    true)
  else
    false

let get_boss_area game =
  match game.room.boss_area with
  | None -> failwithf "missing boss-area for room %s" (Show.room_id game.room.id)
  | Some rect -> rect

let chase_to enemy target_x target_y =
  let dv =
    let dv' = get_attr enemy "chase_dv" in
    (* randomize to prevent two separate enemies from lining up on the exact same path *)
    Random.float_between (dv' *. 0.5) (dv' *. 1.5)
  in
  let new_vx =
    if target_x > enemy.entity.dest.pos.x then
      enemy.entity.v.x +. dv
    else
      enemy.entity.v.x -. dv
  in
  let new_vy =
    if target_y > enemy.entity.dest.pos.y then
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
    | _multiple_collisions -> true)

let face_ghost (enemy : enemy) ~ghost_pos =
  enemy.entity.sprite.facing_right <- ghost_pos.x > rect_center_x enemy.entity.dest

let towards_ghost (enemy : enemy) ~ghost_pos n =
  if ghost_pos.x > rect_center_x enemy.entity.dest then n else -.n

let still_within_boss_area (enemy : enemy) boss_area =
  let left_boundary = boss_area.pos.x in
  let right_boundary = boss_area.pos.x +. boss_area.w in
  if enemy.entity.v.x > 0. then
    enemy.entity.dest.pos.x +. enemy.entity.dest.w < right_boundary
  else
    enemy.entity.dest.pos.x > left_boundary

let move_forward (enemy : enemy) vx =
  enemy.entity.v.x <- (if enemy.entity.sprite.facing_right then vx else -.vx)

let still_airborne (enemy : enemy) ~frame_time ~action_time =
  (* - checks `frame_time - action_time` so the action doesn't cancel on the first frame
     - this ends when hitting a wall, instead of checking if any of the floor
       collisions have direction = UP
  *)
  Option.is_none enemy.entity.current_floor
  && (frame_time -. action_time.at < 0.1 || List.length enemy.floor_collisions_this_frame = 0)

let v_aligned (enemy : enemy) ghost_x =
  enemy.entity.dest.pos.x < ghost_x && ghost_x < enemy.entity.dest.pos.x +. enemy.entity.dest.w

let below_ghost (enemy : enemy) ~ghost_pos =
  enemy.entity.dest.pos.y > ghost_pos.y && v_aligned enemy ghost_pos.x

let above_ghost (enemy : enemy) ~ghost_pos =
  enemy.entity.dest.pos.y < ghost_pos.y && v_aligned enemy ghost_pos.x

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
  val jump : ?vx:float option -> enemy -> t -> ghost_pos:vector -> frame_time:float -> unit
  val idle : enemy -> (unit -> unit) -> frame_time:float -> unit

  val handle_charging :
    ?get_frame_props:(unit -> (string * float) list) ->
    ?charge_attr:string ->
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

  let jump ?(vx = None) enemy action ~ghost_pos ~frame_time =
    face_ghost enemy ~ghost_pos;
    start_and_log enemy action ~frame_time
      ~frame_props:
        [
          ( "random_jump_vx",
            match vx with
            | Some v -> v
            | None ->
              Random.float_between (get_attr enemy "min_jump_vx") (get_attr enemy "max_jump_vx") );
        ]

  let idle enemy start_next_action ~frame_time =
    let duration = get_attr enemy (get_wounded_attr enemy "idle_duration") in
    let still_idling = still_doing enemy "idle" ~duration ~frame_time in
    if not still_idling then
      start_next_action ()

  (* the frame_props are only passed into the action after charging is finished,
     not the charge action *)
  let handle_charging
      ?(get_frame_props = fun () -> [])
      ?(charge_attr = "charge_duration")
      (enemy : enemy)
      ~charge_action
      ~action
      ~ghost_pos
      ~action_time
      ~frame_time =
    face_ghost enemy ~ghost_pos;
    if frame_time -. action_time.at < get_attr enemy charge_attr then
      Actions.set enemy charge_action ~frame_time
    else
      start_and_log enemy action ~frame_time ~frame_props:(get_frame_props ())

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
    | WALK_DEAD

  let to_string (action : t) : string =
    match action with
    | JUMP -> "jumping"
    | LANDED -> "landed"
    | WALK -> "walk"
    | WALK_DEAD -> "walk-dead"

  let from_string (s : string) : t =
    match s with
    | "jumping" -> JUMP
    | "landed" -> LANDED
    | "walking" -> WALK
    | "walking-dead" -> WALK_DEAD
    | _ -> failwithf "Duncan_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let pose_name =
      let walk () =
        let walk_vx = get_attr enemy "walk_vx" in
        if enemy.entity.sprite.facing_right then
          enemy.entity.v.x <- walk_vx
        else
          enemy.entity.v.x <- -1. *. walk_vx
      in
      match action with
      | WALK_DEAD ->
        walk ();
        "walking-dead"
      | WALK ->
        walk ();
        "walking"
      | LANDED ->
        enemy.entity.v.x <- 0.;
        enemy.entity.sprite.facing_right <-
          (* TODO might help to use a prefix like is_ or bool_ for these props that are used as booleans *)
          get_frame_prop "facing_right" = 1.;
        let projectile_duration =
          BOSS_AREA_X (get_frame_prop "boss_area_left", get_frame_prop "boss_area_right")
        in
        let projectile_vx = get_attr enemy "projectile_vx" in
        let spawn x_alignment vx_mult =
          let vx = projectile_vx *. vx_mult in
          let v = { x = vx; y = 0. } in
          spawn_projectile ~scale:3.5 enemy
            ~projectile_pos:(RELATIVE (x_alignment, BOTTOM_INSIDE))
            ~v projectile_duration frame_time
        in
        enemy.projectiles <- [ spawn LEFT_INSIDE 1.; spawn RIGHT_INSIDE (-1.) ] @ enemy.projectiles;
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
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
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
            [
              ("facing_right", if args.ghost_pos.x > enemy.entity.dest.pos.x then 1. else 0.);
              ("boss_area_left", args.boss_area.pos.x);
              ("boss_area_right", args.boss_area.pos.x +. args.boss_area.w);
            ]
      else if frame_time -. landed.at > get_attr enemy "jump_wait_time" then (
        let target_x = Random.x_in args.boss_area in
        let jump_vx =
          let dx = target_x -. enemy.entity.dest.pos.x in
          dx /. airtime
        in
        Action.start_and_log enemy JUMP ~frame_time ~frame_props:[ ("random_jump_vx", jump_vx) ])
end

module Hopping_hippie_actions = struct
  type t =
    | HOP
    | TURN
    | LANDED

  let to_string (action : t) : string =
    match action with
    | HOP -> "hop"
    | TURN -> "turn"
    | LANDED -> "landed"

  let from_string (s : string) : t =
    match s with
    | "hop" -> HOP
    | "turn" -> TURN
    | "landed" -> LANDED
    | _ -> failwithf "Hopping_Hippie_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let _ = (frame_props, frame_time) in
    let pose_name =
      match action with
      | LANDED ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "idle"
      | TURN ->
        enemy.entity.sprite.facing_right <- not enemy.entity.sprite.facing_right;
        enemy.entity.v.x <- -.enemy.entity.v.x;
        "idle"
      | HOP ->
        move_forward enemy (get_attr enemy "hop_vx");
        enemy.entity.v.y <- get_attr enemy "hop_vy";
        enemy.entity.current_floor <- None;
        "idle"
    in
    set_pose enemy pose_name
end

module Hopping_hippie : M = struct
  module Action = Make_loggable (Hopping_hippie_actions)

  type args = {
    frame_time : float;
    ghost_pos : vector;
  }

  let get_args state game : args =
    { frame_time = state.frame.time; ghost_pos = get_rect_center game.player.ghost.entity.dest }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    match enemy.entity.current_floor with
    | None ->
      let last_turn = action_started_at enemy "turn" in
      if List.length enemy.floor_collisions_this_frame > 0 && frame_time -. last_turn.at > 0.5 then
        Action.start_and_log enemy TURN ~frame_time
    | Some (floor, _) ->
      let should_turn =
        let enemy_x = rect_center_x enemy.entity.dest in
        let dx = get_attr enemy "turn_dx" in
        let last_turn = action_started_at enemy "turn" in
        (enemy_x < floor.pos.x +. dx || enemy_x > floor.pos.x +. floor.w -. dx)
        && frame_time -. last_turn.at > 0.5
      in
      Action.start_and_log enemy (if should_turn then TURN else HOP) ~frame_time
end

module Luis_guzman_actions = struct
  type t =
    | IDLE
    | CHARGE_SHOOT
    | SHOOT

  let to_string (action : t) : string =
    match action with
    | IDLE -> "idle"
    | CHARGE_SHOOT -> "charge-shoot"
    | SHOOT -> "shoot"

  let from_string (s : string) : t =
    match s with
    | "idle" -> IDLE
    | "shoot" -> SHOOT
    | "charge-shoot" -> CHARGE_SHOOT
    | _ -> failwithf "Luis_guzman_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop s = get_prop s frame_props in
    let pose_name =
      match action with
      | IDLE ->
        let new_vx = get_prop "new_vx" frame_props in
        let new_vy = get_prop "new_vy" frame_props in
        enemy.entity.v.x <- new_vx;
        enemy.entity.v.y <- new_vy;
        enemy.entity.sprite.facing_right <- new_vx > 0.;
        "idle"
      | CHARGE_SHOOT ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "charge-shoot"
      | SHOOT ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        if get_frame_prop "shoot_hu" = 1. then (
          let new_projectiles =
            let w, _ =
              get_scaled_texture_size
                (get_attr enemy "pancake_width_scale")
                (get_projectile_texture enemy "hu-projectile")
            in
            let maybe_spawn_pancake idx =
              if get_frame_prop (fmt "pancake_%d" idx) = 1. then
                Some
                  (spawn_projectile enemy ~projectile_texture_name:"hu-projectile" ~draw_on_top:true
                     ~gravity_multiplier:0.5 ~scale:3.
                     ~projectile_pos:
                       (ABSOLUTE
                          {
                            x = get_frame_prop "boss_area_x" +. (w *. (idx |> Int.to_float));
                            y = get_frame_prop "boss_area_y";
                          })
                     ~v:{ x = 0.; y = 0. }
                     (BOSS_AREA_Y
                        ( get_frame_prop "boss_area_y",
                          get_frame_prop "boss_area_y" +. get_frame_prop "boss_area_h" ))
                     frame_time)
              else
                None
            in
            List.filter_map maybe_spawn_pancake (Int.range 10)
          in
          enemy.projectiles <- new_projectiles @ enemy.projectiles)
        else (
          let pos = { x = get_frame_prop "spawn_x"; y = get_frame_prop "spawn_y" } in
          let v = { x = get_frame_prop "vx"; y = get_frame_prop "vy" } in
          let name =
            if get_frame_prop "horizontal" = 1. then
              "horizontal"
            else if get_frame_prop "down" = 1. then
              "vertical-down"
            else
              "vertical-up"
          in
          let new_projectile =
            spawn_projectile enemy ~scale:2.
              ~projectile_texture_name:(fmt "xero-projectile-%s" name) ~draw_on_top:true
              ~projectile_pos:(ABSOLUTE pos) ~v
              (TIME_LEFT { seconds = get_attr enemy "xero_projectile_duration" })
              frame_time
          in
          enemy.projectiles <- new_projectile :: enemy.projectiles);
        "shoot"
    in
    set_pose enemy pose_name
end

module Luis_guzman : M = struct
  module Action = Make_loggable (Luis_guzman_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let spawn_no_eyes_projectiles enemy ~frame_time ~boss_area =
    let last_shot = action_started_at enemy "shot_no_eyes" in
    let shoot_duration = get_attr enemy "no_eyes_shoot_duration" in
    if last_shot.at = 0. || frame_time -. last_shot.at > shoot_duration then (
      Action.log enemy "shot_no_eyes" frame_time;
      let new_projectile =
        let vx_mult, spawn_x =
          if Random.bool () then
            (* adjust these by Config.window.center.x so they don't spawn on-screen *)
            (1., boss_area.pos.x -. Config.window.center.x)
          else
            (-1., boss_area.pos.x +. boss_area.w +. Config.window.center.x)
        in
        spawn_projectile enemy ~scale:3. ~projectile_texture_name:"no-eyes-projectile"
          ~update_v:(Some WAVY) ~draw_on_top:true
          ~projectile_pos:(ABSOLUTE { x = spawn_x; y = Random.y_in boss_area })
          ~v:{ x = get_attr enemy "no_eyes_projectile_vx" *. vx_mult; y = 0. }
          (BOSS_AREA_X (boss_area.pos.x, boss_area.pos.x +. boss_area.w))
          frame_time
      in
      enemy.projectiles <- new_projectile :: enemy.projectiles)

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let boss_area = args.boss_area in
    let ghost_pos = args.ghost_pos in
    let set_chase_target () =
      set_prop enemy "chase_x" (Random.x_in boss_area);
      set_prop enemy "chase_y" (Random.y_in boss_area)
    in
    spawn_no_eyes_projectiles enemy ~frame_time ~boss_area;
    let shoot () =
      let min_time, max_time = if is_wounded enemy then (1., 3.) else (2., 4.) in
      set_prop enemy "idle_duration" (Random.float_between min_time max_time);
      set_chase_target ();
      let hu_shot, frame_prop_name =
        (* this is an ugly way to track shot type, but there's only two so it works well enough *)
        if Random.bool () then
          (1., "shoot_hu")
        else
          (0., "shoot_xero")
      in
      set_prop enemy "hu-shot" hu_shot;
      Action.start_and_log enemy CHARGE_SHOOT ~frame_time ~frame_props:[ (frame_prop_name, 1.) ]
    in
    let idle () =
      let duration = get_prop "idle_duration" enemy.status.props in
      let still_idling = Action.still_doing enemy "idle" ~duration ~frame_time in
      if still_idling then (
        let new_vx, new_vy =
          chase_to enemy
            (get_prop "chase_x" enemy.status.props)
            (get_prop "chase_y" enemy.status.props)
        in
        Action.set enemy IDLE ~frame_time
          ~frame_props:([ ("new_vx", new_vx); ("new_vy", new_vy) ] |> List.to_string_map))
      else
        shoot ()
    in
    match enemy.last_performed with
    | None ->
      set_prop enemy "idle_duration" 3.;
      set_chase_target ();
      enemy.projectiles <-
        [
          spawn_projectile enemy ~projectile_texture_name:"markoth-projectile" ~draw_on_top:true
            ~orbiting:(Some (0., true, enemy))
            ~projectile_pos:(ABSOLUTE (Zero.vector ()))
            ~v:{ x = 0.; y = 0. } UNTIL_ENEMY_DEATH frame_time;
        ];
      idle ()
    | Some (action_name, action_time) -> (
      match Action.from_string action_name with
      | IDLE -> idle ()
      | CHARGE_SHOOT ->
        Action.handle_charging enemy ~charge_action:CHARGE_SHOOT ~action:SHOOT ~frame_time
          ~ghost_pos ~action_time ~get_frame_props:(fun () ->
            if get_prop "hu-shot" enemy.status.props = 1. then
              [
                ("shoot_hu", 1.);
                ("boss_area_x", boss_area.pos.x);
                ("boss_area_y", boss_area.pos.y);
                ("boss_area_h", boss_area.h);
              ]
              @ List.map
                  (fun idx -> (fmt "pancake_%d" idx, if Random.bool () then 1. else 0.))
                  (Int.range 10)
            else (
              let v = get_attr enemy "xero_projectile_v" in
              let horizontal_frame_props =
                let d = Config.window.center.x *. 1.5 in
                let vx_mult, dx = if Random.bool () then (1., -.d) else (-1., d) in
                [
                  ("horizontal", 1.);
                  ("spawn_x", ghost_pos.x +. dx);
                  ("spawn_y", ghost_pos.y);
                  ("vx", vx_mult *. v);
                  ("vy", 0.);
                ]
              in
              let vertical_frame_props =
                let d = Config.window.center.y *. 1.5 in
                let vx_mult, dy = if Random.bool () then (1., -.d) else (-1., d) in
                [
                  ("horizontal", 0.);
                  ("spawn_x", ghost_pos.x);
                  ("spawn_y", ghost_pos.y +. dy);
                  ("vx", 0.);
                  ("vy", vx_mult *. v);
                  ("down", vx_mult);
                ]
              in
              [ ("shoot_hu", 0.) ]
              @ if Random.bool () then horizontal_frame_props else vertical_frame_props))
      | SHOOT ->
        let shoot_duration = get_attr enemy "shoot_duration" in
        let still_shooting = frame_time -. action_time.at < shoot_duration in
        if not still_shooting then
          Action.start_and_log enemy IDLE ~frame_time
            ~frame_props:[ ("new_vx", enemy.entity.v.x); ("new_vy", enemy.entity.v.y) ])
end

module Joshua_actions = struct
  type t =
    | IDLE
    | JUMP
    | CHARGE_SHOOT
    | SHOOT
    | CHARGE_DASH
    | DASH

  let to_string (action : t) : string =
    match action with
    | IDLE -> "idle"
    | JUMP -> "jump"
    | CHARGE_SHOOT -> "charge-shoot"
    | SHOOT -> "shoot"
    | CHARGE_DASH -> "charge-dash"
    | DASH -> "dash"

  let from_string (s : string) : t =
    match s with
    | "idle" -> IDLE
    | "jump" -> JUMP
    | "charge-shoot" -> CHARGE_SHOOT
    | "shoot" -> SHOOT
    | "charge-dash" -> CHARGE_DASH
    | "dash" -> DASH
    | _ -> failwithf "Joshua_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let pose_name =
      match action with
      | IDLE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "idle"
      | DASH ->
        let dash_vx = get_attr enemy "dash_vx" in
        if enemy.entity.sprite.facing_right then
          enemy.entity.v.x <- dash_vx
        else
          enemy.entity.v.x <- -1. *. dash_vx;
        "dash"
      | CHARGE_DASH ->
        enemy.entity.v.x <- 0.;
        "charge-dash"
      | CHARGE_SHOOT ->
        enemy.entity.v.x <- 0.;
        "shoot"
      | SHOOT ->
        let vx = get_attr enemy "projectile_vx" in
        let scale = 2.7 in
        let projectile is_right is_first =
          let vx_mult = if is_right then 1. else -1. in
          let vx_scale = if is_first then 1. else 2. in
          let vx = vx *. vx_mult *. vx_scale in
          let vy =
            get_frame_prop (if is_first then "first_projectile_vy" else "second_projectile_vy")
          in
          spawn_projectile enemy ~scale
            ~projectile_pos:(RELATIVE (CENTER, CENTER))
            ~v:{ x = vx; y = vy } ~gravity_multiplier:0.3 ~collide_with_floors:true
            UNTIL_FLOOR_COLLISION frame_time
        in
        let new_projectiles =
          if is_wounded enemy then
            [ projectile true true; projectile false true ]
          else
            [
              projectile true true;
              projectile true false;
              projectile false true;
              projectile false false;
            ]
        in
        enemy.projectiles <- new_projectiles @ enemy.projectiles;
        "shoot"
      | JUMP ->
        let vx = get_frame_prop ~default:(Some enemy.entity.v.x) "random_jump_vx" in
        move_forward enemy vx;
        enemy.entity.v.y <- get_attr enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jump"
    in
    set_pose enemy pose_name
end

module Joshua : M = struct
  module Action = Make_loggable (Joshua_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in

    let maybe_start_action () =
      let actions =
        if ghost_pos.y < enemy.entity.dest.pos.y then
          [ (1., `DASH); (1., `SHOOT); (3., `JUMP) ]
        else if abs_float (ghost_pos.x -. enemy.entity.dest.pos.x) > get_attr enemy "dash_dx" then
          [ (3., `DASH); (1., `SHOOT); (1., `JUMP) ]
        else
          [ (1., `DASH); (1., `SHOOT); (1., `JUMP) ]
      in
      match Random.weighted actions with
      | `DASH -> Action.start_and_log enemy CHARGE_DASH ~frame_time
      | `SHOOT -> Action.start_and_log enemy CHARGE_SHOOT ~frame_time
      | `JUMP -> Action.jump enemy JUMP ~ghost_pos ~frame_time
    in
    let handle_charge ?(get_frame_props = fun () -> []) action_time charge_action action =
      Action.handle_charging enemy ~ghost_pos ~action_time ~charge_action ~action ~frame_time
        ~get_frame_props
    in
    match enemy.last_performed with
    | None -> Action.start_and_log enemy CHARGE_DASH ~frame_time
    | Some (action_name, action_time) -> (
      match Action.from_string action_name with
      | CHARGE_DASH -> handle_charge action_time CHARGE_DASH DASH
      | DASH ->
        let dash_duration = get_attr enemy "dash_duration" in
        let still_dashing =
          Action.still_doing enemy "dash" ~duration:dash_duration ~frame_time
          && still_within_boss_area enemy args.boss_area
        in
        Action.maybe_continue enemy ~continue_action:DASH ~stop_action:IDLE ~frame_time
          still_dashing
      | CHARGE_SHOOT ->
        handle_charge action_time CHARGE_SHOOT SHOOT ~get_frame_props:(fun () ->
            [
              ("first_projectile_vy", Random.float_between (get_attr enemy "min_projectile_vy") 0.);
              ("second_projectile_vy", Random.float_between (get_attr enemy "min_projectile_vy") 0.);
            ])
      | SHOOT ->
        let shoot_duration = get_attr enemy "shoot_duration" in
        let still_shooting = frame_time -. action_time.at < shoot_duration in
        if still_shooting && Random.bool () then
          Action.start_and_log enemy CHARGE_SHOOT ~frame_time
        else
          Action.start_and_log enemy IDLE ~frame_time
      | IDLE -> Action.idle enemy maybe_start_action ~frame_time
      | JUMP ->
        let still_jumping = still_airborne enemy ~frame_time ~action_time in
        if not still_jumping then
          maybe_start_action ())
end

module Dean_actions = struct
  type t =
    | IDLE
    | WALKING
    | JUMP
    | CHARGE_LUNGE
    | LUNGE
    | CHARGE_SWARM
    | SWARM
    | CHARGE_SPIKES
    | SPIKES

  let to_string (action : t) : string =
    match action with
    | IDLE -> "idle"
    | WALKING -> "walking"
    | JUMP -> "jump"
    | CHARGE_LUNGE -> "charge-lunge"
    | LUNGE -> "lunge"
    | CHARGE_SWARM -> "charge-swarm"
    | SWARM -> "swarm"
    | CHARGE_SPIKES -> "charge-spikes"
    | SPIKES -> "spikes"

  let from_string (s : string) : t =
    match s with
    | "idle" -> IDLE
    | "walking" -> WALKING
    | "jump" -> JUMP
    | "charge-lunge" -> CHARGE_LUNGE
    | "lunge" -> LUNGE
    | "charge-swarm" -> CHARGE_SWARM
    | "swarm" -> SWARM
    | "charge-spikes" -> CHARGE_SPIKES
    | "spikes" -> SPIKES
    | _ -> failwithf "Dean_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let pose_name =
      match action with
      | WALKING ->
        move_forward enemy (get_attr enemy "walk_vx");
        "walking"
      | IDLE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "idle"
      | JUMP ->
        let vx = get_frame_prop ~default:(Some enemy.entity.v.x) "random_jump_vx" in
        move_forward enemy vx;
        enemy.entity.v.y <- get_attr enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jump"
      | CHARGE_LUNGE ->
        enemy.entity.v.x <- 0.;
        "charge-lunge"
      | LUNGE ->
        move_forward enemy (get_attr enemy "lunge_vx");
        "lunge"
      | CHARGE_SWARM ->
        enemy.entity.v.x <- 0.;
        "swarm"
      | SWARM ->
        let y_spacing = get_attr enemy "swarm_projectile_y_spacing" in
        let projectile_v idx =
          { x = get_frame_prop (fmt "v_%d" (idx + 1)); y = get_attr enemy "swarm_projectile_vy" }
        in
        let spawn idx =
          let projectile_pos =
            ABSOLUTE
              {
                x = get_frame_prop (fmt "x_%d" (idx + 1));
                y = get_frame_prop "boss_area_y" -. ((idx + 1 |> Int.to_float) *. y_spacing);
              }
          in
          spawn_projectile enemy ~projectile_texture_name:"swarm-projectile"
            ~collide_with_floors:false ~projectile_pos ~v:(projectile_v idx)
            (TIME_LEFT { seconds = get_attr enemy "swarm_projectile_duration" })
            frame_time
        in
        let projectile_count = if is_wounded enemy then 9 else 5 in
        let new_ps = List.map spawn (Int.range projectile_count) in
        enemy.projectiles <- new_ps @ enemy.projectiles;
        "swarm"
      | CHARGE_SPIKES ->
        enemy.entity.v.x <- 0.;
        "spikes"
      | SPIKES ->
        let spike_w = get_attr enemy "spike_w" in
        let spike_vx = get_attr enemy "spike_vx" in
        let core_offset_x, core_offset_y =
          (get_attr enemy "spike_core_offset_x", get_attr enemy "spike_core_offset_y")
        in
        let make_cluster idx detonation_dt =
          let spawn_pos =
            {
              x = get_frame_prop (fmt "spawn_pos_x_%d" idx);
              y = get_frame_prop (fmt "spawn_pos_y_%d" idx);
            }
          in
          let make_spike vx dx =
            let projectile_pos = ABSOLUTE { spawn_pos with x = spawn_pos.x +. dx } in
            spawn_projectile enemy ~projectile_texture_name:"spike-projectile"
              ~gravity_multiplier:0.7 ~collide_with_floors:true ~projectile_pos
              ~v:{ x = vx; y = 0. } UNTIL_FLOOR_COLLISION frame_time
          in
          let down_spike_projectile = make_spike 0. 0. in
          let left_spike_projectile = make_spike (-.spike_vx) (-.spike_w) in
          let right_spike_projectile = make_spike spike_vx spike_w in
          let spike_projectiles =
            [ down_spike_projectile; left_spike_projectile; right_spike_projectile ]
          in
          let core_projectile =
            let projectile_pos =
              ABSOLUTE { x = spawn_pos.x -. core_offset_x; y = spawn_pos.y -. core_offset_y }
            in
            spawn_projectile enemy ~projectile_texture_name:"swarm-projectile"
              ~collide_with_floors:true ~projectile_pos ~v:(Zero.vector ())
              (DETONATE ({ seconds = detonation_dt }, spike_projectiles))
              frame_time
          in
          List.iter (fun (p : projectile) -> Entity.freeze p.entity) spike_projectiles;
          [ core_projectile ] @ spike_projectiles
        in
        enemy.projectiles <-
          make_cluster 1 1.5 @ make_cluster 2 2. @ make_cluster 3 2.5 @ enemy.projectiles;
        "spikes"
    in
    set_pose enemy pose_name
end

module Dean : M = struct
  module Action = Make_loggable (Dean_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in
    let maybe_start_action () =
      let actions =
        if ghost_pos.y < enemy.entity.dest.pos.y then
          [ (1., `LUNGE); (3., `SPIKES); (5., `JUMP); (1., `SWARM) ]
        else if abs_float (ghost_pos.x -. enemy.entity.dest.pos.x) < get_attr enemy "lunge_dx" then
          [ (5., `LUNGE); (1., `SPIKES); (1., `JUMP); (1., `SWARM) ]
        else
          [ (2., `SPIKES); (5., `JUMP); (2., `SWARM) ]
      in
      match Random.weighted actions with
      | `LUNGE -> Action.start_and_log enemy CHARGE_LUNGE ~frame_time
      | `SPIKES -> Action.start_and_log enemy CHARGE_SPIKES ~frame_time
      | `SWARM -> Action.start_and_log enemy CHARGE_SWARM ~frame_time
      | `JUMP -> Action.jump enemy JUMP ~ghost_pos ~frame_time
    in
    let handle_charge ?(get_frame_props = fun () -> []) action_time charge_action action =
      Action.handle_charging enemy ~ghost_pos ~action_time ~charge_action ~action ~frame_time
        ~get_frame_props
    in
    match enemy.last_performed with
    | None -> Action.start_and_log enemy CHARGE_SWARM ~frame_time
    | Some (action_name, action_time) -> (
      match Action.from_string action_name with
      | CHARGE_LUNGE -> handle_charge action_time CHARGE_LUNGE LUNGE
      | LUNGE ->
        let lunge_duration = get_attr enemy "lunge_duration" in
        let still_lunging =
          Action.still_doing enemy "lunge" ~duration:lunge_duration ~frame_time
          && still_within_boss_area enemy args.boss_area
        in
        Action.maybe_continue enemy ~continue_action:LUNGE ~stop_action:IDLE ~frame_time
          still_lunging
      | CHARGE_SPIKES ->
        let third_w = args.boss_area.w /. 3. in
        let get_x idx =
          let offset = (idx |> Int.to_float) *. third_w in
          offset +. Random.float third_w
        in
        let min_y, max_y = (get_attr enemy "spike_min_y", get_attr enemy "spike_max_y") in
        let get_y () = Random.float_between min_y max_y in
        let make_x_prop idx = (fmt "spawn_pos_x_%d" (idx + 1), args.boss_area.pos.x +. get_x idx) in
        let make_y_prop idx = (fmt "spawn_pos_y_%d" (idx + 1), args.boss_area.pos.y +. get_y ()) in
        handle_charge action_time CHARGE_SPIKES SPIKES ~get_frame_props:(fun () ->
            (List.map make_x_prop (Int.range 3) @ List.map make_y_prop (Int.range 3))
            @ [ ("ghost_pos_x", ghost_pos.x); ("ghost_pos_y", ghost_pos.y) ])
      | SPIKES ->
        let spikes_duration = get_attr enemy "spikes_duration" in
        let still_spiking = frame_time -. action_time.at < spikes_duration in
        if not still_spiking then
          Action.start_and_log enemy IDLE ~frame_time
      | CHARGE_SWARM ->
        handle_charge action_time CHARGE_SWARM SWARM ~get_frame_props:(fun () ->
            let padding = get_attr enemy "swarm_padding" in
            let get_x () = Random.x_in ~padding args.boss_area in
            let make_x_prop idx = (fmt "x_%d" (idx + 1), get_x ()) in
            let vx = get_attr enemy "max_swarm_projectile_vx" in
            let make_v_prop idx = (fmt "v_%d" (idx + 1), Random.float_between (-.vx) vx) in
            [ ("boss_area_y", args.boss_area.pos.y) ]
            @ List.map make_x_prop (Int.range 9)
            @ List.map make_v_prop (Int.range 9))
      | SWARM ->
        let swarm_duration = get_attr enemy "swarm_duration" in
        let still_swarming = frame_time -. action_time.at < swarm_duration in
        if not still_swarming then
          Action.start_and_log enemy IDLE ~frame_time
      | IDLE -> Action.idle enemy maybe_start_action ~frame_time
      | JUMP ->
        let still_jumping = still_airborne enemy ~frame_time ~action_time in
        if not still_jumping then
          maybe_start_action ()
      | WALKING ->
        (* only used in interaction *)
        ())
end

module Borchert_actions = struct
  type t =
    | CHARGE_DIVE
    | CHARGE_DASH
    | CHARGE_SHOOT
    | LAND
    | DIVE
    | DASH
    | SHOOT
    | VANISH

  let to_string (action : t) : string =
    match action with
    | CHARGE_DIVE -> "charge-dive"
    | CHARGE_DASH -> "charge-dash"
    | CHARGE_SHOOT -> "charge-shoot"
    | LAND -> "land"
    | DIVE -> "dive"
    | DASH -> "dash"
    | SHOOT -> "shoot"
    | VANISH -> "vanish"

  let from_string (s : string) : t =
    match s with
    | "charge-dive" -> CHARGE_DIVE
    | "charge-dash" -> CHARGE_DASH
    | "charge-shoot" -> CHARGE_SHOOT
    | "land" -> LAND
    | "dive" -> DIVE
    | "dash" -> DASH
    | "shoot" -> SHOOT
    | "vanish" -> VANISH
    | _ -> failwithf "Borchert_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let teleport () =
      match String.Map.find_opt "new_x" frame_props with
      | None -> ()
      | Some new_x ->
        enemy.entity.dest.pos.x <- new_x;
        enemy.entity.dest.pos.y <- String.Map.find "new_y" frame_props
    in
    let pose_name =
      match action with
      | CHARGE_DIVE ->
        teleport ();
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- get_attr enemy "charge_dive_vy";
        "dive"
      | LAND ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        let projectile_duration =
          BOSS_AREA_X (get_frame_prop "boss_area_left", get_frame_prop "boss_area_right")
        in
        let projectile_vx = get_attr enemy "wires_projectile_vx" in
        let spawn x_alignment vx_mult =
          let vx = projectile_vx *. vx_mult in
          let v = { x = vx; y = 0. } in
          spawn_projectile ~scale:3.5 ~projectile_texture_name:"wires-projectile"
            ~projectile_pos:(RELATIVE (x_alignment, BOTTOM_INSIDE))
            ~v enemy projectile_duration frame_time
        in
        enemy.projectiles <- [ spawn LEFT_INSIDE 1.; spawn RIGHT_INSIDE (-1.) ] @ enemy.projectiles;
        "land"
      | DIVE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- get_attr enemy "dive_vy";
        "dive"
      | CHARGE_DASH ->
        teleport ();
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "charge-shoot"
      | DASH ->
        let duration =
          (* these are manually despawned when dash ends *)
          UNTIL_ENEMY_DEATH
        in
        let enemy_vx = get_frame_prop "vx" in
        let spawn mult idx =
          spawn_projectile enemy ~projectile_texture_name:"computer-projectile" ~draw_on_top:true
            ~orbiting:(Some (idx *. mult, enemy_vx > 0., enemy))
            ~projectile_pos:(ABSOLUTE (Zero.vector ()))
            ~v:{ x = 0.; y = 0. } duration frame_time
        in
        let projectile_count = if is_wounded enemy then 6 else 4 in
        enemy.projectiles <-
          List.map
            (spawn (Float.pi /. (projectile_count / 2 |> Int.to_float)))
            (Float.range projectile_count);
        enemy.entity.v.x <- enemy_vx;
        enemy.entity.v.y <- 0.;
        "dash"
      | VANISH ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        enemy.entity.current_floor <- None;
        "vanish"
      | CHARGE_SHOOT ->
        teleport ();
        "charge-shoot"
      | SHOOT ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        let vx' = get_attr enemy "computer_projectile_v" in
        let x_alignment, vx =
          if (get_rect_center enemy.entity.dest).x > get_frame_prop "ghost_x" then
            (LEFT_OUTSIDE, -.vx')
          else
            (RIGHT_OUTSIDE, vx')
        in
        enemy.projectiles <-
          spawn_projectile ~projectile_texture_name:"computer-projectile"
            ~update_v:(Some (HOMING 8.))
            ~projectile_pos:(RELATIVE (x_alignment, CENTER))
            ~draw_on_top:true ~v:{ x = vx; y = 0. } enemy
            (TIME_LEFT { seconds = 2. })
            frame_time
          :: enemy.projectiles;
        "shoot"
    in
    set_pose enemy pose_name
end

module Borchert : M = struct
  module Action = Make_loggable (Borchert_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in
    let boss_area = args.boss_area in
    match enemy.last_performed with
    | None -> Action.start_and_log enemy VANISH ~frame_time
    | Some (action_name, action_time) -> (
      let action = Action.from_string action_name in
      let handle_charge ?attr ?(get_frame_props = fun () -> []) action_time done_charging_action =
        face_ghost enemy ~ghost_pos;
        let charge_attr =
          match attr with
          | Some a -> a
          | None -> get_wounded_attr enemy "charge_duration"
        in
        Action.handle_charging enemy ~charge_attr ~ghost_pos ~action_time ~charge_action:action
          ~action:done_charging_action ~frame_time ~get_frame_props
      in
      let start_new_action () =
        face_ghost enemy ~ghost_pos;
        let actions = [ (1., `DIVE); (1., `DASH); (2., `SHOOT) ] in
        match Random.weighted actions with
        | `DIVE ->
          Action.start_and_log enemy CHARGE_DIVE ~frame_time
            ~frame_props:
              [ ("new_x", ghost_pos.x -. (enemy.entity.dest.w /. 2.)); ("new_y", boss_area.pos.y) ]
        | `SHOOT ->
          let { x = new_x; y = new_y } =
            let rec get_new_pos () =
              let new_pos =
                {
                  x = Random.x_in boss_area;
                  y =
                    (if Random.bool () then
                       boss_area.pos.y
                    else
                      boss_area.pos.y +. boss_area.h -. enemy.entity.dest.h);
                }
              in
              let too_close =
                get_distance new_pos ghost_pos < get_attr enemy "distance_threshold"
              in
              if too_close then get_new_pos () else new_pos
            in
            get_new_pos ()
          in
          Action.start_and_log enemy CHARGE_SHOOT ~frame_time
            ~frame_props:[ ("new_x", new_x); ("new_y", new_y) ]
        | `DASH ->
          Action.start_and_log enemy CHARGE_DASH ~frame_time
            ~frame_props:
              [
                ("new_x", if Random.bool () then boss_area.pos.x else boss_area.pos.x +. boss_area.w);
                ("new_y", boss_area.pos.y);
              ]
      in
      match action with
      | CHARGE_DASH ->
        let vx = get_attr enemy "dash_vx" in
        handle_charge action_time DASH ~get_frame_props:(fun () ->
            [
              ("vx", if enemy.entity.dest.pos.x < rect_center_x boss_area then vx else -.vx);
              ("boss_area_left", boss_area.pos.x);
              ("boss_area_right", boss_area.pos.x +. boss_area.w);
            ])
      | CHARGE_DIVE -> handle_charge ~attr:"dive_charge_duration" action_time DIVE
      | CHARGE_SHOOT ->
        handle_charge action_time SHOOT ~get_frame_props:(fun () ->
            [ ("ghost_x", ghost_pos.x); ("spawn", 1.) ])
      | LAND ->
        let duration = get_attr enemy "land_duration" in
        let still_landing = Action.still_doing enemy "land" ~duration ~frame_time in
        if not still_landing then
          Action.start_and_log enemy VANISH ~frame_time
      | DASH ->
        if not (still_within_boss_area enemy args.boss_area) then (
          enemy.projectiles <- [];
          Action.start_and_log enemy VANISH ~frame_time)
      | VANISH ->
        let duration = animation_loop_duration (String.Map.find "vanish" enemy.textures) in
        let still_vanishing = Action.still_doing enemy "vanish" ~duration ~frame_time in
        if not still_vanishing then
          start_new_action ()
      | DIVE ->
        let still_diving = still_airborne enemy ~frame_time ~action_time in
        if not still_diving then
          Action.start_and_log enemy LAND ~frame_time
            ~frame_props:
              [
                ("boss_area_left", boss_area.pos.x);
                ("boss_area_right", boss_area.pos.x +. boss_area.w);
              ]
      | SHOOT ->
        let duration =
          let d = get_attr enemy "shoot_duration" in
          if is_wounded enemy then d /. 2. else d
        in
        let still_shooting = Action.still_doing enemy "shoot" ~duration ~frame_time in
        if not still_shooting then
          Action.start_and_log enemy VANISH ~frame_time)
end

module Lava_britta_actions = struct
  type t =
    | CHARGE_LUNGE
    | CHARGE_STORM
    | CHARGE_THROW
    | JUMP
    | LUNGE
    | STORM
    | THROW
    | IDLE
    | WALK

  let to_string (action : t) : string =
    match action with
    | CHARGE_LUNGE -> "charge-lunge"
    | CHARGE_STORM -> "charge-storm"
    | CHARGE_THROW -> "charge-throw"
    | JUMP -> "jump"
    | LUNGE -> "lunge"
    | STORM -> "storm"
    | THROW -> "throw"
    | IDLE -> "idle"
    | WALK -> "walking"

  let from_string (s : string) : t =
    match s with
    | "charge-lunge" -> CHARGE_LUNGE
    | "charge-storm" -> CHARGE_STORM
    | "charge-throw" -> CHARGE_THROW
    | "jump" -> JUMP
    | "lunge" -> LUNGE
    | "storm" -> STORM
    | "throw" -> THROW
    | "idle" -> IDLE
    | "walking" -> WALK
    | _ -> failwithf "Lava_britta_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let pose_name =
      let walk () =
        let walk_vx = get_attr enemy "walk_vx" in
        if enemy.entity.sprite.facing_right then
          enemy.entity.v.x <- walk_vx
        else
          enemy.entity.v.x <- -1. *. walk_vx
      in
      match action with
      | WALK ->
        walk ();
        "walking"
      | CHARGE_LUNGE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "charge-lunge"
      | CHARGE_STORM ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "charge-storm"
      | CHARGE_THROW ->
        enemy.entity.v.x <- 0.;
        "charge-throw"
      | JUMP ->
        let vx = get_frame_prop ~default:(Some enemy.entity.v.x) "random_jump_vx" in
        move_forward enemy vx;
        enemy.entity.v.y <- get_attr enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jump"
      | LUNGE ->
        move_forward enemy (get_attr enemy "lunge_vx");
        enemy.entity.v.y <- 0.;
        "lunge"
      | STORM ->
        if get_frame_prop ~default:(Some 0.) "spawn_projectile" = 1. then
          enemy.projectiles <-
            [
              spawn_projectile enemy ~projectile_texture_name:"storm-projectile" ~scale:8.
                ~projectile_pos:(RELATIVE (CENTER, CENTER))
                ~v:(Zero.vector ())
                (TIME_LEFT { seconds = get_attr enemy "storm_duration" })
                frame_time;
            ];
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "storm"
      | THROW ->
        let vx =
          if enemy.entity.sprite.facing_right then
            get_attr enemy "plunger_vx"
          else
            -.get_attr enemy "plunger_vx"
        in
        enemy.projectiles <-
          [
            spawn_projectile enemy ~projectile_texture_name:"plunger-projectile" ~scale:1.2
              ~collide_with_floors:true
              ~projectile_pos:(RELATIVE (IN_FRONT enemy.entity.sprite.facing_right, CENTER))
              ~v:{ x = vx; y = 0. } UNTIL_FLOOR_COLLISION frame_time;
          ];
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "throw"
      | IDLE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "idle"
    in
    set_pose enemy pose_name
end

module Lava_britta : M = struct
  module Action = Make_loggable (Lava_britta_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in
    let boss_area = args.boss_area in
    let start_new_action () =
      let actions =
        if below_ghost enemy ~ghost_pos then
          [ (1., `JUMP) ]
        else if enemy.entity.dest.pos.y > ghost_pos.y then
          [ (5., `JUMP); (1., `THROW); (1., `LUNGE) ]
        else
          [ (1., `JUMP); (1., `THROW); (1., `LUNGE) ]
      in
      match Random.weighted actions with
      | `JUMP ->
        enemy.status.props <-
          String.Map.update "should_storm"
            (fun _ -> Some (if Random.percent 40 then 1. else 0.))
            enemy.status.props;
        Action.jump enemy JUMP ~ghost_pos ~frame_time
      | `THROW -> Action.start_and_log enemy CHARGE_THROW ~frame_time
      | `LUNGE -> Action.start_and_log enemy CHARGE_LUNGE ~frame_time
    in
    match enemy.last_performed with
    | None -> start_new_action ()
    | Some (action_name, action_time) -> (
      let action = Action.from_string action_name in
      let handle_charge
          ?(get_frame_props = fun () -> [])
          ?(charge_attr = "charge_duration")
          done_charging_action =
        Action.handle_charging enemy ~charge_attr ~ghost_pos ~action_time ~charge_action:action
          ~action:done_charging_action ~frame_time ~get_frame_props
      in
      match action with
      | CHARGE_LUNGE -> handle_charge LUNGE
      | CHARGE_STORM ->
        handle_charge STORM ~get_frame_props:(fun () -> [ ("spawn_projectile", 1.) ])
      | CHARGE_THROW -> handle_charge THROW
      | JUMP ->
        (match String.Map.find_opt "should_storm" enemy.status.props with
        | None -> ()
        | Some f ->
          if f = 1. && enemy.entity.dest.pos.y < boss_area.pos.y +. (boss_area.h /. 2.) then
            Action.start_and_log enemy CHARGE_STORM ~frame_time);
        let still_jumping = still_airborne enemy ~frame_time ~action_time in
        if not still_jumping then
          Action.start_and_log enemy IDLE ~frame_time
      | IDLE -> Action.idle enemy start_new_action ~frame_time
      | LUNGE ->
        let duration = get_attr enemy "lunge_duration" in
        let still_lunging =
          Action.still_doing enemy "lunge" ~duration ~frame_time
          && still_within_boss_area enemy boss_area
        in
        if not still_lunging then
          Action.start_and_log enemy IDLE ~frame_time
      | STORM ->
        let duration = get_attr enemy "storm_duration" in
        let still_storming = Action.still_doing enemy "storm" ~duration ~frame_time in
        if still_storming then
          Action.set enemy STORM ~frame_time
        else
          Action.start_and_log enemy IDLE ~frame_time
      | THROW ->
        let duration = get_attr enemy "throw_duration" in
        let still_throwing = Action.still_doing enemy "throw" ~duration ~frame_time in
        if not still_throwing then
          Action.start_and_log enemy IDLE ~frame_time
      | WALK -> ( (* this is only used in cutscenes *) ))
end

module Hickey_actions = struct
  type t =
    | IDLE
    | CHARGE_LUNGE
    | LUNGE
    | OUTBREAK
    | PILLARS_DIVE
    | PILLARS_LAND
    | BOUNCE_FLOAT
    | BOUNCE_DIVE
    | BARRAGE
    | JUMP
    | WALK

  let to_string (action : t) : string =
    match action with
    | IDLE -> "idle"
    | CHARGE_LUNGE -> "charge-lunge"
    | LUNGE -> "lunge"
    | OUTBREAK -> "outbreak"
    | PILLARS_DIVE -> "pillars-dive"
    | PILLARS_LAND -> "pillars-land"
    | BOUNCE_FLOAT -> "bounce-float"
    | BOUNCE_DIVE -> "bounce-dive"
    | BARRAGE -> "barrage"
    | JUMP -> "jump"
    | WALK -> "walk"

  let from_string (s : string) : t =
    match s with
    | "idle" -> IDLE
    | "charge-lunge" -> CHARGE_LUNGE
    | "lunge" -> LUNGE
    | "outbreak" -> OUTBREAK
    | "pillars-dive" -> PILLARS_DIVE
    | "pillars-land" -> PILLARS_LAND
    | "bounce-float" -> BOUNCE_FLOAT
    | "bounce-dive" -> BOUNCE_DIVE
    | "barrage" -> BARRAGE
    | "jump" -> JUMP
    | "walk" -> WALK
    | _ -> failwithf "Hickey_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let get_offset_projectile_pos () =
      let offset = get_attr enemy "projectile_offset" in
      if enemy.entity.sprite.facing_right then
        ABSOLUTE
          {
            x = enemy.entity.dest.pos.x +. enemy.entity.dest.w +. offset;
            y = enemy.entity.dest.pos.y -. offset;
          }
      else
        ABSOLUTE { x = enemy.entity.dest.pos.x -. offset; y = enemy.entity.dest.pos.y -. offset }
    in
    let pose_name =
      match action with
      | IDLE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "idle"
      | CHARGE_LUNGE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "scream"
      | LUNGE ->
        move_forward enemy (get_attr enemy "lunge_vx");
        enemy.entity.v.y <- 0.;
        "lunge"
      | OUTBREAK ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        if get_frame_prop ~default:(Some 0.) "outbreak_shoot" = 1. then (
          let spawn idx =
            let vx = get_frame_prop (fmt "outbreak_vx_%d" idx) in
            let vy = get_frame_prop (fmt "outbreak_vy_%d" idx) in
            spawn_projectile enemy ~scale:1.5 ~gravity_multiplier:0.5
              ~projectile_pos:(get_offset_projectile_pos ()) ~collide_with_floors:true
              ~v:{ x = vx; y = vy } UNTIL_FLOOR_COLLISION frame_time
          in
          enemy.projectiles <- List.map spawn (Int.range 5) @ enemy.projectiles);
        "scream"
      | PILLARS_DIVE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- get_attr enemy "dive_vy";
        "pillars"
      | PILLARS_LAND ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        let boss_area_x = get_frame_prop "boss_area_x" in
        let boss_area_bottom = get_frame_prop "boss_area_bottom" in
        let spawn idx =
          let dx = get_attr enemy "pillars_dx" in
          let dy = get_attr enemy "pillars_dy" in
          let projectile_pos =
            ABSOLUTE { x = boss_area_x +. (idx *. dx); y = boss_area_bottom +. dy }
          in
          let up_projectile =
            spawn_projectile enemy ~scale:2. ~projectile_pos
              ~v:{ x = 0.; y = get_attr enemy "pillars_vy" }
              ~draw_on_top:true ~damage:2
              (TIME_LEFT { seconds = 3. })
              frame_time
          in
          Entity.freeze up_projectile.entity;
          spawn_projectile enemy ~scale:2. ~projectile_pos ~v:(Zero.vector ()) ~draw_on_top:true
            (DETONATE ({ seconds = 1. }, [ up_projectile ]))
            frame_time
          :: [ up_projectile ]
        in
        enemy.projectiles <- List.concat_map spawn (Float.range 12) @ enemy.projectiles;
        "pillars"
      | BOUNCE_FLOAT ->
        enemy.entity.v.x <- get_frame_prop "float_vx";
        enemy.entity.v.y <- get_attr enemy "bounce_float_vy";
        "bounce-float"
      | BOUNCE_DIVE ->
        enemy.entity.v.x <- get_frame_prop "bounce_dive_vx";
        enemy.entity.v.y <- get_attr enemy "dive_vy";
        "dive"
      | BARRAGE ->
        if get_frame_prop ~default:(Some 0.) "shoot_projectile" = 1. then (
          let vx =
            if enemy.entity.sprite.facing_right then
              get_frame_prop "projectile_vx"
            else
              -.get_frame_prop "projectile_vx"
          in
          let vy = get_frame_prop "projectile_vy" in
          let projectile_pos = get_offset_projectile_pos () in
          enemy.projectiles <-
            spawn_projectile enemy ~scale:1.2 ~collide_with_floors:true ~gravity_multiplier:1.
              ~projectile_pos ~v:{ x = vx; y = vy } UNTIL_FLOOR_COLLISION frame_time
            :: enemy.projectiles);
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "scream"
      | JUMP ->
        let vx = get_frame_prop ~default:(Some enemy.entity.v.x) "random_jump_vx" in
        move_forward enemy vx;
        enemy.entity.v.y <- get_attr enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jump"
      | WALK -> "walk"
    in
    set_pose enemy pose_name
end

module Hickey : M = struct
  module Action = Make_loggable (Hickey_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in
    let boss_area = args.boss_area in
    let start_new_action () =
      let actions =
        let dx = abs_float (rect_center_x enemy.entity.dest -. ghost_pos.x) in
        if enemy.entity.dest.pos.y > ghost_pos.y && dx < get_attr enemy "above_dx" then
          [ (1., `JUMP) ]
        else
          [ (1., `JUMP); (1., `LUNGE); (1., `BARRAGE) ]
      in
      match Random.weighted actions with
      | `JUMP ->
        let set prop =
          (* this unsets all previous props *)
          enemy.status.props <- [ (prop, 1.) ] |> List.to_string_map
        in
        (match Random.weighted [ (4., `PILLAR); (1., `OUTBREAK); (2., `BOUNCE); (10., `NONE) ] with
        | `PILLAR -> set "should_pillar"
        | `OUTBREAK ->
          set_prop enemy "outbreak_shot" frame_time;
          set "should_outbreak"
        | `BOUNCE -> set "should_bounce"
        | `NONE -> enemy.status.props <- String.Map.empty);

        Action.jump enemy JUMP ~ghost_pos ~frame_time
      | `LUNGE -> Action.start_and_log enemy CHARGE_LUNGE ~frame_time
      | `BARRAGE ->
        face_ghost enemy ~ghost_pos;
        set_prop enemy "barrage_shot" frame_time;
        set_prop enemy "barrage_projectile_idx" 0.;
        Action.start_and_log enemy BARRAGE ~frame_time
    in
    match enemy.last_performed with
    | None -> start_new_action ()
    | Some (action_name, action_time) -> (
      let action = Action.from_string action_name in
      let bounce_vx ~floating =
        let dvx = get_attr enemy (if floating then "float_dvx" else "bounce_dive_dvx") in
        let max_v = get_attr enemy "bounce_max_v" in
        let bound n = Float.bound (-.max_v) n max_v in
        if ghost_pos.x < rect_center_x enemy.entity.dest then
          bound (enemy.entity.v.x -. dvx)
        else
          bound (enemy.entity.v.x +. dvx)
      in
      let make_outbreak_props idx =
        let min_vx = get_attr enemy "outbreak_min_vx" in
        let max_vx = get_attr enemy "outbreak_max_vx" in
        let min_vy = get_attr enemy "outbreak_min_vy" in
        let max_vy = get_attr enemy "outbreak_max_vy" in
        [
          ("outbreak_shoot", 1.);
          (fmt "outbreak_vx_%d" idx, Random.float_between min_vx max_vx);
          (fmt "outbreak_vy_%d" idx, Random.float_between min_vy max_vy);
        ]
      in
      match action with
      | CHARGE_LUNGE ->
        Action.handle_charging enemy
          ~charge_attr:(get_wounded_attr enemy "charge_duration")
          ~ghost_pos ~action_time ~charge_action:action ~action:LUNGE ~frame_time
      | LUNGE ->
        let duration = get_attr enemy "lunge_duration" in
        let still_lunging =
          Action.still_doing enemy "lunge" ~duration ~frame_time
          && still_within_boss_area enemy boss_area
        in
        if not still_lunging then
          Action.start_and_log enemy IDLE ~frame_time
      | IDLE -> Action.idle enemy start_new_action ~frame_time
      | PILLARS_DIVE ->
        let still_diving = still_airborne enemy ~frame_time ~action_time in
        if not still_diving then
          Action.start_and_log enemy PILLARS_LAND ~frame_time
            ~frame_props:
              [
                ("boss_area_x", boss_area.pos.x);
                ("boss_area_bottom", boss_area.pos.y +. boss_area.h);
              ]
      | PILLARS_LAND ->
        let duration = get_attr enemy (get_wounded_attr enemy "pillars_land_duration") in
        let still_landing = Action.still_doing enemy "pillars-land" ~duration ~frame_time in
        if not still_landing then
          Action.start_and_log enemy IDLE ~frame_time
      | OUTBREAK ->
        let duration = get_attr enemy "outbreak_duration" in
        let still_outbreaking = Action.still_doing enemy "outbreak" ~duration ~frame_time in
        if still_outbreaking then (
          let last_outbreak_shot = get_prop "outbreak_shot" enemy.status.props in
          let frame_props =
            if frame_time -. last_outbreak_shot > get_attr enemy "outbreak_shot_duration" then (
              set_prop enemy "outbreak_shot" frame_time;
              List.concat_map make_outbreak_props (Int.range 5) |> List.to_string_map)
            else
              [] |> List.to_string_map
          in
          Action.set enemy OUTBREAK ~frame_time ~frame_props)
        else
          Action.start_and_log enemy IDLE ~frame_time
      | BOUNCE_FLOAT ->
        face_ghost enemy ~ghost_pos;
        let duration = get_attr enemy "bounce_float_duration" in
        let still_floating = Action.still_doing enemy "bounce-float" ~duration ~frame_time in
        if still_floating then
          Action.set enemy BOUNCE_FLOAT ~frame_time
            ~frame_props:([ ("float_vx", bounce_vx ~floating:true) ] |> List.to_string_map)
        else
          Action.start_and_log enemy BOUNCE_DIVE ~frame_time
            ~frame_props:[ ("bounce_dive_vx", bounce_vx ~floating:false) ]
      | BOUNCE_DIVE ->
        let still_diving = still_airborne enemy ~frame_time ~action_time in
        if still_diving then
          Action.set enemy BOUNCE_DIVE ~frame_time
            ~frame_props:([ ("bounce_dive_vx", bounce_vx ~floating:false) ] |> List.to_string_map)
        else (
          let next_action : Action.t =
            if Random.percent 70 then (
              face_ghost enemy ~ghost_pos;
              BOUNCE_FLOAT)
            else
              IDLE
          in
          Action.start_and_log enemy next_action ~frame_time
            ~frame_props:[ ("float_vx", bounce_vx ~floating:true) ])
      | JUMP ->
        let perform_jump_action ?(frame_props = []) ?(face_center = false) name action_to_start cond
            =
          match String.Map.find_opt (fmt "should_%s" name) enemy.status.props with
          | None -> ()
          | Some f ->
            if f = 1. && cond then (
              if face_center then
                enemy.entity.sprite.facing_right <-
                  rect_center_x enemy.entity.dest < rect_center_x boss_area;
              Action.start_and_log enemy action_to_start ~frame_time ~frame_props)
        in
        perform_jump_action "pillar" PILLARS_DIVE (enemy.entity.v.y > 0.);
        perform_jump_action "bounce" BOUNCE_FLOAT true
          ~frame_props:[ ("float_vx", bounce_vx ~floating:true) ];
        perform_jump_action "outbreak" OUTBREAK ~face_center:true
          ~frame_props:
            (set_prop enemy "outbreak_shot" frame_time;
             List.concat_map make_outbreak_props (Int.range 5))
          (enemy.entity.v.y > 0.
          && enemy.entity.dest.pos.y > boss_area.pos.y +. (boss_area.h *. 0.66666));
        let still_jumping = still_airborne enemy ~frame_time ~action_time in
        if not still_jumping then
          Action.start_and_log enemy IDLE ~frame_time
      | BARRAGE ->
        let duration = get_attr enemy "barrage_duration" in
        let still_barraging = Action.still_doing enemy "barrage" ~duration ~frame_time in
        if still_barraging then (
          let last_barrage_projectile = get_prop "barrage_shot" enemy.status.props in
          let frame_props =
            if frame_time -. last_barrage_projectile > get_attr enemy "barrage_shot_duration" then (
              let barrage_projectile_idx =
                get_prop "barrage_projectile_idx" enemy.status.props |> Float.to_int
              in
              set_prop enemy "barrage_shot" frame_time;
              set_prop enemy "barrage_projectile_idx" (barrage_projectile_idx + 1 |> Int.to_float);
              let vx, vy =
                List.nth
                  [
                    (-200., -200.);
                    (-100., -100.);
                    (0., 0.);
                    (100., -100.);
                    (200., -200.);
                    (300., -300.);
                    (400., -400.);
                    (500., -500.);
                    (600., -600.);
                    (700., -700.);
                    (800., -800.);
                    (900., -900.);
                    (800., -1000.);
                    (700., -1100.);
                  ]
                  barrage_projectile_idx
              in
              [
                ("shoot_projectile", 1.);
                ("projectile_vx", vx *. Config.window.scale);
                ("projectile_vy", vy *. Config.window.scale);
              ]
              |> List.to_string_map)
            else
              [] |> List.to_string_map
          in
          Action.set enemy BARRAGE ~frame_time ~frame_props)
        else
          Action.start_and_log enemy IDLE ~frame_time
      | WALK -> ())
end

module Buddy_actions = struct
  type t =
    | IDLE
    | JUMP
    | BACKDASH
    | CHARGE_YELLOW
    | YELLOW
    | CHARGE_BLUE
    | BLUE
    | CHARGE_PURPLE
    | PURPLE
    | RED

  let to_string (action : t) : string =
    match action with
    | IDLE -> "idle"
    | JUMP -> "jump"
    | BACKDASH -> "backdash"
    | CHARGE_YELLOW -> "charge-yellow"
    | YELLOW -> "yellow"
    | CHARGE_BLUE -> "charge-blue"
    | BLUE -> "blue"
    | CHARGE_PURPLE -> "charge-purple"
    | PURPLE -> "purple"
    | RED -> "red"

  let from_string (s : string) : t =
    match s with
    | "idle" -> IDLE
    | "jump" -> JUMP
    | "backdash" -> BACKDASH
    | "charge-yellow" -> CHARGE_YELLOW
    | "yellow" -> YELLOW
    | "charge-blue" -> CHARGE_BLUE
    | "blue" -> BLUE
    | "charge-purple" -> CHARGE_PURPLE
    | "purple" -> PURPLE
    | "red" -> RED
    | _ -> failwithf "Buddy_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let make_paintball ?(scale = 1.5) ?(gravity_multiplier = 1.) ~projectile_pos ~v color =
      spawn_projectile enemy ~projectile_texture_name:(fmt "%s-projectile" color) ~scale
        ~gravity_multiplier ~reset_texture:false ~collide_with_floors:true ~projectile_pos ~v
        UNTIL_FLOOR_COLLISION frame_time
    in
    let pose_name =
      match action with
      | IDLE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "idle"
      | BACKDASH ->
        let vx = get_attr enemy "backdash_vx" in
        enemy.entity.v.x <- (if enemy.entity.sprite.facing_right then -.vx else vx);
        enemy.entity.v.y <- 0.;
        "jump"
      | JUMP ->
        let vx = get_frame_prop "random_jump_vx" in
        move_forward enemy vx;
        enemy.entity.v.y <- get_attr enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jump"
      | CHARGE_YELLOW ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "yellow"
      | YELLOW ->
        if get_frame_prop ~default:(Some 0.) "should_shoot" = 1. then (
          let vx' = get_frame_prop "projectile_vx" in
          let x_alignment, vx =
            if enemy.entity.sprite.facing_right then
              (RIGHT_OUTSIDE, vx')
            else
              (LEFT_OUTSIDE, -.vx')
          in
          let new_projectile =
            make_paintball ~gravity_multiplier:0.3
              ~projectile_pos:(RELATIVE (x_alignment, CENTER))
              ~v:{ x = vx; y = get_frame_prop "projectile_vy" }
              "yellow"
          in
          enemy.projectiles <- new_projectile :: enemy.projectiles);
        "yellow"
      | CHARGE_BLUE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "blue"
      | BLUE ->
        let x_alignment, vx_mult =
          if enemy.entity.sprite.facing_right then
            (RIGHT_OUTSIDE, 1.)
          else
            (LEFT_OUTSIDE, -1.)
        in
        let new_projectiles =
          let spawn idx =
            let vx = get_attr enemy (fmt "blue_projectile_vx_%d" idx) in
            let vy = get_attr enemy (fmt "blue_projectile_vy_%d" idx) in
            make_paintball
              ~projectile_pos:(RELATIVE (x_alignment, CENTER))
              ~v:{ x = vx *. vx_mult; y = vy }
              "blue"
          in
          List.map spawn (Int.range 3)
        in
        enemy.projectiles <- new_projectiles @ enemy.projectiles;
        "blue"
      | CHARGE_PURPLE ->
        if get_frame_prop ~default:(Some 0.) "should_shoot" = 1. then (
          let x_alignment =
            (* these are backwards because the gun barrel is towards the back *)
            if enemy.entity.sprite.facing_right then
              LEFT_INSIDE
            else
              RIGHT_INSIDE
          in
          let new_projectile =
            make_paintball ~scale:2.5 ~gravity_multiplier:0.
              ~projectile_pos:(RELATIVE (x_alignment, TOP_INSIDE))
              ~v:{ x = 0.; y = get_attr enemy "purple_projectile_up_vy" }
              "purple"
          in
          enemy.projectiles <- new_projectile :: enemy.projectiles);
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "purple"
      | PURPLE ->
        if get_frame_prop ~default:(Some 0.) "should_shoot" = 1. then (
          let new_projectile =
            make_paintball
              ~projectile_pos:
                (ABSOLUTE { x = get_frame_prop "random_x"; y = get_frame_prop "boss_area_y" })
              ~v:(Zero.vector ()) "purple"
          in
          enemy.projectiles <- new_projectile :: enemy.projectiles);
        "idle"
      | RED ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        let new_projectiles =
          let spawn vx =
            make_paintball
              ~projectile_pos:(RELATIVE (CENTER, BOTTOM_INSIDE))
              ~v:{ x = vx; y = 0. } "red"
          in
          let near, far =
            (get_attr enemy "red_projectile_vx_near", get_attr enemy "red_projectile_vx_far")
          in
          List.map spawn [ -.far; -.near; near; far ]
        in
        enemy.projectiles <- new_projectiles @ enemy.projectiles;
        "red"
    in
    set_pose enemy pose_name
end

module Buddy : M = struct
  module Action = Make_loggable (Buddy_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in
    let boss_area = args.boss_area in
    face_ghost enemy ~ghost_pos;
    let start_new_action () =
      let actions =
        let in_half side =
          let left_boundary, right_boundary =
            match side with
            | `LEFT -> (boss_area.pos.x, boss_area.pos.x +. (boss_area.w /. 2.))
            | `RIGHT -> (boss_area.pos.x +. (boss_area.w /. 2.), boss_area.pos.x +. boss_area.w)
          in
          let enemy_center = rect_center_x enemy.entity.dest in
          left_boundary < enemy_center && enemy_center < right_boundary
        in
        if below_ghost enemy ~ghost_pos then
          [ (1., `CHARGE_PURPLE); (1., `JUMP) ]
        else if in_half `RIGHT then
          if enemy.entity.sprite.facing_right then
            [ (1., `BACKDASH); (1., `CHARGE_BLUE) ]
          else
            [ (1., `CHARGE_YELLOW); (1., `CHARGE_PURPLE); (1., `JUMP) ]
        else if in_half `LEFT then
          if enemy.entity.sprite.facing_right then
            [ (1., `CHARGE_YELLOW); (1., `CHARGE_PURPLE); (1., `JUMP) ]
          else
            [ (1., `BACKDASH); (1., `CHARGE_BLUE) ]
        else
          failwith "unreachable"
      in
      match Random.weighted actions with
      | `JUMP ->
        enemy.status.props <-
          String.Map.update "should_shoot_red"
            (fun _ -> Some (if Random.percent 70 then 1. else 0.))
            enemy.status.props;
        Action.jump enemy JUMP ~ghost_pos ~frame_time
      | `BACKDASH -> Action.start_and_log enemy BACKDASH ~frame_time
      | `CHARGE_BLUE -> Action.start_and_log enemy CHARGE_BLUE ~frame_time
      | `CHARGE_PURPLE ->
        Action.start_and_log enemy CHARGE_PURPLE ~frame_time ~frame_props:[ ("should_shoot", 1.) ]
      | `CHARGE_YELLOW -> Action.start_and_log enemy CHARGE_YELLOW ~frame_time
    in
    match enemy.last_performed with
    | None -> start_new_action ()
    | Some (action_name, action_time) -> (
      let action = Action.from_string action_name in
      let handle_charge
          ?(get_frame_props = fun () -> [])
          ?(charge_attr = "charge_duration")
          done_charging_action =
        Action.handle_charging enemy ~charge_attr ~ghost_pos ~action_time ~charge_action:action
          ~action:done_charging_action ~frame_time ~get_frame_props
      in
      let still_shooting color =
        let color_name =
          match color with
          | `PURPLE -> "purple"
          | `YELLOW -> "yellow"
        in
        let duration = get_attr enemy (fmt "%s_duration" color_name) in
        let still_shooting' = Action.still_doing enemy color_name ~duration ~frame_time in
        if still_shooting' then (
          let last_shot = action_started_at enemy (fmt "%s_shot" color_name) in
          let frame_props =
            let should_shoot =
              let shot_duration =
                let attr = get_attr enemy (fmt "%s_shot_duration" color_name) in
                (* shoot faster at low health *)
                if is_wounded enemy then attr /. 2. else attr
              in
              frame_time -. last_shot.at > shot_duration
            in
            let get_color_frame_props () =
              match color with
              | `PURPLE ->
                [
                  ("boss_area_y", boss_area.pos.y);
                  ( "random_x",
                    if Random.percent 30 then
                      ghost_pos.x
                    else
                      Random.x_in boss_area );
                ]
              | `YELLOW ->
                let vx = get_attr enemy "yellow_projectile_vx" in
                let vy = get_attr enemy "yellow_projectile_vy" in
                [
                  ("projectile_vx", Random.float_between (vx /. 3.) vx);
                  ("projectile_vy", Random.float_between (-.vy) vy);
                ]
            in
            if should_shoot then (
              Action.log enemy (fmt "%s_shot" color_name) frame_time;
              [ ("should_shoot", 1.) ] @ get_color_frame_props () |> List.to_string_map)
            else
              String.Map.empty
          in
          Action.set enemy action ~frame_time ~frame_props)
        else
          Action.start_and_log enemy IDLE ~frame_time
      in
      match action with
      | IDLE -> Action.idle enemy start_new_action ~frame_time
      | JUMP ->
        (match String.Map.find_opt "should_shoot_red" enemy.status.props with
        | None -> ()
        | Some f ->
          if f = 1. && enemy.entity.v.y > 0. then
            Action.start_and_log enemy RED ~frame_time);
        let still_jumping = still_airborne enemy ~frame_time ~action_time in
        if not still_jumping then
          Action.start_and_log enemy IDLE ~frame_time
      | BACKDASH ->
        let duration = get_attr enemy "backdash_duration" in
        let still_backdashing =
          Action.still_doing enemy "backdash" ~duration ~frame_time
          && still_within_boss_area enemy boss_area
        in
        if not still_backdashing then
          Action.start_and_log enemy IDLE ~frame_time
      | CHARGE_YELLOW -> handle_charge YELLOW
      | CHARGE_PURPLE -> handle_charge PURPLE
      | CHARGE_BLUE -> handle_charge BLUE
      | YELLOW -> still_shooting `YELLOW
      | PURPLE -> still_shooting `PURPLE
      | BLUE ->
        let duration = get_attr enemy "blue_duration" in
        let still_shooting = Action.still_doing enemy "blue" ~duration ~frame_time in
        if not still_shooting then
          Action.start_and_log enemy IDLE ~frame_time
      | RED ->
        let still_shooting = still_airborne enemy ~frame_time ~action_time in
        if not still_shooting then
          Action.start_and_log enemy IDLE ~frame_time)
end

module Vice_dean_laybourne_actions = struct
  type t =
    | IDLE
    | IDLE_MOVING
    | JUMP
    | DIVE
    | LAND
    | CHARGE_HOP_LUNGE
    | CHARGE_LUNGE
    | LUNGE
    | CASCADE

  let to_string (action : t) : string =
    match action with
    | IDLE -> "idle"
    | IDLE_MOVING -> "idle-moving"
    | JUMP -> "jump"
    | DIVE -> "dive"
    | LAND -> "land"
    | CHARGE_HOP_LUNGE -> "charge-hop-lunge"
    | CHARGE_LUNGE -> "charge-lunge"
    | LUNGE -> "lunge"
    | CASCADE -> "cascade"

  let from_string (s : string) : t =
    match s with
    | "idle" -> IDLE
    | "idle-moving" -> IDLE_MOVING
    | "jump" -> JUMP
    | "dive" -> DIVE
    | "land" -> LAND
    | "charge-hop-lunge" -> CHARGE_HOP_LUNGE
    | "charge-lunge" -> CHARGE_LUNGE
    | "lunge" -> LUNGE
    | "cascade" -> CASCADE
    | _ -> failwithf "Vice_dean_laybourne_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let get_frame_prop ?(default = None) prop_name = get_prop ~default prop_name frame_props in
    let spawn_projectiles () =
      let spawn idx =
        let v = { x = get_frame_prop (fmt "vx_%d" idx); y = get_attr enemy "projectile_vy" } in
        spawn_projectile ~scale:2. ~draw_on_top:true ~gravity_multiplier:(-0.3)
          ~projectile_pos:(RELATIVE (CENTER, BOTTOM_INSIDE))
          enemy ~v
          (TIME_LEFT { seconds = 3. })
          frame_time
      in
      let new_projectiles = List.map spawn (Int.range 5) in
      enemy.projectiles <- new_projectiles @ enemy.projectiles
    in
    let pose_name =
      match action with
      | IDLE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "idle"
      | IDLE_MOVING ->
        move_forward enemy (get_attr enemy "idle_moving_vx");
        enemy.entity.v.y <- 0.;
        "idle-moving"
      | JUMP ->
        let vx = get_frame_prop ~default:(Some enemy.entity.v.x) "random_jump_vx" in
        move_forward enemy vx;
        enemy.entity.v.y <- get_attr enemy "jump_vy";
        enemy.entity.current_floor <- None;
        "jump"
      | CHARGE_HOP_LUNGE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- get_attr enemy "hop_lunge_vy";
        "jump"
      | CHARGE_LUNGE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "dive"
      | LUNGE ->
        move_forward enemy (get_attr enemy "lunge_vx");
        (* set v.y to 0 so hop-lunge stays in the air *)
        enemy.entity.v.y <- 0.;
        "lunge"
      | CASCADE ->
        if get_frame_prop ~default:(Some 0.) "should_shoot" = 1. then
          spawn_projectiles ();
        enemy.entity.v.x <- 0.;
        "cascade"
      | DIVE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- get_attr enemy "dive_vy";
        "dive"
      | LAND ->
        spawn_projectiles ();
        "dive"
    in
    set_pose enemy pose_name
end

module Vice_dean_laybourne : M = struct
  module Action = Make_loggable (Vice_dean_laybourne_actions)

  type args = {
    frame_time : float;
    boss_area : rect;
    ghost_pos : vector;
  }

  let get_args state game : args =
    {
      frame_time = state.frame.time;
      boss_area = get_boss_area game;
      ghost_pos = get_rect_center game.player.ghost.entity.dest;
    }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let ghost_pos = args.ghost_pos in
    let start_new_action () =
      let actions =
        let near_center =
          let third = args.boss_area.w /. 3. in
          let left_boundary = args.boss_area.pos.x +. third in
          let right_boundary = args.boss_area.pos.x +. (third *. 2.) in
          let enemy_center = rect_center_x enemy.entity.dest in
          left_boundary < enemy_center && enemy_center < right_boundary
        in
        if near_center then
          [ (2., `JUMP); (1., `CASCADE) ]
        else
          [ (1., `LUNGE); (1., `HOP_LUNGE); (2., `JUMP) ]
      in
      match Random.weighted actions with
      | `LUNGE -> Action.start_and_log enemy CHARGE_LUNGE ~frame_time
      | `JUMP ->
        enemy.status.props <-
          String.Map.update "should_dive"
            (fun _ -> Some (if Random.bool () then 1. else 0.))
            enemy.status.props;
        Action.jump enemy JUMP ~ghost_pos ~frame_time
      | `CASCADE -> Action.start_and_log enemy CASCADE ~frame_time
      | `HOP_LUNGE -> Action.start_and_log enemy CHARGE_HOP_LUNGE ~frame_time
    in
    let handle_charge
        ?(get_frame_props = fun () -> [])
        ?(charge_attr = "charge_duration")
        action_time
        charge_action
        action =
      Action.handle_charging enemy ~charge_attr ~ghost_pos ~action_time ~charge_action ~action
        ~frame_time ~get_frame_props
    in
    let get_vx_frame_props () =
      let make_vx idx =
        let projectile_x = get_attr enemy "projectile_x_spacing" in
        let x_offset = (idx - 2 |> Int.to_float) *. projectile_x in
        [ (fmt "vx_%d" idx, x_offset +. Random.float projectile_x) ]
      in
      List.map make_vx (Int.range 5) |> List.flatten
    in
    let maybe_continue' action_name duration_name =
      let duration = get_attr enemy duration_name in
      let still_doing = Action.still_doing enemy action_name ~duration ~frame_time in
      if not still_doing then
        start_new_action ()
    in
    match enemy.last_performed with
    | None -> Action.jump enemy JUMP ~ghost_pos ~frame_time
    | Some (action_name, action_time) -> (
      match Action.from_string action_name with
      | IDLE_MOVING ->
        face_ghost enemy ~ghost_pos;
        maybe_continue' "idle-moving" (get_wounded_attr enemy "idle_duration")
      | IDLE ->
        face_ghost enemy ~ghost_pos;
        maybe_continue' "idle" (get_wounded_attr enemy "idle_duration")
      | LAND -> maybe_continue' "land" "land_duration"
      | JUMP ->
        (match String.Map.find_opt "should_dive" enemy.status.props with
        | None -> ()
        | Some f ->
          if f = 1. && Option.is_none enemy.entity.current_floor && above_ghost enemy ~ghost_pos
          then
            Action.start_and_log enemy DIVE ~frame_time);
        let still_jumping = still_airborne enemy ~frame_time ~action_time in
        if not still_jumping then
          Action.start_and_log enemy IDLE_MOVING ~frame_time
      | DIVE ->
        let still_diving = still_airborne enemy ~frame_time ~action_time in
        if not still_diving then
          Action.start_and_log enemy LAND ~frame_time ~frame_props:(get_vx_frame_props ())
      | CHARGE_LUNGE -> handle_charge action_time CHARGE_LUNGE LUNGE
      | CHARGE_HOP_LUNGE -> handle_charge action_time CHARGE_HOP_LUNGE LUNGE
      | LUNGE ->
        let lunge_duration = get_attr enemy "lunge_duration" in
        let still_lunging =
          Action.still_doing enemy "lunge" ~duration:lunge_duration ~frame_time
          && still_within_boss_area enemy args.boss_area
        in
        Action.maybe_continue enemy ~continue_action:LUNGE ~stop_action:IDLE ~frame_time
          still_lunging
      | CASCADE ->
        let cascade_duration = get_attr enemy "cascade_duration" in
        let still_cascading = frame_time -. action_time.at < cascade_duration in
        if still_cascading then (
          let last_shot = action_started_at enemy "cascade_shot" in
          let frame_props =
            let should_shoot =
              let shot_duration = get_attr enemy "cascade_shot_duration" in
              (* check last_shot.at = 0 to handle the first shot *)
              last_shot.at = 0. || frame_time -. last_shot.at > shot_duration
            in
            if should_shoot then (
              Action.log enemy "cascade_shot" frame_time;
              [ ("should_shoot", 1.) ] @ get_vx_frame_props () |> List.to_string_map)
            else
              [ ("should_shoot", 0.) ] |> List.to_string_map
          in
          Action.set enemy CASCADE ~frame_time ~frame_props)
        else
          Action.start_and_log enemy IDLE ~frame_time)
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
        let (x, vx_mult, x_alignment) : float * float * x_alignment =
          if get_frame_prop "random_facing_right" = 1. then (
            enemy.entity.sprite.facing_right <- true;
            (get_frame_prop "boss_area_left", 1., RIGHT_INSIDE))
          else (
            enemy.entity.sprite.facing_right <- false;
            (get_frame_prop "boss_area_right", -1., LEFT_INSIDE))
        in
        let y = get_frame_prop "random_wall_perch_y" in
        Entity.unhide enemy.entity;
        Entity.unfreeze enemy.entity;
        enemy.entity.dest.pos.x <- x;
        enemy.entity.dest.pos.y <- y;
        let projectile_duration =
          BOSS_AREA_X (get_frame_prop "boss_area_left", get_frame_prop "boss_area_right")
        in
        let vx = get_attr enemy "projectile_vx" in
        let v = { x = vx *. vx_mult; y = 0. } in
        enemy.projectiles <-
          [
            spawn_projectile enemy ~scale:0.5 ~pogoable:true ~v
              ~projectile_pos:(RELATIVE (x_alignment, CENTER))
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

  let get_args state game : args = { frame_time = state.frame.time; boss_area = get_boss_area game }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    let continue_action (action : Action.t) (performed : time) =
      match action with
      | VANISH -> Action.set enemy action ~frame_time
      | DASH ->
        if not (still_within_boss_area enemy args.boss_area) then
          set_prop enemy "should_vanish" 1.
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
              ("random_wall_perch_y", Random.y_in args.boss_area);
              ("boss_area_left", args.boss_area.pos.x);
              ("boss_area_right", args.boss_area.pos.x +. args.boss_area.w);
            ]
      | `DIVE ->
        Action.start_and_log enemy DIVE ~frame_time
          ~frame_props:[ ("random_dive_x", Random.x_in args.boss_area) ]
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
    let _ = frame_time in
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
          let projectile = make_frog_explosion enemy frame_time in
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
      if Entity.descending enemy.entity then (
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
    let _ = frame_props in
    let pose_name =
      match action with
      | SHOCK ->
        let action_name = action |> to_string in
        let shock_config = List.assoc action_name enemy.json.texture_configs in
        let projectile_duration =
          TIME_LEFT { seconds = (shock_config.count |> Int.to_float) *. shock_config.duration }
        in
        enemy.projectiles <-
          [
            spawn_projectile enemy ~projectile_texture_name:action_name ~v:(Zero.vector ())
              ~scale:1.6 ~pogoable:false
              ~projectile_pos:(RELATIVE (CENTER, CENTER))
              projectile_duration frame_time;
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

  let get_args state game : args =
    let _ = game in
    { frame_time = state.frame.time }

  let choose_behavior enemy args =
    let last_shock = action_started_at enemy "shock" in
    let shock_duration =
      (* needs to be greater than shock config (count * duration) *)
      get_attr enemy "shock_duration"
    in
    if last_shock.at < args.frame_time -. shock_duration then
      Action.start_and_log enemy SHOCK ~frame_time:args.frame_time
end

module Frog_bomb_actions = struct
  type t = NOTHING

  let to_string (action : t) : string =
    match action with
    | NOTHING -> "nothing"

  let from_string (s : string) : t =
    match s with
    | "nothing" -> NOTHING
    | _ -> failwithf "Frog_bomb_actions.from_string: %s" s

  let set (_enemy : enemy) ?(frame_props = String.Map.empty) (_action : t) ~frame_time =
    let _ = (frame_props, frame_time) in
    ()
end

module Frog_bomb : M = struct
  module Action = Make_loggable (Frog_bomb_actions)

  type args = { frame_time : float }

  let get_args state game : args =
    let _ = game in
    { frame_time = state.frame.time }

  let choose_behavior _enemy _args = ()
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
    let _ = frame_time in
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

  let get_args state game : args =
    let _ = game in
    { frame_time = state.frame.time }

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
    let _ = (frame_props, frame_time) in
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
        let vx_mult = if get_prop "projectile_direction_right" frame_props = 1. then 1. else -1. in
        let projectile scale gravity =
          let v = { x = vx_mult *. (2. -. gravity) *. get_attr enemy "projectile_vx"; y = 0. } in
          spawn_projectile enemy ~scale
            ~projectile_pos:(RELATIVE (CENTER, CENTER))
            ~v
            ~gravity_multiplier:(get_prop "gravity_multiplier" frame_props *. gravity)
            ~collide_with_floors:true projectile_duration frame_time
        in
        let new_projectiles =
          match enemy.level with
          | 1 -> [ projectile 0.7 1. ]
          | 2 -> [ projectile 1. 1.5; projectile 1. 1.; projectile 1. 0.5 ]
          | _ -> failwithf "invalid FLYING_HIPPIE level %d" enemy.level
        in
        enemy.projectiles <- new_projectiles @ enemy.projectiles;
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
      let new_vx, new_vy = chase_to enemy target_pos_x target_pos_y in
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
    let _ = frame_time in
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
      let new_vx, new_vy = chase_to enemy args.ghost_pos.x args.ghost_pos.y in
      Action.set enemy CHASE ~frame_time
        ~frame_props:([ ("new_vx", new_vx); ("new_vy", new_vy) ] |> List.to_string_map)
    in
    if get_bool_prop enemy "is_chasing" then
      handle_chasing ()
    else
      Action.handle_drifting enemy CHANGE_DIRECTION ~ghost_pos:args.ghost_pos ~frame_time
end

module Humbug_actions = struct
  type t =
    | CHANGE_DIRECTION
    | CHASE
    | CHARGE_DIVE
    | DIVE
    | DIVE_COOLDOWN

  let to_string (action : t) : string =
    match action with
    | CHANGE_DIRECTION -> "change_direction"
    | CHASE -> "chase"
    | CHARGE_DIVE -> "charge-dive"
    | DIVE -> "dive"
    | DIVE_COOLDOWN -> "dive-cooldown"

  let from_string (s : string) : t =
    match s with
    | "change_direction" -> CHANGE_DIRECTION
    | "chase" -> CHASE
    | "charge-dive" -> CHARGE_DIVE
    | "dive" -> DIVE
    | "dive-cooldown" -> DIVE_COOLDOWN
    | _ -> failwithf "Humbug_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let _ = frame_time in
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
        "chasing"
      | CHARGE_DIVE ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "charge-diving"
      | DIVE ->
        let dive_vx = get_prop "dive_vx" frame_props in
        let dive_vy = get_prop "dive_vy" frame_props in
        enemy.entity.v.x <- dive_vx;
        enemy.entity.v.y <- dive_vy;
        "diving"
      | DIVE_COOLDOWN ->
        enemy.entity.v.x <- 0.;
        enemy.entity.v.y <- 0.;
        "idle"
    in
    set_pose enemy pose_name
end

module Humbug : M = struct
  module Action = Make_loggable (Humbug_actions)

  type args = {
    frame_time : float;
    ghost_pos : vector;
  }

  let get_args state (game : game) : args =
    { frame_time = state.frame.time; ghost_pos = get_rect_center game.player.ghost.entity.dest }

  let choose_behavior (enemy : enemy) args =
    let ghost_pos = args.ghost_pos in
    let frame_time = args.frame_time in
    let handle_chasing () =
      let offset = get_attr enemy "target_offset" in
      let target_pos_x =
        if args.ghost_pos.x > rect_center_x enemy.entity.dest then
          args.ghost_pos.x -. offset
        else
          args.ghost_pos.x +. offset
      in
      let target_pos_y =
        if args.ghost_pos.y > rect_center_y enemy.entity.dest then
          args.ghost_pos.y -. offset
        else
          args.ghost_pos.y +. offset
      in
      let new_vx, new_vy = chase_to enemy target_pos_x target_pos_y in
      match enemy.last_performed with
      | None -> failwith "unreachable"
      | Some (action_name, action_time) -> (
        match Action.from_string action_name with
        | CHANGE_DIRECTION
        | CHASE ->
          face_ghost enemy ~ghost_pos;
          let last_dive = action_started_at enemy "dive" in
          let dive_duration = get_attr enemy "dive_duration" in
          if frame_time -. last_dive.at > dive_duration then
            Action.start_and_log enemy CHARGE_DIVE ~frame_time
          else
            Action.set enemy CHASE ~frame_time
              ~frame_props:([ ("new_vx", new_vx); ("new_vy", new_vy) ] |> List.to_string_map)
        | CHARGE_DIVE ->
          face_ghost enemy ~ghost_pos;
          let dx = rect_center_x enemy.entity.dest -. ghost_pos.x in
          let dy = rect_center_y enemy.entity.dest -. ghost_pos.y in
          let angle = atan (dy /. dx) in
          let v = get_attr enemy "dive_v" in
          let dive_vx, dive_vy =
            if dx < 0. then
              (v *. cos angle, v *. sin angle)
            else
              (-.v *. cos angle, -.v *. sin angle)
          in
          Action.handle_charging enemy ~charge_attr:"dive_charge_duration" ~ghost_pos ~action_time
            ~charge_action:CHARGE_DIVE ~action:DIVE ~frame_time ~get_frame_props:(fun () ->
              [ ("dive_vx", dive_vx); ("dive_vy", dive_vy) ])
        | DIVE ->
          let still_diving = still_airborne enemy ~frame_time ~action_time in
          if not still_diving then
            Action.start_and_log enemy DIVE_COOLDOWN ~frame_time
        | DIVE_COOLDOWN ->
          face_ghost enemy ~ghost_pos;
          let duration = get_attr enemy "dive_cooldown_duration" in
          let still_cooling_down = frame_time -. action_time.at > duration in
          if not still_cooling_down then
            Action.start_and_log enemy CHASE ~frame_time
              ~frame_props:[ ("new_vx", new_vx); ("new_vy", new_vy) ])
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
    | _ -> failwithf "Bat_actions.from_string: %s" s

  let set (enemy : enemy) ?(frame_props = String.Map.empty) (action : t) ~frame_time =
    let _ = frame_time in
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

  let get_args state (game : game) : args =
    let _ = game in
    { frame_time = state.frame.time }

  let choose_behavior (enemy : enemy) args =
    let frame_time = args.frame_time in
    (* check if v.x/y = 0. to change direction after recoiling from nail hit *)
    if
      enemy.entity.v.x = 0.
      || enemy.entity.v.y = 0.
      || List.length enemy.floor_collisions_this_frame > 0
    then (
      let v = Random.float_between (get_attr enemy "min_v") (get_attr enemy "max_v") in
      let angle = Random.float (2. *. Float.pi) in
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
    let _ = (frame_props, frame_time) in
    let pose_name =
      match action with
      | STANDING ->
        enemy.entity.v.x <- 0.;
        "idle"
      | WALK ->
        move_forward enemy (get_attr enemy "walk_vx");
        "walking"
      | CHARGE_PUNCH ->
        enemy.entity.v.x <- 0.;
        "charge-punch"
      | PUNCH ->
        move_forward enemy (get_attr enemy "punch_vx");
        "punch"
      | CHARGE_KICK ->
        enemy.entity.v.x <- 0.;
        "charge-kick"
      | KICK ->
        move_forward enemy (get_attr enemy "kick_vx");
        if Option.is_some enemy.entity.current_floor then
          enemy.entity.v.y <- get_attr enemy "jump_vy";
        "kick"
      | CHARGE_DASH ->
        enemy.entity.v.x <- 0.;
        "charge-dash"
      | DASH ->
        move_forward enemy (get_attr enemy "dash_vx");
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
    | None -> maybe_start_action ()
    | Some (action_name, action_time) -> (
      match Action.from_string action_name with
      | CHARGE_PUNCH -> handle_charge action_time CHARGE_PUNCH PUNCH
      | PUNCH ->
        let punch_duration = animation_loop_duration (String.Map.find "punch" enemy.textures) in
        let still_punching =
          Action.still_doing enemy "punch" ~duration:punch_duration ~frame_time
        in
        Action.maybe_continue enemy ~continue_action:PUNCH ~stop_action:WALK ~frame_time
          still_punching
      | CHARGE_KICK -> handle_charge action_time CHARGE_KICK KICK
      | KICK ->
        let still_kicking = still_airborne enemy ~frame_time ~action_time in
        Action.maybe_continue enemy ~continue_action:KICK ~stop_action:WALK ~frame_time
          still_kicking
      | CHARGE_DASH -> handle_charge action_time CHARGE_DASH DASH
      | DASH ->
        let still_dashing =
          Action.still_doing enemy "dash" ~duration:(get_attr enemy "dash_duration") ~frame_time
        in
        Action.maybe_continue enemy ~continue_action:DASH ~stop_action:WALK ~frame_time
          still_dashing
      | WALK ->
        if args.frame_time -. action_time.at > get_attr enemy "walk_duration" then
          maybe_start_action ()
        else (
          face_ghost enemy ~ghost_pos;
          Action.set enemy WALK ~frame_time)
      | STANDING -> maybe_start_action ())
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
    let _ = (frame_props, frame_time) in
    let pose_name =
      match action with
      | CHARGE_BITE ->
        enemy.entity.v.x <- 0.;
        "charge-bite"
      | BITE ->
        move_forward enemy (get_attr enemy "bite_vx");
        if Option.is_some enemy.entity.current_floor then
          enemy.entity.v.y <- get_attr enemy "bite_vy";
        "bite"
      | TURN ->
        enemy.entity.sprite.facing_right <- not enemy.entity.sprite.facing_right;
        move_forward enemy (get_attr enemy "walk_vx");
        "idle"
      | WALK ->
        move_forward enemy (get_attr enemy "walk_vx");
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
  | FROG_BOMB -> (module Frog_bomb)
  | ELECTRICITY -> (module Electricity)
  | FISH -> (module Fish)
  | PENGUIN -> (module Penguin)
  | HIPPIE -> (module Hippie)
  | HOPPING_HIPPIE -> (module Hopping_hippie)
  | HUMBUG -> (module Humbug)
  | FLYING_HIPPIE
  | FLYING_HIPPIE_2 ->
    (module Flying_hippie)
  | BIRD -> (module Bird)
  | BAT -> (module Bat)
  | MANICORN
  | MANICORN_2
  | MANICORN_3 ->
    (module Manicorn)
  | JOSHUA -> (module Joshua)
  | DEAN -> (module Dean)
  | VICE_DEAN_LAYBOURNE -> (module Vice_dean_laybourne)
  | BUDDY -> (module Buddy)
  | LUIS_GUZMAN -> (module Luis_guzman)
  | BORCHERT -> (module Borchert)
  | HICKEY -> (module Hickey)
  | LAVA_BRITTA
  | LAVA_BRITTA_2 ->
    (module Lava_britta)

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
      | BAT
      | BIRD
      | BORCHERT
      | BUDDY
      | DEAN
      | DUNCAN
      | ELECTRICITY
      | FISH
      | FLYING_HIPPIE
      | FROG
      | FROG_BOMB
      | HICKEY
      | HIPPIE
      | HOPPING_HIPPIE
      | HUMBUG
      | JOSHUA
      | LAVA_BRITTA
      | LOCKER_BOY
      | LUIS_GUZMAN
      | MANICORN
      | PENGUIN
      | VICE_DEAN_LAYBOURNE ->
        1
      | FLYING_HIPPIE_2
      | LAVA_BRITTA_2
      | MANICORN_2 ->
        2
      | MANICORN_3 -> 3
    in

    let texture_configs : texture_config ne_list =
      List.map (Entity.to_texture_config ENEMIES enemy_asset_dir_name) enemy_config.texture_configs
      |> List.to_ne_list
    in
    let entity, textures =
      match List.assoc_opt id !texture_cache with
      | None ->
        Entity.create_from_texture_configs ~gravity_multiplier:enemy_config.gravity_multiplier
          (Entity.scale_texture_configs (Config.scale.enemy *. enemy_config.scale) texture_configs)
          entity_dest
      | Some textures ->
        Entity.create_from_textures ~gravity_multiplier:enemy_config.gravity_multiplier
          texture_configs textures entity_dest
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
    let collision_shapes =
      match id with
      | BAT
      | BIRD
      | ELECTRICITY
      | FISH
      | FLYING_HIPPIE
      | FLYING_HIPPIE_2
      | FROG
      | FROG_BOMB
      | HIPPIE
      | HOPPING_HIPPIE
      | HUMBUG
      | MANICORN
      | MANICORN_2
      | MANICORN_3
      | PENGUIN
      | BORCHERT
      | BUDDY
      | DEAN
      | DUNCAN
      | JOSHUA
      | LAVA_BRITTA
      | LAVA_BRITTA_2
      | LOCKER_BOY
      | LUIS_GUZMAN
      | VICE_DEAN_LAYBOURNE ->
        []
      | HICKEY ->
        [
          [
            { x = 0. *. Config.window.scale; y = 90. *. Config.window.scale };
            { x = 475. *. Config.window.scale; y = 90. *. Config.window.scale };
            { x = 475. *. Config.window.scale; y = 225. *. Config.window.scale };
            { x = 240. *. Config.window.scale; y = 225. *. Config.window.scale };
            { x = 0. *. Config.window.scale; y = 120. *. Config.window.scale };
          ]
          |> make_shape;
          [
            { x = 475. *. Config.window.scale; y = 90. *. Config.window.scale };
            { x = 540. *. Config.window.scale; y = 70. *. Config.window.scale };
            { x = 540. *. Config.window.scale; y = 170. *. Config.window.scale };
            { x = 475. *. Config.window.scale; y = 210. *. Config.window.scale };
          ]
          |> make_shape;
          [
            { x = 540. *. Config.window.scale; y = 70. *. Config.window.scale };
            { x = 680. *. Config.window.scale; y = 70. *. Config.window.scale };
            { x = 680. *. Config.window.scale; y = 200. *. Config.window.scale };
            { x = 540. *. Config.window.scale; y = 170. *. Config.window.scale };
          ]
          |> make_shape;
          [
            { x = 300. *. Config.window.scale; y = 220. *. Config.window.scale };
            { x = 420. *. Config.window.scale; y = 220. *. Config.window.scale };
            { x = 430. *. Config.window.scale; y = 300. *. Config.window.scale };
            { x = 320. *. Config.window.scale; y = 320. *. Config.window.scale };
          ]
          |> make_shape;
        ]
    in
    {
      id;
      status;
      kind;
      level;
      entity;
      collision_shapes;
      damage = json.damage;
      health = { current = enemy_config.health; max = enemy_config.health };
      textures = textures |> List.Non_empty.to_list |> List.to_string_map;
      history = Enemy_action.Map.empty;
      last_performed = None;
      initial_pos = clone_vector entity.dest.pos;
      floor_collisions_this_frame = [];
      projectiles = [];
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
      | LAVA_BRITTA_2 -> "LAVA_BRITTA"
      | MANICORN_2
      | MANICORN_3 ->
        "MANICORN"
      | MANICORN
      | HIPPIE
      | HOPPING_HIPPIE
      | HUMBUG
      | JOSHUA
      | FLYING_HIPPIE
      | FISH
      | FROG
      | FROG_BOMB
      | ELECTRICITY
      | PENGUIN
      | VICE_DEAN_LAYBOURNE
      | LUIS_GUZMAN
      | BORCHERT
      | DEAN
      | BUDDY
      | LAVA_BRITTA
      | HICKEY
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
      { start_boss_killed_interaction = not (cutscene_name = ""); multiple_enemies }
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
