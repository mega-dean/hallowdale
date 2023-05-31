open Types
open Controls

[@@@ocaml.warning "-26-27-32"]

let parse_name name : ghost_id =
  match name with
  | "ABED" -> ABED
  | "ANNIE" -> ANNIE
  | "BRITTA" -> BRITTA
  | "JEFF" -> JEFF
  | "TROY" -> TROY
  | _ -> failwithf "bad ghost name '%s'" name

let available_ghost_ids ghosts : ghost_id list = ghosts |> List.filter (fun (_id, g) -> g.in_party) |> List.map fst

let maybe_begin_interaction ((state, game) : state * game) name =
  let name_prefix, _ = Utils.separate name '_' in
  let begin_interaction ?(increase_health = false) () =
    game.interaction.name <- Some name;
    game.interaction.steps <- Interactions.get_steps ~increase_health state game name
  in
  match name_prefix with
  | "info" ->
    (* these have no side effects and can be repeated *)
    begin_interaction ()
  | "health" ->
    (* these can be repeated, but health should only be increased once *)
    let increase_health = not (List.mem name game.room.progress.finished_interactions) in
    if increase_health then
      game.room.progress.finished_interactions <- name :: game.room.progress.finished_interactions;
    begin_interaction ~increase_health ()
  | "ability"
  | "dreamer"
  | "weapon"
  | "purple-pen"
  | "boss-killed"
  | "cutscene" ->
    (* these are only viewable once *)
    if not (List.mem name game.room.progress.finished_interactions) then (
      game.room.progress.finished_interactions <- name :: game.room.progress.finished_interactions;
      begin_interaction ())
  | _ -> failwithf "maybe_begin_interaction: unknown interaction prefix %s" name_prefix

let maybe_unset_current_floor (ghost : ghost) =
  match ghost.entity.current_floor with
  | None -> ()
  | Some floor ->
    let walked_over_edge = not (Entity.above_floor ghost.entity floor) in
    let jumping_over_edge = ghost.entity.v.y < 0. in
    if walked_over_edge || jumping_over_edge then
      ghost.entity.current_floor <- None

let find_trigger_collision ghost triggers =
  let colliding (_label, trigger_rect) = Option.is_some (Collision.with_entity ghost.entity trigger_rect) in
  List.find_opt colliding triggers

(* returns the first enemy collision *)
let get_enemy_collision (ghost : ghost) (room : room) : direction option =
  let find_colliding_enemy ((_enemy_id, enemy) : enemy_id * enemy) : direction option =
    if Enemy.is_dead enemy then
      None
    else if Collision.between_entities ghost.entity enemy.entity then (
      let direction : direction =
        (* TODO probably doing this in multiple places, move this to a function in Entity *)
        let mid_x rect = rect.pos.x +. (rect.w /. 2.) in
        if mid_x enemy.entity.dest > mid_x ghost.entity.dest then
          LEFT
        else
          RIGHT
      in
      Some direction)
    else
      None
  in
  List.find_map find_colliding_enemy room.enemies

let pogo ghost =
  ghost.entity.current_floor <- None;
  ghost.can_dash <- true;
  ghost.can_flap <- true;
  ghost.entity.y_recoil <- Some { speed = -800.; time_left = { seconds = 0.2 }; reset_v = true }

(* - adjusts x position for side collisions to place sprite next to colliding rect
   - then adjusts y position for top/bottom collisions
   - updates y velocity for bonking head, but other velocity updates happen elsewhere (set_pose)
*)
(* TODO this is pretty similar to Entity.apply_collisions now, so it might make sense to combine them *)
let apply_collisions (entity : entity) (collisions : (collision * rect) list) : unit =
  let adjust_position ((coll, floor) : collision * rect) =
    let top_of r = r.pos.y in
    let bottom_of r = r.pos.y +. r.h in
    let left_of r = r.pos.x in
    let right_of r = r.pos.x +. r.w in
    match coll.direction with
    | UP ->
      (* checks here prevent floor from being set for a single frame when jumping/pogoing over corners *)
      if Option.is_none entity.y_recoil then (
        entity.current_floor <- Some floor;
        entity.dest.pos.y <- top_of floor -. entity.dest.h)
    | DOWN ->
      if entity.v.y < 0. then (
        entity.v.y <- 0.;
        entity.dest.pos.y <- bottom_of floor)
    | LEFT -> entity.dest.pos.x <- left_of floor -. entity.dest.w
    | RIGHT -> entity.dest.pos.x <- right_of floor
  in
  let left_right_collisions, up_down_collisions =
    let is_left_right ((c, _) : collision * rect) = c.direction = LEFT || c.direction = RIGHT in
    List.partition is_left_right collisions
  in
  (* move sideways first to fix collisions with tiles stacked directly on top of each other
     - TODO this isn't working anymore
  *)
  List.iter adjust_position left_right_collisions;
  List.iter adjust_position up_down_collisions

let make_slash (ghost : ghost) (direction : direction) relative_pos (sprite : sprite) (frame_time : float) : slash =
  let textures = ghost.shared_textures in
  let vertical_slash = ref true in
  let slash_texture : texture =
    match direction with
    | LEFT
    | RIGHT ->
      vertical_slash := false;
      textures.slash
    | UP -> textures.upslash
    | DOWN -> textures.downslash
  in
  let count =
    match slash_texture.animation_src with
    (* TODO slashes should use PARTICLE, and probably just raise error otherwise *)
    | STILL _ -> 1
    | PARTICLE animation
    | LOOPED animation ->
      List.length animation.frames
  in
  let src_w = Raylib.Texture.width slash_texture.image / count |> Int.to_float in
  let src_h = Raylib.Texture.height slash_texture.image |> Int.to_float in
  let dest_w', dest_h' = (src_w *. Config.scale.slash, src_h *. Config.scale.slash) in
  let dest_w, dest_h =
    if !vertical_slash then
      (dest_w', dest_h' *. ghost.current_weapon.scale_y)
    else
      (dest_w' *. ghost.current_weapon.scale_x, dest_h')
  in
  let dest =
    (* pos is arbitrary because this will be adjusted to parent dest based on child's relative_position *)
    { pos = Zero.vector (); h = dest_h; w = dest_w }
  in

  (* TODO move these to config *)
  let sideways_points =
    [
      { x = 0.; y = 0. };
      { x = 111.; y = 8. };
      { x = 158.; y = 16. };
      { x = 184.; y = 26. };
      { x = 196.; y = 37. };
      { x = 195.; y = 49. };
      { x = 178.; y = 64. };
      { x = 136.; y = 77. };
      { x = 60.; y = 86. };
      { x = 1.; y = 87. };
    ]
  in
  let up_points =
    [
      { x = 0.; y = 189. };
      { x = 4.; y = 110. };
      { x = 12.; y = 63. };
      { x = 25.; y = 25. };
      { x = 35.; y = 8. };
      { x = 45.; y = 0. };
      { x = 60.; y = 1. };
      { x = 73.; y = 14. };
      { x = 87.; y = 46. };
      { x = 97.; y = 86. };
      { x = 103.; y = 189. };
    ]
  in
  let down_points = List.map (fun (point : vector) -> { point with y = 189. -. point.y }) up_points in
  let make_scaled_shape ?(scale = Config.scale.slash) points =
    let scale_point point = { x = point.x *. scale; y = point.y *. scale } in
    let scaled_points = List.map scale_point points in
    make_shape scaled_points
  in
  let sprite =
    (* slash will have a sprite with None .collision because there is a (non-optional) .collision on slash itself *)
    Sprite.spawn_particle "slash" slash_texture ~facing_right:sprite.facing_right dest frame_time
  in
  let collision =
    match direction with
    | DOWN -> SHAPE (make_scaled_shape down_points)
    | UP -> SHAPE (make_scaled_shape up_points)
    | _ -> SHAPE (make_scaled_shape sideways_points)
  in
  { sprite; direction; collision }

let get_spell_sprite (ghost : ghost) : (sprite * time) option =
  match ghost.child with
  | None -> None
  | Some child -> (
    match child.kind with
    | WRAITHS -> Some (child.sprite, ghost.history.cast_wraiths.started)
    | DIVE -> Some (child.sprite, ghost.history.cast_dive.started)
    | DIVE_COOLDOWN -> Some (child.sprite, ghost.history.dive_cooldown.started)
    | _ -> None)

let get_current_slash (ghost : ghost) : slash option =
  let get_slash (child : ghost_child) : slash option =
    match child.kind with
    | NAIL slash -> Some slash
    | _ -> None
  in
  Option.bind ghost.child get_slash

let get_dive_sprite (ghost : ghost) : sprite option =
  let get_sprite (child : ghost_child) : sprite option =
    match child.kind with
    | DIVE
    | DIVE_COOLDOWN ->
      Some child.sprite
    | _ -> None
  in
  Option.bind ghost.child get_sprite

let get_particle_child_sprite (ghost : ghost) : sprite option =
  let get_sprite (child : ghost_child) : sprite option =
    match child.kind with
    | DIVE_COOLDOWN
    | WRAITHS ->
      Some child.sprite
    | _ -> None
  in
  Option.bind ghost.child get_sprite

let get_current_attack_direction (ghost : ghost) : direction option =
  match get_current_slash ghost with
  | None -> None
  | Some slash -> Some slash.direction

let get_focus_sparkles ghost : sprite option =
  let get_sprite (child : ghost_child) : sprite option =
    match child.kind with
    | FOCUS -> Some child.sprite
    | _ -> None
  in
  Option.bind ghost.child get_sprite

let get_damage (ghost : ghost) (damage_kind : damage_kind) =
  (* TODO check ghost.abilities.descending_dark/shade_soul *)
  match damage_kind with
  | NAIL -> ghost.weapons |> List.map snd |> List.map (fun (w : Json_t.weapon) -> w.damage) |> List.fold_left ( + ) 0
  | VENGEFUL_SPIRIT -> 15
  | DESOLATE_DIVE -> 15
  | DESOLATE_DIVE_SHOCKWAVE -> 20
  | HOWLING_WRAITHS -> (* TODO this should be 13 with multihits *) 26

let resolve_slash_collisions (state : state) (game : game) =
  match get_current_slash game.ghost with
  | None -> ()
  | Some slash ->
    let resolve_enemy ((_enemy_id, enemy) : enemy_id * enemy) =
      if enemy.status.check_damage_collisions then (
        match Collision.with_slash slash enemy.entity.sprite with
        | None -> ()
        | Some collision ->
          if
            Enemy.maybe_take_damage state enemy game.ghost.history.nail.started NAIL (get_damage game.ghost NAIL)
              enemy.entity.dest
          then (
            match collision.direction with
            | DOWN -> pogo game.ghost
            | LEFT
            | RIGHT ->
              (* TODO recoil enemy *)
              Entity.recoil_backwards game.ghost.entity { speed = 800.; time_left = { seconds = 0.1 }; reset_v = true }
            | UP ->
              if game.ghost.entity.v.y < 0. then
                game.ghost.entity.v.y <- 300.;
              game.ghost.soul.current <-
                Utils.bound_int 0 (game.ghost.soul.current + Config.action.soul_gained_per_nail) game.ghost.soul.max);
          if slash.direction = DOWN && game.ghost.entity.y_recoil = None then (
            let check_projectile_pogo (projectile : projectile) =
              if projectile.pogoable then (
                match Collision.with_slash slash projectile.entity.sprite with
                | None -> ()
                | Some _ -> pogo game.ghost)
            in
            List.iter check_projectile_pogo enemy.spawned_projectiles))
    in
    let resolve_layer (layer : layer) =
      let new_tile_groups : tile_group list ref = ref [] in
      let spawn_fragment (collision : collision) (e : entity) =
        let new_fragment = Entity.clone e in
        new_fragment.dest.pos <- { x = collision.rect.pos.x; y = collision.rect.pos.y };
        new_fragment.v <- { x = Random.float 501. -. 200.; y = Random.float 1000. -. 1000. };
        new_fragment.update_pos <- true;
        new_fragment
      in
      let destroy_object (tile_group : tile_group) (collision : collision) =
        layer.spawned_fragments <- List.map (spawn_fragment collision) tile_group.fragments @ layer.spawned_fragments;
        let idx = List.nth tile_group.tile_idxs 0 in
        (match List.assoc_opt idx game.room.idx_configs with
        | Some (PURPLE_PEN name) -> maybe_begin_interaction (state, game) name
        | _ -> ());
        layer.destroyed_tiles <- tile_group.tile_idxs @ layer.destroyed_tiles;
        if layer.config.permanently_removable then (
          (* only doors should be permanently destroyed in state.progress - decorations refresh when a room is re-entered
             - but this key still needs the layer.name so that render.ml can still draw other layers at that idx
          *)
          let existing =
            match List.assoc_opt layer.name game.room.progress.removed_idxs_by_layer with
            | None -> []
            | Some idxs -> idxs
          in
          game.room.progress.removed_idxs_by_layer <-
            Utils.assoc_replace layer.name (tile_group.tile_idxs @ existing) game.room.progress.removed_idxs_by_layer);
        (* collision direction is opposite of slash direction *)
        tmp "got collision with direction %s, with layer %s pogoable %b" (Show.direction collision.direction) layer.name
          layer.config.pogoable;
        if collision.direction = DOWN && layer.config.pogoable then
          pogo game.ghost;
        match tile_group.stub_sprite with
        | None -> ()
        | Some sprite ->
          layer.spawned_stub_sprites <- (sprite, tile_group.transformation_bits) :: layer.spawned_stub_sprites
      in
      let resolve_tile_group (tile_group : tile_group) =
        match Collision.with_slash' slash tile_group.dest with
        | None -> new_tile_groups := tile_group :: !new_tile_groups
        | Some coll -> (
          match tile_group.door_health with
          | None -> destroy_object tile_group coll
          | Some door_health ->
            if door_health.last_hit_at < game.ghost.history.nail.started.at then (
              door_health.last_hit_at <- state.frame.time;
              if door_health.hits > 1 then (
                let get_random_fragment_idx () = Random.int (List.length tile_group.fragments - 1) in
                let make_random_fragment _n = List.nth tile_group.fragments (get_random_fragment_idx ()) in
                let random_fragments = List.init (Random.int 3) make_random_fragment in
                layer.spawned_fragments <- List.map (spawn_fragment coll) random_fragments @ layer.spawned_fragments;
                door_health.hits <- door_health.hits - 1;
                new_tile_groups := tile_group :: !new_tile_groups)
              else
                destroy_object tile_group coll)
            else
              new_tile_groups := tile_group :: !new_tile_groups;
            ())
      in
      List.iter resolve_tile_group layer.tile_groups;
      layer.tile_groups <- !new_tile_groups
    in
    let layers = List.filter (fun (l : layer) -> l.config.destroyable) game.room.layers in
    List.iter resolve_layer layers;
    List.iter resolve_enemy game.room.enemies

(* state updates for current pose, texture animation, and sprite *)
let set_pose ghost (new_pose : ghost_pose) (frame_time : float) =
  let update_vx multiplier =
    let mult = if ghost.entity.sprite.facing_right then multiplier else -1. *. multiplier in
    ghost.entity.v.x <- mult *. Config.ghost.vx
  in
  (* TODO bad name *)
  let reset_standing_abilities () =
    if Entity.on_ground ghost.entity then (
      ghost.can_dash <- true;
      ghost.can_flap <- true)
  in
  let set_facing_right ?(allow_vertical = true) (direction : direction) =
    match direction with
    | LEFT -> ghost.entity.sprite.facing_right <- false
    | RIGHT -> ghost.entity.sprite.facing_right <- true
    | _ ->
      if not allow_vertical then
        failwithf "bad direction in set_facing_right: %s" (Show.direction direction)
  in
  let handle_cast (spell_kind : spell_kind) =
    match spell_kind with
    | VENGEFUL_SPIRIT ->
      Entity.recoil_backwards ghost.entity { speed = 80.; time_left = { seconds = 0.16666 }; reset_v = true };
      ghost.entity.v.y <- 0.;
      ghost.textures.cast
    | DESOLATE_DIVE -> ghost.textures.dive
    | HOWLING_WRAITHS ->
      ghost.entity.v.x <- 0.;
      ghost.entity.v.y <- 0.;
      ghost.textures.cast
  in

  let handle_action_kind action_kind =
    match action_kind with
    | ATTACK direction ->
      (* handle_attacking is called after handle_walking, so this allows the ghost to walk backwards while attacking *)
      set_facing_right direction;
      ghost.textures.nail
    | DASH ->
      ghost.can_dash <- false;
      ghost.entity.v.y <- 0.;
      update_vx 2.;
      ghost.textures.dash
    | CAST DESOLATE_DIVE ->
      update_vx 0.;
      ghost.entity.v.y <- Config.ghost.dive_vy;
      ghost.textures.dive
    | CAST spell_kind -> handle_cast spell_kind
    | DIVE_COOLDOWN ->
      update_vx 0.;
      ghost.textures.dive
    | FOCUS ->
      update_vx 0.;
      ghost.textures.focus
    | TAKE_DAMAGE direction ->
      let x_recoil_speed =
        match direction with
        | LEFT -> -800.
        | RIGHT -> 800.
        | _ -> if ghost.entity.sprite.facing_right then 800. else -800.
      in
      ghost.entity.x_recoil <- Some { speed = x_recoil_speed; time_left = { seconds = 0.06666 }; reset_v = true };
      ghost.entity.y_recoil <- Some { speed = -800.; time_left = { seconds = 0.06666 }; reset_v = true };
      ghost.health.current <- ghost.health.current - 1;
      if ghost.health.current = 0 then
        (* TODO handle dying:
           - reset in-progress boss interactions
           - respawn somewhere
        *)
        ghost.health.current <- ghost.health.max;
      (* from recoiling upwards *)
      ghost.entity.current_floor <- None;
      ghost.textures.take_damage
    | JUMP ->
      ghost.entity.v.y <- Config.ghost.jump_vy;
      ghost.entity.current_floor <- None;
      ghost.textures.jump
    | WALL_KICK ->
      ghost.entity.v.y <- Config.ghost.wall_jump_vy;
      update_vx 1.;
      ghost.textures.jump
    | FLAP ->
      (* TODO separate config *)
      ghost.entity.v.y <- Config.ghost.jump_vy *. 0.8;
      ghost.textures.flap
  in

  let next_texture : texture =
    match new_pose with
    | PERFORMING action_kind -> handle_action_kind action_kind
    | AIRBORNE new_vy ->
      ghost.entity.v.y <- new_vy;
      ghost.entity.current_floor <- None;
      if ghost.entity.v.y > Config.physics.jump_fall_threshold then
        ghost.textures.fall
      else
        ghost.textures.jump
    | CRAWLING ->
      update_vx 0.;
      ghost.textures.crawl
    | IDLE ->
      reset_standing_abilities ();
      update_vx 0.;
      ghost.textures.idle
    | LANDING floor ->
      ghost.entity.v.y <- 0.;
      ghost.entity.current_floor <- Some floor;
      ghost.can_dash <- true;
      ghost.can_flap <- true;
      ghost.textures.jump
    | READING ->
      update_vx 0.;
      ghost.textures.read
    | WALKING d ->
      reset_standing_abilities ();
      set_facing_right ~allow_vertical:false d;
      update_vx 1.;
      ghost.textures.walk
    | WALL_SLIDING wall ->
      let wall_to_the_left = wall.pos.x < ghost.entity.sprite.dest.pos.x in
      ghost.entity.sprite.facing_right <- wall_to_the_left;
      ghost.current.wall <- Some wall;
      ghost.entity.sprite.dest.pos.x <-
        (if wall_to_the_left then
           wall.pos.x +. wall.w
        else
          wall.pos.x -. (Config.ghost.width *. Config.scale.ghost));
      update_vx 0.;
      ghost.can_dash <- true;
      ghost.can_flap <- true;
      ghost.textures.wall_slide
  in
  Entity.update_sprite_texture ghost.entity next_texture

let past_cooldown pose_frames frame_time : bool = pose_frames.blocked_until.at < frame_time

let spawn_vengeful_spirit ?(start = None) ?(direction : direction option = None) state game =
  let texture = game.ghost.shared_textures.vengeful_cushion in
  let w, h = get_scaled_texture_size ~scale:Config.scale.ghost texture in
  let x, y =
    match start with
    | None -> (game.ghost.entity.dest.pos.x, game.ghost.entity.dest.pos.y)
    | Some v -> (v.x, v.y)
  in
  let dest = { pos = { x; y }; w; h } in
  let facing_right =
    match direction with
    | None -> game.ghost.entity.sprite.facing_right
    | Some d -> (
      match d with
      | LEFT -> false
      | RIGHT -> true
      | _ -> failwithf "spawn_vengeful_spirit invalid direction: %s" (Show.direction d))
  in
  let vengeful_spirit : sprite =
    {
      ident = fmt "vengeful_spirit %d" (List.length game.ghost.spawned_vengeful_spirits);
      texture;
      dest;
      facing_right;
      collision = Some DEST (* TODO add VS collision shape *);
    }
  in
  let vx =
    if facing_right then
      Config.action.vengeful_spirit_vx
    else
      -1. *. Config.action.vengeful_spirit_vx
  in
  let projectile : projectile =
    {
      entity = Entity.create_for_sprite ~v:{ x = vx; y = 0. } vengeful_spirit dest;
      despawn = TIME_LEFT { seconds = Config.action.vengeful_spirit_duration };
      spawned = { at = state.frame.time };
      pogoable = true;
    }
  in

  game.ghost.spawned_vengeful_spirits <- projectile :: game.ghost.spawned_vengeful_spirits

let make_ghost_child (ghost : ghost) kind relative_pos texture w h =
  let name = Show.ghost_child_kind kind in
  let sprite = Sprite.create name texture { pos = Entity.get_child_pos ghost.entity relative_pos w h; w; h } in
  Some { kind; relative_pos; sprite }

let start_action ?(debug = false) ((state, game) : state * game) (action_kind : ghost_action_kind) =
  let cooldown_scale = ref 1.0 in
  let action : ghost_action =
    match action_kind with
    | ATTACK direction ->
      let relative_pos =
        match direction with
        | UP -> ALIGNED (CENTER, BOTTOM)
        | DOWN -> ALIGNED (CENTER, TOP)
        | LEFT
        | RIGHT ->
          IN_FRONT
      in
      let slash = make_slash game.ghost direction relative_pos game.ghost.entity.sprite state.frame.time in
      game.ghost.child <- Some { kind = NAIL slash; relative_pos; sprite = slash.sprite };
      cooldown_scale := game.ghost.current_weapon.cooldown_scale;
      game.ghost.history.nail
    | DASH -> game.ghost.history.dash
    | CAST spell_kind -> (
      game.ghost.soul.current <- game.ghost.soul.current - Config.action.soul_per_cast;
      match spell_kind with
      | VENGEFUL_SPIRIT ->
        spawn_vengeful_spirit state game;
        game.ghost.history.cast_vs
      | DESOLATE_DIVE ->
        let w, h =
          (* TODO these are temporarily scaled so the dive.png image can be reused *)
          (game.ghost.entity.dest.w *. 5., game.ghost.entity.dest.h *. 5.)
        in
        game.ghost.child <-
          make_ghost_child game.ghost DIVE (ALIGNED (CENTER, BOTTOM)) game.ghost.shared_textures.desolate_dive w h;
        game.ghost.history.cast_dive
      | _ ->
        game.ghost.child <-
          make_ghost_child game.ghost WRAITHS
            (ALIGNED (CENTER, BOTTOM))
            game.ghost.shared_textures.howling_wraiths (game.ghost.entity.dest.w *. 5.) (game.ghost.entity.dest.h *. 5.);
        game.ghost.history.cast_wraiths)
    | TAKE_DAMAGE _ ->
      state.camera.shake <- 0.5;
      (* TODO cancel focus *)
      game.ghost.history.take_damage
    | DIVE_COOLDOWN -> game.ghost.history.dive_cooldown
    | FOCUS ->
      game.ghost.soul.at_focus_start <- game.ghost.soul.current;
      game.ghost.soul.health_at_focus_start <- game.ghost.health.current;
      game.ghost.soul.last_decremented <- { at = state.frame.time };
      let focus_sparkles_sprite =
        Sprite.create "focus-sparkles" game.ghost.shared_textures.focus_sparkles
          { game.ghost.entity.dest with w = game.ghost.entity.dest.w *. 2. }
      in
      game.ghost.child <- Some { kind = FOCUS; relative_pos = ALIGNED (CENTER, CENTER); sprite = focus_sparkles_sprite };
      game.ghost.history.focus
    | JUMP -> game.ghost.history.jump
    | FLAP -> game.ghost.history.flap
    | WALL_KICK -> game.ghost.history.wall_kick
  in
  action.started <- { at = state.frame.time };
  action.doing_until.at <- state.frame.time +. action.config.duration.seconds;
  action.blocked_until.at <- state.frame.time +. (action.config.cooldown.seconds *. !cooldown_scale);
  set_pose game.ghost (PERFORMING action_kind) state.frame.time

let maybe_despawn_child ((state, game) : state * game) sprite =
  match Sprite.advance_or_despawn state.frame.time sprite.texture sprite with
  | None -> game.ghost.child <- None
  | Some slash_sprite -> Sprite.advance_animation state.frame.time sprite.texture sprite

let continue_action (game : game) (action_kind : ghost_action_kind) (frame_time : float) =
  (match action_kind with
  | FOCUS ->
    (let decr_dt =
       game.ghost.history.focus.config.duration.seconds /. (Config.action.soul_per_cast + 0 |> Int.to_float)
     in
     (* TODO probably should be checking >= *)
     if
       game.ghost.soul.at_focus_start - game.ghost.soul.current > Config.action.soul_per_cast
       && game.ghost.soul.health_at_focus_start = game.ghost.health.current
     then
       game.ghost.health.current <- Utils.bound_int 0 game.ghost.health.max (game.ghost.health.current + 1)
     else if frame_time -. game.ghost.soul.last_decremented.at > decr_dt then (
       game.ghost.soul.current <- game.ghost.soul.current - 1;
       game.ghost.soul.last_decremented <- { at = frame_time }));
    ()
  | _ -> ());
  set_pose game.ghost (PERFORMING action_kind) frame_time

let is_doing (ghost : ghost) (action_kind : ghost_action_kind) (frame_time : float) : bool =
  let check_action action = action.started.at <= frame_time && frame_time <= action.doing_until.at in
  match action_kind with
  | ATTACK _ -> Option.is_some (get_current_slash ghost)
  | FOCUS -> Option.is_some (get_focus_sparkles ghost)
  | CAST spell_kind -> (
    match spell_kind with
    | VENGEFUL_SPIRIT -> check_action ghost.history.cast_vs
    | HOWLING_WRAITHS -> check_action ghost.history.cast_wraiths
    | DESOLATE_DIVE -> ghost.current.is_diving)
  | DASH -> check_action ghost.history.dash
  | DIVE_COOLDOWN -> check_action ghost.history.dive_cooldown
  | _ -> failwithf "is_doing - invalid action %s" (Show.ghost_action_kind action_kind)

let is_casting ((state, game) : state * game) =
  is_doing game.ghost (CAST VENGEFUL_SPIRIT) state.frame.time
  || game.ghost.current.is_diving
  || is_doing game.ghost (CAST HOWLING_WRAITHS) state.frame.time

let swap_current_ghost (state : state) (game : game) ?(swap_pos = true) target_ghost_id =
  match List.assoc_opt target_ghost_id game.ghosts with
  | None -> failwithf "could not find other ghost %s" (Show.ghost_id target_ghost_id)
  | Some new_ghost ->
    let old_ghost = game.ghost in
    let new_ghosts =
      (* CLEANUP there's probably a way for this to work by keeping all ghosts in game.ghosts, but swapping them out works for now
         - this is causing a few weird issues, like the wrong ghost being unhidden on initial load, and double velocity updates being applied to the ghost
      *)
      (old_ghost.id, old_ghost) :: List.remove_assoc new_ghost.id game.ghosts
    in
    let current_pos = old_ghost.entity.dest.pos in

    (* MAKE_CURRENT_GHOST uses this fn during interactions to update game.ghost, but shouldn't swap places *)
    if swap_pos then (
      Entity.unhide_at new_ghost.entity current_pos;
      new_ghost.entity.sprite.facing_right <- old_ghost.entity.sprite.facing_right;
      new_ghost.entity.v <- old_ghost.entity.v;

      Entity.hide old_ghost.entity;
      old_ghost.entity.current_floor <- None);

    new_ghost.abilities <- old_ghost.abilities;
    new_ghost.health <- old_ghost.health;
    new_ghost.soul <- old_ghost.soul;
    new_ghost.can_dash <- old_ghost.can_dash;
    new_ghost.can_flap <- old_ghost.can_flap;
    new_ghost.child <- old_ghost.child;
    new_ghost.spawned_vengeful_spirits <- old_ghost.spawned_vengeful_spirits;

    game.ghost <- new_ghost;
    game.ghosts <- new_ghosts

let cycle_current_ghost (state : state) (game : game) =
  let ghost_ids = available_ghost_ids game.ghosts in
  tmp "got %d ghosts" (List.length game.ghosts);
  if List.length ghost_ids > 0 then (
    let sorted_ghost_ids : ghost_id list = List.filter (fun id -> id > game.ghost.id) ghost_ids |> List.sort compare in
    let new_ghost_id =
      match List.nth_opt sorted_ghost_ids 0 with
      | Some id -> id
      | None -> List.nth (ghost_ids |> List.sort compare) 0
    in
    swap_current_ghost state game new_ghost_id)

let change_ability ?(debug = false) ?(only_enable = false) ghost ability_name =
  let new_val v =
    if debug then
      print "toggling %s -> %b" ability_name (not v);
    if only_enable then true else not v
  in
  match ability_name with
  | "vengeful_spirit" -> ghost.abilities.vengeful_spirit <- new_val ghost.abilities.vengeful_spirit
  | "desolate_dive" -> ghost.abilities.desolate_dive <- new_val ghost.abilities.desolate_dive
  | "howling_wraiths" -> ghost.abilities.howling_wraiths <- new_val ghost.abilities.howling_wraiths
  | "mothwing_cloak" -> ghost.abilities.mothwing_cloak <- new_val ghost.abilities.mothwing_cloak
  | "mantis_claw" -> ghost.abilities.mantis_claw <- new_val ghost.abilities.mantis_claw
  | "crystal_heart" -> ghost.abilities.crystal_heart <- new_val ghost.abilities.crystal_heart
  | "monarch_wings" -> ghost.abilities.monarch_wings <- new_val ghost.abilities.monarch_wings
  | _ -> failwithf "change_ability bad ability name: %s" ability_name

let enable_ability ghost ability_name = change_ability ~only_enable:true ghost ability_name
let toggle_ability ghost ability_name = change_ability ~debug:true ghost ability_name

let acquire_weapon ((state, game) : state * game) weapon_name =
  match List.assoc_opt weapon_name state.global.weapons with
  | Some weapon_config ->
    let current_weapon_names = List.map fst game.ghost.weapons in
    if not (List.mem weapon_name current_weapon_names) then
      game.ghost.weapons <- (weapon_name, weapon_config) :: game.ghost.weapons
  | None -> failwithf "acquire_weapon bad weapon name: %s" weapon_name

let equip_weapon (ghost : ghost) weapon_name =
  match List.assoc_opt weapon_name ghost.weapons with
  | None -> print "can't equip %s, not in ghost.weapons" weapon_name
  | Some weapon_config ->
    ghost.current_weapon <-
      (let config = weapon_config.tint in
       {
         name = weapon_name;
         tint = Raylib.Color.create config.r config.g config.b config.a;
         scale_x = weapon_config.scale_x;
         scale_y = weapon_config.scale_y;
         cooldown_scale = 2. -. weapon_config.swing_speed;
       })

let get_invincibility_kind ((state, game) : state * game) : invincibility_kind option =
  if not (past_cooldown game.ghost.history.take_damage state.frame.time) then
    Some TOOK_DAMAGE
  else if game.ghost.current.is_diving || not (past_cooldown game.ghost.history.dive_cooldown state.frame.time) then
    Some DIVE_IFRAMES
  else
    None

let is_vulnerable ((state, game) : state * game) : bool = Option.is_none (get_invincibility_kind (state, game))

(* this is used for actions that block other actions from happening during the same frame *)
type handled_action = { this_frame : bool }

let update (game : game) (state : state) =
  let stop_wall_sliding = ref false in
  let key_pressed_or_buffered key_action =
    let (input, buffer) : frame_input * float =
      match key_action with
      | NAIL -> (state.frame_inputs.nail, game.ghost.history.nail.config.input_buffer.seconds)
      | JUMP -> (state.frame_inputs.jump, game.ghost.history.jump.config.input_buffer.seconds)
      | DASH -> (state.frame_inputs.dash, game.ghost.history.dash.config.input_buffer.seconds)
      | CAST -> (state.frame_inputs.cast, game.ghost.history.cast_vs.config.input_buffer.seconds)
      | _ -> failwithf "bad key in key_pressed_or_buffered': %s" (show_key_action key_action)
    in
    let input_buffered () =
      match input.down_since with
      | None -> false
      | Some down_since_time -> input.down && state.frame.time -. buffer < down_since_time.at
    in
    input.pressed || input_buffered ()
  in
  let set_pose' (pose : ghost_pose) = set_pose game.ghost pose state.frame.time in
  let set_interaction_pose' (ghost : ghost) (pose : ghost_pose) = set_pose ghost pose state.frame.time in
  let handle_debug_keys () =
    let dv =
      if state.debug.enabled then
        Config.ghost.small_debug_v /. 3.
      else
        Config.ghost.debug_v
    in
    if key_down DEBUG_UP then
      game.ghost.entity.dest.pos.y <- game.ghost.entity.dest.pos.y -. dv
    else if key_down DEBUG_DOWN then
      game.ghost.entity.dest.pos.y <- game.ghost.entity.dest.pos.y +. dv
    else if key_down DEBUG_RIGHT then
      game.ghost.entity.dest.pos.x <- game.ghost.entity.dest.pos.x +. dv
    else if key_down DEBUG_LEFT then
      game.ghost.entity.dest.pos.x <- game.ghost.entity.dest.pos.x -. dv
    else if key_pressed DEBUG_1 then (
      tmp "cycling current ghost";
      cycle_current_ghost state game
      (* state.loaded_state <- MAIN_MENU main_menu *)
      (* print "current weapons: %s" (game.ghost.weapons |> List.map fst |> join) *)
      (* toggle_ability game.ghost "mantis_claw" *)
      (* toggle_ability game.ghost "vengeful_spirit" *)
      (* print "%s" (Show.shape_lines (Option.get game.ghost.entity.sprite.collision)) *))
    else if key_pressed DEBUG_2 then (
      (* toggle_ability game.ghost "monarch_wings" *)
      game.ghost.soul.current <- game.ghost.soul.max;
      toggle_ability game.ghost "desolate_dive")
    else if key_pressed DEBUG_3 then (
      let weapon_name = "Devil's Drench XJ-11" in
      tmp "acquiring weapon %s" weapon_name;
      acquire_weapon (state, game) weapon_name
      (* equip_weapon game.ghost "Orange Paintball Gun" *)
      (* maybe_begin_interaction state "boss-killed_LOCKER_BOY" *)
      (* () *)
      (* toggle_ability game.ghost "howling_wraiths" *))
    else if key_pressed DEBUG_4 then
      print "ghost x: %0.1f, y: %0.1f" game.ghost.entity.dest.pos.x game.ghost.entity.dest.pos.y
  in

  let get_global_pos (current : vector) (room_location : room_location) : vector =
    { x = current.x +. room_location.global_x; y = current.y +. room_location.global_y }
  in
  let get_local_pos (global : vector) (room_id : room_id) (world : world) : vector =
    let room_location = List.assoc room_id world in
    { x = global.x -. room_location.global_x; y = global.y -. room_location.global_y }
  in

  (* TODO need to block inputs during transitions to prevent re-exiting immediately and warping
     - also bound velocity to prevent falling through floors with high vy through horizontal doors
  *)
  let handle_room_exits () =
    let colliding exit_rect =
      (* TODO this might not be working sometimes with the new collision detection *)
      Collision.with_entity game.ghost.entity exit_rect
    in
    match List.find_map colliding game.room.exits with
    | None -> false
    | Some collision ->
      if collision.rect.w < 10. || collision.rect.h < 10. then
        (* don't trigger the exit immediately when the ghost hits the edge of the screen *)
        false
      else (
        let cr = collision.rect in
        let current_room_location = List.assoc game.room.id state.world in
        let ghost_pos = game.ghost.entity.dest.pos in
        let global_ghost_pos = get_global_pos ghost_pos current_room_location in
        let (path, target_room_id) : string * room_id =
          let global_x, global_y =
            ( cr.pos.x +. (cr.w /. 2.) +. current_room_location.global_x,
              cr.pos.y +. (cr.h /. 2.) +. current_room_location.global_y )
          in
          Tiled.Room.locate state.world global_x global_y
        in
        let room_location = List.assoc target_room_id state.world in
        let exits = Tiled.Room.get_exits room_location in
        let start_pos' = get_local_pos global_ghost_pos target_room_id state.world in
        let start_pos : vector =
          tmp "got room exit direction: %s" (Show.direction collision.direction);

          (* fixes ghost.facing_right, and adjusts the ghost to be further from the edge of screen *)
          match collision.direction with
          | LEFT ->
            game.ghost.entity.sprite.facing_right <- true;
            { start_pos' with x = start_pos'.x +. game.ghost.entity.dest.w }
          | RIGHT ->
            game.ghost.entity.sprite.facing_right <- false;
            { start_pos' with x = start_pos'.x -. game.ghost.entity.dest.w }
          | UP -> { start_pos' with y = start_pos'.y +. game.ghost.entity.dest.h }
          | DOWN -> { start_pos' with y = start_pos'.y -. game.ghost.entity.dest.h }
        in

        let room_uuid = Tiled.Room.get_uuid game.room in
        game.progress <- Utils.assoc_replace room_uuid game.room.progress game.progress;
        let room =
          Room.init
            {
              file_name = path;
              progress = game.progress;
              exits;
              enemy_configs = state.global.enemy_configs;
              npc_configs = state.global.npc_configs;
              pickup_indicator_texture = state.global.textures.pickup_indicator;
            }
        in
        game.ghost.entity.current_floor <- None;
        game.ghost.current.wall <- None;
        game.ghost.spawned_vengeful_spirits <- [];
        game.ghost.entity.dest.pos <- start_pos;
        (* all rooms are using the same tilesets now, but still unload them here (and re-load them
           in load_room) every time because the tilesets could be in a different order per room
           - not really sure about ^this comment, I don't know if different tileset order would break the
             tile lookup code now, so just unload/reload to be safe ¯\_(ツ)_/¯
        *)
        (* TODO probably need to unload things like enemy textures *)
        Tiled.Room.unload_tilesets game.room;
        game.room <- room;
        game.room.layers <- Tiled.Room.get_layer_tile_groups game.room game.room.progress.removed_idxs_by_layer;
        state.camera.raylib <- Tiled.create_camera_at (Raylib.Vector2.create start_pos.x start_pos.y) 0.;
        true)
  in

  let handle_interactions () =
    (* the ghost can only collide with one trigger at a time *)
    let check_for_new_interactions () : bool =
      let interactable_triggers = game.room.triggers.lore @ game.room.triggers.item_pickups in
      (match find_trigger_collision game.ghost interactable_triggers with
      | None -> ()
      | Some (name, _rect) ->
        (* TODO maybe want to also check `Entity.on_ground game.ghost.entity` *)
        if key_pressed INTERACT then
          maybe_begin_interaction (state, game) name);
      (match find_trigger_collision game.ghost game.room.triggers.cutscene with
      | None -> ()
      | Some (name, _rect) -> maybe_begin_interaction (state, game) name);
      List.length game.interaction.steps > 0
    in
    let speed_through_interaction =
      (* holding down d-nail will skip through interactions quickly, but still perform each step
         once so side-effects like gaining abilities still happen
      *)
      state.frame_inputs.d_nail.down
    in
    match game.interaction.text with
    | Some _text ->
      if key_pressed INTERACT then (
        (* press interact key to advance dialogue by one, with a short pause to
           prevent double-pressing and skipping a message *)
        game.interaction.text <- None;
        game.interaction.steps <- STEP (WAIT 0.4) :: game.interaction.steps)
      else if speed_through_interaction then
        game.interaction.text <- None;
      true
    | None -> (
      match List.nth_opt game.interaction.steps 0 with
      | None -> check_for_new_interactions ()
      | Some next_step ->
        let new_wait = ref 0. in
        let still_walking =
          (* it is a little weird that this is only used for walking since it makes it the only synchronous action
             (ie jumping happens and then the next step executes the following frame)
          *)
          ref false
        in
        let tile_w, tile_h = (game.room.json.tile_w, game.room.json.tile_h) in

        (* could divide these up into some smaller groups like TEXT or LAYER to get rid of more of the duplication, but
           probably not really worth it *)
        let handle_general_step (general_step : Interaction.general_step) =
          match general_step with
          | INITIALIZE_INTERACTIONS remove_nail ->
            game.ghost.entity.v <- Zero.vector ();
            if remove_nail then (
              match get_current_slash game.ghost with
              | Some slash -> game.ghost.child <- None
              | None -> ())
          | FADE_SCREEN_OUT -> state.screen_faded <- true
          | FADE_SCREEN_IN -> state.screen_faded <- false
          | TEXT paragraphs ->
            let text : Interaction.text = { content = paragraphs; increases_health = false } in
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
          | FOCUS_ABILITY_TEXT (top_paragraphs, outline_src, bottom_paragraphs) ->
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (FOCUS_ABILITY { top_paragraphs; outline_src; bottom_paragraphs })
          | ABILITY_TEXT (outline_src, bottom_paragraphs) ->
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (ABILITY { top_paragraphs = []; outline_src; bottom_paragraphs })
          | SET_FIXED_CAMERA (tile_x, tile_y) ->
            let tx, ty = Tiled.Tile.tile_coords ~tile_w ~tile_h (tile_x, tile_y) in
            let x, y = (tx *. Config.scale.room, ty *. Config.scale.room) in
            state.camera.subject <- FIXED { x; y }
          | SET_GHOST_CAMERA ->
            game.ghost.entity.update_pos <- true;
            state.camera.subject <- GHOST
          | WAIT time -> new_wait := time -. state.frame.dt
          | HIDE_BOSS_DOORS ->
            (match List.find_opt (fun (l : layer) -> l.name = "hidden-boss-doors") game.room.layers with
            | None -> failwith "HIDE_BOSS_DOORS, expected hidden-boss-doors layer"
            | Some layer_to_hide -> layer_to_hide.hidden <- true);
            game.room.layers <- Tiled.Room.get_layer_tile_groups game.room game.room.progress.removed_idxs_by_layer
          | UNHIDE_BOSS_DOORS ->
            (match List.find_opt (fun (l : layer) -> l.name = "hidden-boss-doors") game.room.layers with
            | None -> failwith "UNHIDE_BOSS_DOORS, expected hidden-boss-doors layer"
            | Some layer_to_hide -> layer_to_hide.hidden <- false);
            game.room.layers <- Tiled.Room.get_layer_tile_groups game.room game.room.progress.removed_idxs_by_layer
          | HIDE_LAYER layer_name -> (
            match List.find_opt (fun (l : layer) -> l.name = layer_name) game.room.layers with
            | None -> ()
            | Some layer_to_hide -> layer_to_hide.hidden <- true)
          | UNHIDE_LAYER layer_name -> (
            match List.find_opt (fun (l : layer) -> l.name = layer_name) game.room.layers with
            | None -> ()
            | Some layer_to_unhide -> layer_to_unhide.hidden <- false)
          | SWAP_HIDDEN_LAYER layer_name ->
            let layer_to_hide : layer = List.find (fun (l : layer) -> l.name = layer_name) game.room.layers in
            let layer_to_unhide : layer =
              List.find (fun (l : layer) -> l.name = fmt "hidden-%s" layer_name) game.room.layers
            in
            layer_to_hide.hidden <- true;
            layer_to_unhide.hidden <- false
          | SPAWN_VENGEFUL_SPIRIT (direction, end_tile_x, end_tile_y) ->
            let tx, ty = Tiled.Tile.tile_coords ~tile_w ~tile_h (end_tile_x, end_tile_y) in
            let end_x, end_y = (tx *. Config.scale.room, ty *. Config.scale.room) in
            let vs_length =
              (* end_tile_x is off by a few tiles to the left, but this step is probably only going to be
                 used once so it's probably not worth fixing *)
              Config.action.vengeful_spirit_duration *. Config.action.vengeful_spirit_vx
            in
            let start_x = end_x -. vs_length in
            let v = { x = start_x; y = end_y } in
            spawn_vengeful_spirit ~start:(Some v) ~direction:(Some direction) state game
          | DIALOGUE (speaker, str) ->
            let text : Interaction.text = { content = [ str ]; increases_health = false } in
            game.interaction.speaker_name <- Some speaker;
            game.interaction.text <- Some (DIALOGUE (speaker, text))
          | PURPLE_PEN_TEXT lines ->
            game.ghost.entity.update_pos <- false;
            game.ghost.entity.v <- Zero.vector ();
            let text : Interaction.text = { content = lines; increases_health = false } in
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
        in

        let handle_entity_step (entity : entity) (entity_step : Interaction.entity_step) =
          match entity_step with
          | SET_FACING direction -> Entity.set_facing direction entity
          | UNHIDE -> Entity.unhide entity
          | UNHIDE_AT (start_tile_x, start_tile_y, x_offset, y_offset) ->
            let tx, ty = Tiled.Tile.tile_coords ~tile_w ~tile_h (start_tile_x, start_tile_y) in
            let x, y = ((tx +. x_offset) *. Config.scale.room, (ty +. y_offset) *. Config.scale.room) in
            Entity.unhide_at entity { x; y }
          | HIDE -> Entity.hide entity
          | FREEZE -> Entity.freeze entity
          | UNFREEZE -> Entity.unfreeze entity
        in

        let add_item (item_kind : Interaction.item_kind) =
          Room.update_pickup_indicators (state, game);
          match item_kind with
          | ABILITY ability_name -> enable_ability game.ghost ability_name
          | DREAMER (item_name, dreamer_item_text) ->
            let text : Interaction.text =
              { content = [ fmt "Got the dreamer item %s" item_name; ""; dreamer_item_text ]; increases_health = false }
            in
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
          | WEAPON weapon_name ->
            let weapon_config = List.assoc weapon_name state.global.weapons in
            let text : Interaction.text =
              { content = [ fmt "Acquired the %s" weapon_name; weapon_config.pickup_text ]; increases_health = false }
            in
            acquire_weapon (state, game) weapon_name;
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
        in

        let handle_ghost_step ghost (step : Interaction.ghost_step) =
          match step with
          | ADD_TO_PARTY -> ghost.in_party <- true
          | REMOVE_FROM_PARTY -> ghost.in_party <- false
          | JUMP (_direction, vx) ->
            ghost.entity.v.x <- vx;
            ghost.entity.current_floor <- None;
            set_interaction_pose' ghost (PERFORMING JUMP)
          | SET_POSE pose -> set_interaction_pose' ghost pose
          | MAKE_CURRENT_GHOST -> swap_current_ghost state ~swap_pos:false game ghost.id
          | ENTITY entity_step ->
            (match entity_step with
            | HIDE ->
              if ghost.id = game.ghost.id then
                failwithf "can't hide current ghost %s" (Show.ghost_id ghost.id)
            | UNHIDE -> failwithf "can't unhide %s, use UNHIDE_AT for ghosts" (Show.ghost_id ghost.id)
            | _ -> ());
            handle_entity_step ghost.entity entity_step
          | WALK_TO target_tile_x ->
            let tx, _ = Tiled.Tile.tile_coords ~tile_w ~tile_h (target_tile_x, 1) in
            let dist = (tx *. Config.scale.room) -. ghost.entity.dest.pos.x in
            still_walking := abs_float dist > 10.;
            if not !still_walking then
              set_interaction_pose' ghost IDLE
            else if dist > 0. then
              set_interaction_pose' ghost (WALKING RIGHT)
            else
              set_interaction_pose' ghost (WALKING LEFT)
          | FILL_LIFE_VAPOR -> ghost.soul.current <- ghost.soul.max
          | INCREASE_HEALTH_TEXT (increases_health, str) ->
            if increases_health then (
              ghost.health.max <- ghost.health.max + 1;
              ghost.health.current <- ghost.health.max);
            let text : Interaction.text = { content = [ str ]; increases_health } in
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
          | ADD_ITEM item_kind -> add_item item_kind
        in

        let handle_enemy_step enemy_id (enemy_step : Interaction.enemy_step) =
          let enemies : enemy list =
            List.filter (fun (_, (e : enemy)) -> e.id = enemy_id) game.room.enemies |> List.map snd
          in
          let apply_to_all fn = List.iter fn enemies in
          let apply_to_only step_name fn =
            if List.length enemies <> 1 then
              failwithf "can't use %s when there are multiple enemies" step_name
            else
              fn (List.nth enemies 0)
          in
          match enemy_step with
          | WALK_TO target_tile_x ->
            apply_to_only "WALK_TO" (fun (e : enemy) ->
                let tx, _ = Tiled.Tile.tile_coords ~tile_w ~tile_h (target_tile_x, 1) in
                let dist = (tx *. Config.scale.room) -. e.entity.dest.pos.x in
                still_walking := abs_float dist > 10.;
                if not !still_walking then (
                  e.entity.v.x <- 0.;
                  Enemy.set_pose e "idle")
                else if dist > 0. then (
                  e.entity.sprite.facing_right <- true;
                  Enemy.start_action e "walking" state.frame.time [])
                else (
                  e.entity.sprite.facing_right <- false;
                  Enemy.start_action e "walking" state.frame.time []))
          | SET_VX new_vx -> apply_to_only "SET_VX" (fun (e : enemy) -> e.entity.v.x <- new_vx)
          | SET_POSE pose_name ->
            apply_to_all (fun (e : enemy) ->
                Enemy.log_action e pose_name state.frame.time;
                Enemy.set_pose e pose_name)
          | SET_ACTION action_name ->
            apply_to_all (fun (e : enemy) -> Enemy.start_action e action_name state.frame.time [])
          | ENTITY entity_step ->
            let choose_behavior =
              match entity_step with
              | UNFREEZE -> true
              | _ -> false
            in
            apply_to_all (fun (e : enemy) ->
                e.status.choose_behavior <- choose_behavior;
                e.status.check_damage_collisions <- choose_behavior;
                handle_entity_step e.entity entity_step)
        in

        let set_npc_pose (npc : npc) pose =
          let texture = List.assoc pose npc.textures in
          Entity.update_sprite_texture npc.entity texture
        in

        let handle_npc_step (npc : npc) (npc_step : Interaction.npc_step) =
          match npc_step with
          | ENTITY entity_step -> handle_entity_step npc.entity entity_step
          | WALK_TO target_tile_x ->
            let tx, _ = Tiled.Tile.tile_coords ~tile_w ~tile_h (target_tile_x, 1) in
            let dist = (tx *. Config.scale.room) -. npc.entity.dest.pos.x in
            still_walking := abs_float dist > 10.;
            if not !still_walking then (
              npc.entity.v.x <- 0.;
              set_npc_pose npc "idle")
            else if dist > 0. then (
              npc.entity.sprite.facing_right <- true;
              (* TODO use a config (maybe npc-specific configs, but a global
                 config for shared things like walk_vx might be nice)
              *)
              npc.entity.v.x <- 400.;
              set_npc_pose npc "walking")
            else (
              npc.entity.sprite.facing_right <- false;
              npc.entity.v.x <- -400.;
              set_npc_pose npc "walking")
          | SET_POSE target_pose -> set_npc_pose npc target_pose
        in

        (match next_step with
        | STEP general_step -> handle_general_step general_step
        | CURRENT_GHOST ghost_step -> handle_ghost_step game.ghost ghost_step
        | GHOST (ghost_id, ghost_step) ->
          let find_ghost () : ghost =
            match List.assoc_opt ghost_id game.ghosts with
            | Some ghost -> ghost
            | None ->
              if game.ghost.id = ghost_id then
                game.ghost
              else
                failwithf "ghost_id %s needs to be the current_ghost, or should be in game.ghosts"
                  (Show.ghost_id ghost_id)
          in
          handle_ghost_step (find_ghost ()) ghost_step
        | NPC (target_npc_id, npc_step) ->
          let find_existing_npc () =
            match List.find_opt (fun (npc : npc) -> npc.id = target_npc_id) game.room.npcs with
            | None -> failwithf "can't find npc %s" (Show.npc_id target_npc_id)
            | Some npc -> npc
          in
          handle_npc_step (find_existing_npc ()) npc_step
        | ENEMY (enemy_id, enemy_step) -> handle_enemy_step enemy_id enemy_step);

        if !still_walking then
          ( (* leave the current step on top of the stack *) )
        else if !new_wait > 0. && not speed_through_interaction then
          game.interaction.steps <- STEP (WAIT !new_wait) :: List.tl game.interaction.steps
        else
          game.interaction.steps <- List.tl game.interaction.steps;
        true)
  in

  let handle_casting () : handled_action =
    let trying_cast =
      key_pressed_or_buffered CAST
      && game.ghost.soul.current >= Config.action.soul_per_cast
      && past_cooldown game.ghost.history.cast_vs state.frame.time
      && (not (is_doing game.ghost (ATTACK RIGHT) state.frame.time))
      && not (is_casting (state, game))
    in

    let this_frame =
      if trying_cast then (
        let cast_spell spell_kind =
          start_action ~debug:true (state, game) (CAST spell_kind);
          true
        in
        let can_howl () =
          game.ghost.abilities.howling_wraiths
          && past_cooldown game.ghost.history.cast_wraiths state.frame.time
          && state.frame_inputs.up.down
        in
        let can_dive () =
          game.ghost.abilities.desolate_dive
          && past_cooldown game.ghost.history.cast_dive state.frame.time
          && state.frame_inputs.down.down
        in
        let can_vs () =
          game.ghost.abilities.vengeful_spirit && past_cooldown game.ghost.history.cast_vs state.frame.time
        in
        if can_howl () then
          cast_spell HOWLING_WRAITHS
        else if can_dive () then (
          game.ghost.current.is_diving <- true;
          cast_spell DESOLATE_DIVE)
        else if can_vs () then
          cast_spell VENGEFUL_SPIRIT
        else
          false)
      else (
        let continuing_spell_kind =
          (* this should not be checking DESOLATE_DIVE because that ends when landing on a floor *)
          List.find_opt (fun s -> is_doing game.ghost (CAST s) state.frame.time) [ VENGEFUL_SPIRIT; HOWLING_WRAITHS ]
        in

        match continuing_spell_kind with
        | None ->
          if game.ghost.current.is_diving then
            if Option.is_some game.ghost.entity.current_floor then (
              state.camera.shake <- 1.;
              game.ghost.current.is_diving <- false;
              start_action (state, game) DIVE_COOLDOWN;
              game.ghost.child <-
                (let dest = game.ghost.entity.dest in
                 let relative_pos = ALIGNED (CENTER, BOTTOM) in
                 let w, h =
                   (* TODO this scaling is temporary so the current dive.png can be used *) (dest.w *. 15., dest.h *. 3.)
                 in
                 make_ghost_child game.ghost DIVE_COOLDOWN relative_pos game.ghost.shared_textures.dive_shockwave w h);
              false)
            else (
              match get_dive_sprite game.ghost with
              | None -> false
              | Some dive_sprite ->
                Sprite.advance_animation state.frame.time dive_sprite.texture dive_sprite;
                continue_action game (CAST DESOLATE_DIVE) state.frame.time;
                true)
          else
            false
        | Some kind ->
          continue_action game (CAST kind) state.frame.time;
          true)
    in

    { this_frame }
  in

  let handle_dashing () : handled_action =
    let starting_dash () =
      (* attack direction is arbitrary *)
      game.ghost.abilities.mothwing_cloak
      && key_pressed_or_buffered DASH
      && (not (is_doing game.ghost (ATTACK RIGHT) state.frame.time))
      && (not (is_casting (state, game)))
      && Option.is_none game.ghost.entity.x_recoil
      && game.ghost.can_dash
      && past_cooldown game.ghost.history.dash state.frame.time
    in
    let still_dashing () = is_doing game.ghost DASH state.frame.time in
    let this_frame =
      if starting_dash () then (
        (* TODO "dash in currently-facing direction" almost always works, but not when walljumping,
           so this should probably be `DASHING of direction` and check LEFT/RIGHT keys
        *)
        start_action (state, game) DASH;
        stop_wall_sliding := true;
        true)
      else if still_dashing () then (
        continue_action game DASH state.frame.time;
        true)
      else
        false
    in
    { this_frame }
  in

  let handle_walking () =
    (* this fn always calls set_pose' to update vx, but it is often overridden by
       another call to set_pose' later in the same frame (eg. if the player is also attacking/jumping/etc) *)
    let move_right () =
      (match game.ghost.current.wall with
      | None -> ()
      | Some wall ->
        if wall.pos.x < game.ghost.entity.sprite.dest.pos.x then
          stop_wall_sliding := true);
      set_pose' (WALKING RIGHT)
    in
    let move_left () =
      (match game.ghost.current.wall with
      | None -> ()
      | Some wall ->
        if wall.pos.x > game.ghost.entity.sprite.dest.pos.x then
          stop_wall_sliding := true);
      set_pose' (WALKING LEFT)
    in
    match (state.frame_inputs.left.down_since, state.frame_inputs.right.down_since) with
    | Some left_down_since, Some right_down_since ->
      if left_down_since > right_down_since then
        move_left ()
      else
        move_right ()
    | None, Some _time -> move_right ()
    | Some _time, None -> move_left ()
    | None, None -> set_pose' IDLE
  in

  let handle_wall_kicking () : handled_action =
    let starting_wall_kick () = Option.is_some game.ghost.current.wall && key_pressed_or_buffered JUMP in
    let continuing_wall_kick () =
      state.frame.time -. game.ghost.history.wall_kick.started.at < game.ghost.history.wall_kick.config.duration.seconds
    in
    let this_frame =
      if starting_wall_kick () then (
        stop_wall_sliding := true;
        start_action (state, game) WALL_KICK;
        true)
      else if continuing_wall_kick () then (
        stop_wall_sliding := true;
        continue_action game WALL_KICK state.frame.time;
        true)
      else
        false
    in
    { this_frame }
  in

  let handle_jumping new_vy =
    match game.ghost.entity.current_floor with
    | None ->
      (* TODO key_pressed_or_buffered detects the same keypress as the initial jump *)
      if game.ghost.abilities.monarch_wings && game.ghost.can_flap && key_pressed JUMP then (
        game.ghost.can_flap <- false;
        (* TODO this should be a delayed action like focus/c-dash so the ghost dips a little before using wings again *)
        set_pose' (PERFORMING FLAP)
        (* TODO continue flapping
           else if (is_doing state FLAPPING) then
             set_pose' FLAPPING
        *))
      else
        set_pose' (AIRBORNE new_vy)
    | Some floor ->
      if key_pressed_or_buffered JUMP then
        (* TODO `start_action state JUMP` also works, but JUMP doesn't use blocked_until/doing_until *)
        set_pose' (PERFORMING JUMP)
      else if game.ghost.entity.v.y > 0. then (
        stop_wall_sliding := true;
        (* TODO this is a very naive hardfall check, probably need to keep track of "airborne_duration" to check dy
           - airborne_duration would be reset when can_dash/flap are reset (pogo, wall_slide)
        *)
        if game.ghost.entity.v.y >= Config.ghost.max_vy then
          state.camera.shake <- 1.;
        set_pose' (LANDING floor))
  in

  let handle_attacking () =
    let starting_attack () = past_cooldown game.ghost.history.nail state.frame.time && key_pressed_or_buffered NAIL in
    if starting_attack () then (
      let direction : direction =
        match (state.frame_inputs.up.down, state.frame_inputs.down.down) with
        | true, false -> UP
        | false, true ->
          if Entity.on_ground game.ghost.entity then
            if game.ghost.entity.sprite.facing_right then RIGHT else LEFT
          else
            DOWN
        | _, _ ->
          (* don't really care about socd for up/down *)
          if game.ghost.entity.sprite.facing_right then RIGHT else LEFT
      in
      start_action (state, game) (ATTACK direction))
    else (
      match get_current_slash game.ghost with
      | None -> ()
      | Some slash ->
        continue_action game (ATTACK slash.direction) state.frame.time;
        maybe_despawn_child (state, game) slash.sprite)
  in

  let handle_focusing () : handled_action =
    (* TODO-4 add a delay before deducting soul *)
    (* TODO-5 add charge_action that can happen before start_action
       - also used for c-dash, dive, wings, nail arts, etc. so it needs to be a reusable way to handle this
    *)
    let starting_focus () =
      state.frame_inputs.focus.pressed
      && Entity.on_ground game.ghost.entity
      && game.ghost.soul.current >= Config.action.soul_per_cast
      && not (is_doing game.ghost (ATTACK RIGHT) state.frame.time)
    in
    let still_focusing () = is_doing game.ghost FOCUS state.frame.time && state.frame_inputs.focus.down in
    let this_frame =
      if starting_focus () then (
        start_action (state, game) FOCUS;
        true)
      else if still_focusing () then (
        continue_action game FOCUS state.frame.time;
        (match get_focus_sparkles game.ghost with
        | None -> ()
        | Some sprite -> Sprite.advance_animation state.frame.time sprite.texture sprite);
        true)
      else (
        if state.frame_inputs.focus.released then
          game.ghost.child <- None;
        false)
    in
    { this_frame }
  in

  let handle_wall_sliding collisions =
    let high_enough_to_slide_on wall =
      let wall_bottom_y = wall.pos.y +. wall.h in
      game.ghost.entity.dest.pos.y +. (game.ghost.entity.dest.h /. 2.) < wall_bottom_y
    in
    let still_wall_sliding = Option.is_some game.ghost.current.wall in
    if game.ghost.abilities.mantis_claw then
      if !stop_wall_sliding then
        game.ghost.current.wall <- None
      else if still_wall_sliding then (
        let wall = Option.get game.ghost.current.wall in
        if high_enough_to_slide_on wall then
          set_pose' (WALL_SLIDING wall)
        else
          game.ghost.current.wall <- None)
      else (
        game.ghost.current.wall <- None;
        if not (is_doing game.ghost (ATTACK RIGHT) state.frame.time) then (
          let check_collision ((c : collision), wall) =
            if (not (Entity.on_ground game.ghost.entity)) && Entity.descending game.ghost.entity then
              if high_enough_to_slide_on wall then (
                match c.direction with
                | LEFT -> set_pose' (WALL_SLIDING wall)
                | RIGHT -> set_pose' (WALL_SLIDING wall)
                | _ -> ())
          in
          List.iter check_collision collisions))
  in

  handle_debug_keys ();

  Entity.apply_v state.frame.dt game.ghost.entity;

  let new_vy =
    let vy' =
      let vy = game.ghost.entity.v.y in
      let ascending = vy < 0. in
      let dvy = Config.physics.gravity *. state.frame.dt in
      if key_up JUMP && ascending then
        (vy *. Config.physics.jump_damping) +. dvy
      else (
        match game.ghost.current.wall with
        (* could check current_floor here and set to 0, but this already happens by set_pose *)
        | Some _ -> Config.ghost.wall_slide_vy
        | None -> vy +. dvy)
    in
    Utils.bound (-1. *. Config.ghost.max_vy) Config.ghost.max_vy vy'
  in
  let exiting = handle_room_exits () in
  if not exiting then (
    let interacting = handle_interactions () in
    if interacting then
      Entity.update_pos game.room game.ghost.entity state.frame.dt
    else (
      let colliding (_label, trigger_rect) = Option.is_some (Collision.with_entity game.ghost.entity trigger_rect) in
      (match List.find_opt colliding game.room.triggers.shadows with
      | None -> ()
      | Some (layer_name, _rect) -> (
        match List.find_opt (fun (l : layer) -> l.name = layer_name) game.room.layers with
        | None -> failwithf "Ghost.update: could not find layer '%s' to reveal" layer_name
        | Some layer ->
          if not (List.mem layer_name game.room.progress.revealed_shadow_layers) then
            game.room.progress.revealed_shadow_layers <- layer_name :: game.room.progress.revealed_shadow_layers;
          layer.hidden <- true));
      (* TODO-3 add c-dashing *)
      let in_dive_cooldown = (not game.ghost.current.is_diving) && is_doing game.ghost DIVE_COOLDOWN state.frame.time in
      if in_dive_cooldown then
        continue_action game DIVE_COOLDOWN state.frame.time
      else (
        let focusing = handle_focusing () in
        if not focusing.this_frame then (
          let dashing = handle_dashing () in
          if not dashing.this_frame then (
            let casting = handle_casting () in
            if not casting.this_frame then (
              let wall_kicking = handle_wall_kicking () in
              if not wall_kicking.this_frame then (
                handle_walking ();
                handle_jumping new_vy);
              handle_attacking ();
              resolve_slash_collisions state game))))));

  (match get_particle_child_sprite game.ghost with
  | None -> ()
  | Some dive_sprite -> maybe_despawn_child (state, game) dive_sprite);
  if is_vulnerable (state, game) then (
    match get_enemy_collision game.ghost game.room with
    | Some direction -> start_action (state, game) (TAKE_DAMAGE direction)
    | None -> ());
  let collisions = Entity.get_floor_collisions game.room game.ghost.entity in
  state.debug.rects <- List.map (fun c -> (Raylib.Color.green, snd c)) collisions;
  apply_collisions game.ghost.entity collisions;
  handle_wall_sliding collisions;
  maybe_unset_current_floor game.ghost;
  Sprite.advance_animation state.frame.time game.ghost.entity.sprite.texture game.ghost.entity.sprite;
  state

let load_shared_textures (shared_texture_configs : (string * texture_config) list) =
  let build_shared_texture ?(particle = false) ?(config_name = None) pose_name =
    let config =
      let config_name' =
        (* the "nail" shared config applies to "slash"/"upslash"/"downslash", but otherwise the pose_name *)
        Option.value config_name ~default:pose_name
      in
      match List.assoc_opt config_name' shared_texture_configs with
      | Some config ->
        (* the `with pose_name` is also because of the "nail" config naming *)
        { config with pose_name }
      | None ->
        {
          asset_dir = GHOSTS;
          character_name = "shared";
          pose_name;
          x_offset = 0.;
          y_offset = 0.;
          duration = { seconds = 0. };
          count = 1;
        }
    in

    Sprite.build_texture_from_config ~particle config
  in

  let build_slash_texture name = build_shared_texture ~particle:true ~config_name:(Some "nail") name in
  {
    slash = build_slash_texture "slash";
    upslash = build_slash_texture "upslash";
    downslash = build_slash_texture "downslash";
    shine = build_shared_texture "shine";
    health = build_shared_texture "health";
    energon_pod = build_shared_texture "energon-pod";
    vengeful_cushion = build_shared_texture "vengeful-cushion";
    desolate_dive = build_shared_texture "desolate-dive";
    dive_shockwave = build_shared_texture ~particle:true "desolate-dive";
    howling_wraiths = build_shared_texture ~particle:true "howling-wraiths";
    focus_sparkles = build_shared_texture "focus-sparkles";
  }

(* CLEANUP too many args *)
let init
    (ghost_id : ghost_id)
    (in_party : bool)
    (idle_texture : texture)
    (action_config : (string * ghost_action_config) list)
    (start_pos : vector)
    (abilities : Json_t.ghost_abilities)
    weapons
    textures
    shared_textures
    max_health =
  let use_json_action_config action_name : ghost_action_config =
    match List.assoc_opt action_name action_config with
    | None -> failwithf "could not find action config for '%s' in ghosts/config.json" action_name
    | Some config -> config
  in
  let dest = Sprite.make_dest start_pos.x start_pos.y idle_texture in
  let make_action (config_name : string) : ghost_action =
    {
      doing_until = { at = -1000. };
      blocked_until = { at = -1000. };
      config = use_json_action_config config_name;
      started = { at = -1000. };
    }
  in

  {
    id = ghost_id;
    in_party;
    current = { wall = None; is_diving = false };
    textures;
    child = None;
    history =
      {
        focus = make_action "focus";
        dash = make_action "dash";
        cast_vs = make_action "cast-vs";
        cast_dive = make_action "cast-dive";
        dive_cooldown = make_action "dive-cooldown";
        cast_wraiths = make_action "cast-wraiths";
        take_damage = make_action "take-damage";
        nail = make_action "nail";
        jump = make_action "jump";
        wall_kick = make_action "wall-kick";
        flap = make_action "flap";
      };
    shared_textures;
    entity =
      Entity.create_for_sprite
        (Sprite.create (fmt "ghost-%s" (Show.ghost_id ghost_id)) ~collision:(Some DEST) idle_texture dest)
        { pos = dest.pos; w = Config.ghost.width *. Config.scale.ghost; h = Config.ghost.height *. Config.scale.ghost };
    can_dash = true;
    can_flap = true;
    health = { current = max_health; max = max_health };
    soul =
      {
        current = Config.action.max_soul;
        max = Config.action.max_soul;
        at_focus_start = 0;
        health_at_focus_start = 0;
        last_decremented = { at = 0. };
      };
    spawned_vengeful_spirits = [];
    abilities;
    weapons;
    current_weapon =
      (* TODO should read these from weapons.json (but do this after adding save files) *)
      { name = "old-nail"; tint = Raylib.Color.raywhite; scale_x = 1.; scale_y = 1.; cooldown_scale = 1. };
  }
