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

let read_config () : ghosts_file =
  let ghost_file : Json_t.ghosts_file = File.read_config "ghosts" Json_j.ghosts_file_of_string in
  let parse_texture_config character_name ((pose_name, ghost_pose) : string * Json_t.texture_config)
      : texture_config =
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
  let parse_ghost_texture
      ((ghost_id_str, ghost_poses) : string * (string * Json_t.texture_config) list) :
      ghost_id * texture_config list =
    let parse_name name : ghost_id =
      match name with
      | "ABED" -> ABED
      | "ANNIE" -> ANNIE
      | "BRITTA" -> BRITTA
      | "JEFF" -> JEFF
      | "TROY" -> TROY
      | _ -> failwithf "bad ghost name '%s'" name
    in
    let texture_configs : texture_config list =
      List.map (parse_texture_config ghost_id_str) ghost_poses
    in
    (parse_name ghost_id_str, texture_configs)
  in
  let textures = List.map parse_ghost_texture ghost_file.individual_textures in
  let shared_textures : (string * texture_config) list =
    let parse_texture_config' (s, tc) : string * texture_config =
      (s, parse_texture_config "shared" (s, tc))
    in
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
  let actions : (string * ghost_action_config) list =
    List.map parse_ghost_action ghost_file.actions
  in
  { actions; textures; shared_textures }

let available_ghost_ids ghosts : ghost_id list =
  ghosts |> List.filter (fun (_id, g) -> g.in_party) |> List.map fst

let maybe_begin_interaction (state : state) (game : game) name =
  let name_prefix, _ = Utils.separate name '_' in
  let begin_interaction ?(increase_health = false) () =
    game.interaction.name <- Some name;
    game.interaction.steps <- Interactions.get_steps ~increase_health state game name
  in
  match name_prefix with
  | "warp"
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
  let colliding (_label, trigger_rect) =
    Option.is_some (Collision.with_entity ghost.entity trigger_rect)
  in
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
  ghost.current.can_dash <- true;
  ghost.current.can_flap <- true;
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
      entity.y_recoil <- None;
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

let make_slash
    (ghost : ghost)
    (direction : direction)
    relative_pos
    (sprite : sprite)
    (frame_time : float) : slash =
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
    | STILL _
    | ONCE _
    | LOOPED _ ->
      failwith "invalid slash texture animation_src - should be PARTICLE"
    | PARTICLE animation -> List.length animation.frames
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
    (* pos is somewhat arbitrary because this will be adjusted to parent dest based on child's relative_position
       - TODO for some reason, it stays in this position for 2 frames before being adjusted
    *)
    { pos = clone_vector ghost.entity.dest.pos; h = dest_h; w = dest_w }
  in

  (* TODO move these to config *)
  let sideways_points =
    [
      { x = 0. *. ghost.current_weapon.scale_x; y = 0. };
      { x = 111. *. ghost.current_weapon.scale_x; y = 8. };
      { x = 158. *. ghost.current_weapon.scale_x; y = 16. };
      { x = 184. *. ghost.current_weapon.scale_x; y = 26. };
      { x = 196. *. ghost.current_weapon.scale_x; y = 37. };
      { x = 195. *. ghost.current_weapon.scale_x; y = 49. };
      { x = 178. *. ghost.current_weapon.scale_x; y = 64. };
      { x = 136. *. ghost.current_weapon.scale_x; y = 77. };
      { x = 60. *. ghost.current_weapon.scale_x; y = 86. };
      { x = 1. *. ghost.current_weapon.scale_x; y = 87. };
    ]
  in
  let up_points =
    [
      { x = 0.; y = 189. *. ghost.current_weapon.scale_y };
      { x = 4.; y = 110. *. ghost.current_weapon.scale_y };
      { x = 12.; y = 63. *. ghost.current_weapon.scale_y };
      { x = 25.; y = 25. *. ghost.current_weapon.scale_y };
      { x = 35.; y = 8. *. ghost.current_weapon.scale_y };
      { x = 45.; y = 0. *. ghost.current_weapon.scale_y };
      { x = 60.; y = 1. *. ghost.current_weapon.scale_y };
      { x = 73.; y = 14. *. ghost.current_weapon.scale_y };
      { x = 87.; y = 46. *. ghost.current_weapon.scale_y };
      { x = 97.; y = 86. *. ghost.current_weapon.scale_y };
      { x = 103.; y = 189. *. ghost.current_weapon.scale_y };
    ]
  in
  let down_points =
    List.map
      (fun (point : vector) -> { point with y = (189. -. point.y) *. ghost.current_weapon.scale_y })
      up_points
  in
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
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) : (sprite * time) option =
    match child_kind with
    | WRAITHS -> Some (child.sprite, ghost.history.cast_wraiths.started)
    | DIVE -> Some (child.sprite, ghost.history.cast_dive.started)
    | DIVE_COOLDOWN -> Some (child.sprite, ghost.history.dive_cooldown.started)
    | _ -> None
  in
  List.find_map get_sprite ghost.children

let get_current_slash (ghost : ghost) : slash option =
  let get_slash ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | NAIL slash -> Some slash
    | _ -> None
  in
  List.find_map get_slash ghost.children

let get_dream_nail (ghost : ghost) : rect option =
  let get_rect ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | DREAM_NAIL -> Some child.sprite.dest
    | _ -> None
  in
  List.find_map get_rect ghost.children

let get_dive_sprite (ghost : ghost) : sprite option =
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | DIVE
    | DIVE_COOLDOWN ->
      Some child.sprite
    | _ -> None
  in
  List.find_map get_sprite ghost.children

let get_focus_sparkles ghost : sprite option =
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | FOCUS -> Some child.sprite
    | _ -> None
  in
  List.find_map get_sprite ghost.children

(* TODO move into spawn_child - can't do this until make_ghost_child goes away *)
let add_child (ghost : ghost) (kind : ghost_child_kind) (child : ghost_child) =
  ghost.children <- Utils.replace_assoc kind child ghost.children

let remove_child (ghost : ghost) (kind : ghost_child_kind) =
  ghost.children <- List.remove_assoc kind ghost.children

(* dive sprites are using this fn with the placeholder image, but it can eventually be
   combined with make_ghost_child'
*)
let make_ghost_child ?(in_front = false) (ghost : ghost) kind relative_pos texture w h =
  let name = Show.ghost_child_kind kind in
  let sprite =
    Sprite.create name texture { pos = Entity.get_child_pos ghost.entity relative_pos w h; w; h }
  in
  { relative_pos; sprite; in_front }

let spawn_child
    ?(in_front = false)
    ?(scale = 1.)
    (ghost : ghost)
    (child_kind : ghost_child_kind)
    alignment
    texture =
  let child =
    let w, h = get_scaled_texture_size ~scale texture in
    make_ghost_child ghost ~in_front child_kind alignment texture w h
  in
  add_child ghost child_kind child

let make_c_dash_child ?(full = false) (ghost : ghost) : unit =
  let texture =
    match (ghost.current.wall, full) with
    | None, true -> ghost.shared_textures.c_dash_crystals_full
    | None, false -> ghost.shared_textures.c_dash_crystals
    | Some _, true -> ghost.shared_textures.c_dash_wall_crystals_full
    | Some _, false -> ghost.shared_textures.c_dash_wall_crystals
  in
  let alignment =
    match (ghost.current.wall, ghost.entity.sprite.facing_right) with
    | None, _ -> ALIGNED (CENTER, BOTTOM)
    | Some _, true -> ALIGNED (LEFT, CENTER)
    | Some _, false -> ALIGNED (RIGHT, CENTER)
  in
  let child_kind =
    match ghost.current.wall with
    | None -> C_DASH_CHARGE_CRYSTALS
    | Some _ -> C_DASH_WALL_CHARGE_CRYSTALS
  in
  Sprite.reset_texture texture;
  spawn_child ghost child_kind alignment ~scale:Config.scale.ghost texture

let maybe_despawn_child (ghost : ghost) (child_kind, child) = ()

let animate_and_despawn_children frame_time ghost : unit =
  let advance (child_kind, child) =
    match child.sprite.texture.animation_src with
    | ONCE _ -> (
      match Sprite.advance_or_despawn frame_time child.sprite.texture child.sprite with
      | None ->
        remove_child ghost child_kind;
        make_c_dash_child ~full:true ghost
      | Some sprite -> ( (* already advanced *) ))
    | PARTICLE _ -> (
      match Sprite.advance_or_despawn frame_time child.sprite.texture child.sprite with
      | None -> remove_child ghost child_kind
      | _ -> ( (* already advanced *) ))
    | STILL _
    | LOOPED _ ->
      Sprite.advance_animation frame_time child.sprite.texture child.sprite
  in
  List.iter advance ghost.children

let get_damage (ghost : ghost) (damage_kind : damage_kind) =
  (* TODO check ghost.abilities.descending_dark/shade_soul *)
  match damage_kind with
  | DREAM_NAIL -> 0
  | NAIL ->
    ghost.weapons
    |> List.map snd
    |> List.map (fun (w : Json_t.weapon) -> w.damage)
    |> List.fold_left ( + ) 0
  | VENGEFUL_SPIRIT -> 15
  | DESOLATE_DIVE -> 15
  | DESOLATE_DIVE_SHOCKWAVE -> 20
  | HOWLING_WRAITHS -> (* TODO this should be 13 with multihits *) 26

let check_dream_nail_collisions (state : state) (game : game) =
  match get_dream_nail game.ghost with
  | None -> ()
  | Some dream_nail_dest ->
    let resolve_enemy ((_enemy_id, enemy) : enemy_id * enemy) =
      if Enemy.is_alive enemy then ((* TODO use collision shape for dream nail *)
        match Collision.between_rects dream_nail_dest enemy.entity.sprite.dest with
        | None -> tmp "no collision with enemy %s" (Show.enemy_id enemy.id)
        | Some c ->
          if game.ghost.history.dream_nail.started > Enemy.took_damage_at enemy DREAM_NAIL then (
            tmp "got a collision";
            (* TODO make a new fn Ghost.add/deduct_soul that bounds between [0, soul max] *)
            game.ghost.soul.current <-
              Utils.bound_int 0
                (game.ghost.soul.current + Config.action.soul_per_cast)
                game.ghost.soul.max;
            let recoil_vx =
              (* TODO move to config *)
              1800.
            in
            let recoil_speed =
              if game.ghost.entity.sprite.facing_right then recoil_vx else -1. *. recoil_vx
            in
            enemy.entity.x_recoil <-
              Some { speed = recoil_speed; time_left = { seconds = 0.1 }; reset_v = true };
            enemy.history <-
              Utils.replace_assoc
                (TOOK_DAMAGE DREAM_NAIL : enemy_action)
                { at = state.frame.time } enemy.history)
          else
            tmp "still colliding")
    in
    List.iter resolve_enemy game.room.enemies

let resolve_slash_collisions (state : state) (game : game) =
  match get_current_slash game.ghost with
  | None -> check_dream_nail_collisions state game
  | Some slash ->
    let resolve_enemy ((_enemy_id, enemy) : enemy_id * enemy) =
      if enemy.status.check_damage_collisions then (
        match Collision.with_slash slash enemy.entity.sprite with
        | None -> ()
        | Some collision ->
          if
            Enemy.maybe_take_damage state enemy game.ghost.history.nail.started NAIL
              (get_damage game.ghost NAIL) enemy.entity.dest
          then (
            (match collision.direction with
            | DOWN -> pogo game.ghost
            | LEFT
            | RIGHT ->
              (* TODO recoil enemy *)
              Entity.recoil_backwards game.ghost.entity
                { speed = 800.; time_left = { seconds = 0.1 }; reset_v = true }
            | UP ->
              if game.ghost.entity.v.y < 0. then
                game.ghost.entity.v.y <- 300.);
            game.ghost.soul.current <-
              Utils.bound_int 0
                (game.ghost.soul.current + Config.action.soul_gained_per_nail)
                game.ghost.soul.max);
          if slash.direction = DOWN && game.ghost.entity.y_recoil = None then (
            (* TODO this isn't working (can't pogo LB projectiles) *)
            let check_projectile_pogo (projectile : projectile) =
              if projectile.pogoable then (
                match Collision.with_slash slash projectile.entity.sprite with
                | None -> ()
                | Some _ -> pogo game.ghost)
            in
            List.iter check_projectile_pogo enemy.spawned_projectiles))
    in

    let destroy_tile_group layer tile_group =
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
          Utils.replace_assoc layer.name (tile_group.tile_idxs @ existing)
            game.room.progress.removed_idxs_by_layer)
    in

    (* destroyable and pogoable layers *)
    let resolve_colliding_layers (layer : layer) =
      if layer.config.destroyable then (
        let new_tile_groups : tile_group list ref = ref [] in
        let spawn_fragment (collision : collision) (e : entity) =
          let new_fragment = Entity.clone e in
          new_fragment.dest.pos <- { x = collision.rect.pos.x; y = collision.rect.pos.y };
          new_fragment.v <- { x = Random.float 501. -. 200.; y = Random.float 1000. -. 1000. };
          new_fragment.update_pos <- true;
          new_fragment
        in
        let destroy_object (tile_group : tile_group) (collision : collision) =
          layer.spawned_fragments <-
            List.map (spawn_fragment collision) tile_group.fragments @ layer.spawned_fragments;
          let idx = List.nth tile_group.tile_idxs 0 in
          (match List.assoc_opt idx game.room.idx_configs with
          | Some (PURPLE_PEN name) -> maybe_begin_interaction state game name
          | _ -> ());
          destroy_tile_group layer tile_group;
          if collision.direction = DOWN && layer.config.pogoable then
            pogo game.ghost;
          match tile_group.stub_sprite with
          | None -> ()
          | Some sprite ->
            layer.spawned_stub_sprites <-
              (sprite, tile_group.transformation_bits) :: layer.spawned_stub_sprites
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
                  let get_random_fragment_idx () =
                    Random.int (List.length tile_group.fragments - 1)
                  in
                  let make_random_fragment _n =
                    List.nth tile_group.fragments (get_random_fragment_idx ())
                  in
                  let random_fragments = List.init (Random.int 3) make_random_fragment in
                  layer.spawned_fragments <-
                    List.map (spawn_fragment coll) random_fragments @ layer.spawned_fragments;
                  door_health.hits <- door_health.hits - 1;
                  new_tile_groups := tile_group :: !new_tile_groups)
                else
                  destroy_object tile_group coll)
              else
                new_tile_groups := tile_group :: !new_tile_groups;
              ())
        in
        List.iter resolve_tile_group layer.tile_groups;
        layer.tile_groups <- !new_tile_groups)
      else if layer.config.pogoable then (
        let resolve_tile_group (tile_group : tile_group) =
          match Collision.with_slash' slash tile_group.dest with
          | None -> ()
          | Some coll -> (
            match coll.direction with
            | DOWN -> pogo game.ghost
            | _ -> ())
        in
        List.iter resolve_tile_group layer.tile_groups)
    in

    let resolve_lever ((door_coords, lever_sprite) : string * sprite) =
      let layer =
        match List.find_opt (fun (l : layer) -> l.name = "lever-doors") game.room.layers with
        | None ->
          failwithf "room %s has levers '%s', but no lever-doors layer" (Show.room_id game.room.id)
            door_coords
        | Some l -> l
      in
      let new_tile_groups : tile_group list ref = ref [] in
      let _direction, coords = Utils.separate door_coords '-' in
      let x', y' = Utils.separate coords '-' in
      let door_tile_idx =
        Tiled.Tile.tile_idx_from_coords ~width:game.room.json.w_in_tiles
          (x' |> float_of_string, y' |> float_of_string)
      in
      match Collision.with_slash slash lever_sprite with
      (* TODO don't really need to check slash collisions after a lever's door has already been opened *)
      | None -> ()
      | Some collision -> (
        lever_sprite.texture <- state.global.textures.door_lever_struck;
        let has_tile_idx tile_group = List.mem door_tile_idx tile_group.tile_idxs in
        match List.find_opt has_tile_idx layer.tile_groups with
        | None ->
          (* TODO maybe check that the lever has actually been destroyed, to validate the door_coords are correct
             - ideally this would happen on room load though
          *)
          ()
        | Some tile_group ->
          layer.tile_groups <-
            List.filter (fun (t : tile_group) -> t.dest <> tile_group.dest) layer.tile_groups;
          destroy_tile_group layer tile_group)
    in

    List.iter resolve_lever game.room.triggers.levers;
    List.iter resolve_colliding_layers game.room.layers;
    List.iter resolve_enemy game.room.enemies

let reset_current_status () =
  {
    wall = None;
    is_diving = false;
    is_c_dashing = false;
    is_charging_c_dash = false;
    is_taking_hazard_damage = false;
    can_dash = true;
    can_flap = true;
  }

(* TODO maybe this should also cancel_action for some things *)
let take_damage (ghost : ghost) =
  ghost.current <- reset_current_status ();
  ghost.children <- [];
  ghost.entity.current_floor <- None;
  ghost.health.current <- ghost.health.current - 1;
  if ghost.health.current = 0 then
    (* TODO handle dying:
       - reset in-progress boss interactions
    *)
    ghost.health.current <- ghost.health.max

(* state updates for current pose, texture animation, and sprite *)
let set_pose ghost (new_pose : ghost_pose) (frame_time : float) =
  let update_vx multiplier =
    let mult = if ghost.entity.sprite.facing_right then multiplier else -1. *. multiplier in
    ghost.entity.v.x <- mult *. Config.ghost.vx
  in
  (* TODO bad name *)
  let reset_standing_abilities () =
    if Entity.on_ground ghost.entity then (
      ghost.current.can_dash <- true;
      ghost.current.can_flap <- true)
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
      Entity.recoil_backwards ghost.entity
        { speed = 80.; time_left = { seconds = 0.16666 }; reset_v = true };
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
    | DREAM_NAIL ->
      update_vx 0.;
      ghost.textures.nail
    | C_DASH_WALL_COOLDOWN ->
      update_vx 0.;
      ghost.entity.v.y <- 0.;
      ghost.textures.wall_slide
    | C_DASH_COOLDOWN ->
      update_vx (3. -. (4. *. (frame_time -. ghost.history.c_dash_cooldown.started.at)));
      ghost.entity.v.y <- 0.;
      (* TODO new image/texture for this *)
      ghost.textures.cast
    | C_DASH ->
      ghost.entity.v.y <- 0.;
      update_vx 3.;
      ghost.textures.dash
    | C_DASH_CHARGE ->
      update_vx 0.;
      ghost.entity.v.y <- 0.;
      ghost.textures.focus
    | SHADE_DASH
    | DASH ->
      ghost.current.can_dash <- false;
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
    | TAKE_DAMAGE_AND_RESPAWN -> ghost.textures.take_damage
    | TAKE_DAMAGE direction ->
      let x_recoil_speed =
        match direction with
        | LEFT -> -800.
        | RIGHT -> 800.
        | _ -> if ghost.entity.sprite.facing_right then 800. else -800.
      in
      ghost.entity.x_recoil <-
        Some { speed = x_recoil_speed; time_left = { seconds = 0.06666 }; reset_v = true };
      ghost.entity.y_recoil <-
        Some { speed = -800.; time_left = { seconds = 0.06666 }; reset_v = true };
      take_damage ghost;
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
    | FLAP -> ghost.textures.flap
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
      ghost.current.can_dash <- true;
      ghost.current.can_flap <- true;
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
      ghost.current.can_dash <- true;
      ghost.current.can_flap <- true;
      ghost.textures.wall_slide
  in
  Entity.update_sprite_texture ghost.entity next_texture

let past_cooldown ?(debug = false) pose_frames frame_time : bool =
  pose_frames.blocked_until.at < frame_time

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

let check_monkey_block_collisions (state : state) (game : game) =
  let layers = List.filter (fun l -> l.config.monkey) game.room.layers in
  (* TODO-2 use destroy_tile_group from resolve_slash_collisions  *)
  ()

let cancel_action (state : state) (game : game) (action_kind : ghost_action_kind) =
  let action =
    match action_kind with
    | FLAP ->
      game.ghost.current.can_flap <- true;
      game.ghost.history.flap
    | WALL_KICK
    | JUMP
    | TAKE_DAMAGE_AND_RESPAWN
    | TAKE_DAMAGE _
    | CAST _
    | DIVE_COOLDOWN
    | SHADE_DASH
    | DASH
    | C_DASH_CHARGE
    | C_DASH
    | C_DASH_COOLDOWN
    | C_DASH_WALL_COOLDOWN
    | DREAM_NAIL
    | ATTACK _
    | FOCUS ->
      failwithf "cannot cancel action: %s" (Show.ghost_action_kind action_kind)
  in
  action.doing_until.at <- state.frame.time;
  ()

let start_action ?(debug = false) (state : state) (game : game) (action_kind : ghost_action_kind) =
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
      let slash =
        make_slash game.ghost direction relative_pos game.ghost.entity.sprite state.frame.time
      in
      let child : ghost_child_kind = NAIL slash in
      game.ghost.children <-
        Utils.replace_assoc child
          { relative_pos; sprite = slash.sprite; in_front = true }
          game.ghost.children;
      cooldown_scale := game.ghost.current_weapon.cooldown_scale;
      game.ghost.history.nail
    | DREAM_NAIL -> game.ghost.history.dream_nail
    | C_DASH_WALL_COOLDOWN ->
      game.ghost.entity.sprite.facing_right <- not game.ghost.entity.sprite.facing_right;
      game.ghost.current.is_c_dashing <- false;
      remove_child game.ghost C_DASH_WHOOSH;
      game.ghost.history.c_dash_wall_cooldown
    | C_DASH_COOLDOWN ->
      game.ghost.current.is_c_dashing <- false;
      game.ghost.children <- List.remove_assoc C_DASH_WHOOSH game.ghost.children;
      game.ghost.history.c_dash_cooldown
    | C_DASH ->
      (* state.camera.shake <- 1.; *)
      game.ghost.current.is_c_dashing <- true;
      game.ghost.current.is_charging_c_dash <- false;
      game.ghost.entity.current_floor <- None;
      let alignment =
        if game.ghost.entity.sprite.facing_right then
          ALIGNED (RIGHT, CENTER)
        else
          ALIGNED (LEFT, CENTER)
      in
      spawn_child game.ghost C_DASH_WHOOSH ~scale:6. alignment
        game.ghost.shared_textures.c_dash_whoosh;
      game.ghost.history.c_dash
    | C_DASH_CHARGE ->
      game.ghost.current.is_charging_c_dash <- true;
      make_c_dash_child game.ghost;
      game.ghost.history.charge_c_dash
    | SHADE_DASH -> game.ghost.history.shade_dash
    | DASH -> game.ghost.history.dash
    | CAST spell_kind -> (
      game.ghost.soul.current <- game.ghost.soul.current - Config.action.soul_per_cast;
      match spell_kind with
      | VENGEFUL_SPIRIT ->
        spawn_vengeful_spirit state game;
        game.ghost.history.cast_vs
      | DESOLATE_DIVE ->
        (* TODO probably should set is_diving in this fn (like how c-dash does it) *)
        let w, h =
          (* TODO these are temporarily scaled so the dive.png image can be reused *)
          (game.ghost.entity.dest.w *. 5., game.ghost.entity.dest.h *. 5.)
        in
        let child =
          make_ghost_child game.ghost DIVE
            (ALIGNED (CENTER, BOTTOM))
            game.ghost.shared_textures.desolate_dive w h
        in
        add_child game.ghost DIVE child;
        game.ghost.history.cast_dive
      | HOWLING_WRAITHS ->
        spawn_child game.ghost WRAITHS
          (ALIGNED (CENTER, BOTTOM))
          ~scale:1.7 game.ghost.shared_textures.howling_wraiths;
        check_monkey_block_collisions state game;
        game.ghost.history.cast_wraiths)
    | TAKE_DAMAGE_AND_RESPAWN ->
      Entity.freeze game.ghost.entity;
      game.ghost.history.take_damage_and_respawn
    | TAKE_DAMAGE _ ->
      state.camera.shake <- 0.5;
      game.ghost.history.take_damage
    | DIVE_COOLDOWN -> game.ghost.history.dive_cooldown
    | FOCUS ->
      game.ghost.soul.at_focus_start <- game.ghost.soul.current;
      game.ghost.soul.health_at_focus_start <- game.ghost.health.current;
      game.ghost.soul.last_decremented <- { at = state.frame.time };
      spawn_child game.ghost FOCUS ~in_front:true ~scale:3.
        (ALIGNED (CENTER, CENTER))
        game.ghost.shared_textures.focus_sparkles;
      game.ghost.history.focus
    | JUMP -> game.ghost.history.jump
    | FLAP -> game.ghost.history.flap
    | WALL_KICK -> game.ghost.history.wall_kick
  in
  action.started <- { at = state.frame.time };
  action.doing_until.at <- state.frame.time +. action.config.duration.seconds;
  action.blocked_until.at <- state.frame.time +. (action.config.cooldown.seconds *. !cooldown_scale);
  set_pose game.ghost (PERFORMING action_kind) state.frame.time

let hazard_respawn (game : game) =
  game.ghost.entity.current_floor <- None;
  Entity.unfreeze game.ghost.entity;
  game.ghost.entity.dest.pos <- clone_vector game.room.respawn_pos

let continue_action (state : state) (game : game) (action_kind : ghost_action_kind) =
  (match action_kind with
  | FOCUS ->
    let decr_dt =
      game.ghost.history.focus.config.duration.seconds
      /. (Config.action.soul_per_cast + 0 |> Int.to_float)
    in
    (* TODO probably should be checking >= *)
    if
      game.ghost.soul.at_focus_start - game.ghost.soul.current > Config.action.soul_per_cast
      && game.ghost.soul.health_at_focus_start = game.ghost.health.current
    then
      game.ghost.health.current <-
        Utils.bound_int 0 game.ghost.health.max (game.ghost.health.current + 1)
    else if state.frame.time -. game.ghost.soul.last_decremented.at > decr_dt then (
      game.ghost.soul.current <- game.ghost.soul.current - 1;
      game.ghost.soul.last_decremented <- { at = state.frame.time })
  | FLAP ->
    let started = game.ghost.history.flap.started.at in
    let duration = game.ghost.history.flap.config.duration.seconds in
    (* TODO move to config
       - need to multiply duration here instead of changing config value because `is_doing FLAP`
         needs to be true for long enough to get here
       - maybe just subtract 0.1 like DREAM_NAIL does (not sure if flap duration is used elsewhere though)
    *)
    if state.frame.time -. started > duration *. 0.666666 then
      game.ghost.entity.v.y <- Config.ghost.jump_vy *. 0.8
  | DREAM_NAIL ->
    if
      state.frame.time -. game.ghost.history.dream_nail.started.at
      > game.ghost.history.dream_nail.config.duration.seconds -. 0.1
    then (
      let alignment =
        if game.ghost.entity.sprite.facing_right then
          ALIGNED (LEFT, CENTER)
        else
          ALIGNED (RIGHT, CENTER)
      in
      spawn_child game.ghost DREAM_NAIL alignment game.ghost.shared_textures.slash)
  | C_DASH
  | C_DASH_CHARGE
  | SHADE_DASH
  | TAKE_DAMAGE_AND_RESPAWN
  | WALL_KICK
  | JUMP
  | TAKE_DAMAGE _
  | CAST _
  | DIVE_COOLDOWN
  | DASH
  | C_DASH_COOLDOWN
  | C_DASH_WALL_COOLDOWN
  | ATTACK _ ->
    ());
  set_pose game.ghost (PERFORMING action_kind) state.frame.time

let is_doing (ghost : ghost) (action_kind : ghost_action_kind) (frame_time : float) : bool =
  let check_action action =
    action.started.at <= frame_time && frame_time <= action.doing_until.at
  in
  match action_kind with
  | ATTACK _ -> Option.is_some (get_current_slash ghost)
  | FOCUS -> Option.is_some (get_focus_sparkles ghost)
  | CAST spell_kind -> (
    match spell_kind with
    | VENGEFUL_SPIRIT -> check_action ghost.history.cast_vs
    | HOWLING_WRAITHS -> check_action ghost.history.cast_wraiths
    | DESOLATE_DIVE -> ghost.current.is_diving)
  | C_DASH_CHARGE -> ghost.current.is_charging_c_dash
  | C_DASH -> ghost.current.is_c_dashing
  | C_DASH_WALL_COOLDOWN -> check_action ghost.history.c_dash_wall_cooldown
  | C_DASH_COOLDOWN -> check_action ghost.history.c_dash_cooldown
  | SHADE_DASH -> check_action ghost.history.shade_dash
  | DASH -> check_action ghost.history.dash
  | DIVE_COOLDOWN -> check_action ghost.history.dive_cooldown
  | TAKE_DAMAGE_AND_RESPAWN -> check_action ghost.history.take_damage_and_respawn
  | FLAP -> check_action ghost.history.flap
  | DREAM_NAIL -> check_action ghost.history.dream_nail
  | WALL_KICK
  | JUMP
  | TAKE_DAMAGE _ ->
    failwithf "is_doing - invalid action %s" (Show.ghost_action_kind action_kind)

let not_doing_any (ghost : ghost) (frame_time : float) (actions : ghost_action_kind list) : bool =
  not (List.exists (fun action -> is_doing ghost action frame_time) actions)

let is_casting (state : state) (game : game) =
  is_doing game.ghost (CAST VENGEFUL_SPIRIT) state.frame.time
  || game.ghost.current.is_diving
  || is_doing game.ghost (CAST HOWLING_WRAITHS) state.frame.time

let swap_current_ghost (state : state) (game : game) ?(swap_pos = true) target_ghost_id =
  match List.assoc_opt target_ghost_id game.ghosts with
  | None -> failwithf "could not find other ghost %s" (Show.ghost_id target_ghost_id)
  | Some new_ghost ->
    let old_ghost = game.ghost in
    let new_ghosts = (old_ghost.id, old_ghost) :: List.remove_assoc new_ghost.id game.ghosts in
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
    new_ghost.current.can_dash <- old_ghost.current.can_dash;
    new_ghost.current.can_flap <- old_ghost.current.can_flap;
    new_ghost.children <- old_ghost.children;
    new_ghost.spawned_vengeful_spirits <- old_ghost.spawned_vengeful_spirits;

    game.ghost <- new_ghost;
    game.ghosts <- new_ghosts

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
  | "shade_cloak" -> ghost.abilities.shade_cloak <- new_val ghost.abilities.shade_cloak
  | "mantis_claw" -> ghost.abilities.mantis_claw <- new_val ghost.abilities.mantis_claw
  | "crystal_heart" -> ghost.abilities.crystal_heart <- new_val ghost.abilities.crystal_heart
  | "monarch_wings" -> ghost.abilities.monarch_wings <- new_val ghost.abilities.monarch_wings
  | _ -> failwithf "change_ability bad ability name: %s" ability_name

let enable_ability ghost ability_name = change_ability ~only_enable:true ghost ability_name
let toggle_ability ghost ability_name = change_ability ~debug:true ghost ability_name

let acquire_weapon (state : state) (game : game) weapon_name =
  match List.assoc_opt weapon_name state.global.weapons with
  | Some weapon_config ->
    let current_weapon_names = List.map fst game.ghost.weapons in
    if not (List.mem weapon_name current_weapon_names) then
      game.ghost.weapons <- (weapon_name, weapon_config) :: game.ghost.weapons
  | None -> failwithf "acquire_weapon bad weapon name: %s" weapon_name

let equip_weapon (ghost : ghost) weapon_name =
  match List.assoc_opt weapon_name ghost.weapons with
  | None ->
    print "can't equip %s, not in ghost.weapons: %s" weapon_name
      (List.map (fun (name, w) -> name) ghost.weapons |> join)
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

let get_invincibility_kind (state : state) (game : game) : invincibility_kind option =
  let in_dive_cooldown () = not (past_cooldown game.ghost.history.dive_cooldown state.frame.time) in
  if not (past_cooldown game.ghost.history.take_damage state.frame.time) then
    Some TOOK_DAMAGE
  else if game.ghost.current.is_diving || in_dive_cooldown () then
    Some DIVE_IFRAMES
  else if is_doing game.ghost SHADE_DASH state.frame.time then
    Some SHADE_CLOAK
  else
    None

let is_vulnerable (state : state) (game : game) : bool =
  Option.is_none (get_invincibility_kind state game)

let handle_debug_keys (game : game) (state : state) =
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
  else if holding_shift () then (
    if key_pressed DEBUG_1 then
      game.ghost.soul.current <- game.ghost.soul.max
    else if key_pressed DEBUG_2 then
      toggle_ability game.ghost "mantis_claw"
    else if key_pressed DEBUG_3 then
      toggle_ability game.ghost "vengeful_spirit"
    else if key_pressed DEBUG_4 then
      toggle_ability game.ghost "desolate_dive")
  else if key_pressed DEBUG_1 then
    game.debug_paused <- not game.debug_paused;
  state

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
      | C_DASH -> (state.frame_inputs.c_dash, game.ghost.history.c_dash.config.input_buffer.seconds)
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
  let set_interaction_pose' (ghost : ghost) (pose : ghost_pose) =
    set_pose ghost pose state.frame.time
  in

  (* TODO need to block inputs during transitions to prevent re-exiting immediately and warping
     - also bound velocity to prevent falling through floors with high vy through horizontal doors
  *)
  let handle_interactions () =
    (* the ghost can only collide with one trigger at a time *)
    let check_for_new_interactions () : bool =
      let interactable_triggers = game.room.triggers.lore @ game.room.triggers.item_pickups in
      (match find_trigger_collision game.ghost interactable_triggers with
      | None -> ()
      | Some (name, _rect) ->
        (* TODO maybe want to also check `Entity.on_ground game.ghost.entity` *)
        if state.frame_inputs.interact.pressed then
          maybe_begin_interaction state game name);
      (match find_trigger_collision game.ghost game.room.triggers.cutscene with
      | None -> ()
      | Some (name, _rect) -> maybe_begin_interaction state game name);
      List.length game.interaction.steps > 0
    in
    let speed_through_interaction =
      (* holding down d-nail will skip through interactions quickly, but still perform each step
         once so side-effects like gaining abilities still happen
      *)
      state.frame_inputs.dream_nail.down
    in
    match game.interaction.text with
    | Some _text ->
      if state.frame_inputs.interact.pressed then (
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
              | Some slash -> game.ghost.children <- []
              | None -> ())
          | FADE_SCREEN_OUT -> state.screen_fade <- Some 160
          | FADE_SCREEN_IN -> state.screen_fade <- None
          | WARP destination ->
            let target_room_name, rest =
              (* this can't be a character that is used in room names *)
              Utils.separate destination '|'
            in
            (* TODO this might be too much stuff to embed in the name, maybe worth
               adding/parsing Custom Properties now
            *)
            let blocking_interaction_name, coords = Utils.separate rest '@' in
            let blocked =
              match blocking_interaction_name with
              | "" -> false
              | s -> not (List.mem (fmt "cutscene_%s" s) game.room.progress.finished_interactions)
            in
            if not blocked then (
              let target_x', target_y' = Utils.separate coords ',' in
              let tile_x, tile_y = (target_x' |> int_of_string, target_y' |> int_of_string) in
              let target_x, target_y =
                Tiled.Tile.tile_dest ~tile_w:game.room.json.tile_w ~tile_h:game.room.json.tile_h
                  (tile_x, tile_y)
              in
              let target_room_location = Tiled.Room.locate_by_name state.world target_room_name in
              Room.change_current_room state game target_room_name target_room_location
                { x = target_x; y = target_y })
          | PUSH_RECT (x, y, w, h) ->
            game.interaction.black_rects <- { pos = { x; y }; w; h } :: game.interaction.black_rects
          | TEXT paragraphs ->
            let text : Interaction.text = { content = paragraphs; increases_health = false } in
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
          | FOCUS_ABILITY_TEXT (top_paragraphs, outline_src, bottom_paragraphs) ->
            game.interaction.speaker_name <- None;
            game.interaction.text <-
              Some (FOCUS_ABILITY { top_paragraphs; outline_src; bottom_paragraphs })
          | ABILITY_TEXT (outline_src, bottom_paragraphs) ->
            game.interaction.speaker_name <- None;
            game.interaction.text <-
              Some (ABILITY { top_paragraphs = []; outline_src; bottom_paragraphs })
          | SET_FIXED_CAMERA (tile_x, tile_y) ->
            let tx, ty = Tiled.Tile.tile_coords ~tile_w ~tile_h (tile_x, tile_y) in
            let x, y = (tx *. Config.scale.room, ty *. Config.scale.room) in
            state.camera.subject <- FIXED { x; y }
          | SET_GHOST_CAMERA ->
            Entity.unfreeze game.ghost.entity;
            state.camera.subject <- GHOST
          | WAIT time -> new_wait := time -. state.frame.dt
          | HIDE_BOSS_DOORS ->
            (match
               List.find_opt (fun (l : layer) -> l.name = "hidden-boss-doors") game.room.layers
             with
            | None -> failwith "HIDE_BOSS_DOORS, expected hidden-boss-doors layer"
            | Some layer_to_hide -> layer_to_hide.hidden <- true);
            game.room.layers <-
              Tiled.Room.get_layer_tile_groups game.room game.room.progress.removed_idxs_by_layer
          | UNHIDE_BOSS_DOORS ->
            (match
               List.find_opt (fun (l : layer) -> l.name = "hidden-boss-doors") game.room.layers
             with
            | None -> failwith "UNHIDE_BOSS_DOORS, expected hidden-boss-doors layer"
            | Some layer_to_hide -> layer_to_hide.hidden <- false);
            game.room.layers <-
              Tiled.Room.get_layer_tile_groups game.room game.room.progress.removed_idxs_by_layer
          | HIDE_LAYER layer_name -> (
            match List.find_opt (fun (l : layer) -> l.name = layer_name) game.room.layers with
            | None -> ()
            | Some layer_to_hide -> layer_to_hide.hidden <- true)
          | UNHIDE_LAYER layer_name -> (
            match List.find_opt (fun (l : layer) -> l.name = layer_name) game.room.layers with
            | None -> ()
            | Some layer_to_unhide -> layer_to_unhide.hidden <- false)
          | SWAP_HIDDEN_LAYER layer_name ->
            let layer_to_hide : layer =
              List.find (fun (l : layer) -> l.name = layer_name) game.room.layers
            in
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
            Entity.freeze game.ghost.entity;
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
            let x, y =
              ((tx +. x_offset) *. Config.scale.room, (ty +. y_offset) *. Config.scale.room)
            in
            Entity.unhide_at entity { x; y }
          | HIDE -> Entity.hide entity
          | FREEZE -> Entity.freeze entity
          | UNFREEZE -> Entity.unfreeze entity
        in

        let add_item (item_kind : Interaction.item_kind) =
          Room.update_pickup_indicators state game;
          match item_kind with
          | ABILITY ability_name -> enable_ability game.ghost ability_name
          | DREAMER (item_name, dreamer_item_text) ->
            let text : Interaction.text =
              {
                content = [ fmt "Got the dreamer item %s" item_name; ""; dreamer_item_text ];
                increases_health = false;
              }
            in
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
          | WEAPON weapon_name ->
            let weapon_config = List.assoc weapon_name state.global.weapons in
            let text : Interaction.text =
              {
                content = [ fmt "Acquired the %s" weapon_name; weapon_config.pickup_text ];
                increases_health = false;
              }
            in
            acquire_weapon state game weapon_name;
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
            | UNHIDE ->
              failwithf "can't unhide %s, use UNHIDE_AT for ghosts" (Show.ghost_id ghost.id)
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
      && not (is_casting state game)
    in

    let this_frame =
      if trying_cast then (
        let cast_spell spell_kind =
          start_action ~debug:true state game (CAST spell_kind);
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
          game.ghost.abilities.vengeful_spirit
          && past_cooldown game.ghost.history.cast_vs state.frame.time
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
          List.find_opt
            (fun s -> is_doing game.ghost (CAST s) state.frame.time)
            [ VENGEFUL_SPIRIT; HOWLING_WRAITHS ]
        in

        match continuing_spell_kind with
        | None ->
          if game.ghost.current.is_diving then
            if Option.is_some game.ghost.entity.current_floor then (
              state.camera.shake <- 1.;
              game.ghost.current.is_diving <- false;
              start_action state game DIVE_COOLDOWN;
              let child =
                let dest = game.ghost.entity.dest in
                let relative_pos = ALIGNED (CENTER, BOTTOM) in
                let w, h =
                  (* TODO this scaling is temporary so the current dive.png can be used *)
                  (dest.w *. 15., dest.h *. 3.)
                in
                make_ghost_child game.ghost DIVE_COOLDOWN relative_pos
                  game.ghost.shared_textures.dive_shockwave w h
              in
              add_child game.ghost DIVE_COOLDOWN child;
              remove_child game.ghost DIVE;
              false)
            else (
              match get_dive_sprite game.ghost with
              | None -> false
              | Some dive_sprite ->
                continue_action state game (CAST DESOLATE_DIVE);
                true)
          else
            false
        | Some kind ->
          continue_action state game (CAST kind);
          true)
    in

    { this_frame }
  in

  let handle_c_dashing () : handled_action =
    let starting_charge () =
      (* attack direction is arbitrary *)
      game.ghost.abilities.crystal_heart
      && key_pressed_or_buffered C_DASH
      && (Entity.on_ground game.ghost.entity
         || (* TODO maybe make a fn for this - after moving to Entity *)
         Option.is_some game.ghost.current.wall)
      && (not (is_doing game.ghost (ATTACK RIGHT) state.frame.time))
      && (not (is_casting state game))
      && (not game.ghost.current.is_c_dashing)
      && not game.ghost.current.is_charging_c_dash
    in
    let released_charge () =
      is_doing game.ghost C_DASH_CHARGE state.frame.time && state.frame_inputs.c_dash.released
    in
    let still_charging () =
      is_doing game.ghost C_DASH_CHARGE state.frame.time && state.frame_inputs.c_dash.down
    in
    let still_c_dashing () = is_doing game.ghost C_DASH state.frame.time in
    let still_cooling_down () = is_doing game.ghost C_DASH_COOLDOWN state.frame.time in

    let maybe_end_c_dash () =
      let end_c_dash () =
        (* this only handles cancelling the dash in mid-air - wall collisions are handled below *)
        state.frame_inputs.jump.pressed || state.frame_inputs.c_dash.pressed
      in
      if end_c_dash () then
        start_action state game C_DASH_COOLDOWN
      else
        continue_action state game C_DASH
    in

    let start_c_dash_or_cancel_charge () =
      let can_start_c_dash () =
        let down_since = state.frame.time -. game.ghost.history.charge_c_dash.started.at in
        down_since > game.ghost.history.charge_c_dash.config.duration.seconds
      in

      if can_start_c_dash () then (
        stop_wall_sliding := true;
        start_action state game C_DASH;
        true)
      else (
        game.ghost.current.is_charging_c_dash <- false;
        false)
    in

    let this_frame =
      if starting_charge () then (
        start_action state game C_DASH_CHARGE;
        true)
      else if still_charging () then (
        continue_action state game C_DASH_CHARGE;
        true)
      else if still_cooling_down () then (
        let action_kind =
          match game.ghost.current.wall with
          | None -> C_DASH_COOLDOWN
          | Some _ -> C_DASH_WALL_COOLDOWN
        in
        continue_action state game action_kind;
        true)
      else if still_c_dashing () then (
        (* this function should still return true if the ghost is ending the c-dash this frame
           because if it is false, the ghost has a frame to input directions to turn around and
           cooldown in the wrong direction
        *)
        maybe_end_c_dash ();
        true)
      else if released_charge () then (
        remove_child game.ghost C_DASH_CHARGE_CRYSTALS;
        remove_child game.ghost C_DASH_WALL_CHARGE_CRYSTALS;
        start_c_dash_or_cancel_charge ())
      else
        false
    in
    { this_frame }
  in

  let handle_dashing () : handled_action =
    let starting_dash () =
      (* attack direction is arbitrary *)
      game.ghost.abilities.mothwing_cloak
      && key_pressed_or_buffered DASH
      && (not (is_doing game.ghost (ATTACK RIGHT) state.frame.time))
      && (not (is_casting state game))
      && Option.is_none game.ghost.entity.x_recoil
      && game.ghost.current.can_dash
      && past_cooldown game.ghost.history.dash state.frame.time
    in
    let still_dashing () = is_doing game.ghost DASH state.frame.time in
    let still_shade_dashing () = is_doing game.ghost SHADE_DASH state.frame.time in
    let this_frame =
      if starting_dash () then (
        (* TODO "dash in currently-facing direction" almost always works, but not when walljumping,
           so this should probably be `DASHING of direction` and check LEFT/RIGHT keys
        *)
        let can_shade_dash () =
          game.ghost.abilities.shade_cloak
          && past_cooldown game.ghost.history.shade_dash state.frame.time
        in

        if can_shade_dash () then (
          spawn_child game.ghost SHADE_DASH_SPARKLES ~in_front:true ~scale:6.
            (ALIGNED (CENTER, CENTER))
            game.ghost.shared_textures.shade_cloak_sparkles;
          start_action state game SHADE_DASH);
        start_action state game DASH;
        stop_wall_sliding := true;
        true)
      else if still_dashing () then (
        if still_shade_dashing () then
          continue_action state game SHADE_DASH;
        continue_action state game DASH;
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
    let starting_wall_kick () =
      Option.is_some game.ghost.current.wall && key_pressed_or_buffered JUMP
    in
    let continuing_wall_kick () =
      state.frame.time -. game.ghost.history.wall_kick.started.at
      < game.ghost.history.wall_kick.config.duration.seconds
    in
    let this_frame =
      if starting_wall_kick () then (
        stop_wall_sliding := true;
        start_action state game WALL_KICK;
        true)
      else if continuing_wall_kick () then (
        stop_wall_sliding := true;
        continue_action state game WALL_KICK;
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
      if
        game.ghost.abilities.monarch_wings
        && game.ghost.current.can_flap
        && state.frame_inputs.jump.pressed
      then (
        game.ghost.current.can_flap <- false;
        start_action state game FLAP)
      else if is_doing game.ghost FLAP state.frame.time then (
        set_pose' (AIRBORNE new_vy);
        if state.frame_inputs.jump.down then
          continue_action state game FLAP
        else
          cancel_action state game FLAP)
      else
        set_pose' (AIRBORNE new_vy)
    | Some floor ->
      if is_doing game.ghost FLAP state.frame.time then
        (* TODO need a check like this for current_wall too (to prevent extra height from buffering jump off wallslide) *)
        cancel_action state game FLAP
      else if key_pressed_or_buffered JUMP then
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
    let starting_attack () =
      past_cooldown game.ghost.history.nail state.frame.time && key_pressed_or_buffered NAIL
    in
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
      start_action state game (ATTACK direction))
    else (
      match get_current_slash game.ghost with
      | None -> ()
      | Some slash -> (
        continue_action state game (ATTACK slash.direction);
        match Sprite.advance_or_despawn state.frame.time slash.sprite.texture slash.sprite with
        | None -> remove_child game.ghost (NAIL slash)
        | Some _ -> ()))
  in

  let handle_dream_nail () : handled_action =
    let starting_dream_nail () =
      state.frame_inputs.dream_nail.pressed
      && Entity.on_ground game.ghost.entity
      && not_doing_any game.ghost state.frame.time [ ATTACK RIGHT; DASH ]
    in
    (* arbitrary directions *)
    let still_dream_nailing () =
      is_doing game.ghost DREAM_NAIL state.frame.time && state.frame_inputs.dream_nail.down
    in
    let this_frame =
      if starting_dream_nail () then (
        start_action state game DREAM_NAIL;
        true)
      else if still_dream_nailing () then (
        continue_action state game DREAM_NAIL;
        true)
      else
        false
    in
    { this_frame }
  in

  let handle_focusing () : handled_action =
    let starting_focus () =
      state.frame_inputs.focus.pressed
      && Entity.on_ground game.ghost.entity
      && game.ghost.soul.current >= Config.action.soul_per_cast
      && not_doing_any game.ghost state.frame.time [ ATTACK RIGHT; DASH ]
      && (not (is_casting state game))
    in
    let still_focusing () =
      is_doing game.ghost FOCUS state.frame.time && state.frame_inputs.focus.down
    in
    let this_frame =
      if starting_focus () then (
        start_action state game FOCUS;
        true)
      else if still_focusing () then (
        continue_action state game FOCUS;
        true)
      else (
        if state.frame_inputs.focus.released then
          remove_child game.ghost FOCUS;
        false)
    in
    { this_frame }
  in

  let handle_collisions () =
    let check_enemy_collisions () =
      if is_vulnerable state game then (
        match get_enemy_collision game.ghost game.room with
        | Some direction -> start_action state game (TAKE_DAMAGE direction)
        | None -> ())
    in
    let check_damage_collisions () =
      let collisions = Entity.get_damage_collisions game.room game.ghost.entity in
      state.debug.rects <-
        List.map (fun c -> (Raylib.Color.red, snd c)) collisions @ state.debug.rects;
      if List.length collisions = 0 then (
        if game.ghost.current.is_taking_hazard_damage && Option.is_some game.ghost.entity.y_recoil
        then (
          (* HACK this is just to get the ghost to continue colliding with the spikes, even if
             they tried to pogo on the same frame they hit the spikes
             - not really sure how else to check this, and it's working well enough
          *)
          game.ghost.entity.dest.pos.y <- game.ghost.entity.dest.pos.y +. 10.;
          game.ghost.entity.y_recoil <- None))
      else if is_doing game.ghost TAKE_DAMAGE_AND_RESPAWN state.frame.time then
        continue_action state game TAKE_DAMAGE_AND_RESPAWN
      else if game.ghost.current.is_taking_hazard_damage then (
        game.ghost.current.is_taking_hazard_damage <- false;
        hazard_respawn game)
      else (
        take_damage game.ghost;
        state.camera.shake <- 1.;
        game.ghost.current.is_taking_hazard_damage <- true;
        start_action state game TAKE_DAMAGE_AND_RESPAWN)
    in
    let handle_wall_collisions collisions =
      let wall_collision =
        List.find_opt
          (fun ((c, _) : collision * 'a) -> List.mem c.direction [ LEFT; RIGHT ])
          collisions
      in
      match wall_collision with
      | None -> ()
      | Some (_, wall_rect) ->
        if game.ghost.current.is_c_dashing then (
          if game.ghost.abilities.mantis_claw then
            game.ghost.current.wall <- Some wall_rect;
          state.camera.shake <- 1.;
          start_action state game C_DASH_WALL_COOLDOWN)
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
              if (not (Entity.on_ground game.ghost.entity)) && Entity.descending game.ghost.entity
              then
                if high_enough_to_slide_on wall then (
                  match c.direction with
                  | LEFT -> set_pose' (WALL_SLIDING wall)
                  | RIGHT -> set_pose' (WALL_SLIDING wall)
                  | _ -> ())
            in
            List.iter check_collision collisions))
    in
    let floor_collisions = Entity.get_floor_collisions game.room game.ghost.entity in

    check_enemy_collisions ();
    state.debug.rects <-
      List.map (fun c -> (Raylib.Color.green, snd c)) floor_collisions @ state.debug.rects;
    apply_collisions game.ghost.entity floor_collisions;
    handle_wall_sliding floor_collisions;
    check_damage_collisions ();
    handle_wall_collisions floor_collisions
  in

  let check_cooldowns cooldowns =
    let cooling_down = ref false in
    let check cooldown =
      if not !cooling_down then
        if is_doing game.ghost cooldown state.frame.time then (
          continue_action state game cooldown;
          cooling_down := true)
    in
    List.iter check cooldowns;
    !cooling_down
  in

  let reveal_shadows () =
    let colliding (_label, trigger_rect) =
      Option.is_some (Collision.with_entity game.ghost.entity trigger_rect)
    in
    match List.find_opt colliding game.room.triggers.shadows with
    | None -> ()
    | Some (layer_name, _rect) -> (
      match List.find_opt (fun (l : layer) -> l.name = layer_name) game.room.layers with
      | None -> failwithf "Ghost.update: could not find layer '%s' to reveal" layer_name
      | Some layer ->
        if not (List.mem layer_name game.room.progress.revealed_shadow_layers) then
          game.room.progress.revealed_shadow_layers <-
            layer_name :: game.room.progress.revealed_shadow_layers;
        layer.hidden <- true)
  in

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
  let exiting = Room.handle_transitions state game in
  if not exiting then (
    let interacting = handle_interactions () in
    if interacting then
      Entity.update_pos game.room game.ghost.entity state.frame.dt
    else (
      reveal_shadows ();
      if game.ghost.entity.update_pos then (
        let cooling_down =
          check_cooldowns [ DIVE_COOLDOWN; C_DASH_COOLDOWN; C_DASH_WALL_COOLDOWN ]
        in
        if not cooling_down then (
          let dream_nailing = handle_dream_nail () in
          if not dream_nailing.this_frame then (
            let focusing = handle_focusing () in
            if not focusing.this_frame then (
              let c_dashing = handle_c_dashing () in
              if not c_dashing.this_frame then (
                let dashing = handle_dashing () in
                if not dashing.this_frame then (
                  let casting = handle_casting () in
                  if not casting.this_frame then (
                    let wall_kicking = handle_wall_kicking () in
                    if not wall_kicking.this_frame then (
                      handle_walking ();
                      handle_jumping new_vy);
                    handle_attacking ();
                    resolve_slash_collisions state game)))))))));

  animate_and_despawn_children state.frame.time game.ghost;
  handle_collisions ();
  maybe_unset_current_floor game.ghost;
  Sprite.advance_animation state.frame.time game.ghost.entity.sprite.texture
    game.ghost.entity.sprite;
  state

let load_shared_textures (shared_texture_configs : (string * texture_config) list) =
  let build_shared_texture ?(particle = false) ?(once = false) ?(config_name = None) pose_name =
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

    Sprite.build_texture_from_config ~particle ~once config
  in

  let build_slash_texture name =
    build_shared_texture ~particle:true ~config_name:(Some "nail") name
  in
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
    c_dash_crystals = build_shared_texture ~once:true "c-dash-crystals";
    c_dash_crystals_full = build_shared_texture "c-dash-crystals-full";
    c_dash_wall_crystals = build_shared_texture ~once:true "c-dash-wall-crystals";
    c_dash_wall_crystals_full = build_shared_texture "c-dash-wall-crystals-full";
    c_dash_whoosh = build_shared_texture "c-dash-whoosh";
    shade_cloak_sparkles = build_shared_texture ~particle:true "shade-cloak-sparkles";
  }

let init
    (ghost_id : ghost_id)
    (textures : ghost_textures)
    (in_party : bool)
    (action_config : (string * ghost_action_config) list)
    (start_pos : vector)
    (save_file : Json_t.save_file)
    weapons_configs
    shared_textures : ghost =
  let use_json_action_config action_name : ghost_action_config =
    match List.assoc_opt action_name action_config with
    | None -> failwithf "could not find action config for '%s' in ghosts/config.json" action_name
    | Some config -> config
  in
  let dest = Sprite.make_dest start_pos.x start_pos.y textures.idle in
  let make_action (config_name : string) : ghost_action =
    {
      doing_until = { at = -1000. };
      blocked_until = { at = -1000. };
      config = use_json_action_config config_name;
      started = { at = -1000. };
    }
  in

  let load_weapon name = (name, List.assoc name weapons_configs) in
  let weapons = List.map load_weapon save_file.weapons in

  let max_health =
    let finished_interaction_names =
      match List.assoc_opt save_file.room_name save_file.progress with
      | None -> []
      | Some progress -> progress.finished_interactions
    in
    let health_increases =
      List.filter (String.starts_with ~prefix:"health_") finished_interaction_names |> List.length
    in
    let base_health = 5 in
    base_health + health_increases
  in

  {
    id = ghost_id;
    in_party;
    current = reset_current_status ();
    textures;
    children = [];
    history =
      {
        cast_dive = make_action "cast-dive";
        cast_vs = make_action "cast-vs";
        cast_wraiths = make_action "cast-wraiths";
        charge_c_dash = make_action "charge-c-dash";
        c_dash = make_action "c-dash";
        c_dash_cooldown = make_action "c-dash-cooldown";
        c_dash_wall_cooldown = make_action "c-dash-cooldown";
        dream_nail = make_action "dream-nail";
        dash = make_action "dash";
        dive_cooldown = make_action "dive-cooldown";
        flap = make_action "flap";
        focus = make_action "focus";
        jump = make_action "jump";
        nail = make_action "nail";
        shade_dash = make_action "shade-dash";
        take_damage = make_action "take-damage";
        take_damage_and_respawn = make_action "take-damage-and-respawn";
        wall_kick = make_action "wall-kick";
      };
    shared_textures;
    entity =
      Entity.create_for_sprite
        (Sprite.create
           (fmt "ghost-%s" (Show.ghost_id ghost_id))
           ~collision:(Some DEST) textures.idle dest)
        {
          pos = clone_vector dest.pos;
          w = Config.ghost.width *. Config.scale.ghost;
          h = Config.ghost.height *. Config.scale.ghost;
        };
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
    abilities = save_file.abilities;
    weapons;
    current_weapon =
      (* TODO should read this from weapons.json *)
      {
        name = "old-nail";
        tint = Raylib.Color.raywhite;
        scale_x = 1.;
        scale_y = 1.;
        cooldown_scale = 1.;
      };
  }
