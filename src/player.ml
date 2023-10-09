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
  let parse_texture_config character_name ((pose_name, config) : string * Json_t.texture_config) :
      texture_config =
    {
      path = { asset_dir = GHOSTS; character_name; pose_name };
      count = config.count;
      duration = { seconds = config.duration };
      x_offset = config.x_offset |> Int.to_float;
      y_offset = config.y_offset |> Int.to_float;
    }
  in
  let parse_ghost_texture
      ((ghost_id_str, ghost_poses) : string * (string * Json_t.texture_config) list) :
      ghost_id * texture_config list =
    let texture_configs : texture_config list =
      List.map (parse_texture_config ghost_id_str) ghost_poses
    in
    (parse_name ghost_id_str, texture_configs)
  in
  let load_textures name textures =
    let parse_texture_config' (s, tc) : string * texture_config =
      (s, parse_texture_config name (s, tc))
    in
    List.map parse_texture_config' textures
  in
  let shared_textures : (string * texture_config) list =
    load_textures "shared" ghost_file.shared_textures
  in
  let body_textures : (string * texture_config) list =
    load_textures "body" ghost_file.body_textures
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
  { actions; body_textures; shared_textures }

let ghost_ids_in_party ghosts : ghost_id list =
  ghosts |> List.filter_map (fun g -> if g.in_party then Some g.ghost.id else None)

let maybe_begin_interaction (state : state) (game : game) trigger =
  let begin_interaction ?(increase_health = false) trigger =
    game.interaction.steps <- Interactions.get_steps ~increase_health state game trigger
  in
  let begin_health_interaction name trigger =
    (* these can be repeated, but health should only be increased once *)
    let increase_health = not (List.mem name game.room.progress.finished_interactions) in
    if increase_health then
      game.room.progress.finished_interactions <- name :: game.room.progress.finished_interactions;
    begin_interaction ~increase_health trigger
  in
  let begin_cutscene_interaction name trigger =
    (* these are only viewable once *)
    if not (List.mem name game.room.progress.finished_interactions) then (
      game.room.progress.finished_interactions <- name :: game.room.progress.finished_interactions;
      begin_interaction trigger)
  in
  let strip_blocking_interaction (full_name : string) : string =
    Utils.maybe_trim_before '|' full_name
  in
  let name = strip_blocking_interaction trigger.full_name in
  match trigger.kind with
  | WARP _
  | PURPLE_PEN
  | INFO ->
    (* these have no side effects and can be repeated *)
    begin_interaction trigger
  | HEALTH -> begin_health_interaction name trigger
  | D_NAIL
  | BOSS_KILLED
  | ITEM
  | CUTSCENE -> (
    match game.mode with
    | STEEL_SOLE -> ()
    | CLASSIC -> begin_cutscene_interaction name trigger)
  | CAMERA _
  | LEVER _
  | SHADOW
  | RESPAWN ->
    failwithf "cannot begin interaction with kind %s" (Show.trigger_kind trigger.kind)

let find_trigger_collision' ghost (xs : 'a list) get_trigger_dest : 'a option =
  let colliding x =
    Option.is_some (Collision.with_entity ghost.ghost.entity (get_trigger_dest x))
  in
  List.find_opt colliding xs

let find_trigger_collision ghost (triggers : trigger list) =
  find_trigger_collision' ghost triggers (fun t -> t.dest)

let find_respawn_trigger_collision ghost (triggers : (vector * trigger) list) =
  find_trigger_collision' ghost triggers (fun (_, t) -> t.dest)

(* returns the first enemy collision *)
let get_enemy_collision (player : player) (room : room) : (int * direction) option =
  let find_colliding_enemy ((_enemy_id, enemy) : enemy_id * enemy) =
    if Enemy.is_dead enemy then
      None
    else if Collision.between_entities player.ghost.entity enemy.entity then (
      let direction : direction =
        if rect_center_x enemy.entity.dest > rect_center_x player.ghost.entity.dest then
          LEFT
        else
          RIGHT
      in
      Some (enemy.damage, direction))
    else
      None
  in
  List.find_map find_colliding_enemy room.enemies

let pogo (player : player) =
  player.ghost.entity.current_floor <- None;
  player.current.can_dash <- true;
  player.current.can_flap <- true;
  player.ghost.entity.y_recoil <-
    Some { speed = -800.; time_left = { seconds = 0.2 }; reset_v = true }

(* - adjusts x position for side collisions to place sprite next to colliding rect
   - then adjusts y position for top/bottom collisions
   - updates y velocity for bonking head, but other velocity updates happen elsewhere (set_pose)
*)
(* TODO this is pretty similar to Entity.apply_collisions now, so it might make sense to combine them *)
let apply_collisions
    ?(floor_v = Zero.vector ())
    (entity : entity)
    (collisions : (collision * rect) list) : unit =
  let adjust_position ((coll, floor) : collision * rect) =
    let top_of r = r.pos.y in
    let bottom_of r = r.pos.y +. r.h in
    let left_of r = r.pos.x in
    let right_of r = r.pos.x +. r.w in
    match coll.direction with
    | UP ->
      (* checks here prevent floor from being set for a single frame when jumping/pogoing over corners *)
      if Option.is_none entity.y_recoil then (
        entity.current_floor <- Some (floor, floor_v);
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
    (player : player)
    (direction : direction)
    relative_pos
    (sprite : sprite)
    (frame_time : float) : slash =
  let textures = player.shared_textures in
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
      (dest_w', dest_h' *. player.current_weapon.scale_y)
    else
      (dest_w' *. player.current_weapon.scale_x, dest_h')
  in
  let dest =
    let pos = Entity.get_child_pos player.ghost.entity relative_pos dest_w dest_h in
    { pos; h = dest_h; w = dest_w }
  in

  (* TODO move these to config *)
  let sideways_points =
    [
      { x = 0. *. player.current_weapon.scale_x; y = 0. };
      { x = 111. *. player.current_weapon.scale_x; y = 8. };
      { x = 158. *. player.current_weapon.scale_x; y = 16. };
      { x = 184. *. player.current_weapon.scale_x; y = 26. };
      { x = 196. *. player.current_weapon.scale_x; y = 37. };
      { x = 195. *. player.current_weapon.scale_x; y = 49. };
      { x = 178. *. player.current_weapon.scale_x; y = 64. };
      { x = 136. *. player.current_weapon.scale_x; y = 77. };
      { x = 60. *. player.current_weapon.scale_x; y = 86. };
      { x = 1. *. player.current_weapon.scale_x; y = 87. };
    ]
  in
  let up_points =
    [
      { x = 0.; y = 189. *. player.current_weapon.scale_y };
      { x = 4.; y = 110. *. player.current_weapon.scale_y };
      { x = 12.; y = 63. *. player.current_weapon.scale_y };
      { x = 25.; y = 25. *. player.current_weapon.scale_y };
      { x = 35.; y = 8. *. player.current_weapon.scale_y };
      { x = 45.; y = 0. *. player.current_weapon.scale_y };
      { x = 60.; y = 1. *. player.current_weapon.scale_y };
      { x = 73.; y = 14. *. player.current_weapon.scale_y };
      { x = 87.; y = 46. *. player.current_weapon.scale_y };
      { x = 97.; y = 86. *. player.current_weapon.scale_y };
      { x = 103.; y = 189. *. player.current_weapon.scale_y };
    ]
  in
  let down_points =
    List.map
      (fun (point : vector) ->
        { point with y = (189. -. point.y) *. player.current_weapon.scale_y })
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

let get_spell_sprite (player : player) : (sprite * time) option =
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) : (sprite * time) option =
    match child_kind with
    | WRAITHS -> Some (child.sprite, player.history.cast_wraiths.started)
    | DIVE -> Some (child.sprite, player.history.cast_dive.started)
    | DIVE_COOLDOWN -> Some (child.sprite, player.history.dive_cooldown.started)
    | _ -> None
  in
  List.find_map get_sprite player.children

let get_current_slash (player : player) : slash option =
  let get_slash ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | NAIL slash -> Some slash
    | _ -> None
  in
  List.find_map get_slash player.children

let get_dream_nail (player : player) : rect option =
  let get_rect ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | DREAM_NAIL -> Some child.sprite.dest
    | _ -> None
  in
  List.find_map get_rect player.children

let get_dive_sprite (player : player) : sprite option =
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | DIVE
    | DIVE_COOLDOWN ->
      Some child.sprite
    | _ -> None
  in
  List.find_map get_sprite player.children

let get_focus_sparkles ghost : sprite option =
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | FOCUS -> Some child.sprite
    | _ -> None
  in
  List.find_map get_sprite ghost.children

(* TODO move into spawn_child - can't do this until make_ghost_child goes away *)
let add_child (player : player) (kind : ghost_child_kind) (child : ghost_child) =
  player.children <- Utils.replace_assoc kind child player.children

let remove_child (player : player) (kind : ghost_child_kind) =
  player.children <- List.remove_assoc kind player.children

(* dive sprites are using this fn with the placeholder image, but it can eventually be
   combined with make_ghost_child'
*)
let make_ghost_child ?(in_front = false) (player : player) kind relative_pos texture w h =
  let name = Show.ghost_child_kind kind in
  let sprite =
    Sprite.create name texture
      { pos = Entity.get_child_pos player.ghost.entity relative_pos w h; w; h }
  in
  { relative_pos; sprite; in_front }

let spawn_child
    ?(in_front = false)
    ?(scale = 1.)
    (player : player)
    (child_kind : ghost_child_kind)
    alignment
    texture =
  let child =
    let w, h = get_scaled_texture_size ~scale texture in
    make_ghost_child player ~in_front child_kind alignment texture w h
  in
  add_child player child_kind child

let make_c_dash_child ?(full = false) (player : player) : unit =
  let texture =
    match (player.current.wall, full) with
    | None, true -> player.shared_textures.c_dash_crystals_full
    | None, false -> player.shared_textures.c_dash_crystals
    | Some _, true -> player.shared_textures.c_dash_wall_crystals_full
    | Some _, false -> player.shared_textures.c_dash_wall_crystals
  in
  let alignment =
    match (player.current.wall, player.ghost.entity.sprite.facing_right) with
    | None, _ -> ALIGNED (CENTER, BOTTOM)
    | Some _, true -> ALIGNED (LEFT, CENTER)
    | Some _, false -> ALIGNED (RIGHT, CENTER)
  in
  let child_kind =
    match player.current.wall with
    | None -> C_DASH_CHARGE_CRYSTALS
    | Some _ -> C_DASH_WALL_CHARGE_CRYSTALS
  in
  Sprite.reset_texture texture;
  spawn_child player child_kind alignment ~scale:Config.scale.ghost texture

let animate_and_despawn_children frame_time ghost : unit =
  let advance ((child_kind, child) : ghost_child_kind * ghost_child) =
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

let get_damage (player : player) (damage_kind : damage_kind) =
  (* TODO check ghost.abilities.descending_dark/shade_soul *)
  match damage_kind with
  | DREAM_NAIL -> 0
  | NAIL ->
    player.weapons
    |> List.map snd
    |> List.map (fun (w : Json_t.weapon) -> w.damage)
    |> List.fold_left ( + ) 0
  | VENGEFUL_SPIRIT -> 15
  | DESOLATE_DIVE -> 15
  | DESOLATE_DIVE_SHOCKWAVE -> 20
  | HOWLING_WRAITHS -> (* TODO this should be 13 with multihits *) 26

(* TODO use collision shape for dream nail *)
let check_dream_nail_collisions (state : state) (game : game) =
  match get_dream_nail game.player with
  | None -> ()
  | Some dream_nail_dest ->
    let resolve_enemy ((_enemy_id, enemy) : enemy_id * enemy) =
      if Enemy.is_alive enemy then
        if enemy.json.dream_nail.vulnerable then (
          match Collision.between_rects dream_nail_dest enemy.entity.sprite.dest with
          | None -> ()
          | Some c ->
            (* TODO add dream nail sound *)
            let text =
              if List.length enemy.json.dream_nail.dialogues = 0 then
                "I have no dreams."
              else
                Utils.sample enemy.json.dream_nail.dialogues
            in
            game.interaction.floating_text <-
              Some { content = text; visible = TIME { at = state.frame.time +. 1. } };
            if game.player.history.dream_nail.started > Enemy.took_damage_at enemy DREAM_NAIL then (
              (* TODO make a new fn Ghost.add/deduct_soul that bounds between [0, soul max] *)
              game.player.soul.current <-
                Utils.bound_int 0
                  (game.player.soul.current + Config.action.soul_per_cast)
                  game.player.soul.max;
              let recoil_speed =
                if game.player.ghost.entity.sprite.facing_right then
                  enemy.json.dream_nail.recoil_vx
                else
                  -.enemy.json.dream_nail.recoil_vx
              in
              enemy.entity.x_recoil <-
                Some { speed = recoil_speed; time_left = { seconds = 0.1 }; reset_v = true };
              enemy.history <-
                Utils.replace_assoc
                  (TOOK_DAMAGE DREAM_NAIL : enemy_action)
                  { at = state.frame.time } enemy.history))
    in
    let resolve_trigger (trigger : trigger) =
      match Collision.between_rects dream_nail_dest trigger.dest with
      | None -> ()
      | Some c -> maybe_begin_interaction state game trigger
    in
    List.iter resolve_enemy game.room.enemies;
    List.iter resolve_trigger game.room.triggers.d_nail

let add_phantom_floor (game : game) (target : vector) =
  game.player.ghost.entity.current_floor <-
    Some
      ( {
          pos = { x = target.x; y = target.y +. game.player.ghost.entity.dest.h };
          w = game.player.ghost.entity.dest.w;
          h = 20.;
        },
        Zero.vector () )

let resolve_slash_collisions (state : state) (game : game) =
  match get_current_slash game.player with
  | None -> check_dream_nail_collisions state game
  | Some slash ->
    let resolve_enemy ((_enemy_id, enemy) : enemy_id * enemy) =
      if enemy.json.can_take_damage && enemy.status.check_damage_collisions then (
        match Collision.with_slash slash enemy.entity.sprite with
        | None -> ()
        | Some collision ->
          play_sound state "punch";
          if
            Enemy.maybe_take_damage state enemy game.player.history.nail.started NAIL
              (get_damage game.player NAIL) collision
          then (
            (match collision.direction with
            | DOWN -> pogo game.player
            | LEFT
            | RIGHT ->
              (* TODO recoil enemy *)
              Entity.recoil_backwards game.player.ghost.entity
                { speed = 800.; time_left = { seconds = 0.1 }; reset_v = true }
            | UP ->
              if game.player.ghost.entity.v.y < 0. then
                game.player.ghost.entity.v.y <- 300.);
            game.player.soul.current <-
              Utils.bound_int 0
                (game.player.soul.current + Config.action.soul_gained_per_nail)
                game.player.soul.max);
          if slash.direction = DOWN && game.player.ghost.entity.y_recoil = None then (
            (* TODO this isn't working (can't pogo LB projectiles) *)
            let check_projectile_pogo (projectile : projectile) =
              if projectile.pogoable then (
                match Collision.with_slash slash projectile.entity.sprite with
                | None -> ()
                | Some _ -> pogo game.player)
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

    let maybe_pogo_platform_spikes (id, rect) =
      match Collision.with_slash' slash rect with
      | None -> ()
      | Some coll -> (
        play_sound state "nail-hit-metal";
        match coll.direction with
        | DOWN -> (
          (* always pogo, but only un-rotate the platform if it is upright *)
          pogo game.player;
          let platform = List.find (fun (p : platform) -> p.id = id) game.room.platforms in
          match platform.kind with
          | Some (ROTATABLE (UPSIDE_DOWN _)) ->
            Platform.reset_rotation platform game state.global.textures
          | _platform_kind_opt -> ())
        | _direction -> ())
    in

    let maybe_pogo sound_name rect =
      match Collision.with_slash' slash rect with
      | None -> ()
      | Some coll -> (
        play_sound state sound_name;
        match coll.direction with
        | DOWN -> pogo game.player
        | _direction -> ())
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
          (* TODO organize these sound effects (probably just combine these all into one) *)
          play_sound state "break";
          play_sound state "alarmswitch";
          play_sound state "punch";
          layer.spawned_fragments <-
            List.map (spawn_fragment collision) tile_group.fragments @ layer.spawned_fragments;
          let idx = List.nth tile_group.tile_idxs 0 in
          (match List.assoc_opt idx game.room.idx_configs with
          | Some (PURPLE_PEN name) ->
            game.progress.steel_sole.purple_pens <-
              (state.frame.idx, name) :: game.progress.steel_sole.purple_pens;
            maybe_begin_interaction state game (make_stub_trigger PURPLE_PEN "purple-pen" name)
          | _ -> ());
          destroy_tile_group layer tile_group;
          (match game.mode with
          | CLASSIC -> ()
          | STEEL_SOLE ->
            if layer.name = "doors" then
              add_phantom_floor game game.player.ghost.entity.dest.pos);
          if collision.direction = DOWN && layer.config.pogoable then
            pogo game.player;
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
              if door_health.last_hit_at < game.player.history.nail.started.at then (
                door_health.last_hit_at <- state.frame.time;
                if door_health.hits > 1 then (
                  (* TODO this isn't quite working - the fragments are spawning on top of the door and not moving *)
                  let make_random_fragment _n = Utils.sample tile_group.fragments in
                  let random_fragments = List.init (Random.int 3) make_random_fragment in
                  layer.spawned_fragments <-
                    List.map (spawn_fragment coll) random_fragments @ layer.spawned_fragments;
                  door_health.hits <- door_health.hits - 1;
                  play_sound state "alarmswitch";
                  play_sound state "punch";
                  new_tile_groups := tile_group :: !new_tile_groups)
                else
                  destroy_object tile_group coll)
              else
                new_tile_groups := tile_group :: !new_tile_groups;
              ())
        in
        List.iter resolve_tile_group layer.tile_groups;
        layer.tile_groups <- !new_tile_groups)
    in

    let resolve_lever ((lever_sprite, _, trigger) : sprite * int * trigger) =
      let layer =
        match List.find_opt (fun (l : layer) -> l.name = "lever-doors") game.room.layers with
        | None ->
          failwithf "room %s has levers '%s', but no lever-doors layer" (Show.room_id game.room.id)
            trigger.full_name
        | Some l -> l
      in
      let new_tile_groups : tile_group list ref = ref [] in
      let door_tile_idx =
        match trigger.kind with
        | LEVER lever -> lever.door_tile_idx
        | _ -> failwith "lever trigger needs LEVER kind"
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
    List.iter (maybe_pogo "nail-hit-metal") game.room.spikes;
    List.iter maybe_pogo_platform_spikes game.room.platform_spikes;
    List.iter resolve_enemy game.room.enemies

let reset_current_status ?(taking_hazard_damage = None) () =
  {
    wall = None;
    water = None;
    is_diving = false;
    is_c_dashing = false;
    is_charging_c_dash = false;
    is_taking_hazard_damage =
      (match taking_hazard_damage with
      | None -> false
      | Some v -> v);
    can_dash = true;
    can_flap = true;
  }

let take_damage (player : player) (damage : int) =
  (* preserve the original value of is_taking_hazard_damage to prevent soft-locks when taking damage while hazard respawning *)
  player.current <-
    reset_current_status ~taking_hazard_damage:(Some player.current.is_taking_hazard_damage) ();
  player.children <- [];
  player.ghost.entity.current_floor <- None;
  player.health.current <- player.health.current - damage;
  if player.health.current <= 0 then
    (* TODO handle dying:
       - reset in-progress boss interactions
    *)
    player.health.current <- player.health.max

let set_new_body_texture ghost body_texture =
  ghost.body_render_offset <- body_texture.render_offset;
  Entity.update_sprite_texture ghost.entity body_texture.texture'

let set_ghost_textures ghost ~head_texture ~body_texture =
  ghost.head <- head_texture;
  set_new_body_texture ghost body_texture

(* state updates for current pose, texture animation, and sprite *)
let set_pose player (new_pose : ghost_pose) (bodies : ghost_body_textures) (frame_time : float) =
  let update_vx multiplier =
    let mult = if player.ghost.entity.sprite.facing_right then multiplier else -1. *. multiplier in
    player.ghost.entity.v.x <- mult *. Config.ghost.vx
  in
  (* TODO bad name *)
  let reset_standing_abilities () =
    if Entity.on_ground player.ghost.entity then (
      player.current.can_dash <- true;
      player.current.can_flap <- true)
  in
  let set_facing_right ?(allow_vertical = true) (direction : direction) =
    Entity.set_facing_right ~allow_vertical player.ghost.entity direction
  in

  let handle_cast (spell_kind : spell_kind) : texture * ghost_body_texture =
    match spell_kind with
    | VENGEFUL_SPIRIT ->
      Entity.recoil_backwards player.ghost.entity
        { speed = 80.; time_left = { seconds = 0.16666 }; reset_v = true };
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.walk, bodies.cast)
    | DESOLATE_DIVE -> (player.ghost.head_textures.look_up, bodies.dive)
    | HOWLING_WRAITHS ->
      player.ghost.entity.v.x <- 0.;
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.walk, bodies.cast)
  in

  let handle_action_kind action_kind : texture * ghost_body_texture =
    match action_kind with
    | ATTACK direction ->
      (* handle_attacking is called after handle_walking, so this allows the ghost to walk backwards while attacking *)
      set_facing_right direction;
      let head =
        match direction with
        | UP -> player.ghost.head_textures.look_up
        | _ -> player.ghost.head_textures.idle
      in
      (head, bodies.nail)
    | DREAM_NAIL ->
      update_vx 0.;
      (player.ghost.head_textures.idle, bodies.nail)
    | C_DASH_WALL_COOLDOWN ->
      update_vx 0.;
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.wall_slide, bodies.wall_slide)
    | C_DASH_COOLDOWN ->
      update_vx (3. -. (4. *. (frame_time -. player.history.c_dash_cooldown.started.at)));
      player.ghost.entity.v.y <- 0.;
      (* TODO new image/texture for this *)
      (player.ghost.head_textures.walk, bodies.cast)
    | C_DASH ->
      player.ghost.entity.v.y <- 0.;
      update_vx 3.;
      (player.ghost.head_textures.idle, bodies.dash)
    | C_DASH_CHARGE ->
      update_vx 0.;
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.look_down, bodies.focus)
    | SHADE_DASH
    | DASH ->
      player.current.can_dash <- false;
      player.ghost.entity.v.y <- 0.;
      update_vx 2.;
      (player.ghost.head_textures.idle, bodies.dash)
    | CAST DESOLATE_DIVE ->
      update_vx 0.;
      player.ghost.entity.v.y <- Config.ghost.dive_vy;
      (player.ghost.head_textures.look_up, bodies.dive)
    | CAST spell_kind -> handle_cast spell_kind
    | DIVE_COOLDOWN ->
      update_vx 0.;
      (player.ghost.head_textures.look_up, bodies.dive)
    | FOCUS ->
      update_vx 0.;
      (player.ghost.head_textures.look_down, bodies.focus)
    | TAKE_DAMAGE_AND_RESPAWN -> (player.ghost.head_textures.take_damage, bodies.take_damage)
    | TAKE_DAMAGE (damage, direction) ->
      let x_recoil_speed =
        match direction with
        | LEFT -> -800.
        | RIGHT -> 800.
        | _ -> if player.ghost.entity.sprite.facing_right then 800. else -800.
      in
      player.ghost.entity.x_recoil <-
        Some { speed = x_recoil_speed; time_left = { seconds = 0.06666 }; reset_v = true };
      player.ghost.entity.y_recoil <-
        Some { speed = -800.; time_left = { seconds = 0.06666 }; reset_v = true };
      take_damage player damage;
      (* from recoiling upwards *)
      player.ghost.entity.current_floor <- None;
      (player.ghost.head_textures.take_damage, bodies.take_damage)
    | JUMP ->
      player.ghost.entity.v.y <- Config.ghost.jump_vy;
      player.ghost.entity.current_floor <- None;
      (player.ghost.head_textures.idle, bodies.jump)
    | WALL_KICK ->
      player.ghost.entity.v.y <- Config.ghost.wall_jump_vy;
      update_vx 1.;
      (player.ghost.head_textures.idle, bodies.jump)
    | FLAP -> (player.ghost.head_textures.idle, bodies.flap)
  in

  let (head_texture, body_texture) : texture * ghost_body_texture =
    match new_pose with
    | PERFORMING action_kind -> handle_action_kind action_kind
    | AIRBORNE new_vy ->
      player.ghost.entity.v.y <- new_vy;
      player.ghost.entity.current_floor <- None;
      if player.ghost.entity.v.y > Config.physics.jump_fall_threshold then
        (player.ghost.head_textures.idle, bodies.fall)
      else
        (player.ghost.head_textures.idle, bodies.jump)
    | CRAWLING ->
      update_vx 0.;
      (player.ghost.head_textures.idle, bodies.crawl)
    | IDLE ->
      reset_standing_abilities ();
      update_vx 0.;
      (player.ghost.head_textures.idle, bodies.idle)
    | LANDING (floor, v) ->
      player.ghost.entity.v.y <- 0.;
      player.ghost.entity.current_floor <- Some (floor, v);
      player.current.can_dash <- true;
      player.current.can_flap <- true;
      (player.ghost.head_textures.idle, bodies.jump)
    | READING ->
      update_vx 0.;
      (player.ghost.head_textures.read, bodies.read)
    | WALKING direction ->
      reset_standing_abilities ();
      Entity.walk_ghost player.ghost.entity direction;
      (player.ghost.head_textures.walk, bodies.walk)
    | WALL_SLIDING wall ->
      let wall_to_the_left = wall.pos.x < player.ghost.entity.sprite.dest.pos.x in
      player.ghost.entity.sprite.facing_right <- wall_to_the_left;
      player.current.wall <- Some wall;
      player.ghost.entity.sprite.dest.pos.x <-
        (if wall_to_the_left then
           wall.pos.x +. wall.w
        else
          wall.pos.x -. (Config.ghost.width *. Config.scale.ghost));
      update_vx 0.;
      player.current.can_dash <- true;
      player.current.can_flap <- true;
      (player.ghost.head_textures.wall_slide, bodies.wall_slide)
    | SWIMMING rect ->
      player.current.can_dash <- true;
      player.current.can_flap <- true;
      player.current.water <- Some rect;
      player.ghost.entity.v.y <- 0.;
      (* TODO add swimming texture *)
      (player.ghost.head_textures.idle, bodies.fall)
  in
  set_ghost_textures player.ghost ~head_texture ~body_texture

let past_cooldown ?(debug = false) pose_frames frame_time : bool =
  pose_frames.blocked_until.at < frame_time

let spawn_vengeful_spirit ?(start = None) ?(direction : direction option = None) state game =
  let texture = game.player.shared_textures.vengeful_cushion in
  let w, h = get_scaled_texture_size ~scale:Config.scale.ghost texture in
  let x, y =
    match start with
    | None -> (game.player.ghost.entity.dest.pos.x, game.player.ghost.entity.dest.pos.y)
    | Some v -> (v.x, v.y)
  in
  let dest = { pos = { x; y }; w; h } in
  let facing_right =
    match direction with
    | None -> game.player.ghost.entity.sprite.facing_right
    | Some d -> (
      match d with
      | LEFT -> false
      | RIGHT -> true
      | _ -> failwithf "spawn_vengeful_spirit invalid direction: %s" (Show.direction d))
  in
  let vengeful_spirit : sprite =
    {
      ident = fmt "vengeful_spirit %d" (List.length game.player.spawned_vengeful_spirits);
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
      (* TODO could try to use this for ghost projectiles, but this field is
         currently "damage done to ghost" for enemy projectiles
      *)
      damage = 1;
    }
  in
  game.player.spawned_vengeful_spirits <- projectile :: game.player.spawned_vengeful_spirits

let cancel_action (state : state) (game : game) (action_kind : ghost_action_kind) =
  let action =
    match action_kind with
    | FLAP ->
      game.player.current.can_flap <- true;
      game.player.history.flap
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
      play_sound state "nail-swing";

      let relative_pos =
        match direction with
        | UP -> ALIGNED (CENTER, BOTTOM)
        | DOWN -> ALIGNED (CENTER, TOP)
        | LEFT
        | RIGHT ->
          IN_FRONT
      in
      let slash =
        make_slash game.player direction relative_pos game.player.ghost.entity.sprite
          state.frame.time
      in
      game.player.children <-
        Utils.replace_assoc
          (NAIL slash : ghost_child_kind)
          { relative_pos; sprite = slash.sprite; in_front = true }
          game.player.children;
      cooldown_scale := game.player.current_weapon.cooldown_scale;
      game.player.history.nail
    | DREAM_NAIL -> game.player.history.dream_nail
    | C_DASH_WALL_COOLDOWN ->
      (* TODO stop playing c-dash sound *)
      game.player.ghost.entity.sprite.facing_right <-
        not game.player.ghost.entity.sprite.facing_right;
      game.player.current.is_c_dashing <- false;
      remove_child game.player C_DASH_WHOOSH;
      game.player.history.c_dash_wall_cooldown
    | C_DASH_COOLDOWN ->
      (* TODO stop playing c-dash sound *)
      game.player.current.is_c_dashing <- false;
      game.player.children <- List.remove_assoc C_DASH_WHOOSH game.player.children;
      game.player.history.c_dash_cooldown
    | C_DASH ->
      (* TODO this probably won't work as a sound, needs to be a music stream that repeats *)
      (* play_sound state "spray"; *)
      (* TODO maybe only should track this stuff when game_mode is STEEL_SOLE *)
      (match game.room.area.id with
      | COMPUTER_WING ->
        (* not counting these for now since there are some rooms that require it  *)
        ()
      | _ -> game.progress.steel_sole.c_dashes <- 1 + game.progress.steel_sole.c_dashes);
      game.player.current.is_c_dashing <- true;
      game.player.current.is_charging_c_dash <- false;
      game.player.ghost.entity.current_floor <- None;
      let alignment =
        if game.player.ghost.entity.sprite.facing_right then
          ALIGNED (RIGHT, CENTER)
        else
          ALIGNED (LEFT, CENTER)
      in
      spawn_child game.player C_DASH_WHOOSH ~scale:6. alignment
        game.player.shared_textures.c_dash_whoosh;
      game.player.history.c_dash
    | C_DASH_CHARGE ->
      game.player.current.is_charging_c_dash <- true;
      make_c_dash_child game.player;
      game.player.history.charge_c_dash
    | SHADE_DASH ->
      play_sound state "dash";
      game.player.history.shade_dash
    | DASH ->
      (* TODO the dash sound should have the footsteps at the end when the ghost lands *)
      play_sound state "dash";
      game.player.ghost.entity.y_recoil <- None;
      game.player.history.dash
    | CAST spell_kind -> (
      game.player.soul.current <- game.player.soul.current - Config.action.soul_per_cast;
      match spell_kind with
      | VENGEFUL_SPIRIT ->
        spawn_vengeful_spirit state game;
        game.player.history.cast_vs
      | DESOLATE_DIVE ->
        (* TODO probably should set is_diving in this fn (like how c-dash does it) *)
        let w, h =
          (* TODO these are temporarily scaled so the dive.png image can be reused *)
          (game.player.ghost.entity.dest.w *. 5., game.player.ghost.entity.dest.h *. 5.)
        in
        let child =
          make_ghost_child game.player DIVE
            (ALIGNED (CENTER, BOTTOM))
            game.player.shared_textures.desolate_dive w h
        in
        add_child game.player DIVE child;
        game.player.history.cast_dive
      | HOWLING_WRAITHS ->
        spawn_child game.player WRAITHS
          (ALIGNED (CENTER, BOTTOM))
          ~scale:1.7 game.player.shared_textures.howling_wraiths;
        game.player.history.cast_wraiths)
    | TAKE_DAMAGE_AND_RESPAWN ->
      (match game.mode with
      | CLASSIC -> ()
      | STEEL_SOLE ->
        game.player.soul.current <- game.player.soul.max;
        let jug_layers : layer list =
          List.filter
            (fun (layer : layer) ->
              String.length layer.name > 5 && Str.last_chars layer.name 5 = "-jugs")
            game.room.layers
        in
        List.iter (fun layer -> layer.destroyed_tiles <- []) jug_layers;
        Tiled.Room.reset_tile_groups game.room);
      Entity.freeze game.player.ghost.entity;
      (* TODO handle dying *)
      game.player.health.current <- game.player.health.current - 1;
      game.player.history.take_damage_and_respawn
    | TAKE_DAMAGE _ ->
      (* TODO separate sound for this *)
      play_sound state "punch";
      state.camera.shake <- 0.5;
      game.player.history.take_damage
    | DIVE_COOLDOWN -> game.player.history.dive_cooldown
    | FOCUS ->
      game.player.soul.at_focus_start <- game.player.soul.current;
      game.player.soul.health_at_focus_start <- game.player.health.current;
      game.player.soul.last_decremented <- { at = state.frame.time };
      spawn_child game.player FOCUS ~in_front:true ~scale:3.
        (ALIGNED (CENTER, CENTER))
        game.player.shared_textures.focus_sparkles;
      game.player.history.focus
    | JUMP -> game.player.history.jump
    | FLAP -> game.player.history.flap
    | WALL_KICK -> game.player.history.wall_kick
  in

  action.started <- { at = state.frame.time };
  action.doing_until.at <- state.frame.time +. action.config.duration.seconds;
  action.blocked_until.at <- state.frame.time +. (action.config.cooldown.seconds *. !cooldown_scale);
  set_pose game.player (PERFORMING action_kind) state.global.textures.ghost_bodies state.frame.time

let continue_action (state : state) (game : game) (action_kind : ghost_action_kind) =
  (match action_kind with
  | FOCUS ->
    let decr_dt =
      game.player.history.focus.config.duration.seconds
      /. (Config.action.soul_per_cast + 0 |> Int.to_float)
    in
    (* TODO probably should be checking >= *)
    if
      game.player.soul.at_focus_start - game.player.soul.current > Config.action.soul_per_cast
      && game.player.soul.health_at_focus_start = game.player.health.current
    then
      game.player.health.current <-
        Utils.bound_int 0 game.player.health.max (game.player.health.current + 1)
    else if state.frame.time -. game.player.soul.last_decremented.at > decr_dt then (
      game.player.soul.current <- game.player.soul.current - 1;
      game.player.soul.last_decremented <- { at = state.frame.time })
  | FLAP ->
    let flap_duration =
      (* this uses jump input buffer to handle flapping vs. buffering a jump off the ground *)
      game.player.history.jump.config.input_buffer.seconds -. 0.05
    in
    if state.frame.time -. game.player.history.flap.started.at > flap_duration then
      game.player.ghost.entity.v.y <- Config.ghost.jump_vy *. 0.8
  | DREAM_NAIL ->
    if
      state.frame.time -. game.player.history.dream_nail.started.at
      > game.player.history.dream_nail.config.duration.seconds -. 0.1
    then (
      let alignment =
        if game.player.ghost.entity.sprite.facing_right then
          ALIGNED (LEFT, CENTER)
        else
          ALIGNED (RIGHT, CENTER)
      in
      spawn_child game.player DREAM_NAIL alignment game.player.shared_textures.slash)
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
  set_pose game.player (PERFORMING action_kind) state.global.textures.ghost_bodies state.frame.time

let is_doing (player : player) (action_kind : ghost_action_kind) (frame_time : float) : bool =
  let check_action action =
    action.started.at <= frame_time && frame_time <= action.doing_until.at
  in
  match action_kind with
  | ATTACK _ -> Option.is_some (get_current_slash player)
  | FOCUS -> Option.is_some (get_focus_sparkles player)
  | CAST spell_kind -> (
    match spell_kind with
    | VENGEFUL_SPIRIT -> check_action player.history.cast_vs
    | HOWLING_WRAITHS -> check_action player.history.cast_wraiths
    | DESOLATE_DIVE -> player.current.is_diving)
  | C_DASH_CHARGE -> player.current.is_charging_c_dash
  | C_DASH -> player.current.is_c_dashing
  | C_DASH_WALL_COOLDOWN -> check_action player.history.c_dash_wall_cooldown
  | C_DASH_COOLDOWN -> check_action player.history.c_dash_cooldown
  | SHADE_DASH -> check_action player.history.shade_dash
  | DASH -> check_action player.history.dash
  | DIVE_COOLDOWN -> check_action player.history.dive_cooldown
  | TAKE_DAMAGE_AND_RESPAWN -> check_action player.history.take_damage_and_respawn
  | FLAP -> check_action player.history.flap
  | DREAM_NAIL -> check_action player.history.dream_nail
  | WALL_KICK
  | JUMP
  | TAKE_DAMAGE _ ->
    failwithf "is_doing - invalid action %s" (Show.ghost_action_kind action_kind)

(* CLEANUP add is_attacking to consolidate the arbitrary directions *)

let not_doing_any (player : player) (frame_time : float) (actions : ghost_action_kind list) : bool =
  not (List.exists (fun action -> is_doing player action frame_time) actions)

let is_casting (state : state) (game : game) =
  is_doing game.player (CAST VENGEFUL_SPIRIT) state.frame.time
  || game.player.current.is_diving
  || is_doing game.player (CAST HOWLING_WRAITHS) state.frame.time

let as_party_ghost (player : player) : party_ghost = { ghost = player.ghost; in_party = true }

let find_party_ghost (id : ghost_id) party : party_ghost option =
  List.find_opt (fun (party_ghost : party_ghost) -> party_ghost.ghost.id = id) party

let swap_current_ghost ?(in_cutscene = false) (state : state) (game : game) target_ghost_id =
  match find_party_ghost target_ghost_id game.party with
  | None -> failwithf "bad ghost_id '%s' in swap_current_ghost" (Show.ghost_id target_ghost_id)
  | Some target_ghost ->
    let old_ghost = as_party_ghost game.player in
    let current_pos = old_ghost.ghost.entity.dest.pos in
    if in_cutscene then (
      let new_party =
        let other_ghosts =
          List.filter (fun (pg : party_ghost) -> pg.ghost.id <> old_ghost.ghost.id) game.party
        in
        { old_ghost with in_party = false } :: other_ghosts
      in
      game.party <- new_party)
    else (
      Entity.unhide_at target_ghost.ghost.entity (clone_vector current_pos);
      (* TODO maybe replace with clone_sprite *)
      target_ghost.ghost.entity.sprite.facing_right <- old_ghost.ghost.entity.sprite.facing_right;
      target_ghost.ghost.entity.v <- clone_vector old_ghost.ghost.entity.v);
    game.player.ghost <- { target_ghost.ghost with entity = clone_entity target_ghost.ghost.entity };
    Entity.hide target_ghost.ghost.entity

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
  | "ismas_tear" -> ghost.abilities.ismas_tear <- new_val ghost.abilities.ismas_tear
  | _ -> failwithf "change_ability bad ability name: %s" ability_name

let enable_ability ghost ability_name = change_ability ~only_enable:true ghost ability_name
let toggle_ability ghost ability_name = change_ability ~debug:true ghost ability_name

let acquire_weapon (state : state) (game : game) weapon_name =
  match List.assoc_opt weapon_name state.global.weapons with
  | Some weapon_config ->
    let current_weapon_names = List.map fst game.player.weapons in
    if not (List.mem weapon_name current_weapon_names) then
      game.player.weapons <- (weapon_name, weapon_config) :: game.player.weapons
  | None -> failwithf "acquire_weapon bad weapon name: %s" weapon_name

let equip_weapon (player : player) weapon_name =
  match List.assoc_opt weapon_name player.weapons with
  | None ->
    print "can't equip %s, not in player.weapons: %s" weapon_name
      (List.map (fun (name, w) -> name) player.weapons |> join)
  | Some weapon_config ->
    player.current_weapon <-
      (let config = weapon_config.tint in
       {
         name = weapon_name;
         tint = Raylib.Color.create config.r config.g config.b config.a;
         scale_x = weapon_config.scale_x;
         scale_y = weapon_config.scale_y;
         cooldown_scale = 2. -. weapon_config.swing_speed;
       })

let get_invincibility_kind (state : state) (game : game) : invincibility_kind option =
  let in_dive_cooldown () =
    not (past_cooldown game.player.history.dive_cooldown state.frame.time)
  in
  if not (past_cooldown game.player.history.take_damage state.frame.time) then
    Some TOOK_DAMAGE
  else if game.player.current.is_diving || in_dive_cooldown () then
    Some DIVE_IFRAMES
  else if is_doing game.player SHADE_DASH state.frame.time then
    Some SHADE_CLOAK
  else
    None

let is_vulnerable (state : state) (game : game) : bool =
  Option.is_none (get_invincibility_kind state game)

let handle_debug_keys (game : game) (state : state) =
  let show_camera_location () =
    let camera =
      match state.camera.subject with
      | FIXED v -> v
      | GHOST -> game.player.ghost.entity.dest.pos
    in
    let v = Raylib.Camera2D.target state.camera.raylib in
    print "camera subject at: %s" (Show.vector camera);
    print "camera at: %f, %f" (Raylib.Vector2.x v) (Raylib.Vector2.y v)
  in
  let show_ghost_location () =
    print "ghost %s at %s"
      (Show.ghost_id game.player.ghost.id)
      (Show.vector game.player.ghost.entity.dest.pos)
  in
  let show_full_ghost_location () =
    let room_location = List.assoc game.room.id state.world in
    print "================\nghost global pos: %s"
      (Show.vector (Room.get_global_pos game.player.ghost.entity.dest.pos room_location));
    print "room %s" (Tiled.Room.get_filename game.room);
    print "full room_id: %s" (Show.room_id game.room.id);
    print "room_location global x/y: %f, %f" room_location.global_x room_location.global_y
  in
  let dv =
    if state.debug.enabled then
      Config.ghost.small_debug_v /. 3.
    else
      Config.ghost.debug_v
  in
  let show_ghost_positions () =
    let positions : string =
      let show_pos (pg : party_ghost) =
        fmt "%s: %s" (Show.ghost_id pg.ghost.id) (Show.vector pg.ghost.entity.dest.pos)
      in
      List.map show_pos game.party |> join ~sep:"\n"
    in
    print "party ghost positions:\n%s" positions;
    print "ghost pos: %s" (Show.vector game.player.ghost.entity.dest.pos)
  in
  if key_down DEBUG_UP then
    game.player.ghost.entity.dest.pos.y <- game.player.ghost.entity.dest.pos.y -. dv
  else if key_down DEBUG_DOWN then
    game.player.ghost.entity.dest.pos.y <- game.player.ghost.entity.dest.pos.y +. dv
  else if key_down DEBUG_RIGHT then
    game.player.ghost.entity.dest.pos.x <- game.player.ghost.entity.dest.pos.x +. dv
  else if key_down DEBUG_LEFT then
    game.player.ghost.entity.dest.pos.x <- game.player.ghost.entity.dest.pos.x -. dv
  else if holding_shift () then (
    if key_pressed DEBUG_1 then
      (* game.ghost.soul.current <- game.ghost.soul.max *)
      (* swap_current_ghost_in_cutscene state game ANNIE *)
      (* show_ghost_location () *)
      tmp "ghost is_taking_hazard_damage: %b" game.player.current.is_taking_hazard_damage
    (* () *)
    (* game.player.ghost.entity.sprite.facing_right <- true *)
    (* show_camera_location () *))
  else if key_pressed DEBUG_2 then
    (* toggle_ability game.ghost "mantis_claw" *)
    (* game.ghost.health.current <- game.ghost.health.current - 1 *)
    game.player.soul.current <- game.player.soul.max
  else if key_pressed DEBUG_3 then (* toggle_ability game.player "vengeful_spirit" *)
    print "player water is_some: %b" (Option.is_some game.player.current.water)
  else if key_pressed DEBUG_4 then
    (* toggle_ability game.player "desolate_dive" *)
    (* toggle_ability game.player "ismas_tear" *)
    ()
  else if key_pressed DEBUG_1 then
    game.debug_paused <- not game.debug_paused;
  state

(* this is used for actions that block other actions from happening during the same frame *)
type handled_action = { this_frame : bool }

let in_water (player : player) : bool = Option.is_some player.current.water

let update (game : game) (state : state) =
  let stop_wall_sliding = ref false in

  let key_pressed_or_buffered key_action =
    let (input, buffer) : frame_input * float =
      match key_action with
      | NAIL -> (state.frame_inputs.nail, game.player.history.nail.config.input_buffer.seconds)
      | JUMP -> (state.frame_inputs.jump, game.player.history.jump.config.input_buffer.seconds)
      | DASH -> (state.frame_inputs.dash, game.player.history.dash.config.input_buffer.seconds)
      | CAST -> (state.frame_inputs.cast, game.player.history.cast_vs.config.input_buffer.seconds)
      | C_DASH -> (state.frame_inputs.c_dash, game.player.history.c_dash.config.input_buffer.seconds)
      | _ -> failwithf "bad key in key_pressed_or_buffered': %s" (show_key_action key_action)
    in
    let input_buffered () =
      match input.down_since with
      | None -> false
      | Some down_since_time -> input.down && state.frame.time -. buffer < down_since_time.at
    in
    input.pressed || input_buffered ()
  in
  let set_pose' (pose : ghost_pose) =
    set_pose game.player pose state.global.textures.ghost_bodies state.frame.time
  in

  (* TODO need to block inputs during transitions to prevent re-exiting immediately and warping
     - also bound velocity to prevent falling through floors with high vy through horizontal doors
  *)
  let handle_interactions () =
    (* the ghost can only collide with one trigger of each type per frame *)
    let all_finished_interactions =
      (* PERFORMANCE maybe add the room name to the blocking_interaction, so it can check that room only
         - this would make mapmaking less convenient
         - could also improve interaction checks to only run the first frame that the ghost collides with the trigger, not every one
         -- although that would not work for triggers that wait for the interact key, so probably not worth it
      *)
      List.map snd game.progress.by_room
      |> List.map (fun (r : Json_t.room_progress) -> r.finished_interactions)
      |> List.flatten
    in

    let check_for_new_interactions () : bool =
      let interactable_triggers = game.room.triggers.lore @ game.room.triggers.item_pickups in
      (match find_trigger_collision game.player interactable_triggers with
      | None -> game.room.interaction_label <- None
      | Some trigger -> (
        let label =
          match trigger.label with
          | None -> ""
          | Some label' -> label'
        in
        let check_interact_key () =
          (* TODO maybe want to also check `Entity.on_ground game.ghost.ghost.entity` *)
          if state.frame_inputs.interact.pressed then
            maybe_begin_interaction state game trigger
        in
        match trigger.blocking_interaction with
        | None -> (
          match trigger.kind with
          | ITEM ->
            (* TODO seems like this should be coordinated with the pickup_indicators updating
               - looks a little weird because the pickup indicator goes away sooner than the label
            *)
            if List.mem trigger.full_name all_finished_interactions then
              game.room.interaction_label <- None
            else (
              check_interact_key ();
              game.room.interaction_label <- Some (label, trigger.dest))
          | _ ->
            game.room.interaction_label <-
              (check_interact_key ();
               Some (label, trigger.dest)))
        | Some blocking_interaction_name ->
          if List.mem blocking_interaction_name all_finished_interactions then (
            game.room.interaction_label <- Some (label, trigger.dest);
            check_interact_key ())));
      (match find_trigger_collision game.player game.room.triggers.cutscene with
      | None -> ()
      | Some trigger -> maybe_begin_interaction state game trigger);
      (match find_respawn_trigger_collision game.player game.room.triggers.respawn with
      | None -> ()
      | Some (new_respawn_pos, trigger) -> (
        match game.mode with
        | CLASSIC -> game.room.respawn_pos <- new_respawn_pos
        | STEEL_SOLE -> ()));
      List.length game.interaction.steps > 0
    in
    let speed_through_interaction =
      (* holding down c-dash will skip through interactions quickly, but still perform each step
         once so side-effects like gaining abilities still happen
         TODO this starts a c-dash after the interaction, not sure if there's a better option
      *)
      state.frame_inputs.c_dash.down
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

        (* CLEANUP get_tile_coords with optional offset args *)
        let set_layer_hidden (layer_name : string) hidden =
          match List.find_opt (fun (l : layer) -> l.name = layer_name) game.room.layers with
          | None -> failwithf "expected %s layer" layer_name
          | Some layer_to_hide -> layer_to_hide.hidden <- hidden
        in

        (* could divide these up into some smaller groups like TEXT or LAYER to get rid of more of the duplication, but
           probably not really worth it *)
        let handle_general_step (general_step : Interaction.general_step) =
          match general_step with
          | SHAKE_SCREEN amount -> state.camera.shake <- amount
          | DEBUG -> ()
          | INITIALIZE_INTERACTIONS remove_nail ->
            game.player.ghost.entity.v <- Zero.vector ();
            if remove_nail then (
              match get_current_slash game.player with
              | Some slash -> game.player.children <- []
              | None -> ())
          | FADE_SCREEN_OUT -> state.screen_fade <- Some 160
          | FADE_SCREEN_IN -> state.screen_fade <- None
          | DOOR_WARP trigger_kind
          | WARP trigger_kind -> (
            match trigger_kind with
            | WARP target ->
              let target_room_location = Tiled.Room.locate_by_name state.world target.room_name in
              let target_pos =
                match game.mode with
                | CLASSIC -> target.pos
                | STEEL_SOLE ->
                  (* TODO 80 seems like too much to adjust by, but this works for the archives exits *)
                  { target.pos with y = target.pos.y -. 80. }
              in
              Room.change_current_room state game target_room_location target_pos
            | _ ->
              failwithf "need WARP trigger kind for WARP steps, got '%s'"
                (Show.trigger_kind trigger_kind))
          | FLOATING_TEXT (text, duration) ->
            game.interaction.floating_text <-
              Some { content = text; visible = TIME { at = state.frame.time +. duration } }
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
          | SET_CAMERA_MOTION motion -> state.camera.motion <- motion
          | SET_GHOST_CAMERA ->
            (* this step is added to the end of every interaction *)
            Entity.unfreeze game.player.ghost.entity;
            state.camera.subject <- GHOST
          | WAIT time -> new_wait := time -. state.frame.dt
          | HIDE_BOSS_DOORS ->
            set_layer_hidden "boss-doors" true;
            Tiled.Room.reset_tile_groups game.room
          | UNHIDE_BOSS_DOORS ->
            set_layer_hidden "boss-doors" false;
            Tiled.Room.reset_tile_groups game.room
          | HIDE_LAYER layer_name ->
            set_layer_hidden layer_name true;
            Tiled.Room.reset_tile_groups game.room
          | UNHIDE_LAYER layer_name ->
            set_layer_hidden layer_name false;
            Tiled.Room.reset_tile_groups game.room
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
          | PURPLE_PEN_TEXT line ->
            Entity.freeze game.player.ghost.entity;
            let text : Interaction.text =
              let content =
                match game.mode with
                | CLASSIC -> [ "Found a purple pen with a note:"; fmt "{{purple}} %s" line ]
                | STEEL_SOLE -> [ line ]
              in
              { content; increases_health = false }
            in
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
            state.debug.rects <- (Raylib.Color.green, entity.dest) :: state.debug.rects;
            Entity.unhide_at entity { x; y }
          | HIDE -> Entity.hide entity
          | FREEZE -> Entity.freeze entity
          | UNFREEZE -> Entity.unfreeze entity
        in

        let add_item (item_kind : Interaction.item_kind) =
          Room.update_pickup_indicators state game;
          match item_kind with
          | ABILITY ability_name -> enable_ability game.player ability_name
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

        let handle_party_ghost_step
            ghost_id
            (party_ghost : party_ghost)
            (step : Interaction.party_ghost_step) =
          let set_interaction_pose pose =
            let head_texture, body_texture =
              match pose with
              | IDLE ->
                party_ghost.ghost.entity.v.x <- 0.;
                (party_ghost.ghost.head_textures.idle, state.global.textures.ghost_bodies.idle)
              | PERFORMING action -> (
                match action with
                | JUMP ->
                  party_ghost.ghost.entity.v.y <- Config.ghost.jump_vy;
                  (party_ghost.ghost.head_textures.idle, state.global.textures.ghost_bodies.jump)
                | ATTACK _ ->
                  (party_ghost.ghost.head_textures.idle, state.global.textures.ghost_bodies.nail)
                | CAST DESOLATE_DIVE ->
                  party_ghost.ghost.entity.v.y <- Config.ghost.dive_vy;
                  party_ghost.ghost.entity.v.x <- 0.;
                  (* TODO maybe add dive children
                     - would probably need a new step UNSET_CHILDREN to remove the child
                     - would need to move player.children to ghost, and update draw_party_ghost in render.ml
                  *)
                  (party_ghost.ghost.head_textures.look_up, state.global.textures.ghost_bodies.dive)
                | _ ->
                  failwithf "bad PERFORMING action for party ghost: %s"
                    (Show.ghost_action_kind action))
              | AIRBORNE _ ->
                (party_ghost.ghost.head_textures.idle, state.global.textures.ghost_bodies.fall)
              | CRAWLING ->
                (party_ghost.ghost.head_textures.look_down, state.global.textures.ghost_bodies.focus)
              | WALKING direction ->
                Entity.walk_ghost party_ghost.ghost.entity direction;
                if speed_through_interaction then
                  (* can't go faster than 2x because it breaks the locker-boys-killed
                     cutscene because Abed doesn't have enough time to fall to the lower level *)
                  party_ghost.ghost.entity.v.x <- party_ghost.ghost.entity.v.x *. 2.;
                (party_ghost.ghost.head_textures.walk, state.global.textures.ghost_bodies.walk)
              | LANDING _
              | READING
              | WALL_SLIDING _
              | SWIMMING _ ->
                failwithf "bad interaction pose '%s' for party ghost %s" (Show.ghost_pose pose)
                  (Show.ghost_id ghost_id)
            in
            set_ghost_textures party_ghost.ghost ~head_texture ~body_texture
          in
          match step with
          | ADD_TO_PARTY -> party_ghost.in_party <- true
          | REMOVE_FROM_PARTY -> party_ghost.in_party <- false
          | JUMP (_direction, vx) ->
            (* TODO fix the jump cutscene *)
            set_interaction_pose (PERFORMING JUMP);
            party_ghost.ghost.entity.v.x <- vx;
            party_ghost.ghost.entity.current_floor <- None
          | SET_POSE pose -> set_interaction_pose pose
          | MAKE_CURRENT_GHOST -> swap_current_ghost ~in_cutscene:true state game ghost_id
          | ENTITY entity_step ->
            (match entity_step with
            | HIDE ->
              if ghost_id = game.player.ghost.id then
                failwithf "can't hide current ghost %s" (Show.ghost_id ghost_id)
            | UNHIDE ->
              failwithf "can't unhide %s, use UNHIDE_AT for ghosts" (Show.ghost_id ghost_id)
            | _ -> ());
            handle_entity_step party_ghost.ghost.entity entity_step
          | WALK_TO target_tile_x ->
            let tx, _ = Tiled.Tile.tile_coords ~tile_w ~tile_h (target_tile_x, 1) in
            let dist = (tx *. Config.scale.room) -. party_ghost.ghost.entity.dest.pos.x in
            still_walking := abs_float dist > 10.;
            if not !still_walking then
              set_interaction_pose IDLE
            else if dist > 0. then
              set_interaction_pose (WALKING RIGHT)
            else
              set_interaction_pose (WALKING LEFT)
        in

        let handle_ghost_step (player : player) (step : Interaction.ghost_step) =
          match step with
          | SET_POSE pose ->
            set_pose player pose state.global.textures.ghost_bodies state.frame.time
          | FILL_LIFE_VAPOR -> player.soul.current <- player.soul.max
          | INCREASE_HEALTH_TEXT (increases_health, str) ->
            if increases_health then (
              player.health.max <- player.health.max + 1;
              player.health.current <- player.health.max);
            let text : Interaction.text = { content = [ str ]; increases_health } in
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
          | ADD_ITEM item_kind -> add_item item_kind
          | UNSET_FLOOR -> game.player.ghost.entity.current_floor <- None
          | ENTITY entity_step -> handle_entity_step player.ghost.entity entity_step
          | PARTY party_step ->
            handle_party_ghost_step player.ghost.id (as_party_ghost player) party_step
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
                (* TODO figure out how to do nested modules in .mli files *)
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
            (* CLEANUP duplicated in walk_ghost *)
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
        | CURRENT_GHOST ghost_step -> handle_ghost_step game.player ghost_step
        | PARTY_GHOST (ghost_id, party_ghost_step) ->
          let find_party_ghost () : party_ghost =
            match find_party_ghost ghost_id game.party with
            | Some ghost -> ghost
            | None -> failwithf "ghost_id %s needs to be in game.players" (Show.ghost_id ghost_id)
          in
          handle_party_ghost_step ghost_id (find_party_ghost ()) party_ghost_step
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
      && game.player.soul.current >= Config.action.soul_per_cast
      && past_cooldown game.player.history.cast_vs state.frame.time
      && (not (is_doing game.player (ATTACK RIGHT) state.frame.time))
      && not (is_casting state game)
    in

    let this_frame =
      if trying_cast then (
        let cast_spell spell_kind =
          start_action ~debug:true state game (CAST spell_kind);
          true
        in
        let can_howl () =
          game.player.abilities.howling_wraiths
          && past_cooldown game.player.history.cast_wraiths state.frame.time
          && state.frame_inputs.up.down
        in
        let can_dive () =
          game.player.abilities.desolate_dive
          && past_cooldown game.player.history.cast_dive state.frame.time
          && state.frame_inputs.down.down
        in
        let can_vs () =
          game.player.abilities.vengeful_spirit
          && past_cooldown game.player.history.cast_vs state.frame.time
        in
        if can_howl () then
          cast_spell HOWLING_WRAITHS
        else if can_dive () then (
          game.player.current.is_diving <- true;
          cast_spell DESOLATE_DIVE)
        else if can_vs () then
          cast_spell VENGEFUL_SPIRIT
        else
          false)
      else (
        let continuing_spell_kind =
          (* this should not be checking DESOLATE_DIVE because that ends when landing on a floor *)
          List.find_opt
            (fun s -> is_doing game.player (CAST s) state.frame.time)
            [ VENGEFUL_SPIRIT; HOWLING_WRAITHS ]
        in

        match continuing_spell_kind with
        | None ->
          if game.player.current.is_diving then
            if Option.is_some game.player.ghost.entity.current_floor then (
              state.camera.shake <- 1.;
              game.player.current.is_diving <- false;
              start_action state game DIVE_COOLDOWN;
              let child =
                let dest = game.player.ghost.entity.dest in
                let relative_pos = ALIGNED (CENTER, BOTTOM) in
                let w, h =
                  (* TODO this scaling is temporary so the current dive.png can be used *)
                  (dest.w *. 15., dest.h *. 3.)
                in
                make_ghost_child game.player DIVE_COOLDOWN relative_pos
                  game.player.shared_textures.dive_shockwave w h
              in
              add_child game.player DIVE_COOLDOWN child;
              remove_child game.player DIVE;
              false)
            else (
              match get_dive_sprite game.player with
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
    let still_on_surface () =
      (* TODO maybe make a fn for this - after moving to Entity *)
      (* this is used for things like disappearing platforms *)
      Entity.on_ground game.player.ghost.entity || Option.is_some game.player.current.wall
    in
    let starting_charge () =
      (* attack direction is arbitrary *)
      game.player.abilities.crystal_heart
      && key_pressed_or_buffered C_DASH
      && still_on_surface ()
      && (not (is_doing game.player (ATTACK RIGHT) state.frame.time))
      && (not (is_casting state game))
      && (not game.player.current.is_c_dashing)
      && not game.player.current.is_charging_c_dash
    in
    let released_charge () =
      is_doing game.player C_DASH_CHARGE state.frame.time && state.frame_inputs.c_dash.released
    in
    let still_charging () =
      is_doing game.player C_DASH_CHARGE state.frame.time && state.frame_inputs.c_dash.down
    in
    let still_c_dashing () = is_doing game.player C_DASH state.frame.time in
    let still_cooling_down () = is_doing game.player C_DASH_COOLDOWN state.frame.time in

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
        let down_since = state.frame.time -. game.player.history.charge_c_dash.started.at in
        still_on_surface ()
        && down_since > game.player.history.charge_c_dash.config.duration.seconds
      in

      if can_start_c_dash () then (
        stop_wall_sliding := true;
        start_action state game C_DASH;
        true)
      else (
        game.player.current.is_charging_c_dash <- false;
        false)
    in

    let cancel_c_dash () =
      remove_child game.player C_DASH_CHARGE_CRYSTALS;
      remove_child game.player C_DASH_WALL_CHARGE_CRYSTALS;
      start_c_dash_or_cancel_charge ()
    in

    let this_frame =
      if starting_charge () then (
        start_action state game C_DASH_CHARGE;
        true)
      else if still_charging () then
        if not (still_on_surface ()) then
          cancel_c_dash ()
        else (
          continue_action state game C_DASH_CHARGE;
          true)
      else if still_cooling_down () then (
        let action_kind =
          match game.player.current.wall with
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
      else if released_charge () then
        cancel_c_dash ()
      else
        false
    in
    { this_frame }
  in

  let handle_dashing () : handled_action =
    let starting_dash () =
      let not_attacking () =
        (not (is_doing game.player (ATTACK RIGHT) state.frame.time))
        (* CLEANUP this mostly works to allow dashing immediately after pogoing, but if
           the ghost bonks on a ceiling it cancels the y_recoil
        *)
        || Option.is_some game.player.ghost.entity.y_recoil
      in
      game.player.abilities.mothwing_cloak
      && key_pressed_or_buffered DASH
      && not_attacking ()
      && (not (is_casting state game))
      && Option.is_none game.player.ghost.entity.x_recoil
      && game.player.current.can_dash
      && past_cooldown game.player.history.dash state.frame.time
    in
    let still_dashing () = is_doing game.player DASH state.frame.time in
    let still_shade_dashing () = is_doing game.player SHADE_DASH state.frame.time in
    let this_frame =
      if starting_dash () then (
        (* TODO "dash in currently-facing direction" almost always works, but not when walljumping,
           so this should probably be `DASHING of direction` and check LEFT/RIGHT keys
        *)
        let can_shade_dash () =
          game.player.abilities.shade_cloak
          && past_cooldown game.player.history.shade_dash state.frame.time
        in

        if can_shade_dash () then (
          spawn_child game.player SHADE_DASH_SPARKLES ~in_front:true ~scale:6.
            (ALIGNED (CENTER, CENTER))
            game.player.shared_textures.shade_cloak_sparkles;
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
      (match game.player.current.wall with
      | None -> ()
      | Some wall ->
        if wall.pos.x < game.player.ghost.entity.sprite.dest.pos.x then
          stop_wall_sliding := true);
      set_pose' (WALKING RIGHT)
    in
    let move_left () =
      (match game.player.current.wall with
      | None -> ()
      | Some wall ->
        if wall.pos.x > game.player.ghost.entity.sprite.dest.pos.x then
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
      Option.is_some game.player.current.wall && key_pressed_or_buffered JUMP
    in
    let continuing_wall_kick () =
      state.frame.time -. game.player.history.wall_kick.started.at
      < game.player.history.wall_kick.config.duration.seconds
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
    match game.player.ghost.entity.current_floor with
    | None -> (
      match game.player.current.water with
      | None ->
        if
          (* TODO key_pressed_or_buffered detects the same keypress as the initial jump *)
          game.player.abilities.monarch_wings
          && game.player.current.can_flap
          && state.frame_inputs.jump.pressed
        then (
          game.player.current.can_flap <- false;
          start_action state game FLAP)
        else if is_doing game.player FLAP state.frame.time then (
          set_pose' (AIRBORNE new_vy);
          if state.frame_inputs.jump.down then
            continue_action state game FLAP
          else
            cancel_action state game FLAP)
        else
          set_pose' (AIRBORNE new_vy)
      | Some water ->
        if is_doing game.player FLAP state.frame.time then
          cancel_action state game FLAP)
    | Some (floor, floor_v) ->
      if is_doing game.player FLAP state.frame.time then
        (* TODO need a check like this for current_wall too (to prevent extra height from buffering jump off wallslide) *)
        cancel_action state game FLAP
      else if key_pressed_or_buffered JUMP then
        set_pose' (PERFORMING JUMP)
      else if game.player.ghost.entity.v.y > 0. then (
        stop_wall_sliding := true;
        (* TODO this is a very naive hardfall check, probably need to keep track of "airborne_duration" to check dy
           - airborne_duration would be reset when can_dash/flap are reset (pogo, wall_slide)
           - use ghost.history
        *)
        (* if game.player.ghost.entity.v.y >= Config.ghost.max_vy then
         *   state.camera.shake <- 1.; *)
        set_pose' (LANDING (floor, floor_v)))
  in

  let handle_attacking () =
    let starting_attack () =
      past_cooldown game.player.history.nail state.frame.time && key_pressed_or_buffered NAIL
    in
    if starting_attack () then (
      let direction : direction =
        match (state.frame_inputs.up.down, state.frame_inputs.down.down) with
        | true, false -> UP
        | false, true ->
          if Entity.on_ground game.player.ghost.entity then
            if game.player.ghost.entity.sprite.facing_right then RIGHT else LEFT
          else
            DOWN
        | _, _ ->
          (* don't really care about socd for up/down *)
          if game.player.ghost.entity.sprite.facing_right then RIGHT else LEFT
      in
      start_action state game (ATTACK direction))
    else (
      match get_current_slash game.player with
      | None -> ()
      | Some slash -> (
        continue_action state game (ATTACK slash.direction);
        match Sprite.advance_or_despawn state.frame.time slash.sprite.texture slash.sprite with
        | None -> remove_child game.player (NAIL slash)
        | Some _ -> ()))
  in

  let handle_dream_nail () : handled_action =
    let starting_dream_nail () =
      state.frame_inputs.dream_nail.pressed
      && Entity.on_ground game.player.ghost.entity
      && not_doing_any game.player state.frame.time [ ATTACK RIGHT; DASH ]
    in
    (* arbitrary directions *)
    let still_dream_nailing () =
      is_doing game.player DREAM_NAIL state.frame.time && state.frame_inputs.dream_nail.down
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
      && Entity.on_ground game.player.ghost.entity
      && game.player.soul.current >= Config.action.soul_per_cast
      && not_doing_any game.player state.frame.time [ ATTACK RIGHT; DASH ]
      && not (is_casting state game)
    in
    let still_focusing () =
      is_doing game.player FOCUS state.frame.time && state.frame_inputs.focus.down
    in
    let this_frame =
      if starting_focus () then (
        start_action state game FOCUS;
        true)
      else if still_focusing () then (
        (* TODO this only heals once *)
        continue_action state game FOCUS;
        true)
      else (
        if state.frame_inputs.focus.released then
          remove_child game.player FOCUS;
        false)
    in
    { this_frame }
  in

  let start_swimming (player : player) (rect : rect) =
    if not (in_water game.player) then
      game.player.ghost.entity.dest.pos.y <-
        game.player.ghost.entity.dest.pos.y +. (game.player.ghost.entity.dest.h /. 2.);
    (* seems weird to check jump inputs here instead of checking current.water in handle_jumping,
       but set_pose SWIMMING resets ghost.v.y <- 0
       - could use another ref (like stop_wall_sliding), but that doesn't really seem better
    *)
    if state.frame_inputs.jump.pressed then (
      game.player.current.water <- None;
      game.player.ghost.entity.dest.pos.y <-
        game.player.ghost.entity.dest.pos.y -. (game.player.ghost.entity.dest.h /. 2.);
      start_action state game JUMP)
    else
      set_pose' (SWIMMING rect)
  in

  let handle_collisions () =
    let check_enemy_collisions () =
      if is_vulnerable state game then (
        match get_enemy_collision game.player game.room with
        | Some (damage, direction) -> start_action state game (TAKE_DAMAGE (damage, direction))
        | None -> ())
    in
    let check_damage_collisions hazard_collisions platform_spike_collisions =
      state.debug.rects <-
        List.map (fun c -> (Raylib.Color.red, snd c)) hazard_collisions @ state.debug.rects;
      let collisions' =
        if game.player.abilities.ismas_tear then
          hazard_collisions
        else (
          let acid_collisions = Entity.get_acid_collisions game.room game.player.ghost.entity in
          hazard_collisions @ acid_collisions)
      in
      if List.length collisions' = 0 && not game.player.current.is_taking_hazard_damage then (
        match List.nth_opt platform_spike_collisions 0 with
        | Some spike_collision -> start_action state game (TAKE_DAMAGE (1, UP))
        | None -> (
          let projectile_collisions =
            if is_vulnerable state game then
              Entity.get_loose_projectile_collisions game.room game.player.ghost.entity
            else
              []
          in
          match List.nth_opt projectile_collisions 0 with
          | Some (collision, projectile) ->
            state.camera.shake <- 1.;
            (* arbitrary direction *)
            start_action state game (TAKE_DAMAGE (projectile.damage, UP))
          | None ->
            (* TODO doesn't seem like this is working, can still "pogo" upwards while taking damage *)
            if
              game.player.current.is_taking_hazard_damage
              && Option.is_some game.player.ghost.entity.y_recoil
            then
              game.player.ghost.entity.y_recoil <- None))
      else if is_doing game.player TAKE_DAMAGE_AND_RESPAWN state.frame.time then
        continue_action state game TAKE_DAMAGE_AND_RESPAWN
      else if game.player.current.is_taking_hazard_damage then (
        game.player.current.is_taking_hazard_damage <- false;
        let hazard_respawn (state : state) (game : game) =
          (match game.mode with
          | CLASSIC ->
            let transitioned_from_below =
              game.room.respawn_pos.y +. 50. > Tiled.Room.get_h game.room.json
            in
            if transitioned_from_below then
              add_phantom_floor game game.room.respawn_pos;
            ()
          | STEEL_SOLE -> add_phantom_floor game game.room.respawn_pos);
          Entity.unfreeze game.player.ghost.entity;
          state.camera.update_instantly <- true;
          game.player.ghost.entity.dest.pos <- clone_vector game.room.respawn_pos
        in
        hazard_respawn state game)
      else (
        state.camera.shake <- 1.;
        game.player.current.is_taking_hazard_damage <- true;
        start_action state game TAKE_DAMAGE_AND_RESPAWN)
    in
    let handle_c_dash_wall_collisions collisions =
      let wall_collision =
        List.find_opt
          (fun ((c, _) : collision * 'a) -> List.mem c.direction [ LEFT; RIGHT ])
          collisions
      in
      match wall_collision with
      | None -> ()
      | Some (_, wall_rect) ->
        if game.player.current.is_c_dashing then (
          if game.player.abilities.mantis_claw then
            game.player.current.wall <- Some wall_rect;
          state.camera.shake <- 1.;
          start_action state game C_DASH_WALL_COOLDOWN)
    in
    let handle_wall_sliding collisions =
      let high_enough_to_slide_on wall =
        let wall_bottom_y = wall.pos.y +. wall.h in
        game.player.ghost.entity.dest.pos.y +. (game.player.ghost.entity.dest.h /. 2.)
        < wall_bottom_y
      in
      let still_wall_sliding = Option.is_some game.player.current.wall in
      if game.player.abilities.mantis_claw then
        if !stop_wall_sliding then
          game.player.current.wall <- None
        else if still_wall_sliding then (
          let wall = Option.get game.player.current.wall in
          (* TODO also check that wall is still within x range too
             - would fix wall-cling storage for hazard respawns
          *)
          if high_enough_to_slide_on wall then
            set_pose' (WALL_SLIDING wall)
          else (
            let dx =
              (* can't check sprite.facing_right because it gets updated for the frame based on input, so it depends on whether a direction is being held *)
              if Entity.get_center game.player.ghost.entity > get_rect_center wall then
                -1.
              else
                1.
            in
            game.player.ghost.entity.dest.pos.x <- game.player.ghost.entity.dest.pos.x +. dx;
            match
              List.find_opt
                (fun (_, rect) -> rect <> wall)
                (Entity.get_floor_collisions game.room game.player.ghost.entity)
            with
            | None ->
              game.player.ghost.entity.dest.pos.x <- game.player.ghost.entity.dest.pos.x -. dx;
              game.player.current.wall <- None
            | Some (collision, new_wall) -> set_pose' (WALL_SLIDING new_wall)))
        else (
          game.player.current.wall <- None;
          if not (is_doing game.player (ATTACK RIGHT) state.frame.time) then (
            let check_collision ((c, wall) : collision * rect) =
              if
                (not (Entity.on_ground game.player.ghost.entity))
                && Entity.descending game.player.ghost.entity
              then (
                let wall_is_tall_enough = wall.h > game.player.ghost.entity.dest.h /. 2. in
                if high_enough_to_slide_on wall && wall_is_tall_enough then (
                  match c.direction with
                  | LEFT -> set_pose' (WALL_SLIDING wall)
                  | RIGHT -> set_pose' (WALL_SLIDING wall)
                  | _ -> ()))
            in
            List.iter check_collision collisions))
    in

    let floor_collisions' = Entity.get_floor_collisions game.room game.player.ghost.entity in
    let floor_v, floor_collisions =
      match Entity.get_conveyor_belt_collision game.room game.player.ghost.entity with
      | None -> (Zero.vector (), floor_collisions')
      | Some (collision, rect, floor_v) -> (floor_v, [ (collision, rect) ])
    in

    let hazard_collisions, platform_spike_collisions' =
      let damage = Entity.get_damage_collisions game.room game.player.ghost.entity in
      match game.mode with
      | CLASSIC -> (damage.hazards, damage.platform_spikes)
      | STEEL_SOLE -> (
        let up_floor_collision : (collision * rect) option =
          (* this is supposed to fix the steel sole hazard respawns on wall seams *)
          if List.length floor_collisions > 1 || game.player.ghost.entity.v.y < 0. then
            None
          else
            List.find_opt (fun ((c : collision), _) -> c.direction = UP) floor_collisions
        in
        match up_floor_collision with
        | None -> (damage.hazards, damage.platform_spikes)
        | Some coll ->
          game.progress.steel_sole.dunks <- 1 + game.progress.steel_sole.dunks;
          ([ coll ], damage.platform_spikes))
    in
    let platform_spike_collisions =
      if is_vulnerable state game then
        platform_spike_collisions'
      else
        []
    in
    let bench_collisions = Entity.get_bench_collisions game.room game.player.ghost.entity in
    (* since these are collisions from above, they are only detected for the frame that the
       ghost collides (and then again the frame after, which is a bug)
    *)
    (match List.nth_opt bench_collisions 0 with
    | None -> ()
    | Some (coll, rect) -> (
      match coll.direction with
      | UP ->
        game.interaction.corner_text <-
          Some { content = "Game saved."; visible = TIME { at = state.frame.time +. 1.5 } };
        state.should_save <- true;
        game.player.health.current <- game.player.health.max
      | _ -> ()));
    let liquid_collisions =
      let water_collisions = Entity.get_water_collisions game.room game.player.ghost.entity in
      if game.player.abilities.ismas_tear then (
        let acid_collisions = Entity.get_acid_collisions game.room game.player.ghost.entity in
        water_collisions @ acid_collisions)
      else
        water_collisions
    in
    (match List.nth_opt liquid_collisions 0 with
    | None -> game.player.current.water <- None
    | Some (_coll, rect) -> start_swimming game.player rect);
    check_enemy_collisions ();
    state.debug.rects <-
      List.map (fun c -> (Raylib.Color.green, snd c)) floor_collisions @ state.debug.rects;
    apply_collisions ~floor_v game.player.ghost.entity floor_collisions;
    handle_wall_sliding floor_collisions;
    check_damage_collisions hazard_collisions platform_spike_collisions;
    handle_c_dash_wall_collisions (floor_collisions @ hazard_collisions)
  in

  let check_cooldowns cooldowns =
    let cooling_down = ref false in
    let check cooldown =
      if not !cooling_down then
        if is_doing game.player cooldown state.frame.time then (
          continue_action state game cooldown;
          cooling_down := true)
    in
    List.iter check cooldowns;
    !cooling_down
  in

  let reveal_shadows () =
    let colliding (trigger : trigger) =
      Option.is_some (Collision.with_entity game.player.ghost.entity trigger.dest)
    in
    match List.find_opt colliding game.room.triggers.shadows with
    | None -> ()
    | Some trigger -> (
      match List.find_opt (fun (l : layer) -> l.name = trigger.name_suffix) game.room.layers with
      | None -> failwithf "Player.update: could not find layer '%s' to reveal" trigger.name_suffix
      | Some layer ->
        if not (List.mem trigger.name_suffix game.room.progress.revealed_shadow_layers) then
          game.room.progress.revealed_shadow_layers <-
            trigger.name_suffix :: game.room.progress.revealed_shadow_layers;
        layer.hidden <- true)
  in

  game.player.ghost.entity.current_platforms <- [];
  Entity.apply_v
    ~debug:(if state.debug.enabled then Some (fmt "%d" state.frame.idx) else None)
    state.frame.dt game.player.ghost.entity;
  (match game.player.ghost.entity.current_floor with
  | None -> ()
  | Some (floor, v) ->
    let e = game.player.ghost.entity in
    let dt = state.frame.dt in
    (* only need to update x here because conveyor belts are only horizontal *)
    e.dest.pos.x <- e.dest.pos.x +. (v.x *. dt));

  let new_vy =
    let vy' =
      let vy = game.player.ghost.entity.v.y in
      let ascending = vy < 0. in
      let dvy = Config.physics.gravity *. state.frame.dt in
      if key_up JUMP && ascending then
        (vy *. Config.physics.jump_damping) +. dvy
      else (
        match game.player.current.wall with
        (* could check current_floor here and set to 0, but this already happens by set_pose *)
        | Some _ -> Config.ghost.wall_slide_vy
        | None -> vy +. dvy)
    in
    (* no max_vy applied when ascending, only descending *)
    Utils.bound (-1. *. Float.max_float) vy' Config.ghost.max_vy
  in
  let exiting = Room.handle_transitions state game in
  if not exiting then (
    let interacting = handle_interactions () in
    if interacting then
      Entity.update_pos game.room game.player.ghost.entity state.frame.dt
    else (
      reveal_shadows ();
      if game.player.ghost.entity.update_pos then (
        let cooling_down =
          check_cooldowns [ DIVE_COOLDOWN; C_DASH_COOLDOWN; C_DASH_WALL_COOLDOWN ]
        in
        if not cooling_down then
          if not (in_water game.player) then (
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
                      resolve_slash_collisions state game))))))
          else (
            handle_walking ();
            handle_jumping new_vy))));

  animate_and_despawn_children state.frame.time game.player;
  handle_collisions ();
  Entity.maybe_unset_current_floor game.player.ghost.entity game.room;
  Sprite.advance_animation state.frame.time game.player.ghost.entity.sprite.texture
    game.player.ghost.entity.sprite;
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
        { config with path = { config.path with pose_name } }
      | None ->
        {
          path = { asset_dir = GHOSTS; character_name = "shared"; pose_name };
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

(* the idle_texture is for setting the initial entity.sprite.texture *)
let init
    (ghost_id : ghost_id)
    (idle_texture : texture)
    (head_textures : ghost_head_textures)
    (action_config : (string * ghost_action_config) list)
    (start_pos : vector)
    (save_file : Json_t.save_file)
    weapons_configs
    shared_textures : player =
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

  let load_weapon name = (name, List.assoc name weapons_configs) in
  let weapons = List.map load_weapon save_file.weapons in

  let max_health =
    let finished_interaction_names =
      match List.assoc_opt save_file.room_name save_file.progress.by_room with
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
    ghost =
      {
        id = ghost_id;
        head = head_textures.idle;
        head_textures;
        body_render_offset = Zero.vector ();
        entity =
          Entity.create_for_sprite
            (Sprite.create
               (fmt "ghost-%s" (Show.ghost_id ghost_id))
               ~collision:(Some DEST) idle_texture dest)
            {
              pos = clone_vector dest.pos;
              w = Config.ghost.width *. Config.scale.ghost;
              h = Config.ghost.height *. Config.scale.ghost;
            };
      };
    current = reset_current_status ();
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

let init_party ghost_id (head_textures : ghost_head_textures) idle_texture start_pos in_party :
    party_ghost =
  let w, h = get_scaled_texture_size ~scale:Config.scale.ghost idle_texture in
  let dest = { pos = start_pos; w; h } in
  let entity =
    let entity' =
      Entity.create_for_sprite
        (Sprite.create
           (fmt "ghost-%s" (Show.ghost_id ghost_id))
           ~collision:(Some DEST) idle_texture dest)
        {
          pos = clone_vector start_pos;
          w = Config.ghost.width *. Config.scale.ghost;
          h = Config.ghost.height *. Config.scale.ghost;
        }
    in
    Entity.hide entity';
    entity'
  in
  {
    ghost =
      {
        id = ghost_id;
        head = head_textures.idle;
        body_render_offset = Zero.vector ();
        head_textures;
        entity;
      };
    in_party;
  }
