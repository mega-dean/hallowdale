open Utils
open Types

let above_floor (entity : entity) (floor : rect) =
  floor.pos.x < entity.dest.pos.x +. entity.dest.w && floor.pos.x +. floor.w > entity.dest.pos.x

(* same thing because it only considers p.x *)
let below_floor entity floor = above_floor entity floor

let recoil_backwards (entity : entity) (recoil : recoil) =
  let positive_r, negative_r =
    if recoil.speed > 0. then
      (recoil, { recoil with speed = recoil.speed *. -1. })
    else
      ({ recoil with speed = recoil.speed *. -1. }, recoil)
  in
  if entity.sprite.facing_right then
    entity.x_recoil <- Some negative_r
  else
    entity.x_recoil <- Some positive_r

let recoil (entity : entity) (direction : direction) =
  let recoil' scale =
    {
      speed = Config.ghost.recoil_speed *. scale;
      time_left = { seconds = Config.ghost.nail_recoil_time };
      reset_v = true;
    }
  in
  match direction with
  | RIGHT -> entity.x_recoil <- Some (recoil' 1.)
  | LEFT -> entity.x_recoil <- Some (recoil' (-1.))
  | DOWN -> entity.y_recoil <- Some (recoil' 1.)
  | UP ->
    entity.current_floor <- None;
    entity.y_recoil <- Some (recoil' (-1.))

let freeze (entity : entity) =
  entity.v <- Zero.vector ();
  entity.update_pos <- false

let unfreeze (entity : entity) = entity.update_pos <- true

let move_offscreen (entity : entity) =
  entity.dest.pos.x <- -1. *. abs_float entity.dest.pos.x;
  entity.dest.pos.y <- -1. *. abs_float entity.dest.pos.y

let move_onscreen (entity : entity) =
  entity.dest.pos.x <- abs_float entity.dest.pos.x;
  entity.dest.pos.y <- abs_float entity.dest.pos.y

let hide (entity : entity) =
  freeze entity;
  entity.current_floor <- None;
  move_offscreen entity

let unhide (entity : entity) = move_onscreen entity

let unhide_at (entity : entity) (pos : vector) =
  unfreeze entity;
  entity.dest.pos <- clone_vector pos

let hidden (entity : entity) : bool = entity.dest.pos.x < 0. && entity.dest.pos.y < 0.

let set_facing (direction : direction) (entity : entity) =
  match direction with
  | RIGHT -> entity.sprite.facing_right <- true
  | LEFT -> entity.sprite.facing_right <- false
  | _ -> failwithf "Entity.set_facing bad direction %s" (Show.direction direction)

let apply_v ?(debug = None) dt (entity : entity) =
  (match entity.y_recoil with
  | None -> entity.dest.pos.y <- entity.dest.pos.y +. (entity.v.y *. dt)
  | Some recoil ->
    if recoil.reset_v then
      entity.v.y <- 0.;
    entity.dest.pos.y <- entity.dest.pos.y +. (recoil.speed *. dt);
    if recoil.time_left.seconds > 0. then
      recoil.time_left <- { seconds = recoil.time_left.seconds -. dt }
    else
      entity.y_recoil <- None);
  match entity.x_recoil with
  | None -> entity.dest.pos.x <- entity.dest.pos.x +. (entity.v.x *. dt)
  | Some recoil ->
    if recoil.reset_v then
      entity.v.x <- 0.;
    entity.dest.pos.x <- entity.dest.pos.x +. (recoil.speed *. dt);
    if recoil.time_left.seconds > 0. then
      recoil.time_left <- { seconds = recoil.time_left.seconds -. dt }
    else
      entity.x_recoil <- None

let get_rect_collisions (entity : entity) (rects : rect list) : collision list =
  let collisions : collision list ref = ref [] in
  let check_collision (rect : rect) =
    match Collision.with_entity entity rect with
    | None -> ()
    | Some coll -> collisions := coll :: !collisions
  in
  List.iter check_collision rects;
  !collisions

let get_platform_collisions (entity : entity) (platforms : platform list) : collision list =
  let collisions : collision list ref = ref [] in
  let check_collision (platform : platform) =
    match Collision.with_entity entity platform.sprite.dest with
    | None -> ()
    | Some coll -> (
      collisions := coll :: !collisions;
      match platform.kind with
      | None -> ()
      | Some (TEMPORARY _)
      | Some (DISAPPEARABLE _) ->
        if coll.collided_from = UP then
          entity.current_platforms <- platform :: entity.current_platforms
      | Some (LOCKED_DOOR _)
      | Some (ROTATABLE _) -> entity.current_platforms <- platform :: entity.current_platforms)
  in
  List.iter check_collision platforms;
  !collisions

(* TODO could consolidate these *)
let get_tile_collisions (layers : layer list) (entity : entity) : collision list =
  let collisions : collision list ref = ref [] in
  let check_collision (layer : layer) =
    let check_tile_group (tile_group : tile_group) =
      match Collision.with_entity entity tile_group.dest with
      | None -> ()
      | Some coll -> collisions := coll :: !collisions
    in
    List.iter check_tile_group layer.tile_groups
  in
  List.iter check_collision layers;
  !collisions

let get_bench_collisions (room : room) (entity : entity) : collision list =
  let layers = List.filter (fun (l : layer) -> l.name = "benches") room.layers in
  get_tile_collisions layers entity

let get_conveyor_belt_collision (room : room) (entity : entity) : (collision * vector) option =
  let collisions : (collision * vector) list ref = ref [] in
  let check_collision ((rect, speed) : rect * float) =
    match Collision.with_entity entity rect with
    | None -> ()
    | Some coll ->
      let v = { x = speed; y = 0. } in
      collisions := (coll, v) :: !collisions
  in
  List.iter check_collision room.conveyor_belts;
  List.nth_opt !collisions 0

let get_floor_collisions (room : room) (entity : entity) : collision list =
  let layers = List.filter (fun (l : layer) -> l.config.collides_with_ghost) room.layers in
  get_tile_collisions layers entity
  @ get_platform_collisions entity room.platforms
  @ get_rect_collisions entity room.floors

let get_collisions name room entity =
  let layers = List.filter (fun (l : layer) -> l.name = name) room.layers in
  get_tile_collisions layers entity

let get_water_collisions (room : room) (entity : entity) : collision list =
  get_collisions "water" room entity

let get_acid_collisions (room : room) (entity : entity) : collision list =
  get_collisions "acid" room entity

type damage_collisions = {
  hazards : collision list;
  platform_spikes : collision list;
}

(* this excludes acid collisions because they are handled separately (based on Isma's Tear) *)
let get_damage_collisions (room : room) (entity : entity) =
  {
    hazards = get_rect_collisions entity (room.hazards @ room.spikes);
    platform_spikes =
      get_rect_collisions entity (room.platform_spikes |> String.Map.to_list |> List.map snd);
  }

let get_loose_projectile_collisions (room : room) entity : (collision * projectile) list =
  let get_projectile_collision (projectile : projectile) =
    match Collision.with_entity entity projectile.entity.dest with
    | None -> None
    | Some c -> Some (c, projectile)
  in
  List.filter_map get_projectile_collision room.loose_projectiles

let apply_collisions
    ?(floor_v = Zero.vector ())
    ?(_debug = false)
    (entity : entity)
    (collisions : collision list) : unit =
  let adjust_position (coll : collision) =
    let floor = coll.other_rect in
    let top_of r = r.pos.y in
    let bottom_of r = r.pos.y +. r.h in
    let left_of r = r.pos.x in
    let right_of r = r.pos.x +. r.w in
    match coll.collided_from with
    | UP ->
      let not_recoiling_upward =
        match entity.y_recoil with
        | None -> true
        | Some r -> r.speed > 0.
      in
      if not_recoiling_upward && above_floor entity floor then (
        if entity.v.y > 0. then
          entity.v.y <- 0.;
        entity.dest.pos.y <- top_of floor -. entity.dest.h;
        (* the floor shouldn't be set for fragments (because it forces the new_vy to be 0.) *)
        if entity.config.bounce < 0.01 then
          entity.current_floor <- Some (floor, floor_v));
      if entity.config.inanimate then
        if abs_float entity.v.x < 10. then (
          entity.v.y <- 0.;
          entity.update_pos <- false)
        else (
          entity.v.x <- entity.v.x *. 0.3;
          entity.v.y <- entity.v.y *. -1. *. entity.config.bounce)
    | DOWN ->
      if entity.v.y < 0. || Option.is_some entity.y_recoil then (
        entity.v.y <- 0.;
        entity.dest.pos.y <- bottom_of floor)
    | LEFT -> entity.dest.pos.x <- left_of floor -. entity.dest.w
    | RIGHT -> entity.dest.pos.x <- right_of floor
  in
  let left_right_collisions, up_down_collisions =
    let is_left_right (c : collision) = c.collided_from = LEFT || c.collided_from = RIGHT in
    List.partition is_left_right collisions
  in
  (* move sideways first to fix collisions with tiles stacked directly on top of each other *)
  List.iter adjust_position left_right_collisions;
  List.iter adjust_position up_down_collisions

let update_pos
    ?(_debug = false)
    ?(gravity_multiplier_override = None)
    ?(apply_floor_collisions = true)
    (room : room)
    (dt : float)
    (entity : entity) : collision list =
  if entity.update_pos then (
    let dvy =
      match entity.current_floor with
      | Some _ -> 0.
      | None ->
        let gravity_multiplier =
          match gravity_multiplier_override with
          | None -> entity.config.gravity_multiplier
          | Some v -> v
        in
        Config.physics.gravity *. dt *. gravity_multiplier
    in
    apply_v dt entity;
    entity.v.y <- entity.v.y +. dvy;
    let collisions = get_floor_collisions room entity in
    if apply_floor_collisions then (
      apply_collisions ~_debug entity collisions;
      collisions)
    else
      [])
  else
    []

(* this is the same as update_pos, but doesn't return the collision list *)
let update_pos_
    ?(gravity_multiplier_override = None)
    ?(apply_floor_collisions = true)
    (room : room)
    (dt : float)
    (entity : entity) : unit =
  update_pos ~gravity_multiplier_override ~apply_floor_collisions room dt entity |> ignore

let maybe_unset_current_floor (entity : entity) (room : room) =
  match entity.current_floor with
  | None -> ()
  | Some (floor, _) ->
    let walked_over_edge =
      if not (above_floor entity floor) then (
        (* smoothly walk to adjacent floors by (temporarily) pushing the ghost down a pixel to
           check Entity.get_floor_collisions
        *)
        entity.dest.pos.y <- entity.dest.pos.y +. 1.;
        match List.nth_opt (get_floor_collisions room entity) 0 with
        | None ->
          entity.dest.pos.y <- entity.dest.pos.y -. 1.;
          true
        | Some collision ->
          entity.current_floor <- Some (collision.other_rect, Zero.vector ());
          false)
      else
        false
    in
    let jumping_over_edge = entity.v.y < 0. in
    if walked_over_edge || jumping_over_edge then
      entity.current_floor <- None

let get_child_pos'
    (parent_dest : rect)
    (facing_right : bool)
    (relative_pos : relative_position)
    child_w
    child_h =
  let to_the_left () =
    { y = parent_dest.pos.y; x = parent_dest.pos.x -. child_w +. (parent_dest.w /. 2.) }
  in
  let to_the_right () = { y = parent_dest.pos.y; x = rect_center_x parent_dest } in
  match relative_pos with
  | IN_FRONT ->
    if facing_right then
      to_the_right ()
    else
      to_the_left ()
  | BEHIND ->
    if facing_right then
      to_the_left ()
    else
      to_the_right ()
  | ALIGNED (x_alignment, y_alignment) -> align x_alignment y_alignment parent_dest child_w child_h

let get_child_pos (parent : entity) (relative_pos : relative_position) child_w child_h =
  get_child_pos' parent.dest parent.sprite.facing_right relative_pos child_w child_h

let on_ground (e : entity) = e.current_floor <> None
let descending (e : entity) = e.v.y > 0.
let is_on_screen' (r : rect) = not (r.pos.x < 0. && r.pos.y < 0.)
let is_on_screen (e : entity) = is_on_screen' e.dest
let is_off_screen (e : entity) = not (is_on_screen e)
let get_center (entity : entity) : vector = get_rect_center entity.dest

let set_facing_right ?(allow_vertical = true) (entity : entity) (direction : direction) =
  match direction with
  | LEFT -> entity.sprite.facing_right <- false
  | RIGHT -> entity.sprite.facing_right <- true
  | _ ->
    if not allow_vertical then
      failwithf "bad direction in set_facing_right: %s" (Show.direction direction)

(* this uses Config.ghost.vx, which works fine for npcs in cutscenes *)
let update_vx (entity : entity) multiplier =
  let mult = if entity.sprite.facing_right then multiplier else -1. *. multiplier in
  entity.v.x <- mult *. Config.ghost.vx

let walk_ghost (entity : entity) (direction : direction) : unit =
  set_facing_right entity direction;
  update_vx entity 1.

(* called once per frame to align sprite.dest position with entity.dest position *)
let adjust_sprite_dest ?(skip_coll_offset = false) (e : entity) =
  let offset = if skip_coll_offset then Zero.vector () else e.sprite.texture.coll_offset in
  e.sprite.dest.pos.y <- e.dest.pos.y -. offset.y;
  if e.sprite.facing_right then
    e.sprite.dest.pos.x <- e.dest.pos.x -. offset.x
  else
    e.sprite.dest.pos.x <- e.dest.pos.x -. (e.sprite.dest.w -. e.dest.w -. offset.x)

let update_sprite_texture (entity : entity) (texture : texture) =
  entity.sprite.texture <- texture;
  (* this is needed to adjust the w/h *)
  entity.sprite.dest <- Sprite.make_dest entity.sprite.dest.pos.x entity.sprite.dest.pos.y texture

let clone (orig : entity) : entity =
  let dest_clone =
    { pos = { x = orig.dest.pos.x; y = orig.dest.pos.y }; w = orig.dest.w; h = orig.dest.h }
  in
  let v_clone = { x = orig.v.x; y = orig.v.y } in
  {
    orig with
    dest = dest_clone;
    v = v_clone;
    sprite = Sprite.clone orig.sprite;
    x_recoil = None;
    y_recoil = None;
  }

let create
    (path : string)
    ?(scale = Config.scale.ghost)
    ?(inanimate = false)
    ?(gravity_multiplier = 1.)
    ?(v = Zero.vector ())
    ?(facing_right = true)
    ?(collision = None)
    initial_texture
    (dest : rect) : entity =
  let animation_src = get_src initial_texture in
  let sprite_w, sprite_h = (animation_src.w *. scale, animation_src.h *. scale) in
  {
    dest;
    sprite =
      Sprite.create path initial_texture ~facing_right ~collision
        { pos = { x = dest.pos.x; y = dest.pos.y }; w = sprite_w; h = sprite_h };
    config = { bounce = (if inanimate then 0.2 else 0.); inanimate; gravity_multiplier };
    update_pos = (not inanimate) && is_on_screen' dest;
    v;
    current_floor = None;
    x_recoil = None;
    y_recoil = None;
    current_platforms = [];
  }

let create_for_sprite
    (sprite : sprite)
    ?(inanimate = false)
    ?(gravity_multiplier = 1.)
    ?(v = Zero.vector ())
    (dest : rect) : entity =
  {
    dest;
    sprite;
    config = { bounce = (if inanimate then 0.2 else 0.); inanimate; gravity_multiplier };
    update_pos = (not inanimate) && is_on_screen' dest;
    v;
    current_floor = None;
    x_recoil = None;
    y_recoil = None;
    current_platforms = [];
  }

let to_texture_config asset_dir character_name ((pose_name, json) : string * Json_t.texture_config)
    : texture_config =
  {
    count = json.count;
    duration = { seconds = json.duration };
    x_offset = json.x_offset;
    y_offset = json.y_offset;
    path = { asset_dir; character_name; pose_name };
  }

let load_pose (texture_config : texture_config) : string * texture =
  (texture_config.path.pose_name, Sprite.build_texture_from_config texture_config)

let create_from_textures
    ?(collision = None)
    ?(gravity_multiplier = 1.)
    (texture_configs : texture_config list)
    (textures : (string * texture) list)
    (entity_dest : rect) : entity * (string * texture) list =
  let config =
    (* texture_configs can't be empty *)
    List.nth texture_configs 0
  in
  let validate_configs_are_complete () =
    let get_filenames asset_dir char_name =
      File.ls (File.make_assets_path [ asset_dir; char_name ])
    in
    let config_names =
      texture_configs |> List.map (fun (t : texture_config) -> fmt "%s.png" t.path.pose_name)
    in
    let png_names =
      get_filenames (Show.asset_dir config.path.asset_dir) config.path.character_name
    in

    let validate_png_name png_name =
      if not (List.mem png_name config_names) then
        failwithf "found %s image '%s' that has no corresponding config in enemies.json"
          config.path.character_name png_name
    in

    let validate_config_name config_name =
      if not (List.mem config_name png_names) then (
        let short =
          (* strip .png from the end *)
          Str.first_chars config_name (String.length config_name - 4)
        in
        failwithf "found %s texture_config for '%s' that has no corresponding %s.png"
          config.path.character_name short short)
    in
    List.iter validate_png_name png_names;
    List.iter validate_config_name config_names
  in
  let initial_texture =
    match List.assoc_opt "idle" textures with
    | None -> failwithf "could not find 'idle' texture for %s" config.path.character_name
    | Some texture -> texture
  in
  let texture_config = List.nth texture_configs 0 in
  validate_configs_are_complete ();
  ( create ~collision ~gravity_multiplier texture_config.path.character_name initial_texture
      entity_dest,
    textures )

let create_from_texture_configs
    ?(collision = None)
    ?(gravity_multiplier = 1.)
    (texture_configs : texture_config list)
    (entity_dest : rect) : entity * (string * texture) list =
  let textures = List.map load_pose texture_configs in
  create_from_textures ~collision ~gravity_multiplier texture_configs textures entity_dest
