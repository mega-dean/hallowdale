open Types

[@@@ocaml.warning "-26-27-32"]

let above_floor (e : entity) floor =
  floor.pos.x < e.dest.pos.x +. e.dest.w && floor.pos.x +. floor.w > e.dest.pos.x

(* same thing because it only considers p.x *)
let below_floor s floor = above_floor s floor

let positive_and_negative r =
  if r.speed > 0. then
    (r, { r with speed = r.speed *. -1. })
  else
    ({ r with speed = r.speed *. -1. }, r)

(* these functions adjust the sign of r.amount based on sprite.facing_right *)
let recoil_forwards (e : entity) (r : recoil) =
  let positive_r, negative_r = positive_and_negative r in
  if e.sprite.facing_right then
    e.x_recoil <- Some positive_r
  else
    e.x_recoil <- Some negative_r

let recoil_backwards (e : entity) (r : recoil) =
  let positive_r, negative_r = positive_and_negative r in
  if e.sprite.facing_right then
    e.x_recoil <- Some negative_r
  else
    e.x_recoil <- Some positive_r

let freeze (e : entity) =
  e.v <- Zero.vector ();
  e.update_pos <- false

let unfreeze (e : entity) = e.update_pos <- true

let move_offscreen (e : entity) =
  e.dest.pos.x <- -1. *. abs_float e.dest.pos.x;
  e.dest.pos.y <- -1. *. abs_float e.dest.pos.y

let move_onscreen (e : entity) =
  e.dest.pos.x <- abs_float e.dest.pos.x;
  e.dest.pos.y <- abs_float e.dest.pos.y

let hide entity =
  freeze entity;
  entity.current_floor <- None;
  move_offscreen entity

let unhide (e : entity) = move_onscreen e

let unhide_at (e : entity) (p : vector) =
  unfreeze e;
  e.dest.pos <- clone_vector p

let hidden (entity : entity) : bool = entity.dest.pos.x < 0. && entity.dest.pos.y < 0.

let set_facing (d : direction) (e : entity) =
  match d with
  | RIGHT -> e.sprite.facing_right <- true
  | LEFT -> e.sprite.facing_right <- false
  | _ -> failwithf "Entity.set_facing bad direction %s" (Show.direction d)

let apply_v ?(debug = None) dt (e : entity) =
  (match e.y_recoil with
  | None -> e.dest.pos.y <- e.dest.pos.y +. (e.v.y *. dt)
  | Some recoil ->
    if recoil.reset_v then
      e.v.y <- 0.;
    e.dest.pos.y <- e.dest.pos.y +. (recoil.speed *. dt);
    if recoil.time_left.seconds > 0. then
      recoil.time_left <- { seconds = recoil.time_left.seconds -. dt }
    else
      e.y_recoil <- None);
  match e.x_recoil with
  | None -> e.dest.pos.x <- e.dest.pos.x +. (e.v.x *. dt)
  | Some recoil ->
    if recoil.reset_v then
      e.v.x <- 0.;
    e.dest.pos.x <- e.dest.pos.x +. (recoil.speed *. dt);
    if recoil.time_left.seconds > 0. then
      recoil.time_left <- { seconds = recoil.time_left.seconds -. dt }
    else
      e.x_recoil <- None

let get_rect_collisions (entity : entity) (rects : rect list) : (collision * rect) list =
  let collisions : (collision * rect) list ref = ref [] in
  let check_collision (rect : rect) =
    match Collision.with_entity entity rect with
    | None -> ()
    | Some coll -> collisions := (coll, rect) :: !collisions
  in
  List.iter check_collision rects;
  !collisions

let get_platform_collisions (entity : entity) (platforms : platform list) : (collision * rect) list
    =
  let collisions : (collision * rect) list ref = ref [] in
  let check_collision (platform : platform) =
    match Collision.with_entity entity platform.sprite.dest with
    | None -> ()
    | Some coll -> (
      collisions := (coll, platform.sprite.dest) :: !collisions;
      match platform.kind with
      | None -> ()
      | Some (TEMPORARY _)
      | Some (DISAPPEARABLE _) ->
        if coll.direction = UP then
          entity.current_platforms <- platform :: entity.current_platforms
      | Some (ROTATABLE _) -> entity.current_platforms <- platform :: entity.current_platforms)
  in
  List.iter check_collision platforms;
  !collisions

(* TODO could consolidate these *)
let get_tile_collisions (layers : layer list) (entity : entity) : (collision * rect) list =
  let collisions : (collision * rect) list ref = ref [] in
  let check_collision (layer : layer) =
    let check_tile_group (tile_group : tile_group) =
      match Collision.with_entity entity tile_group.dest with
      | None -> ()
      | Some coll -> collisions := (coll, tile_group.dest) :: !collisions
    in
    List.iter check_tile_group layer.tile_groups
  in
  List.iter check_collision layers;
  !collisions

let get_bench_collisions (room : room) (entity : entity) : (collision * rect) list =
  let layers = List.filter (fun (l : layer) -> l.name = "benches") room.layers in
  get_tile_collisions layers entity

let get_conveyor_belt_collision (room : room) (entity : entity) : (collision * rect * vector) option
    =
  let collisions : (collision * rect * vector) list ref = ref [] in
  let check_collision ((rect, speed) : rect * float) =
    match Collision.with_entity entity rect with
    | None -> ()
    | Some coll ->
      let v = { x = speed; y = 0. } in
      collisions := (coll, rect, v) :: !collisions
  in
  List.iter check_collision room.conveyor_belts;
  List.nth_opt !collisions 0

let get_floor_collisions (room : room) (entity : entity) : (collision * rect) list =
  let layers = List.filter (fun (l : layer) -> l.config.collides_with_ghost) room.layers in
  get_tile_collisions layers entity
  @ get_platform_collisions entity room.platforms
  @ get_rect_collisions entity room.floors

let get_water_collisions (room : room) (entity : entity) : (collision * rect) list =
  let layers = List.filter (fun (l : layer) -> l.config.water) room.layers in
  get_tile_collisions layers entity

let get_acid_collisions (room : room) (entity : entity) : (collision * rect) list =
  let layers = List.filter (fun (l : layer) -> l.name = "acid") room.layers in
  get_tile_collisions layers entity

(* this excludes acid collisions because they are handled separately (based on Isma's Tear) *)
let get_damage_collisions (room : room) (entity : entity) =
  ( get_rect_collisions entity (room.hazards @ room.spikes),
    get_rect_collisions entity (List.map snd room.platform_spikes) )

let get_loose_projectile_collisions (room : room) entity : (collision * projectile) list =
  let get_projectile_collision (projectile : projectile) =
    match Collision.with_entity entity projectile.entity.dest with
    | None -> None
    | Some c -> Some (c, projectile)
  in
  List.filter_map get_projectile_collision room.loose_projectiles

let apply_collisions (e : entity) ?(_debug = false) (collisions : (collision * rect) list) : unit =
  let adjust_position ((coll, floor) : collision * rect) =
    let top_of r = r.pos.y in
    let bottom_of r = r.pos.y +. r.h in
    let left_of r = r.pos.x in
    let right_of r = r.pos.x +. r.w in
    match coll.direction with
    | UP ->
      if above_floor e floor then (
        if e.config.bounce < 0.01 then
          (* this is a little weird, but the floor shouldn't be set for fragments
             (because it forces the new_vy to be 0.) *)
          e.current_floor <- Some (floor, Zero.vector ());
        e.dest.pos.y <- top_of floor -. e.dest.h);
      if abs_float e.v.x < 10. then (
        e.v.y <- 0.;
        if e.config.inanimate then
          e.update_pos <- false)
      else (
        e.v.x <- e.v.x *. 0.3;
        e.v.y <- e.v.y *. -1. *. e.config.bounce)
    | DOWN ->
      if below_floor e floor && e.v.y < 0. then (
        e.v.y <- 0.;
        e.dest.pos.y <- bottom_of floor)
    | LEFT -> e.dest.pos.x <- left_of floor -. e.dest.w
    | RIGHT -> e.dest.pos.x <- right_of floor
  in
  let left_right_collisions, up_down_collisions =
    let is_left_right ((c, _) : collision * rect) = c.direction = LEFT || c.direction = RIGHT in
    List.partition is_left_right collisions
  in
  (* move sideways first to fix collisions with tiles stacked directly on top of each other *)
  List.iter adjust_position left_right_collisions;
  List.iter adjust_position up_down_collisions

(* TODO maybe consolidate these *)
let update_pos ?(debug = None) (room : room) (entity : entity) (dt : float) : unit =
  if entity.update_pos then (
    let dvy =
      match entity.current_floor with
      | Some _ -> 0.
      | None -> Config.physics.gravity *. dt *. entity.config.gravity_multiplier
    in
    apply_v ~debug dt entity;
    entity.v.y <- entity.v.y +. dvy;
    get_floor_collisions room entity |> apply_collisions entity)

(* TODO maybe move this to enemy.ml *)
(* returns true when there was a floor collision (to set floor_collision_this_frame) *)
let update_enemy_pos
    ?(debug = None)
    ?(gravity_multiplier' = None)
    (room : room)
    (entity : entity)
    dt : bool =
  if entity.update_pos then (
    let gravity_multiplier =
      match gravity_multiplier' with
      | None -> entity.config.gravity_multiplier
      | Some mult -> mult
    in
    let dvy =
      match entity.current_floor with
      | Some _ -> 0.
      | None -> Config.physics.gravity *. dt *. gravity_multiplier
    in
    apply_v ~debug dt entity;
    entity.v.y <- entity.v.y +. dvy;
    let collisions = get_floor_collisions room entity in
    apply_collisions entity collisions;
    List.length collisions > 0)
  else
    false

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
        | Some (collision, new_floor) ->
          entity.current_floor <- Some (new_floor, Zero.vector ());
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
  let to_the_right () = { y = parent_dest.pos.y; x = parent_dest.pos.x +. (parent_dest.w /. 2.) } in
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
  | ALIGNED (x_alignment, y_alignment) ->
    let x =
      match x_alignment with
      | LEFT -> parent_dest.pos.x
      | RIGHT -> parent_dest.pos.x +. parent_dest.w -. child_w
      | CENTER -> parent_dest.pos.x +. ((parent_dest.w -. child_w) /. 2.)
    in
    let y =
      match y_alignment with
      | TOP -> parent_dest.pos.y
      | BOTTOM -> parent_dest.pos.y +. parent_dest.h -. child_h
      | CENTER -> parent_dest.pos.y +. ((parent_dest.h -. child_h) /. 2.)
    in
    { x; y }

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

let walk (entity : entity) (direction : direction) : unit =
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
    x_offset = json.x_offset |> Int.to_float;
    y_offset = json.y_offset |> Int.to_float;
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
  (* let textures = List.map load_pose texture_configs in *)
  let config =
    (* texture_configs can't be empty *)
    List.nth texture_configs 0
  in
  let validate_configs_are_complete () =
    let get_filenames asset_dir char_name = File.ls (fmt "../assets/%s/%s" asset_dir char_name) in
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
