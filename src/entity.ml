open Types

[@@@ocaml.warning "-26-27-32"]

let above_floor (e : entity) floor = floor.pos.x < e.dest.pos.x +. e.dest.w && floor.pos.x +. floor.w > e.dest.pos.x

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

let hide (e : entity) =
  freeze e;
  move_offscreen e

let unhide (e : entity) = move_onscreen e

let unhide_at (e : entity) (p : vector) =
  unfreeze e;
  e.dest.pos <- clone_vector p

let set_facing (d : direction) (e : entity) =
  match d with
  | RIGHT -> e.sprite.facing_right <- true
  | LEFT -> e.sprite.facing_right <- false
  | _ -> failwithf "Entity.set_facing bad direction %s" (Show.direction d)

let apply_v ?(debug = None) dt (e : entity) =
  (match debug with
  | None -> ()
  | Some context -> print " +++ %s +++ apply_v for %s" context (Show.entity_name e));
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

let get_floor_collisions state (e : entity) : (collision * rect) list =
  let collisions : (collision * rect) list ref = ref [] in
  let check_collision (layer : layer) =
    let check_tile_group (tile_group : tile_group) =
      match collision_between e tile_group.dest with
      | None -> ()
      | Some coll -> collisions := (coll, tile_group.dest) :: !collisions
    in
    List.iter check_tile_group layer.tile_groups
  in
  let layers = List.filter (fun (l : layer) -> l.config.collides_with_ghost) state.room.layers in
  List.iter check_collision layers;
  !collisions

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
          e.current_floor <- Some floor;
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

let update_pos (state : state) ?(debug = None) (e : entity) =
  if e.update_pos then (
    let dvy =
      match e.current_floor with
      | Some _ -> 0.
      | None -> Config.physics.gravity *. state.frame.dt
    in
    apply_v ~debug state.frame.dt e;
    e.v.y <- e.v.y +. dvy;
    get_floor_collisions state e |> apply_collisions e)

let get_child_pos (parent : entity) (relative_pos : relative_position) child_w child_h =
  let to_the_left () = { y = parent.dest.pos.y; x = parent.dest.pos.x -. child_w +. (parent.dest.w /. 2.) } in
  let to_the_right () = { y = parent.dest.pos.y; x = parent.dest.pos.x +. (parent.dest.w /. 2.) } in
  match relative_pos with
  (* TODO this isn't really above or below, it's used for upslash/downslash which overlap with the ghost *)
  | ABOVE -> { x = parent.dest.pos.x +. ((parent.dest.w -. child_w) /. 2.); y = parent.dest.pos.y -. (child_h /. 2.) }
  | BELOW -> { x = parent.dest.pos.x +. ((parent.dest.w -. child_w) /. 2.); y = parent.dest.pos.y }
  | IN_FRONT ->
    if parent.sprite.facing_right then
      to_the_right ()
    else
      to_the_left ()
  | BEHIND ->
    if parent.sprite.facing_right then
      to_the_left ()
    else
      to_the_right ()
  | ALIGN_CENTERS ->
    {
      x = parent.dest.pos.x +. ((parent.dest.w -. child_w) /. 2.);
      y = parent.dest.pos.y +. ((parent.dest.h -. child_h) /. 2.);
    }

let on_ground (e : entity) = e.current_floor <> None
let descending (e : entity) = e.v.y > 0.
let is_on_screen' (r : rect) = not (r.pos.x < 0. && r.pos.y < 0.)
let is_on_screen (e : entity) = is_on_screen' e.dest
let is_off_screen (e : entity) = not (is_on_screen e)

(* called once per frame to align sprite.dest position with entity.dest position *)
let adjust_sprite_dest (e : entity) =
  e.sprite.dest.pos.y <- e.dest.pos.y -. e.sprite.texture.coll_offset.y;
  if e.sprite.facing_right then
    e.sprite.dest.pos.x <- e.dest.pos.x -. e.sprite.texture.coll_offset.x
  else
    e.sprite.dest.pos.x <- e.dest.pos.x -. (e.sprite.dest.w -. e.dest.w -. e.sprite.texture.coll_offset.x)

let update_sprite_texture (e : entity) (texture : texture) =
  e.sprite.texture <- texture;
  e.sprite.dest <- Sprite.make_dest e.sprite.dest.pos.x e.sprite.dest.pos.y texture

let clone (orig : entity) : entity =
  let dest_clone = { pos = { x = orig.dest.pos.x; y = orig.dest.pos.y }; w = orig.dest.w; h = orig.dest.h } in
  let v_clone = { x = orig.v.x; y = orig.v.y } in
  { orig with dest = dest_clone; v = v_clone; sprite = Sprite.clone orig.sprite; x_recoil = None; y_recoil = None }

let create
    (path : string)
    ?(scale = Config.scale.ghost)
    ?(inanimate = false)
    ?(v = Zero.vector ())
    ?(facing_right = true)
    initial_texture
    (dest : rect) : entity =
  let animation_src = get_src initial_texture in
  let sprite_w, sprite_h = (animation_src.w *. scale, animation_src.h *. scale) in
  {
    dest;
    sprite =
      Sprite.create path initial_texture ~facing_right
        { pos = { x = dest.pos.x; y = dest.pos.y }; w = sprite_w; h = sprite_h };
    config = { bounce = (if inanimate then 0.2 else 0.); inanimate };
    update_pos = (not inanimate) && is_on_screen' dest;
    v;
    current_floor = None;
    x_recoil = None;
    y_recoil = None;
  }

let create_for_sprite (sprite : sprite) ?(inanimate = false) ?(v = Zero.vector ()) (dest : rect) : entity =
  {
    dest;
    sprite;
    config = { bounce = (if inanimate then 0.2 else 0.); inanimate };
    update_pos = (not inanimate) && is_on_screen' dest;
    v;
    current_floor = None;
    x_recoil = None;
    y_recoil = None;
  }

let to_texture_config asset_dir character_name ((pose_name, json) : string * Json_t.texture_config) : texture_config =
  {
    count = json.count;
    duration = { seconds = json.duration };
    x_offset = json.x_offset |> Int.to_float;
    y_offset = json.y_offset |> Int.to_float;
    asset_dir;
    character_name;
    pose_name;
  }

let load_pose (texture_config : texture_config) : string * texture =
  (texture_config.pose_name, Sprite.build_texture_from_config texture_config)

let create_from_textures (texture_configs : texture_config list) (entity_dest : rect) : entity * (string * texture) list
    =
  (* let animation_src = get_src sprite.texture in *)
  let textures = List.map load_pose texture_configs in
  let validate_configs_are_complete () =
    let tc =
      (* texture_configs can't be empty *)
      List.nth texture_configs 0
    in
    let get_filenames asset_dir char_name =
      Sys.readdir (Tiled.convert_path (fmt "../assets/%s/%s" asset_dir char_name)) |> Array.to_list
    in
    let config_names = texture_configs |> List.map (fun (t : texture_config) -> fmt "%s.png" t.pose_name) in
    let png_names = get_filenames (Show.asset_dir tc.asset_dir) tc.character_name in

    let validate_png_name png_name =
      if not (List.mem png_name config_names) then
        failwithf "found %s image '%s' that has no corresponding config in enemies.json" tc.character_name png_name
    in

    let validate_config_name config_name =
      if not (List.mem config_name png_names) then (
        let short =
          (* strip .png from the end *)
          Str.first_chars config_name (String.length config_name - 4)
        in
        failwithf "found %s texture_config for '%s' that has no corresponding %s.png" tc.character_name short short)
    in
    List.iter validate_png_name png_names;
    List.iter validate_config_name config_names
  in
  let initial_texture = List.nth textures 0 |> snd in
  let texture_config = List.nth texture_configs 0 in
  validate_configs_are_complete ();
  (create texture_config.character_name initial_texture entity_dest, textures)
