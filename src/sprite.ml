open Types

let make_dest x y (t : texture) : rect =
  let w =
    match t.animation_src with
    | STILL _ -> Raylib.Texture.width t.image |> Int.to_float
    | ONCE animation
    | PARTICLE animation
    | LOOPED animation -> (
      try abs (Raylib.Texture.width t.image / List.length animation.frames) |> Int.to_float with
      | Division_by_zero -> 1.)
  in
  (* w/h here is the animation_frame's full image w/h, not the physical w/h *)
  {
    pos = { x; y };
    h = (Raylib.Texture.height t.image |> Int.to_float) *. Config.scale.ghost;
    w = w *. Config.scale.ghost;
  }

let get_current_frame_idx (animation_src : animation_src) : int =
  match animation_src with
  | STILL _ ->
    (* TODO maybe just return -1 if crashing is too inconvenient *)
    failwith "can't get current_idx of STILL animation"
  | ONCE animation
  | PARTICLE animation
  | LOOPED animation ->
    animation.frame_idx

let reset_texture (texture : texture) : unit =
  match texture.animation_src with
  | STILL _ -> ()
  | ONCE animation
  | PARTICLE animation
  | LOOPED animation ->
    animation.frame_idx <- 0

let advance_animation (current_clock : float) next_texture (sprite : sprite) =
  match sprite.texture.animation_src with
  | STILL _ -> ()
  | ONCE animation
  | PARTICLE animation
  | LOOPED animation ->
    let current_animation_idx = animation.frame_idx mod List.length animation.frames in
    let animation_frame =
      if List.length animation.frames = 0 then
        { src = Zero.rect (); duration = { seconds = Float.max_float } }
      else
        List.nth animation.frames current_animation_idx
    in
    let should_advance_frame () =
      current_clock -. animation.frame_started.at > animation_frame.duration.seconds
    in
    if should_advance_frame () then (
      match next_texture.animation_src with
      | STILL _ -> ()
      | ONCE next_animation
      | PARTICLE next_animation
      | LOOPED next_animation ->
        next_animation.frame_idx <- current_animation_idx + 1;
        next_animation.frame_started.at <- current_clock)

type particle_animation = { should_despawn : bool }

(* this always advances the animation, and returns None when the animation should despawn *)
let advance_or_despawn (current_clock : float) next_texture (sprite : sprite) : sprite option =
  let advance_particle_animation (current_clock : float) next_texture (sprite : sprite) :
      particle_animation =
    match sprite.texture.animation_src with
    | STILL _ -> { should_despawn = false }
    | LOOPED animation -> failwith "can't despawn a LOOPED animation"
    | ONCE animation
    | PARTICLE animation ->
      let start_idx = get_current_frame_idx sprite.texture.animation_src in
      advance_animation current_clock next_texture sprite;
      let should_despawn = animation.frame_idx = List.length animation.frames in
      { should_despawn }
  in
  let particle = advance_particle_animation current_clock next_texture sprite in
  if particle.should_despawn then None else Some sprite

let texture_path_to_string (texture_path : texture_path) : string =
  File.make_path
    [
      Show.asset_dir texture_path.asset_dir;
      texture_path.character_name;
      fmt "%s.png" texture_path.pose_name;
    ]

let build_texture'
    ?(scale = Config.scale.ghost)
    ?(particle = false)
    ?(once = false)
    ?(animation_src_rect = None)
    (texture_config : texture_config)
    (image : image) : texture =
  let count = texture_config.count in
  let w = Raylib.Texture.width image / count |> Int.to_float in
  let h = Raylib.Texture.height image |> Int.to_float in
  let animation_src =
    match animation_src_rect with
    | Some rect -> STILL rect
    | None ->
      if count = 1 then
        STILL (make_single_frame ~w ~h)
      else (
        let make_frames frame_count ~w ~h ~duration =
          (* creates an animation_frame list with all the same duration *)
          let make_frame (frame_idx : float) : animation_frame =
            {
              src = { w; h; pos = { y = 0.; x = frame_idx *. w } };
              duration = { seconds = duration };
            }
          in
          List.map make_frame (Utils.rangef frame_count)
        in
        let frames = make_frames count ~w ~h ~duration:texture_config.duration.seconds in
        if once then
          ONCE { frame_idx = 0; frames; frame_started = { at = 0. } }
        else if particle then
          PARTICLE { frame_idx = 0; frames; frame_started = { at = 0. } }
        else (
          let frame_started =
            (* frame_started.at will be updated before the first time it starts, so this value is arbitrary *)
            { at = -100. }
          in
          LOOPED { frame_idx = 0; frames; frame_started }))
  in
  {
    ident = texture_path_to_string texture_config.path;
    image;
    coll_offset = { x = texture_config.x_offset *. scale; y = texture_config.y_offset *. scale };
    animation_src;
  }

let build_texture_from_path
    ?(scale = Config.scale.ghost)
    ?(particle = false)
    ?(once = false)
    (path : texture_path) =
  let image = load_image (texture_path_to_string path) in
  let texture_config =
    {
      (* texture location isn't used because image has already been loaded *)
      path = { asset_dir = NPCS; character_name = ""; pose_name = "" };
      count = 1;
      duration = { seconds = 0. };
      x_offset = 0.;
      y_offset = 0.;
    }
  in
  build_texture' ~scale ~particle ~once texture_config image

let build_texture_from_config
    ?(scale = Config.scale.ghost)
    ?(particle = false)
    ?(once = false)
    (texture_config : texture_config) : texture =
  let path = texture_path_to_string texture_config.path in
  let image = load_image path in
  build_texture' ~scale ~particle ~once texture_config image

let build_static_texture ?(asset_dir = NPCS) name =
  build_texture_from_config
    {
      path = { asset_dir; character_name = "shared"; pose_name = name };
      count = 1;
      duration = { seconds = 0. };
      x_offset = 0.;
      y_offset = 0.;
    }

let build_texture_from_image
    ?(scale = Config.scale.ghost)
    ?(particle = false)
    (image : image)
    animation_src_rect : texture =
  let texture_config =
    {
      (* texture location isn't used because image has already been loaded *)
      path = { asset_dir = NPCS; character_name = ""; pose_name = "" };
      count = 1;
      duration = { seconds = 0. };
      x_offset = 0.;
      y_offset = 0.;
    }
  in
  build_texture' ~scale ~particle ~animation_src_rect texture_config image

let clone (orig : sprite) : sprite =
  let dest_clone =
    { pos = { x = orig.dest.pos.x; y = orig.dest.pos.y }; w = orig.dest.w; h = orig.dest.h }
  in
  { orig with dest = dest_clone }

let create
    (name : string)
    (texture : texture)
    ?(facing_right = true)
    ?(collision = None)
    (dest : rect) : sprite =
  let validate_shape_is_inside_sprite_dest () =
    match collision with
    | Some (SHAPE shape) ->
      let first xs = List.nth xs 0 in
      let last xs = List.nth xs (List.length xs - 1) in
      let aligned_shape = align_shape_with_parent dest facing_right shape in
      let points = get_points aligned_shape in
      let get_points' fn = List.map fn points |> List.sort Float.compare in
      let xs, ys = (get_points' (fun v -> v.x), get_points' (fun v -> v.y)) in
      let sprite_min_x, sprite_max_x = (dest.pos.x, dest.pos.x +. dest.w) in
      let sprite_min_y, sprite_max_y = (dest.pos.y, dest.pos.y +. dest.h) in
      if first xs < sprite_min_x then
        failwithf "collision shape min x %f is too small, needs to be bigger than sprite dest %f"
          (first xs) sprite_min_x;
      if last xs > sprite_max_x then
        failwithf "collision shape max x %f is too big, needs to be smaller than sprite dest %f"
          (last xs) sprite_max_x;
      if first ys < sprite_min_y then
        failwithf "collision shape min y %f is too small, needs to be bigger than sprite dest %f"
          (first ys) sprite_min_y;
      if last ys > sprite_max_y then
        failwithf "collision shape max y %f is too big, needs to be smaller than sprite dest %f"
          (last ys) sprite_max_y
    | None
    | Some _ ->
      ()
  in
  validate_shape_is_inside_sprite_dest ();
  { ident = fmt "Sprite[%s]" name; texture; facing_right; dest; collision }

(* TODO maybe this should take a relative_pos option arg, and use Entity.get_child_pos when it's provided *)
let spawn_particle
    ?(facing_right = true)
    ?(collision = None)
    (name : string)
    (texture : texture)
    (dest : rect)
    frame_time : sprite =
  (match texture.animation_src with
  | STILL _
  | LOOPED _
  | ONCE _ ->
    failwithf "tried to spawn particle with %s" (Show.animation_src texture.animation_src)
  | PARTICLE animation ->
    animation.frame_idx <- 0;
    animation.frame_started.at <- frame_time);
  { ident = fmt "Sprite[%s]" name; texture; facing_right; dest; collision }
