open Types

[@@@ocaml.warning "-26-27-32"]

let make_dest x y (t : texture) : rect =
  let w =
    match t.animation_src with
    | STILL _ -> Raylib.Texture.width t.image |> Int.to_float
    | PARTICLE animation -> (
      try abs (Raylib.Texture.width t.image / List.length animation.frames) |> Int.to_float with
      | Division_by_zero -> 1.)
    | ANIMATED animation -> (
      try abs (Raylib.Texture.width t.image / List.length animation.frames) |> Int.to_float with
      | Division_by_zero -> 1.)
  in
  (* w/h here is the animation_frame's full image w/h, not the physical w/h *)
  {
    pos = { x; y };
    h = (Raylib.Texture.height t.image |> Int.to_float) *. Config.scale.ghost;
    w = w *. Config.scale.ghost;
  }

let advance_animation (current_clock : float) next_texture (sprite : sprite) : unit =
  match sprite.texture.animation_src with
  | STILL _ -> ()
  | PARTICLE animation
  | ANIMATED animation ->
    (* TODO this should restart the idx at 0 when the texture changes
       - this looks especially bad for the locker-boys vanish (it starts fine but desyncs as the fight goes on)
       - probably have separate fn for advance_animation and change_animation, since next_texture here is almost always the same
    *)
    let current_animation_idx = animation.current_idx mod List.length animation.frames in
    let animation_frame =
      if List.length animation.frames = 0 then
        { src = Zero.rect (); duration = { s = Float.max_float } }
      else
        List.nth animation.frames current_animation_idx
    in
    let should_advance_frame () = current_clock -. animation.current_frame_clock > animation_frame.duration.s in
    if should_advance_frame () then (
      match next_texture.animation_src with
      | STILL _ -> ()
      | PARTICLE next_animation
      | ANIMATED next_animation ->
        next_animation.current_idx <- current_animation_idx + 1;
        next_animation.current_frame_clock <- current_clock)

let advance_particle_animation (current_clock : float) next_texture (sprite : sprite) : bool =
  match sprite.texture.animation_src with
  | PARTICLE next_animation ->
    advance_animation current_clock next_texture sprite;
    next_animation.current_idx < List.length next_animation.frames
  | _ -> failwith "advance_particle_animation on non-PARTICLE animation"

let get_path (texture_config : texture_config) : string =
  fmt "%s/%s/%s" (get_asset_dir texture_config.asset_dir) texture_config.character_name texture_config.pose_name

let build_texture'
    ?(scale = Config.scale.ghost)
    ?(particle = false)
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
            { src = { w; h; pos = { y = 0.; x = frame_idx *. w } }; duration = { s = duration } }
          in
          List.map make_frame (Utils.rangef frame_count)
        in
        let frames = make_frames count ~w ~h ~duration:texture_config.duration.s in
        if particle then
          PARTICLE { current_idx = 0; frames; current_frame_clock = 0. }
        else
          (* this starts at the final index because Sprite.advance_animation is called on the frame
             that they are created, which advances the current_idx to 0
             TODO not sure about ^this, check if it is still true
          *)
          ANIMATED { current_idx = List.length frames - 1; frames; current_frame_clock = 0. })
  in
  {
    ident = get_path texture_config;
    image;
    coll_offset = { x = texture_config.x_offset *. scale; y = texture_config.y_offset *. scale };
    animation_src;
  }

let build_texture_from_config ?(scale = Config.scale.ghost) ?(particle = false) (texture_config : texture_config) :
    texture =
  let path = get_path texture_config in
  let image = Types.load_image path in
  build_texture' ~scale ~particle texture_config image

let build_texture_from_image ?(scale = Config.scale.ghost) ?(particle = false) (image : image) animation_src_rect :
    texture =
  let texture_config =
    {
      (* texture location isn't used because image has already been loaded *)
      asset_dir = NPCS;
      character_name = "";
      pose_name = "";
      count = 1;
      duration = { s = 0. };
      x_offset = 0.;
      y_offset = 0.;
    }
  in

  build_texture' ~scale ~particle ~animation_src_rect texture_config image

let clone (orig : sprite) : sprite =
  let dest_clone = { pos = { x = orig.dest.pos.x; y = orig.dest.pos.y }; w = orig.dest.w; h = orig.dest.h } in
  { orig with dest = dest_clone }

let create (name : string) (texture : texture) ?(facing_right = true) (dest : rect) : sprite =
  { ident = fmt "Sprite[%s]" name; texture; facing_right; dest }
