open Types
module Color = Raylib.Color
module Rectangle = Raylib.Rectangle

type text_config = Interaction.text_config

[@@@ocaml.warning "-26-27-32"]

(* these are just Raylib aliases now, but things like draw_entity could be here too *)
module Draw = struct
  open Raylib

  let circle = draw_circle
  let circle_v = draw_circle_v
  let rect_lines = draw_rectangle_lines_ex
  let rect = draw_rectangle
    (* FIXME need to rename this now that I'm using actual Raylib draw_image *)
  let image = draw_texture_pro
  let line_ex = draw_line_ex
end

(* adjusts sign of w based on sprite.facing_right *)
let src_Rect (s : sprite) : Raylib.Rectangle.t =
  let src = get_src s.texture in
  let w =
    if s.facing_right then
      abs_float src.w
    else
      -1. *. abs_float src.w
  in
  Raylib.Rectangle.create src.pos.x src.pos.y w src.h

let debug_shape_outline ?(size = 1.) ?(color = Color.raywhite) (sprite : sprite) (shape : shape) =
  let adjusted_shape : shape = align_shape_with_parent_sprite sprite shape in
  let points = get_points adjusted_shape in
  let draw_edge point_idx (point : vector) =
    (* TODO duplicated in make_shape
       - could add a fn that operates on every pair of points in a shape, but probably not worth it yet
    *)
    let next_point = List.nth points ((point_idx + 1) mod List.length points) in
    Draw.line_ex
      (Raylib.Vector2.create point.x point.y)
      (Raylib.Vector2.create next_point.x next_point.y)
      size color
  in
  let points' = List.map Show.vector points |> join in
  List.iteri draw_edge points

let debug_rect_outline ?(size = 2.) ?(color = Color.raywhite) (rect : rect) =
  Draw.rect_lines (rect |> to_Rect) size color

let debug_rect' color (rect : rect) =
  let color' =
    Raylib.Color.create (Raylib.Color.r color) (Raylib.Color.g color) (Raylib.Color.b color) 100
  in
  Draw.rect (rect.pos.x |> Float.to_int)
    (rect.pos.y -. 2. |> Float.to_int)
    (rect.w |> Float.to_int) (rect.h |> Float.to_int) color'

let debug_rect ?(r = 0) ?(g = 200) ?(b = 200) ?(a = 100) (rect : rect) =
  debug_rect' (Color.create r g b a) rect

let debug_xy x y = Draw.circle (x |> Float.to_int) (y |> Float.to_int) 4. Color.green
let debug_v (v : Raylib.Vector2.t) = Draw.circle_v v 4. Color.green

(* TODO probably want to rename to be less similar to draw_texture_pro *)
let draw_texture
    ?(debug = false)
    ?(tint = Color.raywhite)
    (t : texture)
    (dest : rect)
    transformation_bits =
  let src, rotation =
    let r = get_src t in
    match transformation_bits with
    | 0 -> (r, 0.)
    | 1 -> ({ r with h = -1. *. r.h }, 90.)
    | 2 -> ({ r with h = -1. *. r.h }, 0.)
    | 3 -> (r, 270.)
    | 4 -> ({ r with w = -1. *. r.w }, 0.)
    | 5 -> (r, 90.)
    | 6 -> ({ r with w = -1. *. r.w; h = -1. *. r.h }, 0.)
    | 7 -> ({ r with w = -1. *. r.w }, 90.)
    | _n -> (r, 0.)
  in
  let tile_size =
    (* FIXME maybe move to a config
       ... except now it _really_ won't change, so maybe not
    *)
    12. *. Config.scale.room
  in
  let dest' =
    match rotation with
    | 0. -> dest |> to_Rect
    | 90. -> { dest with pos = { dest.pos with x = dest.pos.x +. tile_size } } |> to_Rect
    | 270. -> { dest with pos = { dest.pos with y = dest.pos.y +. tile_size } } |> to_Rect
    | _ -> dest |> to_Rect
  in
  if debug then
    debug_rect_outline dest;
  Draw.image t.image (src |> to_Rect) dest' (Raylib.Vector2.create 0. 0.) rotation tint

(* this function should only be used by sprites that don't have an entity
   - when an entity is present, use draw_entity because it corrects the sprite position before rendering
*)
let draw_sprite ?(debug = false) ?(tint = Color.create 255 255 255 255) (sprite : sprite) =
  let dest = sprite.dest |> to_Rect in
  Draw.image sprite.texture.image (src_Rect sprite) dest (Raylib.Vector2.create 0. 0.) 0. tint;
  match sprite.collision with
  | Some (SHAPE shape) ->
    if debug then
      debug_shape_outline sprite shape
  | _ -> ()

let draw_entity ?(debug = false) ?(tint = Color.create 255 255 255 255) (e : entity) =
  (* drawing entity and drawing sprite are the same thing since sprite.dest is updated every frame to match entity.dest *)
  Entity.adjust_sprite_dest e;
  draw_sprite ~debug ~tint e.sprite

let center (r : rect) : Raylib.Vector2.t =
  Raylib.Vector2.create (r.pos.x +. (r.w /. 2.)) (r.pos.y +. (r.h /. 2.))

let draw_velocity (e : entity) : unit =
  let end_x, end_y = (e.sprite.dest.pos.x +. (e.v.x /. 3.), e.sprite.dest.pos.y +. (e.v.y /. 3.)) in
  Draw.line_ex
    (Raylib.Vector2.create e.sprite.dest.pos.x e.sprite.dest.pos.y)
    (Raylib.Vector2.create end_x end_y)
    5. Color.purple;
  Draw.circle_v (Raylib.Vector2.create end_x end_y) 5. Color.blue

let draw_tiled_layer
    ?(debug = false)
    (room : room)
    camera_x
    camera_y
    frame_idx
    ?(tint = Color.create 255 255 255 255)
    ?(parallax = None)
    (layer : layer) : unit =
  let within_camera dest_x dest_y =
    let camera_min_x, camera_max_x =
      (camera_x -. 100., camera_x +. (Config.window.width + 100 |> Int.to_float))
    in
    let camera_min_y, camera_may_y =
      (camera_y -. 100., camera_y +. (Config.window.height + 100 |> Int.to_float))
    in
    camera_min_x < dest_x && dest_x < camera_max_x && camera_min_y < dest_y && dest_y < camera_may_y
  in
  if not layer.hidden then (
    let draw_stub (sprite, transformation_bits) =
      draw_texture ~tint sprite.texture sprite.dest transformation_bits
    in
    let tint' = if layer.config.shaded then Color.black else tint in
    let render_data_tile (idx : int) (gid : int) =
      if gid <> 0 && not (List.mem idx layer.destroyed_tiles) then (
        let x, y =
          Tiled.Room.dest_xy room.json ~parallax_opt:parallax layer.json.offset_x
            layer.json.offset_y idx layer.json.w
        in
        if within_camera x y then (
          let texture, transformations =
            let animation_offset =
              if layer.config.animated then
                (* TODO this should probably just use an animated sprite instead of
                   this weird animation_offset for the tile gid
                *)
                (* FIXME probably needs `mod 16` now
                   - probably will be more complicated that that since it needs to skip every-other tile
                *)
                4 * frame_idx / Config.window.fps mod 8
              else
                0
            in
            Tiled.Room.look_up_tile ~animation_offset room.json room.cache gid
          in
          let w, h = Tiled.Room.dest_wh room.json () in
          let dest = { pos = { x; y }; w; h } in
          (* FIXME instead of drawing texture, use image_draw on the render buffer *)
          draw_texture ~tint:tint' texture dest transformations))
    in
    let draw_spawned_fragment (f : entity) =
      if debug then
        debug_rect f.dest;
      draw_entity ~tint f
    in
    List.iteri render_data_tile layer.json.data;
    List.iter draw_spawned_fragment layer.spawned_fragments;
    List.iter draw_stub layer.spawned_stub_sprites)

let draw_tiles room camera_x camera_y frame_idx layers : unit =
  let draw_parallax_layer (layer : layer) =
    let parallax =
      (* TODO maybe just pass in the parallax values and do this calculation somewhere like Tiled.Tile.dest_xy *)
      Some
        {
          x = camera_x *. (1. -. layer.json.parallax_x);
          y = camera_y *. (1. -. layer.json.parallax_y);
        }
    in
    draw_tiled_layer ~tint:room.area.tint ~parallax room camera_x camera_y frame_idx layer
  in
  List.iter draw_parallax_layer layers

let draw_solid_tiles room camera_x camera_y frame_idx : unit =
  List.iter
    (draw_tiled_layer room camera_x camera_y frame_idx)
    (List.filter (fun layer -> layer.config.collides_with_ghost || layer.config.hazard) room.layers)

let draw_bg_tiles room camera_x camera_y frame_idx : unit =
  draw_tiles room camera_x camera_y frame_idx
    (List.filter (fun (layer : layer) -> layer.config.render.bg) room.layers)

let draw_fg_tiles room camera_x camera_y frame_idx : unit =
  draw_tiles room camera_x camera_y frame_idx
    (List.filter (fun (layer : layer) -> layer.config.render.fg) room.layers)

let draw_floating_platforms (room : room) camera_x camera_y frame_idx : unit =
  List.iter
    (fun (platform : platform) ->
      (* not sure if platforms should take the tint of the current area, since it makes them stand out from the bg less *)
      draw_sprite ~tint:room.area.tint platform.sprite)
    room.platforms

(* - lines are broken up into line_segments based on colors - this is three line_segments:
   I {{red}} said {{white}} "Betty Grable"
   - a line_segment belongs to a specific line, so it can't be split across multiple lines
*)
type line_segment = {
  color : Raylib.Color.t;
  mutable content : string;
  mutable dest : rect;
}

type line = {
  mutable segments : line_segment list;
  mutable w : int;
}

let show_segment (line_segment : line_segment) : string =
  if line_segment.color = Raylib.Color.raywhite then
    line_segment.content
  else
    fmt "(color)%s" line_segment.content

let show_line (line : line) : string = String.concat " | " (List.map show_segment line.segments)
let font_size = Config.scale.font_size
let line_height = font_size |> Int.to_float
let measure_text s = Raylib.measure_text s font_size

(* TODO this works ok for two kinds of interaction text input:
   - one single long line (that gets wrapped by this fn)
   - several short/non-wrapping lines

   using several long lines sets the vertical spacing incorrectly so lines overlap each other
*)
let get_lines ?(_debug = false) (measure : string -> int) (w : int) (words : string list) :
    line list =
  let new_segment ?(color = Color.raywhite) content content_w line_w =
    {
      content;
      dest =
        {
          pos = { x = line_w |> Int.to_float; y = 0. };
          w = content_w |> Int.to_float;
          h = line_height;
        };
      color;
    }
  in
  let empty_line () = { segments = []; w = 0 } in

  let add_segment_to_last_line lines (segment : line_segment) : line list =
    let current_line = List.nth lines (List.length lines - 1) in
    current_line.w <- current_line.w + measure segment.content;
    (* segments are appended in the correct order, so they never need to be reversed *)
    current_line.segments <- current_line.segments @ [ segment ];
    lines
  in

  let start_new_line lines (segment : line_segment) : line list =
    add_segment_to_last_line lines segment @ [ empty_line () ]
  in

  let add_word_to_segment (first_segment_in_line : bool) lines segment word segment_w word_w =
    let segments =
      if segment.content = "" && first_segment_in_line then
        {
          segment with
          content = word;
          dest = { segment.dest with w = segment_w + word_w |> Int.to_float };
        }
      else (
        let content = fmt "%s %s" segment.content word in
        { segment with content; dest = { segment.dest with w = measure content |> Int.to_float } })
    in
    (lines, segments)
  in

  let add_word lines segment word : line list * line_segment =
    let word_w = measure (fmt "%s " word) in
    let segment_w = measure segment.content in
    let current_line = List.nth lines (List.length lines - 1) in
    let change_color color =
      (add_segment_to_last_line lines segment, new_segment ~color "" 0 (segment_w + current_line.w))
    in
    match word with
    | "{{white}}" -> change_color Raylib.Color.raywhite
    | "{{blue}}" -> change_color Raylib.Color.blue
    | "{{red}}" -> change_color Raylib.Color.red
    | "{{green}}" -> change_color Raylib.Color.green
    | "{{pink}}" -> change_color Raylib.Color.pink
    | "{{orange}}" -> change_color Raylib.Color.orange
    | "{{purple}}" -> change_color Raylib.Color.purple
    | "{{yellow}}" -> change_color Raylib.Color.yellow
    | "{{gold}}" -> change_color Raylib.Color.gold
    | "{{magenta}}" -> change_color Raylib.Color.magenta
    | "{{maroon}}" -> change_color Raylib.Color.maroon
    | "{{darkblue}}" -> change_color Raylib.Color.darkblue
    | "{{darkgreen}}" -> change_color Raylib.Color.darkgreen
    | "{{darkpurple}}" -> change_color Raylib.Color.darkpurple
    | _ ->
      if word_w + segment_w + current_line.w > w then
        (start_new_line lines segment, new_segment ~color:segment.color word word_w 0)
      else
        add_word_to_segment
          (List.length current_line.segments = 0)
          lines segment word segment_w word_w
  in

  let rec iter (lines : line list) (segment : line_segment) (rest : string list) : line list =
    if List.length rest = 0 then
      add_segment_to_last_line lines segment
    else (
      let next_lines, next_segment = add_word lines segment (List.hd rest) in
      iter next_lines next_segment (List.tl rest))
  in
  let first_line : line = { segments = []; w = 0 } in
  iter [ first_line ] (new_segment "" 0 0) words

module Tests = struct
  let short_text = "I said \"Betty Grable\""
  let color_text = "I {{blue}} said {{white}} \"Betty Grable\""
  let color_text' = "I {{blue}} said \"Betty {{white}} Grable\""
  let color_text_on_boundary = "One {{blue}} two {{white}} three four {{blue}} five"

  let long_color_text =
    "One {{blue}} two {{white}} three four five six seven eight nine {{blue}} ten {{white}} eleven \
     twelve."

  let medium_text = "one two three four five"

  let long_text =
    "Pillows, but no sleep. Feathers, but no birds. Pajamas without children. Violence without \
     purpose. I saw Mommy kissing Exxon Mobil."

  (* Raylib.measure_text isn't working since the fonts aren't loaded, and I couldn't figure out how to force loading them
     - this is just being used to measure words (or more specifically, words with a space after them)
     - this function stubs word length to 10, and the width arg to get_lines' (45) allows four words per line
  *)
  let fake_measure_text str =
    if String.length str = 0 then
      0
    else (
      let base = 10 in
      let res = List.length (String.split_on_char ' ' (String.trim str)) * base in
      res)

  let get_lines' ?(_debug = false) ?(width = 45) str =
    get_lines ~_debug fake_measure_text width (String.split_on_char ' ' str)

  let run_test ?(verbose = false) text =
    let lines = get_lines' ~_debug:verbose text in
    print "line count: %d" (List.length lines);
    print "line segment counts: %s"
      (List.map (fun line -> List.length line.segments |> Int.to_string) lines |> join);
    let all_lines = String.concat "\n" (List.map show_line lines) in
    print "line contents:\n\n---\n%s" all_lines;
    print "---"

  let show_dests ?(verbose = false) text =
    let lines = get_lines' ~_debug:verbose text in
    let show_line_with_dest (line : line) =
      let segments =
        let segments' = List.map (fun s -> Show.rect s.dest) line.segments in
        String.concat "\n    " segments'
      in
      fmt "%s\n  with segments:\n    %s" (show_line line) segments
    in
    let all_lines = String.concat "\n" (List.map show_line_with_dest lines) in
    print "line contents:\n====\n%s" all_lines;
    print "===="

  let%expect_test "single long line - split at width" =
    run_test long_text;
    [%expect
      {|
      line count: 5
      line segment counts: 1, 1, 1, 1, 1
      line contents:

      ---
      Pillows, but no sleep.
      Feathers, but no birds.
      Pajamas without children. Violence
      without purpose. I saw
      Mommy kissing Exxon Mobil.
      --- |}]

  let%expect_test "colored line - three segments" =
    run_test color_text;
    [%expect
      {|
        line count: 1
        line segment counts: 3
        line contents:

        ---
        I | (color) said |  "Betty Grable"
        --- |}]

  let%expect_test "multiple lines with colors" =
    run_test long_color_text;
    [%expect
      {|
        line count: 3
        line segment counts: 3, 1, 3
        line contents:

        ---
        One | (color) two |  three four
        five six seven eight
        nine | (color) ten |  eleven twelve.
        --- |}]

  let%expect_test "last word can wrap" =
    run_test medium_text;
    [%expect
      {|
      line count: 2
      line segment counts: 1, 1
      line contents:

      ---
      one two three four
      five
      --- |}]

  let%expect_test "multiple lines with colors" =
    run_test color_text_on_boundary;
    [%expect
      {|
      line count: 2
      line segment counts: 4, 1
      line contents:

      ---
      One | (color) two |  three four | (color)
      (color)five
      --- |}]

  let%expect_test "sets dest correctly" =
    show_dests short_text;
    [%expect
      {|
      line contents:
      ====
      I said "Betty Grable"
        with segments:
          [w: 40.0, h: 24.0, at (0.0, 0.0)]
      ==== |}]

  let%expect_test "sets dest correctly with colors" =
    show_dests color_text';
    [%expect
      {|
      line contents:
      ====
      I | (color) said "Betty |  Grable"
        with segments:
          [w: 10.0, h: 24.0, at (0.0, 0.0)]
          [w: 20.0, h: 24.0, at (10.0, 0.0)]
          [w: 10.0, h: 24.0, at (30.0, 0.0)]
      ==== |}]

  let%expect_test "sets dest correctly with colors" =
    show_dests long_color_text;
    [%expect
      {|
      line contents:
      ====
      One | (color) two |  three four
        with segments:
          [w: 10.0, h: 24.0, at (0.0, 0.0)]
          [w: 10.0, h: 24.0, at (10.0, 0.0)]
          [w: 20.0, h: 24.0, at (20.0, 0.0)]
      five six seven eight
        with segments:
          [w: 40.0, h: 24.0, at (0.0, 0.0)]
      nine | (color) ten |  eleven twelve.
        with segments:
          [w: 10.0, h: 24.0, at (0.0, 0.0)]
          [w: 10.0, h: 24.0, at (10.0, 0.0)]
          [w: 20.0, h: 24.0, at (20.0, 0.0)]
      ==== |}]

  let%expect_test "sets dest correctly with colors" =
    show_dests color_text_on_boundary;
    [%expect
      {|
      line contents:
      ====
      One | (color) two |  three four | (color)
        with segments:
          [w: 10.0, h: 24.0, at (0.0, 0.0)]
          [w: 10.0, h: 24.0, at (10.0, 0.0)]
          [w: 20.0, h: 24.0, at (20.0, 0.0)]
          [w: 0.0, h: 24.0, at (40.0, 0.0)]
      (color)five
        with segments:
          [w: 10.0, h: 24.0, at (0.0, 0.0)]
      ==== |}]

  let%expect_test "sets dest correctly wrapping final word" =
    show_dests medium_text;
    [%expect
      {|
      line contents:
      ====
      one two three four
        with segments:
          [w: 40.0, h: 24.0, at (0.0, 0.0)]
      five
        with segments:
          [w: 10.0, h: 24.0, at (0.0, 0.0)]
      ==== |}]
end

let tick (state : state) =
  let camera_x, camera_y =
    ( Raylib.Vector2.x (Raylib.Camera2D.target state.camera.raylib) -. Config.window.center_x,
      Raylib.Vector2.y (Raylib.Camera2D.target state.camera.raylib) -. Config.window.center_y )
  in
  let text_box_width (config : text_config) = Config.window.width - (2 * config.margin_x) in

  let draw_screen_fade alpha =
    Raylib.draw_rectangle
      (camera_x -. 5. |> Float.to_int)
      (camera_y -. 5. |> Float.to_int)
      (Config.window.width + 10) (Config.window.height + 10) (Color.create 0 0 0 alpha)
  in

  let draw_text_bg_box ?(color = Color.create 0 0 0 200) (config : text_config) =
    let w = Config.window.width - (2 * config.margin_x) in
    let h = Config.window.height - (config.margin_y + config.margin_y_bottom) in
    Draw.rect
      ((camera_x |> Float.to_int) + config.margin_x)
      ((camera_y |> Float.to_int) + config.margin_y)
      w h color
  in

  let display_paragraph (config : text_config) y_offset paragraph_idx (paragraph : string) =
    let display_line (config : text_config) y_offset line_number (line : line) =
      let display_segment (segment : line_segment) =
        let centered_x =
          if config.centered then (text_box_width config - line.w) / 2 else config.padding_x
        in
        let line_spacing = line_height *. (line_number |> Int.to_float) in
        let dest_y = line_spacing +. camera_y +. y_offset in
        Raylib.draw_text segment.content
          ((segment.dest.pos.x +. camera_x |> Float.to_int) + config.margin_x + centered_x)
          ((dest_y |> Float.to_int) + (line_number * font_size))
          font_size segment.color
      in
      List.iter display_segment line.segments
    in

    let paragraph_offset = paragraph_idx * Config.scale.paragraph_spacing in
    let y_offset' =
      paragraph_offset + config.margin_y + config.padding_y + y_offset |> Int.to_float
    in
    let lines =
      let w = text_box_width config - (2 * config.padding_x) in
      get_lines measure_text w (String.split_on_char ' ' paragraph)
    in
    List.iteri (display_line config y_offset') lines
  in

  let maybe_draw_text (game : game option) (interaction_text : Interaction.text_kind option) =
    (* TODO add offset_x based on ghost.id, and make ability-outlines.png a grid of images *)
    let draw_outline ?(offset_y = 0) (ability_text : Interaction.ability_text) =
      let texture =
        let animation_src = STILL ability_text.outline_src in
        { state.global.textures.ability_outlines with animation_src }
      in
      let w = ability_text.outline_src.w *. Config.scale.room in
      let h = ability_text.outline_src.h *. Config.scale.room in
      let x = camera_x +. Config.window.center_x -. (w /. 2.) in
      let y = camera_y +. Config.window.center_y -. (h /. 2.) -. (offset_y |> Int.to_float) in
      let dest = { pos = { x; y }; w; h } in
      draw_texture texture dest 0
    in

    (* TODO probably worth moving all these magic numbers into a config *)
    match interaction_text with
    | None -> ()
    | Some (ABILITY ability_text) ->
      let margin_x = 50 in
      let config : text_config =
        {
          margin_x;
          margin_y = 20;
          margin_y_bottom = 20;
          outline_offset_y = Config.window.height / 4;
          padding_x = 50;
          padding_y = 50;
          centered = true;
        }
      in

      draw_screen_fade 160;
      draw_text_bg_box config;
      draw_outline ~offset_y:config.outline_offset_y ability_text;

      List.iteri
        (display_paragraph config (config.outline_offset_y + 100))
        ability_text.bottom_paragraphs
    | Some (FOCUS_ABILITY ability_text) ->
      let margin_x = 50 in
      let config : text_config =
        {
          margin_x;
          margin_y = 20;
          margin_y_bottom = 20;
          outline_offset_y = 0;
          padding_x = 50;
          padding_y = 50;
          centered = true;
        }
      in

      draw_screen_fade 160;
      draw_text_bg_box config;
      draw_outline ability_text;

      (* lots of hardcoded stuff in here, but I'm not sure if this will be used besides the focus-info text *)
      let top_y_offset, bottom_y_offset = (0, 400) in
      List.iteri (display_paragraph config top_y_offset) ability_text.top_paragraphs;
      List.iteri (display_paragraph config bottom_y_offset) ability_text.bottom_paragraphs
    | Some (DIALOGUE (speaker_name, text')) ->
      let margin_x = 250 in
      let config : text_config =
        {
          margin_x;
          margin_y = 50;
          margin_y_bottom =
            (* this allows for 4 lines of dialogue, but the bottom one is pretty close so < 3 lines works best
            *)
            450;
          outline_offset_y = 0;
          padding_x = 30;
          padding_y = 30;
          centered = false;
        }
      in
      draw_text_bg_box config;
      let hd = List.hd text'.content in
      if List.length (List.tl text'.content) > 0 then
        (* TODO when one character says several lines in a row, it would be nice
           to not "close" the text box in between each one *)
        failwith "unsupported: DIALOGUE can only have one line"
      else (
        let color_str =
          match speaker_name with
          | "Britta" -> "{{gold}}"
          | "Annie" -> "{{orange}}"
          | "Jeff" -> "{{darkblue}}"
          | "Troy" -> "{{darkgreen}}"
          | "Abed" -> "{{darkpurple}}"
          | "Neil" -> "{{purple}}"
          | "Chang"
          | "Hickey"
          | "Duncan" ->
            "{{maroon}}"
          | _ -> failwithf "unknown speaker: %s" speaker_name
        in
        display_paragraph config 0 0 (fmt "%s %s:  {{white}} %s" color_str speaker_name hd))
    | Some (PLAIN text') ->
      let margin_y_bottom =
        let tall_text =
          (* TODO it's worth cleaning this up if there are any long dialogs needed besides the ACB note *)
          String.length (List.nth text'.content (List.length text'.content - 1)) > 300
        in
        if tall_text then
          50
        else
          350
      in
      let margin_x = 150 in
      let config : text_config =
        {
          margin_x;
          margin_y = 50;
          margin_y_bottom;
          outline_offset_y = 0;
          padding_x = 50;
          padding_y = 50;
          centered = false;
        }
      in
      draw_text_bg_box config;
      if text'.increases_health then (
        let increase_health_text_margin_y =
          (* print the line at the bottom of the text box *)
          Config.window.height - config.margin_y_bottom - (2 * config.padding_y)
        in
        List.iteri (display_paragraph config 0) text'.content;
        List.iteri
          (display_paragraph { config with margin_y = increase_health_text_margin_y } 0)
          [ "{{green}} max health increased by one" ])
      else
        List.iteri (display_paragraph config 0) text'.content
    | Some (MENU (menu, save_slots)) ->
      let margin_x, margin_y =
        match List.nth menu.choices 0 with
        (* TODO probably need these to be based on font size *)
        | PAUSE_MENU _ -> (250, 220)
        | CHANGE_WEAPON_MENU _ -> (150, 50)
        | CHANGE_GHOST_MENU _ -> (150, 50)
        | MAIN_MENU _ -> (50, 360)
        | SAVE_FILES _ -> (50, 200)
      in

      let margin_y_bottom = margin_y in
      let config : text_config =
        {
          margin_x;
          margin_y;
          margin_y_bottom;
          outline_offset_y = Config.window.height / 4;
          padding_x = 50;
          padding_y = 50;
          centered = true;
        }
      in
      draw_text_bg_box config;
      let is_new_game (_, b) = b in

      (match save_slots with
      | Some save_slots' ->
        let show_save_slot idx ((slot, is_new_game) : Json_t.save_file * bool) =
          fmt "save %d: %s" (idx + 1) (if is_new_game then "New Game" else "Continue")
        in
        let save_slot_choices =
          List.mapi show_save_slot
            [ save_slots'.slot_1; save_slots'.slot_2; save_slots'.slot_3; save_slots'.slot_4 ]
          @ [ Show.save_files_choice BACK ]
        in
        List.iteri (display_paragraph config 0) save_slot_choices
      | None ->
        List.iteri (display_paragraph config 0) (List.map (Show.menu_choice game) menu.choices));
      (* TODO better cursor for current item *)
      display_paragraph config 0 menu.current_choice_idx
        "*                                           *"
  in

  let draw_other_text game =
    (match game.interaction.corner_text with
    | None -> ()
    | Some (text, end_time) ->
      let dest =
        {
          x = camera_x;
          y = camera_y +. ((Config.window.height |> Int.to_float) -. (font_size |> Int.to_float));
        }
      in
      let alpha = 256. -. ((state.frame.time -. end_time.at) *. (255. /. 1.5)) |> Float.to_int in
      Raylib.draw_text text (dest.x |> Float.to_int) (dest.y |> Float.to_int) font_size
        (Raylib.Color.create 255 255 255 alpha));

    match game.interaction.floating_text with
    | None -> ()
    | Some (text, end_time) ->
      if String.length text > 160 then
        failwithf "dream nail text is too long: %s" text;

      (* this config works pretty well for text that is one or two lines long *)
      let config : text_config =
        {
          margin_x = 150;
          margin_y = 50;
          margin_y_bottom = Config.window.height * 3 / 4;
          outline_offset_y = 0;
          padding_x = 50;
          padding_y = 50;
          centered = true;
        }
      in

      draw_text_bg_box ~color:(Color.create 0 155 50 100) config;
      List.iteri (display_paragraph config 0) [ text ]
  in

  let show_main_menu menu save_slots =
    Raylib.begin_drawing ();
    (* TODO bg image for main menu *)
    Raylib.clear_background (Color.create 0 0 0 255);
    Raylib.begin_mode_2d state.camera.raylib;
    let w, h = get_scaled_texture_size state.global.textures.main_menu in
    let x' = ((Config.window.width |> Int.to_float) -. w) /. 2. in
    let x, y = (camera_x +. x', camera_y +. 50.) in
    let dest = { pos = { x; y }; w; h } in
    draw_texture state.global.textures.main_menu dest 0;
    maybe_draw_text None (Some (MENU (menu, save_slots)));
    Raylib.draw_fps
      ((camera_x |> Float.to_int) + Config.window.width - 100)
      (camera_y |> Float.to_int);
    Raylib.end_mode_2d ();
    Raylib.end_drawing ();
    state
  in

  match state.game_context with
  | SAVE_FILES (menu, save_slots) -> show_main_menu menu (Some save_slots)
  | MAIN_MENU (menu, save_slots) -> show_main_menu menu None
  | IN_PROGRESS game ->
    let draw_object_trigger_indicators () =
      List.iter
        (fun (sprite : sprite) ->
          if state.debug.enabled then
            debug_rect_outline sprite.dest;
          draw_sprite sprite)
        game.room.pickup_indicators;
      match game.room.interaction_label with
      | None -> ()
      | Some (label, dest) ->
        let pos =
          let w = measure_text label in
          Entity.get_child_pos' dest true (ALIGNED (CENTER, TOP)) (w |> Int.to_float) 10.
        in
        Raylib.draw_text label (pos.x |> Float.to_int) (pos.y |> Float.to_int) font_size
          Color.raywhite
    in

    let draw_projectile (p : projectile) =
      draw_entity p.entity;
      if state.debug.enabled then (
        debug_rect p.entity.dest;
        debug_rect_outline p.entity.sprite.dest)
    in

    let draw_loose_projectiles () = List.iter draw_projectile game.room.loose_projectiles in

    let draw_levers () =
      List.iter
        (fun ((sprite, transformation_bits, trigger) : sprite * int * trigger) ->
          if state.debug.enabled then (
            match sprite.collision with
            | Some (SHAPE shape) -> debug_shape_outline sprite shape
            | _ -> ());
          draw_texture sprite.texture sprite.dest transformation_bits)
        game.room.triggers.levers
    in

    let draw_debug_info () =
      let draw_ghost_debug () =
        let s = game.ghost.entity.sprite in
        if game.ghost.entity.sprite.facing_right then
          Draw.rect
            (s.dest.pos.x +. s.dest.w |> Float.to_int)
            (s.dest.pos.y +. (s.dest.h /. 2.) |> Float.to_int)
            (s.dest.w |> Float.to_int) 3 Color.green
        else
          Draw.rect
            (s.dest.pos.x -. s.dest.w |> Float.to_int)
            (s.dest.pos.y +. (s.dest.h /. 2.) |> Float.to_int)
            (s.dest.w |> Float.to_int) 4 Color.green;
        draw_velocity game.ghost.entity;
        (match game.ghost.entity.current_floor with
        | None -> ()
        | Some (floor, v) -> debug_rect ~r:0 ~g:0 ~b:200 floor);
        (match game.ghost.current.wall with
        | None -> ()
        | Some wall -> debug_rect ~r:150 ~g:0 ~b:150 wall);
        debug_rect_outline ~size:2. ~color:Color.green game.ghost.entity.sprite.dest;
        debug_rect_outline ~size:3. ~color:Color.yellow game.ghost.entity.dest
      in

      List.iter
        (fun (l : layer) ->
          List.iter (fun (tg : tile_group) -> debug_rect_outline tg.dest) l.tile_groups)
        game.room.layers;
      List.iter (fun (_color, rect) -> debug_rect' _color rect) state.debug.rects;
      draw_ghost_debug ()
    in

    let draw_frame_inputs () =
      let button = 40. in
      let padding = 10. in
      let inputs_container =
        {
          x =
            camera_x +. (Config.window.width |> Int.to_float) -. ((8. *. button) +. (10. *. padding));
          y =
            camera_y +. (Config.window.height |> Int.to_float) -. ((2. *. button) +. (3. *. padding));
        }
      in
      let draw_input (frame_input : frame_input) label dest =
        Raylib.draw_text label
          (dest.pos.x +. 5. |> Float.to_int)
          (dest.pos.y +. (button /. 4.) |> Float.to_int)
          12 Color.raywhite;

        debug_rect_outline dest;
        if frame_input.down then
          debug_rect ~r:0 ~g:200 ~b:200 dest
      in
      let draw_input' row_idx idx (label, input) =
        draw_input input label
          {
            pos =
              {
                x =
                  inputs_container.x
                  +. ((idx |> Int.to_float) *. (button +. padding))
                  +. ((button +. padding) /. 2. *. row_idx);
                y = inputs_container.y +. (row_idx *. (button +. padding));
              };
            w = button;
            h = button;
          }
      in
      let draw_input_at label input x y =
        draw_input input label
          {
            pos = { x = inputs_container.x +. x; y = inputs_container.y +. y };
            w = button;
            h = button;
          }
      in
      debug_rect ~r:20 ~g:20 ~b:20 ~a:200
        {
          pos = { x = inputs_container.x -. padding; y = inputs_container.y -. padding };
          w = 1000.;
          h = 1000.;
        };

      draw_input_at "interact" state.frame_inputs.interact
        (((button +. padding) *. 5.) -. (button /. 2.))
        padding;
      draw_input_at "" state.frame_inputs.up ((button +. padding) *. 6.) padding;
      draw_input_at "pause" state.frame_inputs.pause
        (((button +. padding) *. 7.) +. (button /. 2.))
        padding;

      draw_input_at "" state.frame_inputs.left
        ((button +. padding) *. 5.)
        ((2. *. padding) +. button);
      draw_input_at "" state.frame_inputs.down
        ((button +. padding) *. 6.)
        ((2. *. padding) +. button);
      draw_input_at "" state.frame_inputs.right
        ((button +. padding) *. 7.)
        ((2. *. padding) +. button);

      List.iteri (draw_input' 0.)
        [
          ("focus", state.frame_inputs.focus);
          ("c-dash", state.frame_inputs.c_dash);
          ("d-nail", state.frame_inputs.dream_nail);
          ("cast", state.frame_inputs.cast);
        ];
      List.iteri (draw_input' 1.)
        [
          ("jump", state.frame_inputs.jump);
          ("nail", state.frame_inputs.nail);
          ("dash", state.frame_inputs.dash);
        ]
    in

    let get_flashing_tint d = 25 * (((d *. 100. |> Float.to_int) mod 8) + 2) in

    let draw_enemies (enemies : (enemy_id * enemy) list) =
      List.iter
        (fun ((_, e) : enemy_id * enemy) ->
          if state.debug.enabled then (
            debug_rect_outline ~color:Color.purple e.entity.dest;
            debug_rect e.entity.sprite.dest);
          if e.entity.dest.pos.x > -1. && e.entity.dest.pos.y > -1. then (
            let tint =
              let d = state.frame.time -. (Enemy.last_damage e).at in
              (* TODO move to config *)
              if d < 0.25 then (
                let gb = get_flashing_tint d in
                Color.create 200 gb gb 255)
              else
                Color.create 255 255 255 255
            in
            draw_entity ~tint e.entity;
            List.iter draw_projectile e.spawned_projectiles;
            List.iter draw_sprite e.damage_sprites))
        enemies
    in

    let draw_npcs (npcs : npc list) =
      let draw_npc (n : npc) =
        draw_entity n.entity;
        if state.debug.enabled then (
          debug_rect n.entity.dest;
          debug_rect_outline n.entity.sprite.dest)
      in
      List.iter draw_npc npcs
    in

    let draw_ghosts (ghosts_by_id : (ghost_id * party_ghost) list) =
      let draw_ghost ((ghost_id, party_ghost) : ghost_id * party_ghost) =
        draw_entity party_ghost.entity;
        if state.debug.enabled then (
          debug_rect party_ghost.entity.dest;
          debug_rect_outline party_ghost.entity.sprite.dest)
      in
      List.iter draw_ghost ghosts_by_id
    in

    let draw_ghost (ghost : ghost) =
      let tint =
        match Ghost.get_invincibility_kind state game with
        | None -> Color.create 255 255 255 255
        | Some invincibility_kind -> (
          let d = state.frame.time -. game.ghost.history.take_damage.started.at in
          let a = get_flashing_tint d in
          match invincibility_kind with
          | TOOK_DAMAGE -> Color.create 255 255 255 a
          | DIVE_IFRAMES -> Color.create a a a 255
          | SHADE_CLOAK -> Color.create 0 100 200 255)
      in

      let draw_child ((child_kind, child) : ghost_child_kind * ghost_child) =
        let get_child_pos child_w child_h =
          Entity.get_child_pos ghost.entity child.relative_pos child_w child_h
        in
        let draw_child_sprite (sprite : sprite) tint =
          sprite.facing_right <- ghost.entity.sprite.facing_right;
          sprite.dest.pos <- get_child_pos sprite.dest.w sprite.dest.h;
          draw_sprite ~tint sprite
        in
        match child_kind with
        | NAIL slash ->
          if state.debug.enabled then
            debug_rect_outline ~size:2. ~color:Color.purple slash.sprite.dest;
          let tint = ghost.current_weapon.tint in
          draw_child_sprite slash.sprite ghost.current_weapon.tint
        | DREAM_NAIL
        | C_DASH_WHOOSH
        | SHADE_DASH_SPARKLES
        | C_DASH_CHARGE_CRYSTALS
        | C_DASH_WALL_CHARGE_CRYSTALS
        | FOCUS
        | WRAITHS
        | DIVE_COOLDOWN
        | DIVE ->
          if state.debug.enabled then
            debug_rect_outline ~size:2. ~color:Color.purple child.sprite.dest;
          draw_child_sprite child.sprite Color.raywhite
      in
      let draw_vengeful_spirit (p : projectile) =
        if state.debug.enabled then
          debug_rect_outline ~size:2. ~color:Color.red p.entity.dest;
        draw_entity p.entity
      in
      List.iter draw_vengeful_spirit ghost.spawned_vengeful_spirits;
      let shine_sprite : sprite =
        (* TODO maybe have different shine for each area (this one might be too bright for basement or computer-wing) *)
        (* TODO same as nail swings - this sprite should be cached somewhere instead of created every frame *)
        let size = 1200. in
        {
          ident = "shine";
          texture = game.ghost.shared_textures.shine;
          dest =
            {
              pos = Entity.get_child_pos ghost.entity (ALIGNED (CENTER, CENTER)) size size;
              w = size;
              h = size;
            };
          facing_right = false;
          collision = None;
        }
      in
      let children_in_front, children_behind =
        List.partition (fun (_, child) -> child.in_front) ghost.children
      in
      draw_sprite shine_sprite;
      List.iter draw_child children_behind;
      draw_entity ~tint ghost.entity;
      List.iter draw_child children_in_front
    in

    let draw_hud () =
      let padding = 8. in
      let energon_pod_image = game.ghost.shared_textures.energon_pod.image in
      let pod_src_w, pod_src_h =
        ( Raylib.Texture.width energon_pod_image / 2 |> Int.to_float,
          Raylib.Texture.height energon_pod_image |> Int.to_float )
      in
      let pod_dest_w, pod_dest_h =
        (pod_src_w *. Config.scale.soul, pod_src_h *. Config.scale.soul)
      in
      let draw_soul (soul : soul) : unit =
        (* draws the complete empty energon pod, then draws the filled portion on top of that
           TODO the base of the energon pod shouldn't be considered in this height, currently can't see the difference between 0% - 25% soul
        *)
        let full_src_h =
          (soul.current |> Int.to_float) /. (soul.max |> Int.to_float) *. pod_src_h
        in
        let full_dest_h = full_src_h *. Config.scale.soul in
        let empty_src_h = pod_src_h -. full_src_h in
        let empty_dest_h = pod_dest_h -. full_dest_h in
        let full_src = Rectangle.create 0. empty_src_h pod_src_w full_src_h in
        let empty_src = Rectangle.create pod_src_w 0. pod_src_w pod_src_h in
        let empty_dest =
          {
            pos = { x = camera_x +. padding; y = camera_y +. padding };
            h = pod_dest_h;
            w = pod_dest_w;
          }
        in
        let full_dest =
          {
            pos = { x = camera_x +. padding; y = camera_y +. padding +. empty_dest_h };
            h = full_dest_h;
            w = pod_dest_w;
          }
        in
        Draw.image energon_pod_image empty_src (empty_dest |> to_Rect) (Raylib.Vector2.zero ()) 0.0
          Color.raywhite;
        Draw.image energon_pod_image full_src (full_dest |> to_Rect) (Raylib.Vector2.zero ()) 0.0
          Color.raywhite
      in
      let draw_head i idx =
        let image = game.ghost.shared_textures.health.image in
        let w, h =
          ( Raylib.Texture.width image / 2 |> Int.to_float,
            Raylib.Texture.height image |> Int.to_float )
        in
        let dest_w, dest_h = (w *. Config.scale.health, h *. Config.scale.health) in
        let dest =
          Rectangle.create
            (camera_x +. pod_dest_h +. padding +. (idx *. (dest_w +. padding)))
            (camera_y +. 10.) dest_w dest_h
        in
        let src =
          if (idx |> Float.to_int) + 1 > game.ghost.health.current then
            Rectangle.create w 0. w h
          else
            Rectangle.create 0. 0. w h
        in
        Draw.image image src dest (Raylib.Vector2.zero ()) 0.0 Color.raywhite
      in
      draw_soul game.ghost.soul;
      List.iteri draw_head (Utils.rangef game.ghost.health.max)
    in

    Raylib.begin_drawing ();
    (* bright green to make tile seams more obvious *)
    (* Raylib.clear_background (Color.create 0 255 0 255); *)
    Raylib.clear_background game.room.area.bg_color;
    Raylib.begin_mode_2d state.camera.raylib;
    draw_bg_tiles game.room camera_x camera_y state.frame.idx;
    draw_levers ();
    draw_npcs game.room.npcs;
    draw_solid_tiles game.room camera_x camera_y state.frame.idx;
    draw_ghosts game.ghosts';
    draw_ghost game.ghost;
    draw_enemies game.room.enemies;
    draw_floating_platforms game.room camera_x camera_y state.frame.idx;
    draw_object_trigger_indicators ();
    draw_loose_projectiles ();
    draw_fg_tiles game.room camera_x camera_y state.frame.idx;
    draw_hud ();
    (match state.screen_fade with
    | None -> ()
    | Some alpha ->
      (* this is slightly larger than the window to add some padding for when the camera is moving *)
      draw_screen_fade alpha);
    (let interaction_text =
       match state.pause_menu with
       | None -> game.interaction.text
       | Some pause_menu -> Some (MENU (pause_menu, None))
     in
     maybe_draw_text (Some game) interaction_text);
    draw_other_text game;
    Raylib.draw_fps
      ((camera_x |> Float.to_int) + Config.window.width - 100)
      (camera_y |> Float.to_int);
    if state.debug.enabled then
      draw_debug_info ();
    if state.debug.show_frame_inputs then
      draw_frame_inputs ();
    Raylib.end_mode_2d ();
    Raylib.end_drawing ();
    state
