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

(* TODO probably want to rename to be less similar to draw_texture_pro *)
let draw_texture ?(_debug = false) ?(tint = Color.raywhite) (t : texture) (dest : rect) transformation_bits =
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
    (* TODO should probably pass in tile_w/tile_h from room.json, but they are all 24 so this works for now *)
    24. *. Config.scale.room
  in
  let dest' =
    match rotation with
    | 0. -> dest |> to_Rect
    | 90. -> { dest with pos = { dest.pos with x = dest.pos.x +. tile_size } } |> to_Rect
    | 270. -> { dest with pos = { dest.pos with y = dest.pos.y +. tile_size } } |> to_Rect
    | _ -> dest |> to_Rect
  in
  Draw.image t.image (src |> to_Rect) dest' (Raylib.Vector2.create 0. 0.) rotation tint

let debug_rect_outline ?(size = 2.) ?(color = Color.raywhite) (rect : rect) =
  Draw.rect_lines (rect |> to_Rect) size color

let debug_rect ?(r = 0) ?(g = 200) ?(b = 200) ?(a = 100) (rect : rect) =
  Draw.rect (rect.pos.x |> Float.to_int)
    (rect.pos.y -. 2. |> Float.to_int)
    (rect.w |> Float.to_int) (rect.h |> Float.to_int) (Color.create r g b a)

let debug_xy x y = Draw.circle (x |> Float.to_int) (y |> Float.to_int) 4. Color.green
let debug_v (v : Raylib.Vector2.t) = Draw.circle_v v 4. Color.green

(* this function should only be used by sprites that don't have an entity
   - when an entity is present, use draw_entity because it corrects the sprite position before rendering
*)
let draw_sprite ?(_debug = false) ?(tint = Color.create 255 255 255 255) (s : sprite) =
  let dest = s.dest |> to_Rect in
  Draw.image s.texture.image (src_Rect s) dest (Raylib.Vector2.create 0. 0.) 0. tint

let draw_entity ?(_debug = false) ?(tint = Color.create 255 255 255 255) (e : entity) =
  (* drawing entity and drawing sprite are the same thing since sprite.dest is updated every frame to match entity.dest *)
  Entity.adjust_sprite_dest e;
  draw_sprite ~_debug ~tint e.sprite

let center (r : rect) : Raylib.Vector2.t = Raylib.Vector2.create (r.pos.x +. (r.w /. 2.)) (r.pos.y +. (r.h /. 2.))

let draw_velocity (e : entity) : unit =
  let end_x, end_y = (e.sprite.dest.pos.x +. (e.v.x /. 3.), e.sprite.dest.pos.y +. (e.v.y /. 3.)) in
  Draw.line_ex
    (Raylib.Vector2.create e.sprite.dest.pos.x e.sprite.dest.pos.y)
    (Raylib.Vector2.create end_x end_y) 5. Color.purple;
  Draw.circle_v (Raylib.Vector2.create end_x end_y) 5. Color.blue

let draw_tiled_layer state camera_x camera_y ?(tint = Color.create 255 255 255 255) ?(parallax = None) (layer : layer) :
    unit =
  let within_camera dest_x dest_y =
    let camera_min_x, camera_max_x = (camera_x -. 100., camera_x +. (Config.window.width + 100 |> Int.to_float)) in
    let camera_min_y, camera_may_y = (camera_y -. 100., camera_y +. (Config.window.height + 100 |> Int.to_float)) in
    camera_min_x < dest_x && dest_x < camera_max_x && camera_min_y < dest_y && dest_y < camera_may_y
  in
  if not layer.hidden then (
    let draw_stub (sprite, transformation_bits) = draw_texture ~tint sprite.texture sprite.dest transformation_bits in
    let tint' = if layer.config.shaded then Color.black else tint in
    let render_data_tile (idx : int) (gid : int) =
      if gid <> 0 && not (List.mem idx layer.destroyed_tiles) then (
        let x, y =
          Tiled.Room.dest_xy state.room.json ~parallax_opt:parallax layer.json.offset_x layer.json.offset_y idx
            layer.json.w
        in
        if within_camera x y then (
          let texture, transformations = Tiled.Room.look_up_tile state.room.json state.room.cache gid in
          let w, h = Tiled.Room.dest_wh state.room.json () in
          let dest = { pos = { x; y }; w; h } in
          draw_texture ~tint:tint' texture dest transformations))
    in
    let draw_spawned_fragment (f : entity) =
      if state.debug.enabled then
        debug_rect f.dest;
      draw_entity ~tint f
    in
    List.iteri render_data_tile layer.json.data;
    List.iter draw_spawned_fragment layer.spawned_fragments;
    List.iter draw_stub layer.spawned_stub_sprites)

let draw_tiles state camera_x camera_y layers : unit =
  let draw_parallax_layer (layer : layer) =
    let parallax =
      (* TODO maybe just pass in the parallax values and do this calculation somewhere like Tiled.Tile.dest_xy *)
      Some { x = camera_x *. (1. -. layer.json.parallax_x); y = camera_y *. (1. -. layer.json.parallax_y) }
    in
    draw_tiled_layer ~tint:state.room.area.tint ~parallax state camera_x camera_y layer
  in
  List.iter draw_parallax_layer layers

let draw_solid_tiles state camera_x camera_y : unit =
  List.iter
    (draw_tiled_layer state camera_x camera_y)
    (List.filter (fun layer -> layer.config.collides_with_ghost) state.room.layers)

let draw_bg_tiles state camera_x camera_y : unit =
  draw_tiles state camera_x camera_y (List.filter (fun (layer : layer) -> layer.config.render.bg) state.room.layers)

let draw_fg_tiles state camera_x camera_y : unit =
  draw_tiles state camera_x camera_y (List.filter (fun (layer : layer) -> layer.config.render.fg) state.room.layers)

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

type paragraph = line list

let show_segment (line_segment : line_segment) : string =
  if line_segment.color = Raylib.Color.raywhite then
    line_segment.content
  else
    fmt "(color)%s" line_segment.content

let show_line (line : line) : string = String.concat " | " (List.map show_segment line.segments)
let font_size = Config.scale.font_size
let line_height = font_size |> Int.to_float
let measure_text s = Raylib.measure_text s font_size

let get_lines ?(_debug = false) (measure : string -> int) (w : int) (words : string list) : line list =
  let new_segment ?(color = Color.raywhite) content content_w line_w =
    {
      content;
      dest = { pos = { x = line_w |> Int.to_float; y = 0. }; w = content_w |> Int.to_float; h = line_height };
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
        { segment with content = word; dest = { segment.dest with w = segment_w + word_w |> Int.to_float } }
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
        add_word_to_segment (List.length current_line.segments = 0) lines segment word segment_w word_w
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
    "One {{blue}} two {{white}} three four five six seven eight nine {{blue}} ten {{white}} eleven twelve."

  let medium_text = "one two three four five"

  let long_text =
    "Pillows, but no sleep. Feathers, but no birds. Pajamas without children. Violence without purpose. I saw Mommy \
     kissing Exxon Mobil."

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
    print "line segment counts: %s" (List.map (fun line -> List.length line.segments |> Int.to_string) lines |> join);
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

let text_box_width (config : text_config) = Config.window.width - (2 * config.margin_x)

let tick state : state =
  let draw_debug_info () =
    let draw_ghost_debug () =
      let s = state.ghost.entity.sprite in
      if state.ghost.entity.sprite.facing_right then
        Draw.rect
          (s.dest.pos.x +. s.dest.w |> Float.to_int)
          (s.dest.pos.y +. (s.dest.h /. 2.) |> Float.to_int)
          (s.dest.w |> Float.to_int) 3 Color.green
      else
        Draw.rect
          (s.dest.pos.x -. s.dest.w |> Float.to_int)
          (s.dest.pos.y +. (s.dest.h /. 2.) |> Float.to_int)
          (s.dest.w |> Float.to_int) 4 Color.green;
      draw_velocity state.ghost.entity;
      match state.ghost.entity.current_floor with
      | None -> ()
      | Some floor -> debug_rect ~r:0 ~g:0 ~b:200 floor
    in
    let draw_object_layer_debug () =
      let draw_object_layer (layer : json_layer) =
        match layer with
        | `TILE_LAYER _ -> ()
        | `OBJECT_LAYER json ->
          let draw_object (cr : Json_t.coll_rect) =
            debug_rect ~r:100 ~g:200 ~b:200 ~a:50 (Tiled.scale_rect cr.x cr.y cr.w cr.h)
          in
          List.iter draw_object json.objects
      in
      List.iter draw_object_layer state.room.json.layers
    in
    List.iter
      (fun (l : layer) -> List.iter (fun (tg : tile_group) -> debug_rect_outline tg.dest) l.tile_groups)
      state.room.layers;
    List.iter (fun (_color, rect) -> debug_rect rect) state.debug.rects;
    draw_ghost_debug ();
    draw_object_layer_debug ()
  in

  let draw_enemies (enemies : (enemy_id * enemy) list) =
    let draw_projectile (p : projectile) =
      draw_entity p.entity;
      if state.debug.enabled then (
        debug_rect p.entity.dest;
        debug_rect_outline p.entity.sprite.dest)
    in
    (* let draw_damage_sprite (dest, s) =
     *   draw_sprite s
     * in *)
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
              let gb = 25 * (((d *. 100. |> Float.to_int) mod 8) + 2) in
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

  let draw_ghosts (ghosts_by_id : (ghost_id * ghost) list) =
    let draw_ghost ((_, g) : ghost_id * ghost) =
      draw_entity g.entity;
      if state.debug.enabled then (
        debug_rect g.entity.dest;
        debug_rect_outline g.entity.sprite.dest)
    in
    List.iter draw_ghost ghosts_by_id
  in

  let draw_ghost (ghost : ghost) =
    let draw_child () =
      match ghost.child with
      | None -> ()
      | Some child -> (
        let get_child_dest child_w child_h = Entity.get_child_pos ghost.entity child.relative_pos child_w child_h in
        let draw_child_sprite (sprite : sprite) =
          sprite.dest.pos <- get_child_dest sprite.dest.w sprite.dest.h;
          Draw.image sprite.texture.image (src_Rect sprite) (sprite.dest |> to_Rect) (Raylib.Vector2.zero ()) 0.0
            Color.raywhite
        in
        match child.kind with
        | NAIL slash -> draw_child_sprite slash.sprite
        | _ -> ())
    in
    let draw_vengeful_spirit (p : projectile) =
      if state.debug.enabled then
        debug_rect_outline ~size:2. ~color:Color.red p.entity.dest;
      draw_entity p.entity
    in
    List.iter draw_vengeful_spirit ghost.spawned_vengeful_spirits;
    if state.debug.enabled then (
      debug_rect_outline ~size:2. ~color:Color.green ghost.entity.sprite.dest;
      debug_rect_outline ~size:3. ~color:Color.yellow ghost.entity.dest);
    let shine_sprite : sprite =
      (* TODO maybe have different shine for each area (this one might be too bright for basement or computer-wing) *)
      (* TODO same as nail swings - this sprite should be cached somewhere instead of created every frame *)
      let size = 1200. in
      {
        ident = "shine";
        texture = state.ghost.shared_textures.shine;
        dest =
          {
            pos =
              {
                x = ghost.entity.dest.pos.x -. (size /. 2.) +. (Config.ghost.width *. Config.scale.ghost /. 2.);
                y = ghost.entity.dest.pos.y -. (size /. 2.) +. (Config.ghost.height *. Config.scale.ghost /. 2.);
              };
            w = size;
            h = size;
          };
        facing_right = false;
      }
    in
    (* TODO add new child position ALIGN_CENTERS *)
    draw_sprite shine_sprite;
    draw_entity ghost.entity;
    draw_child ()
  in

  let draw_hud camera_x camera_y =
    let padding = 8. in
    let energon_pod_image = state.ghost.shared_textures.energon_pod.image in
    let pod_src_w, pod_src_h =
      ( Raylib.Texture.width energon_pod_image / 2 |> Int.to_float,
        Raylib.Texture.height energon_pod_image |> Int.to_float )
    in
    let pod_dest_w, pod_dest_h = (pod_src_w *. Config.scale.soul, pod_src_h *. Config.scale.soul) in
    let draw_soul (soul : soul) : unit =
      (* draws the complete empty energon pod, then draws the filled portion on top of that
         TODO the base of the energon pod shouldn't be considered in this height, currently can't see the difference between 0% - 25% soul
      *)
      let full_src_h = (soul.current |> Int.to_float) /. (soul.max |> Int.to_float) *. pod_src_h in
      let full_dest_h = full_src_h *. Config.scale.soul in
      let empty_src_h = pod_src_h -. full_src_h in
      let empty_dest_h = pod_dest_h -. full_dest_h in
      let full_src = Rectangle.create 0. empty_src_h pod_src_w full_src_h in
      let empty_src = Rectangle.create pod_src_w 0. pod_src_w pod_src_h in
      let empty_dest = { pos = { x = camera_x +. padding; y = camera_y +. padding }; h = pod_dest_h; w = pod_dest_w } in
      let full_dest =
        { pos = { x = camera_x +. padding; y = camera_y +. padding +. empty_dest_h }; h = full_dest_h; w = pod_dest_w }
      in
      Draw.image energon_pod_image empty_src (empty_dest |> to_Rect) (Raylib.Vector2.zero ()) 0.0 Color.raywhite;
      Draw.image energon_pod_image full_src (full_dest |> to_Rect) (Raylib.Vector2.zero ()) 0.0 Color.raywhite
    in
    let draw_head idx =
      let image = state.ghost.shared_textures.health.image in
      let w, h = (Raylib.Texture.width image / 2 |> Int.to_float, Raylib.Texture.height image |> Int.to_float) in
      let dest_w, dest_h = (w *. Config.scale.health, h *. Config.scale.health) in
      let dest =
        Rectangle.create
          (camera_x +. pod_dest_h +. padding +. (idx *. (dest_w +. padding)))
          (camera_y +. 10.) dest_w dest_h
      in
      let src =
        if (idx |> Float.to_int) + 1 > state.ghost.health.current then
          Rectangle.create w 0. w h
        else
          Rectangle.create 0. 0. w h
      in
      Draw.image image src dest (Raylib.Vector2.zero ()) 0.0 Color.raywhite
    in
    draw_soul state.ghost.soul;
    List.iter draw_head (Utils.rangef state.ghost.health.max)
  in

  let draw_screen_fade camera_x camera_y =
    Raylib.draw_rectangle
      (camera_x -. 5. |> Float.to_int)
      (camera_y -. 5. |> Float.to_int)
      (Config.window.width + 10) (Config.window.height + 10) (Color.create 0 0 0 160)
  in

  let draw_interaction_text camera_x camera_y =
    let draw_text_bg_box (config : text_config) =
      let w = Config.window.width - (2 * config.margin_x) in
      let h = Config.window.height - (config.margin_y + config.margin_y_bottom) in
      Draw.rect
        ((camera_x |> Float.to_int) + config.margin_x)
        ((camera_y |> Float.to_int) + config.margin_y)
        w h (Color.create 0 0 0 200)
    in

    let display_line (config : text_config) y_offset line_number (line : line) : unit =
      let display_segment (segment : line_segment) =
        let centered_x = if config.centered then (text_box_width config - line.w) / 2 else config.padding_x in
        let line_spacing = line_height *. (line_number |> Int.to_float) in
        let dest_y = line_spacing +. camera_y +. y_offset in
        Raylib.draw_text segment.content
          ((segment.dest.pos.x +. camera_x |> Float.to_int) + config.margin_x + centered_x)
          ((dest_y |> Float.to_int) + (line_number * font_size))
          font_size segment.color
      in
      List.iter display_segment line.segments
    in

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

    let display_paragraph (config : text_config) y_offset paragraph_idx (paragraph : string) : unit =
      let paragraph_offset = paragraph_idx * Config.scale.paragraph_spacing in
      let y_offset' = paragraph_offset + config.margin_y + config.padding_y + y_offset |> Int.to_float in
      let lines =
        let w = text_box_width config - (2 * config.padding_x) in
        get_lines measure_text w (String.split_on_char ' ' paragraph)
      in
      List.iteri (display_line config y_offset') lines
    in

    let get_text_box_width margin_x = Config.window.width - (2 * margin_x) in

    match state.interaction.text with
    | None -> ()
    | Some (ABILITY ability_text) ->
      let margin_x = 50 in
      let config : text_config =
        {
          text_box_width = get_text_box_width margin_x;
          margin_x;
          margin_y = 20;
          margin_y_bottom = 20;
          outline_offset_y = Config.window.height / 4;
          padding_x = 50;
          padding_y = 50;
          centered = true;
        }
      in

      draw_screen_fade camera_x camera_y;
      draw_text_bg_box config;
      draw_outline ~offset_y:config.outline_offset_y ability_text;

      List.iteri (display_paragraph config (config.outline_offset_y + 100)) ability_text.bottom_paragraphs
    | Some (FOCUS_ABILITY ability_text) ->
      let margin_x = 50 in
      let config : text_config =
        {
          text_box_width = get_text_box_width margin_x;
          margin_x;
          margin_y = 20;
          margin_y_bottom = 20;
          outline_offset_y = 0;
          padding_x = 50;
          padding_y = 50;
          centered = true;
        }
      in

      draw_screen_fade camera_x camera_y;
      draw_text_bg_box config;
      draw_outline ability_text;

      (* lots of hardcoded stuff in here, but I'm not sure if this will be used besides the focus-info text *)
      let top_y_offset, bottom_y_offset = (0, 400) in
      List.iteri (display_paragraph config top_y_offset) ability_text.top_paragraphs;
      List.iteri (display_paragraph config bottom_y_offset) ability_text.bottom_paragraphs
    | Some (DIALOGUE (speaker_name, text')) ->
      let margin_x = 150 in
      let config : text_config =
        {
          text_box_width = get_text_box_width margin_x;
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
          | "Chang"
          | "Hickey"
          | "Duncan" ->
            "{{maroon}}"
          | _ -> failwithf "unknown speaker: %s" speaker_name
        in
        display_paragraph config 0 0 (fmt "%s %s:  {{white}} %s" color_str speaker_name hd))
    | Some (PLAIN text') ->
      let margin_x = 150 in
      let config : text_config =
        {
          text_box_width = get_text_box_width margin_x;
          margin_x;
          margin_y = 50;
          margin_y_bottom = 350;
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
          config.margin_y_bottom - config.padding_y - (line_height |> Float.to_int)
        in
        List.iteri (display_paragraph config 0) text'.content;
        List.iteri
          (display_paragraph { config with margin_y = increase_health_text_margin_y } 0)
          [ "{{green}} max health increased by one" ]
        (* List.iteri (display_paragraph config 0) (text'.content @ [""; "{{green}} max health increased by one"])) *))
      else
        List.iteri (display_paragraph config 0) text'.content
  in

  let camera_x, camera_y =
    ( Raylib.Vector2.x (Raylib.Camera2D.target state.camera) -. Config.window.center_x,
      Raylib.Vector2.y (Raylib.Camera2D.target state.camera) -. Config.window.center_y )
  in
  Raylib.begin_drawing ();
  (* bright green to make tile seams more obvious *)
  (* Raylib.clear_background (Color.create 0 255 0 255); *)
  Raylib.clear_background state.room.area.bg_color;
  Raylib.begin_mode_2d state.camera;
  draw_bg_tiles state camera_x camera_y;
  draw_solid_tiles state camera_x camera_y;
  draw_npcs state.room.npcs;
  draw_ghosts state.ghosts;
  draw_ghost state.ghost;
  draw_enemies state.room.enemies;
  draw_fg_tiles state camera_x camera_y;
  draw_hud camera_x camera_y;
  if state.screen_faded then
    (* this is slightly larger than the window to add some padding for when the camera is moving *)
    draw_screen_fade camera_x camera_y;
  draw_interaction_text camera_x camera_y;
  Raylib.draw_fps ((camera_x |> Float.to_int) + Config.window.width - 100) (camera_y |> Float.to_int);
  if state.debug.enabled then
    draw_debug_info ();
  Raylib.end_mode_2d ();
  Raylib.end_drawing ();
  state
