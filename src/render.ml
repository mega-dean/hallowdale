open Utils
open Types
module Color = Raylib.Color
module Rectangle = Raylib.Rectangle

type text_config = Interaction.text_config

let font_size = Config.scale.font_size
let line_height ?(font_scale = 1.) () = font_size *. font_scale

let measure_text ?(font_scale = 1.) s =
  Raylib.measure_text s (font_size *. font_scale |> Float.to_int)

module Draw = struct
  open Raylib

  let circle = draw_circle
  let circle_v = draw_circle_v
  let rect_lines = draw_rectangle_lines_ex

  let rect (r : rect) (color : color) =
    draw_rectangle_pro (r |> rect_to_Rect) Zero.raylib_vector 0. color

  (* TODO rename this *)
  let image = draw_texture_pro
  let line_ex = draw_line_ex

  let text ?(color = Color.white) ?(font_scale = 1.) (content : string) (pos : vector) =
    draw_text_ex (get_font_default ()) content
      (Raylib.Vector2.create pos.x pos.y)
      (font_size *. font_scale) Config.text.spacing color
end

let debug_shape_outline
    ?(size = 1.)
    ?(color = Color.raywhite)
    (sprite : sprite option)
    (shape : shape) =
  let adjusted_shape : shape =
    match sprite with
    | Some sprite -> align_shape_with_parent_sprite sprite shape
    | None -> shape
  in
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
  List.iteri draw_edge points

let debug_rect_outline ?(size = 2.) ?(color = Color.raywhite) (rect : rect) =
  Draw.rect_lines (rect |> rect_to_Rect) size color

let debug_rect' color (rect : rect) =
  let transparent_color = Raylib.Color.(create (r color) (g color) (b color)) 100 in
  Draw.rect rect transparent_color

let debug_rect ?(r = 0) ?(g = 200) ?(b = 200) ?(a = 100) (rect : rect) =
  debug_rect' (Color.create r g b a) rect

let debug_xy x y = Draw.circle (x |> Float.to_int) (y |> Float.to_int) 4. Color.green
let debug_pos (pos : vector) = debug_xy pos.x pos.y

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
  let dest' =
    match rotation with
    | 0. -> dest |> rect_to_Rect
    | 90. ->
      { dest with pos = { dest.pos with x = dest.pos.x +. Config.window.dest_tile_size } }
      |> rect_to_Rect
    | 270. ->
      { dest with pos = { dest.pos with y = dest.pos.y +. Config.window.dest_tile_size } }
      |> rect_to_Rect
    | _ -> dest |> rect_to_Rect
  in
  if debug then
    debug_rect_outline dest;
  Draw.image t.image (src |> rect_to_Rect) dest' (Raylib.Vector2.create 0. 0.) rotation tint

(* this function should only be used by sprites that don't have an entity
   - when an entity is present, use draw_entity because it corrects the sprite position before rendering
*)
let draw_sprite ?(debug = false) ?(tint = Color.white) ?(render_offset = None) (sprite : sprite) =
  let dest =
    match render_offset with
    | None -> sprite.dest
    | Some offset ->
      {
        sprite.dest with
        pos =
          {
            x =
              (if sprite.facing_right then
                 sprite.dest.pos.x +. offset.x
              else
                sprite.dest.pos.x -. offset.x);
            y = offset.y +. sprite.dest.pos.y;
          };
      }
  in
  if debug then
    debug_rect sprite.dest;
  draw_texture sprite.texture ~tint dest (if sprite.facing_right then 0 else 4);
  match sprite.collision with
  | Some (SHAPE shape) ->
    if debug then
      debug_shape_outline (Some sprite) shape
  | _ -> ()

let draw_entity ?(debug = false) ?(tint = Color.white) (entity : entity) =
  Entity.adjust_sprite_dest entity;
  draw_sprite ~debug ~tint entity.sprite

let draw_ghost_head ~tint ghost =
  let head_dest =
    {
      pos =
        {
          x = ghost.entity.dest.pos.x +. Config.ghost.entity_neck_x -. Config.ghost.neck_x;
          y = ghost.entity.dest.pos.y +. Config.ghost.entity_neck_y -. Config.ghost.neck_y;
        };
      w = Config.ghost.head_w;
      h = Config.ghost.head_h;
    }
  in
  draw_texture ~tint ghost.head head_dest (if ghost.entity.sprite.facing_right then 0 else 4)

let center (r : rect) : Raylib.Vector2.t = Raylib.Vector2.create (rect_center_x r) (rect_center_y r)

let draw_tiled_layer
    ?(debug = false)
    (room : room)
    camera_x
    camera_y
    state
    ?(tint = Color.create 255 255 255 255)
    ?(parallax = None)
    (layer : layer) : unit =
  let w, h = Tiled.JsonRoom.dest_wh room.json () in
  if not layer.hidden then (
    let draw_stub (sprite, transformation_bits) =
      draw_texture ~tint sprite.texture sprite.dest transformation_bits
    in
    let tint' = if layer.config.shaded then Color.black else tint in
    let render_tile (idx : int) (gid : int) =
      if gid <> 0 && not (List.mem idx layer.destroyed_tiles) then (
        let x, y =
          Tiled.JsonRoom.dest_xy ~parallax layer.json.offset_x layer.json.offset_y idx layer.json.w
        in
        let texture, transformations =
          let animation_offset =
            if layer.config.animated then (
              (* TODO this should probably just use an animated sprite instead of
                 this weird animation_offset for the tile gid
              *)
              let speed =
                match layer.name with
                | "fast-conveyor-belt" -> 40
                | "slow-conveyor-belt" -> 20
                | _ -> 4
              in
              speed * state.frame.idx / Config.window.fps mod 8)
            else
              0
          in
          Tiled.JsonRoom.look_up_tile ~animation_offset room.json room.cache gid
        in
        let dest = { pos = { x; y }; w; h } in
        draw_texture ~tint:tint' texture dest transformations)
    in
    let draw_spawned_fragment (f : entity) =
      if debug then
        debug_rect f.dest;
      draw_entity ~tint f
    in

    let mx, my =
      let pos =
        match parallax with
        | None -> { x = camera_x; y = camera_y }
        | Some p -> { x = camera_x -. p.x; y = camera_y -. p.y }
      in
      pos |> Tiled.Tile.pos_to_dest_coords
    in

    let layer_data_in_camera : int array array =
      (* Config.window.w/h_tiles are the exact window size, so  *)
      match Matrix.sub layer.data mx my (Config.window.w_tiles + 1) (Config.window.h_tiles + 1) with
      | Ok m -> m
      | Error msg -> failwithf "layer %s: %s" layer.name msg
    in
    let render_data_tile col_idx (rows : int array) =
      let render row_idx (gid : int) =
        let idx =
          (row_idx + mx, col_idx + my) |> Tiled.Tile.coords_to_idx ~width:room.json.w_in_tiles
        in
        render_tile idx gid
      in
      Array.iteri render rows
    in
    Array.iteri render_data_tile layer_data_in_camera;
    List.iter draw_spawned_fragment layer.spawned_fragments;
    List.iter draw_stub layer.spawned_stub_sprites)

let draw_tiles room camera_x camera_y state layers : unit =
  let draw_parallax_layer (layer : layer) =
    let parallax =
      (* TODO maybe just pass in the parallax values and do this calculation somewhere like Tiled.Tile.dest_xy *)
      Some
        {
          x = camera_x *. (1. -. layer.json.parallax_x);
          y = camera_y *. (1. -. layer.json.parallax_y);
        }
    in
    draw_tiled_layer ~tint:room.area.tint ~parallax room camera_x camera_y state layer
  in
  List.iter draw_parallax_layer layers

let draw_solid_tiles room camera_x camera_y state : unit =
  List.iter
    (draw_tiled_layer room camera_x camera_y state)
    (List.filter (fun layer -> layer.config.collides_with_ghost || layer.config.hazard) room.layers)

let draw_bg_tiles room camera_x camera_y state : unit =
  draw_tiles room camera_x camera_y state
    (List.filter (fun (layer : layer) -> layer.config.render.bg) room.layers)

let draw_fg_tiles room camera_x camera_y state : unit =
  draw_tiles room camera_x camera_y state
    (List.filter (fun (layer : layer) -> layer.config.render.fg) room.layers)

let draw_floating_platforms (room : room) : unit =
  List.iter
    (fun (platform : platform) -> draw_sprite ~tint:room.area.tint platform.sprite)
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
  mutable w : float;
}

let show_segment (line_segment : line_segment) : string =
  if line_segment.color = Raylib.Color.raywhite then
    line_segment.content
  else
    fmt "(color)%s" line_segment.content

let show_line (line : line) : string = String.concat " | " (List.map show_segment line.segments)

(* TODO this works ok for two kinds of interaction text input:
   - one single long line (that gets wrapped by this fn)
   - several short/non-wrapping lines

   using several long lines sets the vertical spacing incorrectly so lines overlap each other
*)
let get_lines ?(_debug = false) ?(font_scale = 1.) (w : float) (words : string list) : line list =
  let measure s : float = measure_text ~font_scale s |> Int.to_float in
  let new_segment ?(color = Color.raywhite) content content_w line_w =
    {
      content;
      dest = { pos = { x = line_w; y = 0. }; w = content_w; h = line_height ~font_scale () };
      color;
    }
  in
  let empty_line () = { segments = []; w = 0. } in

  let add_segment_to_last_line lines (segment : line_segment) : line list =
    let current_line = List.last lines in
    current_line.w <- current_line.w +. measure segment.content;
    (* segments are appended in the correct order, so they never need to be reversed *)
    current_line.segments <- current_line.segments @ [ segment ];
    lines
  in

  let start_new_line lines (segment : line_segment) : line list =
    add_segment_to_last_line lines segment @ [ empty_line () ]
  in

  let add_word_to_segment
      (first_segment_in_line : bool)
      lines
      segment
      word
      (segment_w : float)
      (word_w : float) =
    let segments =
      if segment.content = "" && first_segment_in_line then
        { segment with content = word; dest = { segment.dest with w = segment_w +. word_w } }
      else (
        let content = fmt "%s %s" segment.content word in
        { segment with content; dest = { segment.dest with w = measure content } })
    in
    (lines, segments)
  in

  let add_word lines segment word : line list * line_segment =
    let word_w = measure (fmt "%s " word) in
    let segment_w = measure segment.content in
    let current_line = List.last lines in
    let change_color color =
      let start_content =
        (* this is needed to adjust spacing between colored words because Raylib.measure_text does
           not return "expected" ratios for different font sizes
           eg. `measure_text str 18` /. `measure_text str 24` is less than 0.75
           - this is probably because the default font is intended just for debugging, and real games
             use fonts
        *)
        if 12. < font_size && font_size <= 14. then
          " "
        else if 14. < font_size && font_size <= 16. then
          "  "
        else if 16. < font_size && font_size <= 19. then
          "   "
        else
          ""
      in
      ( add_segment_to_last_line lines segment,
        new_segment ~color start_content 0. (segment_w +. current_line.w) )
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
    | "{{darkpink}}" -> change_color (Raylib.Color.create 146 24 118 255)
    | "{{gray}}" -> change_color Raylib.Color.gray
    | _ ->
      (* subtracting archives_text_padding fixes text in archives *)
      if word_w +. segment_w +. current_line.w > w -. Config.text.archives_extra_text_padding then
        (start_new_line lines segment, new_segment ~color:segment.color word word_w 0.)
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
  let first_line : line = { segments = []; w = 0. } in
  iter [ first_line ] (new_segment "" 0. 0.) words

let tick (state : state) =
  let camera_x, camera_y =
    (* the Raylib coordinates are the center of the screen, so this is the top-left corner of
       the screen
    *)
    ( Raylib.Vector2.x (Raylib.Camera2D.target state.camera.raylib) -. Config.window.center.x,
      Raylib.Vector2.y (Raylib.Camera2D.target state.camera.raylib) -. Config.window.center.y )
  in
  let text_box_width (config : text_config) = Config.window.w -. (2. *. config.margin_x) in

  let draw_screen_fade alpha =
    (* this is slightly larger than the window to add some padding for when the camera is moving *)
    Draw.rect
      {
        pos = { x = camera_x -. 5.; y = camera_y -. 5. };
        w = Config.window.w +. 10.;
        h = Config.window.h +. 10.;
      }
      (Color.create 0 0 0 (Int.bound 0 alpha 255))
  in

  let draw_text_bg_box ?(color = Color.create 0 0 0 200) (config : text_config) =
    let w = Config.window.w -. (2. *. config.margin_x) in
    let h = Config.window.h -. (config.margin_y_top +. config.margin_y_bottom) in
    let dest =
      { pos = { x = camera_x +. config.margin_x; y = camera_y +. config.margin_y_top }; w; h }
    in
    if state.debug.enabled then
      debug_rect_outline dest;
    Draw.rect dest color
  in

  let draw_cursor (config : text_config) (choice_idx : int) =
    let choice_offset = (choice_idx |> Int.to_float) *. Config.scale.paragraph_spacing in
    let left_x = camera_x +. config.margin_x +. config.cursor_padding in
    let right_x = camera_x +. Config.window.w -. config.margin_x -. config.cursor_padding in
    let y = camera_y +. config.margin_y_top +. config.padding.y +. choice_offset in
    Draw.text "*" { x = left_x; y };
    Draw.text "*" { x = right_x; y }
  in

  let display_paragraph
      ?(force_spaces = false)
      ?(y_offset = 0.)
      ?(font_scale = 1.)
      (config : text_config)
      (paragraph_idx : int)
      (paragraph : string) =
    let is_steel_sole, word_separator =
      match state.context with
      | IN_PROGRESS game -> (
        let sep =
          if Room.in_teachers_archives game.room && not force_spaces then
            '-'
          else
            ' '
        in
        match game.mode with
        | CLASSIC
        | DEMO ->
          (false, sep)
        | STEEL_SOLE -> (true, sep))
      | MAIN_MENU (_, _)
      | RETURN_TO_MAIN_MENU _
      | SAVE_FILES (_, _)
      | RELOAD_LAST_SAVED_GAME _ ->
        (false, ' ')
    in
    let display_line (config : text_config) y_offset line_idx (line : line) =
      let display_segment (segment : line_segment) =
        let padding_x =
          if config.centered then
            (text_box_width config -. line.w) /. 2.
          else
            config.padding.x
        in
        let line_spacing = line_height ~font_scale () *. (line_idx |> Int.to_float) in
        let dest_y = line_spacing +. camera_y +. y_offset in
        let content =
          match word_separator with
          | ' ' -> segment.content
          | '-' ->
            let c =
              Str.global_replace (Str.regexp " ") (String.make 1 word_separator) segment.content
            in
            fmt "-%s-" c
          | _ -> failwithf "unknown word_separator: %c" word_separator
        in
        let color = if is_steel_sole then Raylib.Color.purple else segment.color in

        let pos =
          {
            x = segment.dest.pos.x +. camera_x +. config.margin_x +. padding_x;
            y = dest_y +. ((line_idx |> Int.to_float) *. font_size *. font_scale);
          }
        in

        Draw.text ~font_scale ~color content pos
      in
      List.iter display_segment line.segments
    in

    let paragraph_offset = (paragraph_idx |> Int.to_float) *. Config.scale.paragraph_spacing in
    let y_offset' = paragraph_offset +. config.margin_y_top +. config.padding.y +. y_offset in
    let lines =
      let w = text_box_width config -. (2. *. config.padding.x) in
      get_lines ~font_scale w (String.split_on_char word_separator paragraph)
    in
    List.iteri (display_line config y_offset') lines
  in

  let maybe_draw_text (game_opt : game option) (interaction_text : Interaction.text_kind option) =
    (* TODO add offset_x based on ghost.id, and make ability-outlines.png a grid of images *)
    let draw_outline ?(offset_y = 0.) (ability_text : Interaction.ability_text) =
      let texture =
        let animation_src = STILL ability_text.outline_src in
        { state.global.textures.ability_outlines with animation_src }
      in
      let w = ability_text.outline_src.w *. Config.scale.room in
      let h = ability_text.outline_src.h *. Config.scale.room in
      let x = camera_x +. Config.window.center.x -. (w /. 2.) in
      let y = camera_y +. Config.window.center.y -. (h /. 2.) -. offset_y in
      let dest = { pos = { x; y }; w; h } in
      draw_texture texture dest 0
    in
    let base_config = Config.text.base_config in
    let ability_config = { base_config with centered = true } in

    match interaction_text with
    | None -> ()
    | Some (ABILITY ability_text) ->
      draw_text_bg_box ability_config;
      draw_outline ~offset_y:Config.text.outline_offset_y ability_text;

      List.iteri
        (display_paragraph
           ~y_offset:(Config.text.outline_offset_y +. Config.text.outline_h)
           ability_config)
        ability_text.bottom_paragraphs
    | Some (FOCUS_ABILITY ability_text) ->
      draw_text_bg_box ability_config;
      draw_outline ~offset_y:Config.text.focus_outline_offset_y ability_text;

      List.iteri (display_paragraph ability_config) ability_text.top_paragraphs;
      List.iteri
        (display_paragraph ability_config ~y_offset:Config.text.focus_outline_bottom_offset_y)
        ability_text.bottom_paragraphs
    | Some (DIALOGUE (speaker_name, text')) ->
      (* TODO when one character says several lines in a row, it would be nice
         to not "close" the text box in between each one *)
      let config : text_config = Config.text.dialogue_config in
      draw_text_bg_box config;
      let color_str =
        match speaker_name with
        | "Britta" -> "{{gold}}"
        | "Annie" -> "{{darkpurple}}"
        | "Jeff" -> "{{darkblue}}"
        | "Shirley" -> "{{darkpink}}"
        | "Troy"
        | "Troy and Abed" ->
          "{{darkgreen}}"
        | "Abed" -> "{{orange}}"
        | "Neil"
        | "Annie's Boobs"
        | "Jerry"
        | "Professor Holly"
        | "Hilda"
        | "Frankie"
        | "Vicki"
        | "Leonard"
        | "Garrett" ->
          "{{purple}}"
        | "Chang"
        | "Dean"
        | "Buddy"
        | "Luis Guzman"
        | "Borchert"
        | "Hickey"
        | "Duncan" ->
          "{{maroon}}"
        | _ -> failwithf "unknown speaker: %s" speaker_name
      in
      let force_spaces =
        match game_opt with
        | Some game -> (
          match game.interaction.use_dashes_in_archives with
          | Some false -> true
          | _ -> false)
        | None -> false
      in
      display_paragraph ~force_spaces config 0
        (fmt "%s %s:  {{white}} %s" color_str speaker_name text')
    | Some (PLAIN (centered, lines)) ->
      let margin_y_bottom =
        let tall_text =
          (* TODO it's worth cleaning this up if there are any long dialogs needed besides the ACB note *)
          String.length (List.last lines) > 700
        in
        if tall_text then
          Config.text.tall_margin_y_bottom
        else
          Config.text.short_margin_y_bottom
      in
      let config : text_config = { (Config.get_plain_text_config margin_y_bottom) with centered } in
      draw_text_bg_box config;
      let force_spaces =
        match game_opt with
        | Some game -> (
          match game.interaction.use_dashes_in_archives with
          | Some false -> true
          | _ -> false)
        | None -> false
      in
      List.iteri (display_paragraph ~force_spaces config) lines
    | Some (MENU (menu, save_slots')) ->
      let config' : text_config = Config.get_menu_text_config (List.nth menu.choices 0) in
      let menu_choices, config =
        match save_slots' with
        | None -> (List.map (Show.menu_choice state game_opt) menu.choices, config')
        | Some save_slots ->
          ( List.map
              (Show.menu_choice ~save_slots state game_opt)
              (Menu.get_save_file_choices save_slots),
            { config' with centered = false } )
      in
      draw_text_bg_box config;
      List.iteri (display_paragraph ~force_spaces:true config) menu_choices;
      draw_cursor config menu.current_choice_idx
  in

  let draw_other_text game =
    (match game.interaction.corner_text with
    | None -> ()
    | Some tt ->
      let dest = { x = camera_x; y = camera_y +. (Config.window.h -. font_size) } in
      let alpha =
        match tt.visible with
        | UNTIL_UNSET
        | PAUSE_MENU_OPEN ->
          255
        | UNTIL (duration, end_time) ->
          255. *. ((end_time.at -. state.frame.time) /. duration) |> Float.to_int
      in
      Draw.text ~color:(Color.create 255 255 255 alpha) tt.content dest);

    match game.interaction.floating_text with
    | None -> ()
    | Some text ->
      (* this config works pretty well for text that is one or two lines long *)
      let config : text_config = Config.text.floating_config in
      draw_text_bg_box ~color:(Color.create 0 0 0 100) config;
      display_paragraph ~force_spaces:true ~font_scale:text.scale config ~y_offset:0. 0 text.content
  in

  let show_main_menu menu save_slots =
    Raylib.begin_drawing ();
    (* TODO bg image for main menu *)
    Raylib.clear_background (Color.create 0 0 0 255);
    Raylib.begin_mode_2d state.camera.raylib;
    let w, h = get_scaled_texture_size Config.scale.room state.global.textures.main_menu in
    let x' = (Config.window.w -. w) /. 2. in
    let x, y = (camera_x +. x', camera_y +. Config.other.main_menu_y_offset) in
    let dest = { pos = { x; y }; w; h } in
    draw_texture state.global.textures.main_menu dest 0;
    maybe_draw_text None (Some (MENU (menu, save_slots)));
    if Env.development then
      Raylib.draw_fps
        (camera_x +. Config.window.w -. 100. |> Float.to_int)
        (camera_y |> Float.to_int);
    Raylib.end_mode_2d ();
    Raylib.end_drawing ();
    state
  in

  match state.context with
  | SAVE_FILES (menu, save_slots) -> show_main_menu menu (Some save_slots)
  | MAIN_MENU (menu, _save_slots) -> show_main_menu menu None
  | RETURN_TO_MAIN_MENU _
  | RELOAD_LAST_SAVED_GAME _ ->
    state
  | IN_PROGRESS game ->
    let draw_skybox tint =
      (* TODO could add some parallax scrolling here, but this won't be visible very often *)
      let src = get_src state.global.textures.skybox in
      let dest = { pos = { x = camera_x; y = camera_y }; w = src.w; h = src.h } in
      draw_texture ~tint state.global.textures.skybox dest 0
    in
    let draw_world_map world_map =
      let dest =
        { pos = { x = camera_x; y = camera_y }; w = Config.window.w; h = Config.window.h }
      in
      let draw_black_rect rect =
        Draw.rect
          { rect with pos = { x = rect.pos.x +. camera_x; y = rect.pos.y +. camera_y } }
          Raylib.Color.black
      in
      let draw_ghost_circle radius r g b speed =
        let ghost_color r g b a = Raylib.Color.create r g b a in
        Raylib.draw_circle
          (world_map.ghost_pos.x +. camera_x |> Float.to_int)
          (world_map.ghost_pos.y +. camera_y |> Float.to_int)
          radius (ghost_color r g b speed)
      in
      let alpha =
        let n = Config.window.fps * 3 in
        let m = state.frame.idx * 5 mod n in
        if m > n / 2 then
          -m + n
        else
          m
      in

      draw_texture state.global.textures.world_map dest 0;
      List.iter draw_black_rect world_map.black_rects;
      if Room.show_ghost_on_map game.room then (
        draw_ghost_circle 7. 0 0 0 alpha;
        draw_ghost_circle 5. 255 255 255 alpha)
    in

    let draw_object_trigger_indicators () =
      match game.room.interaction_label with
      | None -> ()
      | Some (label, dest) ->
        let w = measure_text label in
        let pos = align (CENTER, TOP_INSIDE) dest (w |> Int.to_float) (line_height ()) in
        Raylib.draw_text label (pos.x |> Float.to_int) (pos.y |> Float.to_int)
          (font_size |> Float.to_int) Color.raywhite
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
        (fun lever ->
          draw_texture lever.sprite.texture lever.sprite.dest lever.transformation;
          if state.debug.enabled then (
            match lever.sprite.collision with
            | Some (SHAPE shape) -> debug_shape_outline (Some lever.sprite) shape
            | _ -> ()))
        game.room.triggers.levers
    in

    let draw_debug_info () =
      let draw_debug_ghost () =
        let draw_velocity (e : entity) : unit =
          let end_x, end_y =
            (e.sprite.dest.pos.x +. (e.v.x /. 3.), e.sprite.dest.pos.y +. (e.v.y /. 3.))
          in
          Draw.line_ex
            (Raylib.Vector2.create e.sprite.dest.pos.x e.sprite.dest.pos.y)
            (Raylib.Vector2.create end_x end_y)
            5. Color.purple;
          Draw.circle_v (Raylib.Vector2.create end_x end_y) 5. Color.blue
        in
        let s = game.player.ghost.entity.sprite in
        if game.player.ghost.entity.sprite.facing_right then
          Draw.rect
            {
              pos = { x = s.dest.pos.x +. s.dest.w; y = s.dest.pos.y +. (s.dest.h /. 2.) };
              w = s.dest.w;
              h = 3.;
            }
            Color.green
        else
          Draw.rect
            {
              pos = { x = s.dest.pos.x -. s.dest.w; y = s.dest.pos.y +. (s.dest.h /. 2.) };
              w = s.dest.w;
              h = 3.;
            }
            Color.green;
        draw_velocity game.player.ghost.entity;
        (match game.player.ghost.entity.current_floor with
        | None -> ()
        | Some (floor, _v) -> debug_rect ~r:0 ~g:0 ~b:200 floor);
        (match game.player.current.wall with
        | None -> ()
        | Some wall -> debug_rect ~r:150 ~g:0 ~b:150 wall);
        debug_rect_outline ~size:2. ~color:Color.green game.player.ghost.entity.sprite.dest;
        debug_rect_outline ~size:3. ~color:Color.yellow game.player.ghost.entity.dest
      in

      List.iter
        (fun (l : layer) ->
          List.iter (fun (tg : tile_group) -> debug_rect_outline tg.dest) l.tile_groups)
        game.room.layers;
      List.iter (fun (_color, rect) -> debug_rect' _color rect) state.debug.rects;
      draw_debug_ghost ()
    in

    let draw_frame_inputs () =
      (* TODO window scale *)
      let button = 40. in
      let padding = 10. in
      let inputs_container =
        {
          x = camera_x +. Config.window.w -. ((8. *. button) +. (10. *. padding));
          y = camera_y +. Config.window.h -. ((2. *. button) +. (3. *. padding));
        }
      in
      let draw_input' (frame_input : frame_input) label dest =
        Raylib.draw_text label
          (dest.pos.x +. 5. |> Float.to_int)
          (dest.pos.y +. (button /. 4.) |> Float.to_int)
          12 Color.raywhite;

        debug_rect_outline dest;
        if frame_input.down then
          debug_rect ~r:0 ~g:200 ~b:200 dest
      in
      let draw_input row_idx idx (label, input) =
        draw_input' input label
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
        draw_input' input label
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

      List.iteri (draw_input 0.)
        [
          ("focus", state.frame_inputs.focus);
          ("c-dash", state.frame_inputs.c_dash);
          ("d-nail", state.frame_inputs.dream_nail);
          ("cast", state.frame_inputs.cast);
        ];
      List.iteri (draw_input 1.)
        [
          ("jump", state.frame_inputs.jump);
          ("nail", state.frame_inputs.nail);
          ("dash", state.frame_inputs.dash);
        ]
    in

    let get_flashing_tint d = 25 * (((d *. 100. |> Float.to_int) mod 8) + 2) in

    let draw_enemies (enemies : enemy list) =
      let projectiles_on_top = ref [] in
      List.iter
        (fun (enemy : enemy) ->
          if state.debug.enabled then (
            List.iter
              (fun shape ->
                debug_shape_outline ~size:5. ~color:Color.purple (Some enemy.entity.sprite) shape)
              enemy.collision_shapes;
            debug_rect_outline ~color:Color.purple enemy.entity.dest;
            debug_rect enemy.entity.sprite.dest);
          if enemy.entity.dest.pos.x > -1. && enemy.entity.dest.pos.y > -1. then (
            let tint =
              let d = state.frame.time -. (Enemy.last_damage enemy).at in
              (* TODO move to config *)
              if d < 0.25 then (
                let gb = get_flashing_tint d in
                Color.create 200 gb gb 255)
              else
                Color.white
            in
            draw_entity ~tint enemy.entity;
            let maybe_draw projectile =
              if projectile.draw_on_top then
                projectiles_on_top := [ projectile ] @ !projectiles_on_top
              else
                draw_projectile projectile
            in
            List.iter maybe_draw enemy.projectiles;
            List.iter draw_sprite enemy.damage_sprites))
        enemies;
      !projectiles_on_top
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

    let draw_party_ghosts (ghosts_by_id : party_ghost list) =
      let draw_party_ghost (party_ghost : party_ghost) =
        draw_entity party_ghost.ghost.entity;
        draw_ghost_head ~tint:Color.raywhite party_ghost.ghost;
        if state.debug.enabled then (
          debug_rect party_ghost.ghost.entity.dest;
          debug_rect_outline party_ghost.ghost.entity.sprite.dest)
      in
      List.iter draw_party_ghost ghosts_by_id
    in

    let draw_player (player : player) =
      let tint =
        match Player.get_invincibility_kind state game with
        | None -> Color.create 255 255 255 255
        | Some invincibility_kind -> (
          let d = state.frame.time -. game.player.history.take_damage.started.at in
          let a = get_flashing_tint d in
          match invincibility_kind with
          | TAKING_HAZARD_DAMAGE -> Color.white
          | TOOK_DAMAGE -> Color.create 255 255 255 a
          | DIVE_IFRAMES -> Color.create a a a 255
          | SHADE_CLOAK -> Color.create 0 100 200 255)
      in

      let draw_child (child_kind : ghost_child_kind) (child : ghost_child) =
        let get_child_pos child_w child_h =
          Entity.get_child_pos player.ghost.entity child.relative_pos child_w child_h
        in
        let draw_child_sprite (sprite : sprite) tint =
          sprite.facing_right <- player.ghost.entity.sprite.facing_right;
          sprite.dest.pos <- get_child_pos sprite.dest.w sprite.dest.h;
          draw_sprite ~tint sprite
        in
        let tint =
          match child_kind with
          | NAIL slash ->
            if state.debug.enabled then (
              debug_rect_outline ~size:2. ~color:Color.purple slash.sprite.dest;
              match slash.collision with
              | SHAPE shape ->
                debug_shape_outline ~size:2. ~color:Color.red (Some slash.sprite) shape
              | _ -> ());
            player.current_weapon.tint
          | WRAITHS -> if game.player.abilities.abyss_shriek then Color.purple else Color.white
          | DIVE_COOLDOWN
          | DIVE ->
            if game.player.abilities.descending_dark then Color.red else Color.white
          | DREAM_NAIL
          | C_DASH_WHOOSH
          | SHADE_DASH_SPARKLES
          | C_DASH_CHARGE_CRYSTALS
          | C_DASH_WALL_CHARGE_CRYSTALS
          | FOCUS ->
            Color.white
        in
        draw_child_sprite child.sprite tint
      in
      let draw_vengeful_spirit (p : projectile) =
        if state.debug.enabled then
          debug_rect_outline ~size:2. ~color:Color.red p.entity.dest;
        draw_entity p.entity
      in
      List.iter draw_vengeful_spirit player.spawned_vengeful_spirits;
      let children_in_front, children_behind =
        Ghost_child_kind.Map.partition (fun _ child -> child.in_front) player.children
      in
      let shine_dest =
        let size = Config.ghost.shine_size in
        {
          pos = Entity.get_child_pos player.ghost.entity (CENTER, CENTER) size size;
          w = size;
          h = size;
        }
      in
      draw_texture game.player.shared_textures.shine shine_dest 0;
      Ghost_child_kind.Map.iter draw_child children_behind;

      draw_entity ~tint player.ghost.entity;
      draw_ghost_head ~tint player.ghost;
      Ghost_child_kind.Map.iter draw_child children_in_front
    in

    let draw_hud () =
      let padding = Config.other.hud_padding in
      let energon_pod_base_image = game.player.shared_textures.energon_pod_base.image in
      let energon_pod_image =
        match game.player.soul.max with
        | 99 -> game.player.shared_textures.energon_pod_1.image
        | 132 -> game.player.shared_textures.energon_pod_2.image
        | 165 -> game.player.shared_textures.energon_pod_3.image
        | 198 -> game.player.shared_textures.energon_pod_4.image
        | max -> failwithf "invalid soul.max value: %d" max
      in
      let pod_src_w, pod_src_h =
        ( Raylib.Texture.width energon_pod_image / 2 |> Int.to_float,
          Raylib.Texture.height energon_pod_image |> Int.to_float )
      in
      let pod_base_src_w, pod_base_src_h =
        ( Raylib.Texture.width energon_pod_base_image |> Int.to_float,
          Raylib.Texture.height energon_pod_base_image |> Int.to_float )
      in
      let pod_dest_w, pod_dest_h =
        (pod_src_w *. Config.scale.soul, pod_src_h *. Config.scale.soul)
      in
      let draw_soul (soul : soul) : unit =
        (* draws the complete empty energon pod, then draws the filled portion on top of that *)
        let full_src_h =
          (soul.current |> Int.to_float) /. (soul.max |> Int.to_float) *. pod_src_h
        in
        let full_dest_h = full_src_h *. Config.scale.soul in
        let empty_src_h = pod_src_h -. full_src_h in
        let empty_dest_h = pod_dest_h -. full_dest_h in
        let full_src = Rectangle.create 0. empty_src_h pod_src_w full_src_h in
        let empty_src = Rectangle.create pod_src_w 0. pod_src_w pod_src_h in
        let base_src = Rectangle.create 0. 0. pod_base_src_w pod_base_src_h in
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
        let base_dest =
          {
            pos = { x = camera_x +. padding; y = camera_y +. padding +. pod_dest_h };
            h = pod_base_src_h *. Config.scale.soul;
            w = pod_base_src_w *. Config.scale.soul;
          }
        in
        Draw.image energon_pod_image empty_src (empty_dest |> rect_to_Rect) (Raylib.Vector2.zero ())
          0.0 Color.raywhite;
        Draw.image energon_pod_image full_src (full_dest |> rect_to_Rect) (Raylib.Vector2.zero ())
          0.0 Color.raywhite;
        Draw.image energon_pod_base_image base_src (base_dest |> rect_to_Rect)
          (Raylib.Vector2.zero ()) 0.0 Color.raywhite
      in
      let draw_head idx =
        let image = game.player.shared_textures.health.image in
        let w, h =
          ( Raylib.Texture.width image / 2 |> Int.to_float,
            Raylib.Texture.height image |> Int.to_float )
        in
        let dest_w, dest_h = (w *. Config.scale.health, h *. Config.scale.health) in
        let dest_x, dest_y =
          if idx < 10 then
            ( camera_x +. pod_dest_w +. padding +. ((idx |> Int.to_float) *. (dest_w +. padding)),
              camera_y +. padding )
          else
            ( camera_x
              +. pod_dest_w
              +. padding
              +. ((idx - 10 |> Int.to_float) *. (dest_w +. padding)),
              camera_y +. (2. *. padding) +. dest_h )
        in
        let dest = Rectangle.create dest_x dest_y dest_w dest_h in
        let src =
          if idx + 1 > game.player.health.current then
            Rectangle.create w 0. w h
          else
            Rectangle.create 0. 0. w h
        in
        Draw.image image src dest (Raylib.Vector2.zero ()) 0.0 Color.raywhite
      in
      draw_soul game.player.soul;
      match game.mode with
      | CLASSIC
      | DEMO ->
        List.iter draw_head (Int.range game.player.health.max)
      | STEEL_SOLE -> ()
    in

    let draw_progress () =
      draw_text_bg_box Config.text.progress_config;
      let row_y n =
        camera_y +. Config.other.progress_y_padding +. (n *. line_height ~font_scale:2. ())
      in
      let draw_progress_row idx (row : Progress.row) =
        Draw.text
          (fmt "%s: %d / %d  %s" row.name row.found row.total row.suffix)
          { x = camera_x +. Config.other.progress_x_padding; y = row_y (idx |> Int.to_float) }
      in
      let all_progress = Progress.get_all_progress state game in
      let (rows : Progress.row list) =
        [
          all_progress.weapons;
          all_progress.dreamer_items;
          all_progress.keys;
          all_progress.abilities;
          all_progress.purple_pens;
        ]
      in
      List.iteri draw_progress_row rows;
      Draw.text
        (fmt "Total: %0.2f%s" all_progress.total "%")
        { x = camera_x +. Config.other.progress_x_padding; y = row_y 8. }
    in

    let maybe_show_screen_fade ~under_ghost : bool =
      match state.screen_fade with
      | None -> false
      | Some fade ->
        if under_ghost = fade.show_ghost then (
          let target =
            match fade.timer with
            | None -> 255
            | Some timer ->
              timer.left <- { seconds = timer.left.seconds -. state.frame.dt };
              if fade.target_alpha > fade.starting_alpha then (
                let target' =
                  (fade.target_alpha |> Int.to_float)
                  *. (1. -. (timer.left.seconds /. timer.total.seconds))
                  |> Float.to_int
                in
                Int.min target' fade.target_alpha)
              else (
                let target' =
                  fade.starting_alpha
                  - ((fade.starting_alpha - fade.target_alpha |> Int.to_float)
                     *. (1. -. (timer.left.seconds /. timer.total.seconds))
                    |> Float.to_int)
                in
                if target' < 0 && timer.left.seconds < 0. then (
                  state.screen_fade <- None;
                  0)
                else
                  target')
          in
          draw_screen_fade target;
          true)
        else
          false
    in

    Raylib.begin_drawing ();
    Raylib.clear_background game.room.area.bg_color;
    Raylib.begin_mode_2d state.camera.raylib;
    draw_skybox game.room.area.tint;
    draw_bg_tiles game.room camera_x camera_y state;
    draw_levers ();
    draw_npcs game.room.npcs;
    draw_solid_tiles game.room camera_x camera_y state;
    draw_party_ghosts game.party;
    let screen_faded = maybe_show_screen_fade ~under_ghost:true in
    draw_player game.player;
    if not screen_faded then (
      let enemy_projectiles = draw_enemies game.room.enemies in
      draw_floating_platforms game.room;
      draw_object_trigger_indicators ();
      draw_loose_projectiles ();
      (match game.reflection_x with
      | None -> ()
      | Some reflection_x ->
        let original_pos = game.player.ghost.entity.dest.pos in
        game.player.ghost.entity.dest.pos <-
          { original_pos with x = reflection_x -. (original_pos.x -. reflection_x) };
        game.player.ghost.entity.sprite.facing_right <-
          not game.player.ghost.entity.sprite.facing_right;
        draw_entity ~tint:Color.white game.player.ghost.entity;
        draw_ghost_head ~tint:Color.white game.player.ghost;
        game.player.ghost.entity.dest.pos <- original_pos;
        game.player.ghost.entity.sprite.facing_right <-
          not game.player.ghost.entity.sprite.facing_right);
      draw_fg_tiles game.room camera_x camera_y state;
      List.iter draw_projectile enemy_projectiles;
      if game.room.area.id = CITY_OF_CHAIRS then (
        let draw (sprite : sprite) =
          let y_offset =
            Config.other.raindrop_speed
            *. (state.frame.idx mod (sprite.dest.h /. Config.other.raindrop_speed |> Float.to_int)
               |> Int.to_float)
          in
          draw_sprite ~render_offset:(Some { x = camera_x; y = camera_y +. y_offset }) sprite
        in
        List.iter draw game.room.raindrops);
      draw_hud ();
      ignore (maybe_show_screen_fade ~under_ghost:false));
    (let interaction_text =
       match state.pause_menu with
       | None -> game.interaction.text
       | Some PROGRESS ->
         draw_progress ();
         None
       | Some (WORLD_MAP world_map) ->
         draw_world_map world_map;
         None
       | Some (MENU pause_menu) -> Some (MENU (pause_menu, None))
     in
     maybe_draw_text (Some game) interaction_text);
    draw_other_text game;
    if Env.development then (
      Raylib.draw_fps
        (camera_x +. Config.window.w -. 100. |> Float.to_int)
        (camera_y |> Float.to_int);
      if state.debug.enabled then
        draw_debug_info ();
      if state.debug.show_frame_inputs then
        draw_frame_inputs ());
    Raylib.end_mode_2d ();
    Raylib.end_drawing ();
    state
