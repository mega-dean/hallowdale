open Utils
open Types

(* file_name should not have ".json" at the end *)
let parse_room_filename source file_name : area_id * room_id =
  match file_name with
  | "ac-repair_b" -> (AC_REPAIR_ANNEX, AC_B)
  | "ac-repair_c" -> (AC_REPAIR_ANNEX, AC_C)
  | "ac-repair_d" -> (AC_REPAIR_ANNEX, AC_D)
  | "ac-repair_e" -> (AC_REPAIR_ANNEX, AC_E)
  | "ac-repair_f" -> (AC_REPAIR_ANNEX, AC_F)
  | "basement_a" -> (BASEMENT, BASE_A)
  | "basement_b" -> (BASEMENT, BASE_B)
  | "basement_c" -> (BASEMENT, BASE_C)
  | "basement_d" -> (BASEMENT, BASE_D)
  | "basement_e" -> (BASEMENT, BASE_E)
  | "beenz_a" -> (MEOW_MEOW_BEENZ, MMB_A)
  | "city_a" -> (CITY_OF_CHAIRS, CITY_A)
  | "city_b" -> (CITY_OF_CHAIRS, CITY_B)
  | "city_d" -> (CITY_OF_CHAIRS, CITY_D)
  | "city_e" -> (CITY_OF_CHAIRS, CITY_E)
  | "computer_a" -> (COMPUTER_WING, CPU_A)
  | "computer_b" -> (COMPUTER_WING, CPU_B)
  | "computer_c" -> (COMPUTER_WING, CPU_C)
  | "computer_f" -> (COMPUTER_WING, CPU_F)
  | "computer_g" -> (COMPUTER_WING, CPU_G)
  | "computer_h" -> (COMPUTER_WING, CPU_H)
  | "computer_i" -> (COMPUTER_WING, CPU_I)
  | "computer_k" -> (COMPUTER_WING, CPU_K)
  | "computer_m" -> (COMPUTER_WING, CPU_M)
  | "forgotten_a" -> (FORGOTTEN_CLASSROOMS, FORG_A)
  | "forgotten_b" -> (FORGOTTEN_CLASSROOMS, FORG_B)
  | "forgotten_c" -> (FORGOTTEN_CLASSROOMS, FORG_C)
  | "forgotten_d" -> (FORGOTTEN_CLASSROOMS, FORG_D)
  | "forgotten_deans-pass" -> (FORGOTTEN_CLASSROOMS, FORG_DEANS_PASS)
  | "forgotten_e" -> (FORGOTTEN_CLASSROOMS, FORG_E)
  | "forgotten_f" -> (FORGOTTEN_CLASSROOMS, FORG_F)
  | "forgotten_g" -> (FORGOTTEN_CLASSROOMS, FORG_G)
  | "forgotten_h" -> (FORGOTTEN_CLASSROOMS, FORG_H)
  | "forgotten_test" -> (FORGOTTEN_CLASSROOMS, FORG_TEST)
  | "infected_a" -> (INFECTED_CLASSROOMS, INF_A)
  | "infected_b" -> (INFECTED_CLASSROOMS, INF_B)
  | "infected_c" -> (INFECTED_CLASSROOMS, INF_C)
  | "infected_d" -> (INFECTED_CLASSROOMS, INF_D)
  | "library_a" -> (LIBRARY, LIB_A)
  | "library_b" -> (LIBRARY, LIB_B)
  | "library_c" -> (LIBRARY, LIB_C)
  | "library_d" -> (LIBRARY, LIB_D)
  | "library_e" -> (LIBRARY, LIB_E)
  | "library_f" -> (LIBRARY, LIB_F)
  | "library_g" -> (LIBRARY, LIB_G)
  | "library_h" -> (LIBRARY, LIB_H)
  | "library_i" -> (LIBRARY, LIB_I)
  | "outlands_a" -> (OUTLANDS, OUT_A)
  | "outlands_b" -> (OUTLANDS, OUT_B)
  | "outlands_c" -> (OUTLANDS, OUT_C)
  | "outlands_d" -> (OUTLANDS, OUT_D)
  | "outlands_e" -> (OUTLANDS, OUT_E)
  | "trampoline_a" -> (TRAMPOLINEPATH, TRAMP_A)
  | "trampoline_b" -> (TRAMPOLINEPATH, TRAMP_B)
  | "trampoline_c" -> (TRAMPOLINEPATH, TRAMP_C)
  | "trampoline_d" -> (TRAMPOLINEPATH, TRAMP_D)
  | "trampoline_e" -> (TRAMPOLINEPATH, TRAMP_E)
  | "trampoline_f" -> (TRAMPOLINEPATH, TRAMP_F)
  | "trampoline_g" -> (TRAMPOLINEPATH, TRAMP_G)
  | "trampoline_h" -> (TRAMPOLINEPATH, TRAMP_H)
  | "ventways_hub" -> (VENTWAYS, VENT_HUB)
  | _ -> failwithf "bad file name '%s' (from %s)" file_name source

module Tile = struct
  let coords_to_idx ~width (x, y) : int = x + (y * width)

  let coords_to_pos ((x, y) : int * int) : vector =
    {
      x = (x |> Int.to_float) *. Config.window.tile_size;
      y = (y |> Int.to_float) *. Config.window.tile_size;
    }

  let pos_to_dest_coords (pos : vector) : int * int =
    ( pos.x /. Config.window.dest_tile_size |> Float.to_int,
      pos.y /. Config.window.dest_tile_size |> Float.to_int )

  let pos_to_coords (pos : vector) : int * int =
    ( pos.x /. Config.window.tile_size |> Float.to_int,
      pos.y /. Config.window.tile_size |> Float.to_int )

  let tile_dest ~tile_w ~tile_h (x, y) : float * float =
    ( (x |> Int.to_float) *. tile_w *. Config.scale.room,
      (y |> Int.to_float) *. tile_h *. Config.scale.room )

  let src_pos idx width : vector =
    let tile_coords idx width : int * int = (idx mod width, idx / width) in
    tile_coords idx width |> coords_to_pos

  let scale_dest_size w h = (w *. Config.scale.room, h *. Config.scale.room)

  let dest_xy ?(parallax = None) offset_x offset_y tile_w tile_h idx width : float * float =
    let scale_dest_position dest =
      match parallax with
      | None ->
        ((dest.x +. offset_x) *. Config.scale.room, (dest.y +. offset_y) *. Config.scale.room)
      | Some frame_parallax ->
        ( ((dest.x +. offset_x) *. Config.scale.room) +. frame_parallax.x,
          ((dest.y +. offset_y) *. Config.scale.room) +. frame_parallax.y )
    in
    src_pos idx width |> scale_dest_position

  let raw_gid (gid : int) : int = 0b00001111111111111111111111111111 land gid

  let transformation_bits gid =
    (* Tiled uses the fourth bit for hexagonal maps so we don't need to keep track of it *)
    (0b11100000000000000000000000000000 land gid) lsr 29
end

module JsonRoom = struct
  type t = Json_t.room

  let get_h (room : t) = (room.h_in_tiles |> Int.to_float) *. room.tile_h

  let tile_idx (room : t) (x, y) =
    (x /. room.tile_w |> Float.to_int, y /. room.tile_h |> Float.to_int)
    |> Tile.coords_to_idx ~width:room.w_in_tiles

  let coords_to_dest (json_room : t) (coords : string) : vector =
    let target_x', target_y' = String.split_at_first ',' coords in
    let tile_x, tile_y = (target_x' |> int_of_string, target_y' |> int_of_string) in
    let x, y = Tile.tile_dest ~tile_w:json_room.tile_w ~tile_h:json_room.tile_h (tile_x, tile_y) in
    { x; y }

  let locate_by_coords (world : world) global_x global_y : room_id * room_location =
    let in_location ((_room_id, room_location) : room_id * room_location) : bool =
      room_location.global_x < global_x
      && global_x < room_location.global_x +. room_location.w
      && room_location.global_y < global_y
      && global_y < room_location.global_y +. room_location.h
    in
    match List.find_opt in_location world with
    | None -> failwithf "no room found at %0.1f, %0.1f" global_x global_y
    | Some (room_id, room_location) -> (room_id, room_location)

  let locate_by_name (world : world) room_name : room_location =
    let _area_id, room_id = parse_room_filename (fmt ".world file") room_name in
    List.assoc room_id world

  let get_exits (room_location : room_location) : rect list =
    let left = 0. in
    let top = 0. in
    let right = room_location.w in
    let bottom = room_location.h in
    let rect_size = 50. *. Config.scale.room in
    [
      (* top *)
      { pos = { x = left; y = top -. rect_size }; w = room_location.w; h = rect_size };
      (* bottom *)
      { pos = { x = left; y = bottom }; w = room_location.w; h = rect_size };
      (* left *)
      { pos = { x = left -. rect_size; y = top }; w = rect_size; h = room_location.h };
      (* right *)
      { pos = { x = right; y = top }; w = rect_size; h = room_location.h };
    ]

  let src_xy (json_room : t) idx width : vector = Tile.src_pos idx width

  (* TODO too many args, add labels *)
  let dest_xy (json_room : t) ?(parallax = None) offset_x offset_y idx width : float * float =
    Tile.dest_xy offset_x offset_y json_room.tile_w json_room.tile_h idx width ~parallax

  let dest_wh (json_room : t) () : float * float =
    Tile.scale_dest_size json_room.tile_w json_room.tile_h

  let look_up_platform (json_room : t) platform_textures_by_name (gid' : int) :
      string * texture * platform_kind option =
    let gid = Tile.raw_gid gid' in
    let bits = Tile.transformation_bits gid' in
    let platforms_tileset_source =
      List.find
        (fun (source : Json_t.tileset_source) ->
          String.ends_with ~suffix:"platforms.json" source.source)
        json_room.tileset_sources
    in

    let platform_names =
      [
        (* this order needs to match the order in platforms.json tileset *)
        ("atm", None);
        ("cold-drinks", None);
        ("couch", None);
        ("file-cabinet", None);
        ("fresh-coffee", None);
        ("vending-machine", None);
        ("drawers", None);
        ("cart-wide", None);
        ("cart", None);
        ("small-chair", None);
        ("desk", None);
        ("drawers-tall", None);
        ("file-cabinet-square", None);
        ("couch-wide", None);
        ("small-stool", None);
        ("bookshelf", None);
        ("bamboo", Some (DISAPPEARABLE VISIBLE));
        ("bamboo-wide", Some (DISAPPEARABLE VISIBLE));
        ("rotatable", Some (ROTATABLE UPRIGHT));
        ("bench", None);
        ("kp-drop", Some (TEMPORARY VISIBLE));
        ("bookshelf-small", None);
        ("bookshelf-wide", None);
        ("deans-closet", Some (LOCKED_DOOR ("Dean's Closet Key", VISIBLE)));
        ("deans-brand", Some (LOCKED_DOOR ("Dean's Brand", VISIBLE)));
        ("jukebox", Some (LOCKED_DOOR ("Song 127", VISIBLE)));
        ("room-temperature-room", Some (LOCKED_DOOR ("Laybourne's Breathprint", VISIBLE)));
        ("chair2", None);
        ("chair3", None);
        ("chair4", None);
        ("chair2x2", None);
        ("chair3x2", None);
      ]
    in
    let texture_name, platform_kind =
      match List.nth_opt platform_names (gid - platforms_tileset_source.firstgid - 1) with
      | None ->
        failwithf
          "need to add new platform texture name to list (gid %d, firstgid %d, idx %d, length %d)"
          gid platforms_tileset_source.firstgid
          (gid - platforms_tileset_source.firstgid - 1)
          (List.length platform_names)
      | Some (name, kind) -> (name, kind)
    in
    match String.Map.find_opt texture_name platform_textures_by_name with
    | None ->
      failwithf "could not find platform %s, keys: %s" texture_name
        (platform_textures_by_name |> String.Map.to_list |> List.map fst |> String.join)
    | Some t -> (texture_name, t, platform_kind)

  let look_up_tile (json_room : t) ?(animation_offset = 0) room_cache (gid' : int) : texture * int =
    (* TODO this is checking all layers for transformations, even though it's only being used for jugs *)
    let gid = Tile.raw_gid gid' in
    let bits = Tile.transformation_bits gid' in
    let get_tileset (source : Json_t.tileset_source) =
      match String.Map.find_opt source.source room_cache.tilesets_by_path with
      | None -> failwithf "could not find cached tileset %s, gid: %d" source.source gid'
      | Some t -> t
    in
    let gid_in_tileset (tileset_source : Json_t.tileset_source) =
      (* these paths are coming from Tiled exports, so they don't need to use Filename.dir_sep *)
      if tileset_source.source = "../tilesets/world-map.json" then
        false
      else (
        let tileset : tileset = get_tileset tileset_source in
        gid >= tileset_source.firstgid && gid < tileset_source.firstgid + tileset.json.tile_count)
    in
    let tileset_sources =
      List.filter
        (fun (source : Json_t.tileset_source) -> source.source <> "../platforms/platforms.json")
        json_room.tileset_sources
    in
    match List.find_opt gid_in_tileset tileset_sources with
    | None -> failwithf "could not find gid %d in any tilesets" gid
    | Some tileset_source -> (
      let tileset = get_tileset tileset_source in
      try (tileset.tiles.(gid - tileset_source.firstgid + animation_offset), bits) with
      | Invalid_argument _ ->
        failwithf "look_up_tile error: tileset '%s', gid %d, %d tiles, %d" tileset.json.name gid
          (Array.length tileset.tiles) tileset_source.firstgid)
end

let get_object_collision (json_tileset : Json_t.tileset) (firstgid : int) (id : int) :
    Json_t.coll_rect option =
  let this_tile (collision : Json_t.collision) : bool = collision.id + firstgid = id in
  match List.find_opt this_tile json_tileset.collisions with
  | None -> None
  | Some collision ->
    if List.length collision.objectgroup.objects = 1 then
      Some (List.nth collision.objectgroup.objects 0)
    else
      failwithf "expected 1 collision rect object, got %d for firstgid %d, id %d"
        (List.length collision.objectgroup.objects)
        firstgid id

let load_tiles
    (room : Json_t.room)
    (json_tileset : Json_t.tileset)
    image
    (tileset_source : Json_t.tileset_source) : texture array =
  let load_tile idx : texture =
    let src = JsonRoom.src_xy room idx json_tileset.columns in
    {
      ident = fmt "tile %d" idx;
      image;
      animation_src = STILL { w = json_tileset.tile_w; h = json_tileset.tile_h; pos = src };
      coll_offset = Zero.vector ();
    }
  in
  Array.init json_tileset.tile_count load_tile

let load_tilesets (room : Json_t.room) : (string * tileset) list =
  let load_tileset source' : (string * tileset) option =
    let parse_tileset (source : Json_t.tileset_source) : Json_t.tileset option =
      let ignore_tileset () =
        (* the platforms tileset has to be parsed separately because Tiled uses the key "objects"
           for different things, so image-based tilesets (like platforms) aren't compatible
           with regular tilesets *)
        (* these paths are coming from Tiled exports, so they don't need to use Filename.dir_sep *)
        String.equal "../tilesets/world-map.json" source.source
        || String.equal ":/automap-tiles.tsx" source.source
        || String.starts_with ~prefix:"../platforms/" source.source
      in
      if ignore_tileset () then
        None
      else if not (String.starts_with ~prefix:"../tilesets/" source.source) then
        failwithf "bad tileset path '%s' (needs to be in assets/tiled/tilesets)" source.source
      else (
        let full_path =
          (* kinda weird that this path ends up including "/tilesets/../tilesets/" *)
          File.make_assets_path [ "tiled"; "tilesets"; source.source ]
        in
        Some (File.read full_path |> Json_j.tileset_of_string))
    in

    match parse_tileset source' with
    | None -> None
    | Some json ->
      let image = load_tiled_asset (File.make_path [ "tilesets"; json.source ]) in
      Some (source'.source, { json; image; tiles = load_tiles room json image source' })
  in
  List.filter_map load_tileset room.tileset_sources

let create_camera_at v (shake : float) (center_x : float) (center_y : float) =
  let rotation = 0. in
  let zoom = 1. in
  let offset =
    if shake <= 0. then
      Raylib.Vector2.(create center_x center_y)
    else (
      let angle = Random.float (3.14159 /. 2.) in
      let x_offset = shake *. cos angle *. 10. in
      let y_offset = shake *. sin angle *. 10. in
      Raylib.Vector2.(create (center_x +. x_offset) (center_y +. y_offset)))
  in
  Raylib.Camera2D.create offset v rotation zoom

let init_world (path : string) : (room_id * room_location) list =
  let full_path = File.make_assets_path [ "tiled"; "rooms"; fmt "%s.world" path ] in
  let json_world : Json_t.world = File.read full_path |> Json_j.world_of_string in
  let filenames_in_world_file : String.Set.t ref = ref String.Set.empty in
  let get_room_location (global_map : Json_t.global_map) : room_id * room_location =
    let filename = Str.first_chars global_map.file_name (String.length global_map.file_name - 5) in
    filenames_in_world_file := String.Set.add filename !filenames_in_world_file;
    let _area_id, room_id = parse_room_filename (fmt "%s.world file" path) filename in
    ( room_id,
      {
        filename;
        global_x = (global_map.x |> Int.to_float) *. Config.scale.room;
        global_y = (global_map.y |> Int.to_float) *. Config.scale.room;
        w = (global_map.w_in_pixels |> Int.to_float) *. Config.scale.room;
        h = (global_map.h_in_pixels |> Int.to_float) *. Config.scale.room;
      } )
  in
  let rooms = List.map get_room_location json_world.global_maps in
  let files_in_rooms_dir = ref String.Set.empty in
  let add_to_set s =
    if Str.last_chars s 5 = ".json" then
      files_in_rooms_dir :=
        String.Set.add (Str.first_chars s (String.length s - 5)) !files_in_rooms_dir
  in
  File.ls (File.make_assets_path [ "tiled"; "rooms" ]) |> List.iter add_to_set;
  let filenames_without_files : String.Set.t =
    String.Set.diff !filenames_in_world_file !files_in_rooms_dir
  in
  let files_without_filenames : String.Set.t =
    String.Set.diff !files_in_rooms_dir !filenames_in_world_file
  in
  if not (String.Set.is_empty filenames_without_files) then
    print "WARN: got filenames in .world that have no corresponding .json file:\n%s"
      (String.Set.elements filenames_without_files |> String.join_lines);
  if not (String.Set.is_empty files_without_filenames) then
    print "WARN: got .json files that aren't part of .world:\n%s"
      (String.Set.elements files_without_filenames |> String.join_lines);
  rooms
