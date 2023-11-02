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
  (* TODO add this again *)
  | "ventways_hub" -> (VENTWAYS, VENT_HUB)
  | _ -> failwithf "bad file name '%s' (from %s)" file_name source

module Tileset = struct
  type t = tileset

  let image (tileset : t) : image = (List.nth (tileset.tiles |> Array.to_list) 0).image
end

module Tile = struct
  let tile_idx_from_coords ~width (x, y) : int =
    let tile_x, tile_y = (x |> Float.to_int, y |> Float.to_int) in
    tile_x + (tile_y * width)

  let tile_idx ~tile_w ~tile_h ~width (x, y) : int =
    tile_idx_from_coords ~width (x /. tile_w, y /. tile_h)

  let tile_coords ~tile_w ~tile_h (x, y) : float * float =
    ((x |> Int.to_float) *. tile_w, (y |> Int.to_float) *. tile_h)

  let tile_dest ~tile_w ~tile_h (x, y) : float * float =
    ( (x |> Int.to_float) *. tile_w *. Config.scale.room,
      (y |> Int.to_float) *. tile_h *. Config.scale.room )

  let tile_xy idx width : int * int = (idx mod width, idx / width)

  let src_xy tile_w tile_h idx width : float * float =
    tile_coords ~tile_w ~tile_h (tile_xy idx width)

  let scale_dest_size ?(scale = 1.) w h =
    (w *. Config.scale.room *. scale, h *. Config.scale.room *. scale)

  let dest_xy ?(scale = 1.) ?(parallax = None) offset_x offset_y tile_w tile_h idx width :
      float * float =
    let scale_dest_position x y =
      match parallax with
      (* | None -> ((x *. Config.scale.room) +. offset_x, (y *. Config.scale.room) +. offset_y) *)
      | None -> ((x +. offset_x) *. Config.scale.room, (y +. offset_y) *. Config.scale.room)
      | Some frame_parallax ->
        ( (((x +. offset_x) *. Config.scale.room) +. frame_parallax.x) *. scale,
          (((y +. offset_y) *. Config.scale.room) +. frame_parallax.y) *. scale )
    in
    let x', y' = src_xy tile_w tile_h idx width in
    scale_dest_position x' y'

  let raw_gid (gid : int) : int = 0b00001111111111111111111111111111 land gid

  let transformation_bits gid =
    (* Tiled uses the fourth bit for hexagonal maps so we don't need to keep track of it *)
    (0b11100000000000000000000000000000 land gid) lsr 29
end

module Room = struct
  (* TODO this module is a little weird since it works on both json_room and room types *)
  type t = Json_t.room

  let get_h (room : t) = (room.h_in_tiles |> Int.to_float) *. room.tile_h

  let get_filename' area_id room_id : string =
    fmt "%s_%s" (Show.area_id area_id) (Show.room_id_filename room_id)

  let get_filename (room : room) : string = get_filename' room.area.id room.id

  let tile_idx (room : Json_t.room) (x, y) =
    Tile.tile_idx ~tile_w:room.tile_w ~tile_h:room.tile_h ~width:room.w_in_tiles (x, y)

  let dest_from_coords' (json_room : Json_t.room) (coords : string) : vector =
    let target_x', target_y' = String.split_at_first ',' coords in
    let tile_x, tile_y = (target_x' |> int_of_string, target_y' |> int_of_string) in
    let x, y = Tile.tile_dest ~tile_w:json_room.tile_w ~tile_h:json_room.tile_h (tile_x, tile_y) in
    { x; y }

  let dest_from_coords (room : room) (coords : string) : vector = dest_from_coords' room.json coords

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

  let unload_tilesets (room : room) : unit =
    let unload_tileset (_path, tileset) = Raylib.unload_texture (Tileset.image tileset) in
    List.iter unload_tileset room.cache.tilesets_by_path

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

  let tile_coords (json_room : t) ~tile_x ~tile_y : float * float =
    Tile.tile_coords ~tile_w:json_room.tile_w ~tile_h:json_room.tile_h (tile_x, tile_y)

  let src_wh (json_room : t) : float * float = (json_room.tile_w, json_room.tile_h)

  let src_xy (json_room : t) idx width : float * float =
    Tile.src_xy json_room.tile_w json_room.tile_h idx width

  (* TODO too many args, add labels *)
  let dest_xy ?(scale = 1.) (json_room : t) ?(parallax_opt = None) offset_x offset_y idx width :
      float * float =
    Tile.dest_xy offset_x offset_y json_room.tile_w json_room.tile_h idx width
      ~parallax:parallax_opt ~scale

  let dest_wh (json_room : t) ?(scale = 1.) () : float * float =
    Tile.scale_dest_size json_room.tile_w json_room.tile_h ~scale

  let look_up_platform (json_room : t) platform_textures_by_name (gid' : int) :
      string * texture * platform_kind option =
    let gid = Tile.raw_gid gid' in
    let bits = Tile.transformation_bits gid' in
    (* TODO get rid of this fn and just try accessing `tileset.tiles.(gid - firstgid)` for each
       tileset_source, and rescue out-of-bounds *)
    let platforms_tileset_source =
      List.find
        (fun (source : Json_t.tileset_source) ->
          String.ends_with ~suffix:"platforms.json" source.source)
        json_room.tileset_sources
    in

    let platform_names =
      [
        (* TODO this order is copied from the platforms.json tileset, so it should be read from there
           - if it can't for some reason, it's still probably worth maintaining this list manually for now, since it removes the requirement for platforms to have names
           - this could definitely be done for change_current_room, but initial room load will be tricky
             -- actually maybe not, maybe just read the file and call the Json_j.tileset_of_string fn directly in Room.init

           - this will be harder to read from the file now that this has platform_kind too
        *)
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
      | Some name -> name
    in
    match List.assoc_opt texture_name platform_textures_by_name with
    | None ->
      failwithf "could not find platform %s, keys: %s" texture_name
        (platform_textures_by_name |> List.map fst |> join)
    | Some t -> (texture_name, t, platform_kind)

  let look_up_tile (json_room : t) ?(animation_offset = 0) room_cache (gid' : int) : texture * int =
    (* TODO this is checking all layers for transformations, even though it's only being used for jugs *)
    let gid = Tile.raw_gid gid' in
    let bits = Tile.transformation_bits gid' in
    let get_tileset (source : Json_t.tileset_source) =
      match List.assoc_opt source.source room_cache.tilesets_by_path with
      | None -> failwithf "could not find cached tileset %s, gid: %d" source.source gid'
      | Some t -> t
    in
    (* TODO get rid of this fn and just try accessing `tileset.tiles.(gid - firstgid)` for each
       tileset_source, and rescue out-of-bounds *)
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

  let lookup_coll_offsets room (gid : int) (json_room : Json_t.room) : vector =
    let tile, _ = look_up_tile json_room room.cache gid in
    tile.coll_offset

  let get_layer_tile_groups
      ?(debug = false)
      (room : room)
      (removed_idxs_by_layer : (string * int list) list) : layer list =
    let get_rectangle_tile_groups (json_layer : Json_t.tile_layer) (layer_name : string) :
        tile_group list =
      let tile_w, tile_h = (room.json.tile_w, room.json.tile_h) in
      let tile_groups : tile_group list ref =
        (* keeps track of gid indexes too so they can be not rendered after they are destroyed *)
        ref []
      in
      let available_idxs : bool ref array = Array.make (List.length json_layer.data) (ref true) in
      let removed_idxs =
        match List.assoc_opt layer_name removed_idxs_by_layer with
        | None -> []
        | Some idxs -> idxs
      in
      let partition_rects idx tile_gid =
        if not (List.mem idx removed_idxs) then (
          let x, y =
            Tile.dest_xy json_layer.offset_x json_layer.offset_y tile_w tile_h idx json_layer.w
          in
          let get_rectangle_tile_group () : tile_group =
            let idxs = ref [ idx ] in
            let mark_idx_as_nonzero i' =
              available_idxs.(i') <- ref false;
              idxs := i' :: !idxs
            in
            let get_width () : int =
              (* iterate horizontally until 0 or end of row *)
              let rec find_idx i w =
                let at_end_of_row () = i mod json_layer.w = 0 in
                let at_end_of_data () = i = List.length json_layer.data in
                if at_end_of_data () || List.nth json_layer.data i = 0 || at_end_of_row () then
                  w
                else (
                  mark_idx_as_nonzero i;
                  find_idx (i + 1) (w + 1))
              in
              find_idx (idx + 1) 1
            in
            let get_height width' : int =
              (* iterate vertically until 0 or bottom row *)
              let rec find_idx i h =
                let at_bottom_row () = i >= List.length json_layer.data in
                if at_bottom_row () || List.nth json_layer.data i = 0 then
                  h
                else (
                  let idxs' = List.init width' (fun n -> n + i) in
                  List.iter mark_idx_as_nonzero idxs';
                  find_idx (i + json_layer.w) (h + 1))
              in
              find_idx (idx + json_layer.w) 1
            in
            let w = get_width () in
            let h = get_height w in
            let rect =
              {
                pos = { x; y };
                w = (w |> Int.to_float) *. (tile_w *. Config.scale.room);
                h = (h |> Int.to_float) *. (tile_h *. Config.scale.room);
              }
            in
            let stub_sprite, fragments =
              let all_raw_gids =
                List.map (fun idx -> Tile.raw_gid (List.nth json_layer.data idx)) (!idxs |> List.uniq)
                |> List.uniq
              in
              let keys = List.map fst room.cache.jug_fragments_by_gid in
              match List.find_opt (fun raw_gid -> List.mem raw_gid keys) all_raw_gids with
              | None -> (None, [])
              | Some raw_gid -> (
                match List.assoc_opt raw_gid room.cache.jug_fragments_by_gid with
                | None -> (None, [])
                | Some destroy_resources ->
                  let sprite =
                    match destroy_resources.stub with
                    | None -> None
                    | Some stub ->
                      let y_offset =
                        (* multiply by 2 because jugs are 2 tiles high *)
                        rect.h -. (2. *. tile_h *. Config.scale.room)
                      in
                      let stub_dest =
                        {
                          pos = { x = rect.pos.x; y = rect.pos.y +. y_offset };
                          w = rect.w;
                          h = rect.h -. y_offset;
                        }
                      in
                      Some
                        {
                          ident = "sprite stub";
                          texture = stub;
                          dest = stub_dest;
                          (* sprite stubs keep track of exact transformation bits so this isn't used *)
                          facing_right = true;
                          collision = None;
                        }
                  in
                  (sprite, destroy_resources.fragments))
            in
            let door_health =
              (* TODO these numbers are "number of hits - 1" because checking `> 0` in the slash-resolving
                 code makes things a little simpler *)
              match List.assoc_opt idx room.idx_configs with
              | Some (DOOR_HITS n) -> Some { hits = n; last_hit_at = -1. }
              | _ -> None
            in
            {
              tile_idxs = !idxs |> List.uniq;
              dest = rect;
              stub_sprite;
              fragments;
              transformation_bits = Tile.transformation_bits tile_gid;
              door_health;
            }
          in
          if tile_gid <> 0 then
            if !(available_idxs.(idx)) then
              tile_groups := get_rectangle_tile_group () :: !tile_groups)
      in
      List.iteri partition_rects json_layer.data;
      !tile_groups
    in
    let set_tile_groups (layer : layer) =
      let rects =
        if
          (layer.config.collides_with_ghost
          || layer.config.water
          || layer.config.destroyable
          || layer.config.hazard)
          && not layer.hidden
        then
          get_rectangle_tile_groups layer.json layer.name
        else
          []
      in
      layer.tile_groups <- rects;
      layer
    in
    List.map set_tile_groups room.layers

  let reset_tile_groups (room : room) =
    room.layers <- get_layer_tile_groups room room.progress.removed_idxs_by_layer
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
    let x, y = Room.src_xy room idx json_tileset.columns in
    let get_coll_offsets id : vector =
      match get_object_collision json_tileset tileset_source.firstgid id with
      | None -> Zero.vector ()
      | Some coll_rect ->
        { y = coll_rect.y *. Config.scale.room; x = coll_rect.x *. Config.scale.room }
    in
    {
      ident = fmt "tile %d" idx;
      image;
      animation_src = STILL { w = json_tileset.tile_w; h = json_tileset.tile_h; pos = { x; y } };
      coll_offset = get_coll_offsets (idx + tileset_source.firstgid);
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
      Some (source'.source, { json; tiles = load_tiles room json image source' })
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
  let filenames_in_world_file : str_set ref = ref StrSet.empty in
  let get_room_location (global_map : Json_t.global_map) : room_id * room_location =
    let filename = Str.first_chars global_map.file_name (String.length global_map.file_name - 5) in
    filenames_in_world_file := StrSet.add filename !filenames_in_world_file;
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
  let files_in_rooms_dir = ref StrSet.empty in
  let add_to_set s =
    if Str.last_chars s 5 = ".json" then
      files_in_rooms_dir := StrSet.add (Str.first_chars s (String.length s - 5)) !files_in_rooms_dir
  in
  File.ls (File.make_assets_path [ "tiled"; "rooms" ]) |> List.iter add_to_set;
  let filenames_without_files : str_set =
    StrSet.diff !filenames_in_world_file !files_in_rooms_dir
  in
  let files_without_filenames : str_set =
    StrSet.diff !files_in_rooms_dir !filenames_in_world_file
  in
  if not (StrSet.is_empty filenames_without_files) then
    print "WARN: got filenames in .world that have no corresponding .json file:\n%s"
      (StrSet.elements filenames_without_files |> join ~sep:"\n");
  if not (StrSet.is_empty files_without_filenames) then
    print "WARN: got .json files that aren't part of .world:\n%s"
      (StrSet.elements files_without_filenames |> join ~sep:"\n");
  rooms
