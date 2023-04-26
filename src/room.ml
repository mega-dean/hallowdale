open Types

[@@@ocaml.warning "-26-27-32"]

let init
    (file_name : string)
    (progress : progress)
    (exits : rect list)
    (enemy_configs : (enemy_id * json_enemy_config) list)
    (npc_configs : (npc_id * json_npc_config) list) : room =
  (* TODO sometimes this function gets called when area/room kinds are already known, so this lookup is redundant *)
  let (area_id, room_id) : area_id * room_id = Tiled.parse_room_filename "init_room" file_name in
  let room_key = Tiled.Room.get_uuid' area_id room_id in
  let new_room_progress () =
    (* room_progress gets created here, but isn't saved into state.progress.rooms until the room is being unloaded at an exit *)
    { finished_interactions = []; revealed_shadow_layers = []; removed_idxs_by_layer = [] }
  in
  let room_progress =
    match List.assoc_opt room_key progress.rooms with
    | None -> new_room_progress ()
    | Some rp -> rp
  in
  let parse_room (path : string) : json_room =
    let full_path = fmt "../assets/tiled/rooms/%s" path in
    (* TODO cache these instead of reloading the tileset between every room *)
    Tiled.read_whole_file full_path |> Json_j.room_of_string
  in

  let json_room = parse_room (fmt "%s.json" file_name) in
  let idx_configs : (int * idx_config) list ref = ref [] in
  let camera_triggers : (string * rect) list ref = ref [] in
  let shadow_triggers : (string * rect) list ref = ref [] in
  let lore_triggers : (string * rect) list ref = ref [] in
  let cutscene_triggers : (string * rect) list ref = ref [] in
  let enemy_rects : (enemy_id * rect) list ref = ref [] in
  let npc_rects : (npc_id * rect * bool) list ref = ref [] in
  let get_object_rects (jl : json_layer) =
    let get_object_rect ?(floor = false) ?(hidden = false) name (coll_rect : Json_t.coll_rect) =
      let rect = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      if floor then
        ( name,
          {
            pos = { x = rect.pos.x |> Float.floor; y = rect.pos.y |> Float.floor };
            w = rect.w |> Float.floor;
            h = rect.h |> Float.floor;
          } )
      else if hidden then
        (name, { rect with pos = { x = -1. *. rect.pos.x; y = -1. *. rect.pos.y } })
      else
        (name, rect)
    in
    let categorize (coll_rect : Json_t.coll_rect) =
      (* TODO maybe want to use a different separator because enemy rects use variant names that
         can have an underscore, eg `enemy_LOCKER_BOY`
         - it still works because .separate only finds the first occurrence, but it looks weird
      *)
      let name_prefix, name = Utils.separate coll_rect.name '_' in
      let tile_idx () =
        (* this fn finds the tile_idx that the trigger object's top-left corner is in, so trigger objects that are
           used like this (purple-pen, door-health) don't need to be placed exactly at that tile's coordinates *)
        Tiled.Tile.tile_idx ~tile_w:json_room.tile_w ~tile_h:json_room.tile_h ~width:json_room.w_in_tiles
          (coll_rect.x, coll_rect.y)
      in
      match name_prefix with
      | "camera" -> camera_triggers := get_object_rect ~floor:true name coll_rect :: !camera_triggers
      | "door-health" -> idx_configs := (tile_idx (), DOOR_HITS (coll_rect.h |> Float.to_int)) :: !idx_configs
      | "purple-pen" -> idx_configs := (tile_idx (), PURPLE_PEN coll_rect.name) :: !idx_configs
      | "hide" -> shadow_triggers := get_object_rect name coll_rect :: !shadow_triggers
      | "info"
      | "health"
      | "ability" ->
        lore_triggers := get_object_rect coll_rect.name coll_rect :: !lore_triggers
      | "npc" ->
        let npc_id, dest = get_object_rect (Npc.parse_name (fmt "Tiled rect npc_%s" name) name) coll_rect in
        npc_rects := (npc_id, dest, true) :: !npc_rects
      | "mirrored-npc" ->
        let npc_id, dest = get_object_rect (Npc.parse_name (fmt "Tiled rect hidden-npc_%s" name) name) coll_rect in
        npc_rects := (npc_id, dest, false) :: !npc_rects
      | "hidden-npc" ->
        let npc_id, dest =
          get_object_rect ~hidden:true (Npc.parse_name (fmt "Tiled rect hidden-npc_%s" name) name) coll_rect
        in
        npc_rects := (npc_id, dest, true) :: !npc_rects
      | "enemy" ->
        enemy_rects :=
          get_object_rect (Enemy.parse_name (fmt "Tiled rect enemy_%s" name) name) coll_rect :: !enemy_rects
      | "hidden-enemy" ->
        enemy_rects :=
          get_object_rect ~hidden:true (Enemy.parse_name (fmt "Tiled rect hidden-enemy_%s" name) name) coll_rect
          :: !enemy_rects
      | "cutscene" -> cutscene_triggers := get_object_rect coll_rect.name coll_rect :: !cutscene_triggers
      | _ ->
        failwithf
          "init_room invalid interaction name '%s' - needs to start with 'camera_', 'health_', 'cutscene_', etc."
          coll_rect.name
    in
    match jl with
    | `OBJECT_LAYER json -> (
      match json.name with
      | "triggers" -> List.iter categorize json.objects
      | "world-map-labels" -> ( (* only used for generating world map *) )
      | json_name -> failwithf "init_room bad object layer name: '%s'" json_name)
    | `TILE_LAYER _json -> ()
  in
  List.iter get_object_rects json_room.layers;

  (* need this for on_destroy cache keys *)
  let jug_firstgid =
    let is_jug (ts : json_tileset_source) =
      String.length ts.source > 10 && Str.last_chars ts.source 10 = "/jugs.json"
    in
    match List.find_opt is_jug json_room.tileset_sources with
    | Some tileset_source -> tileset_source.firstgid
    | None -> (* if a room doesn't use the jugs tileset, it never needs to use the cache *) 0
  in

  let enemies : enemy list = Enemy.create_from_rects !enemy_rects room_progress.finished_interactions enemy_configs in
  let npcs : npc list = Npc.create_from_rects !npc_rects npc_configs in

  let tilesets_by_path = Tiled.load_tilesets json_room in

  let tile_layers =
    let layers = ref [] in
    let layer_names = ref [] in
    let build_tile_layer (json_layer : json_layer) =
      match json_layer with
      | `OBJECT_LAYER _ -> ()
      | `TILE_LAYER json ->
        let add_new_layer () =
          let (layer_name', hidden) : string * bool =
            let str_starts_with s prefix =
              (* TODO move to Utils *)
              Str.string_match (Str.regexp prefix) s 0
            in
            let prefix = "hidden-" in
            if str_starts_with json.name prefix then
              (Str.string_after json.name (String.length prefix), true)
            else
              (json.name, List.mem json.name room_progress.revealed_shadow_layers)
          in
          let config =
            (* layer_number has to be unique, but it's checked later with List.mem json.name !layer_names *)
            let layer_name, _layer_number =
              let full_name = layer_name' in
              let name, number =
                let r = Str.regexp "[0-9]$" in
                try
                  let number_idx = Str.search_forward r full_name 0 in
                  (Str.first_chars full_name number_idx, int_of_string (Str.matched_string full_name))
                with
                | Not_found -> (full_name, 1)
              in
              (name, number)
            in
            let configs =
              match layer_name with
              | "floors" -> [ "collides" ]
              | "boss-doors" -> [ "collides" ]
              | "doors" -> [ "collides"; "destroyable"; "permanently_removable" ]
              | "bg-iso"
              | "bg-iso-lava"
              | "bg-iso-walls"
              | "bg-far" ->
                [ "bg" ]
              | "fg"
              | "shadow" ->
                [ "fg" ]
              | "close-fg" -> [ "fg"; "shaded" ]
              | "fg-jugs" -> [ "fg"; "destroyable"; "pogo" ]
              | "bg-jugs" -> [ "bg"; "destroyable"; "pogo" ]
              | json_name -> failwithf "unknown layer name: %s" json_name
            in
            let build_config config_parts =
              let has name = List.mem name config_parts in
              let depth_configs = ref 0 in
              if has "bg" then incr depth_configs;
              if has "fg" then incr depth_configs;
              if has "collides" then incr depth_configs;
              if !depth_configs <> 1 then
                failwithf "bad layer config for %s: needs to have exactly one of 'bg', 'fg', or 'collides'" layer_name
              else
                {
                  render = { bg = has "bg"; fg = has "fg" };
                  collides_with_ghost = has "collides";
                  damages_ghost = has "damages";
                  pogo = has "pogo";
                  destroyable = has "destroyable";
                  permanently_removable = has "permanently_removable";
                  shaded = has "shaded";
                }
            in
            build_config configs
          in

          let destroyed_tiles =
            match List.assoc_opt json.name room_progress.removed_idxs_by_layer with
            | None -> []
            | Some idxs -> idxs
          in

          layers :=
            {
              json;
              name = json.name;
              config;
              (* tile_groups gets populated later *)
              tile_groups = [];
              destroyed_tiles;
              spawned_stub_sprites = [];
              spawned_fragments = [];
              hidden;
            }
            :: !layers
        in
        if List.mem json.name !layer_names then
          (* TODO error message looks weird when the already-parsed layer name has a number
             - duplicate "shadow4" layers says "append numbers to disambiguate, eg shadow42"
          *)
          failwithf "already parsed layer '%s' - append numbers to disambiguate, eg %s2" json.name json.name
        else
          layer_names := json.name :: !layer_names;
        if List.mem json.name [ "world-map"; "camera-reference" ] then
          ()
        else
          add_new_layer ()
    in
    List.iter build_tile_layer json_room.layers;
    (* reverse so they are rendered in the same order they are defined *)
    List.rev !layers
  in

  let cache =
    match List.assoc_opt "../tilesets/jugs.json" tilesets_by_path with
    | None -> { jug_fragments_by_gid = []; tilesets_by_path }
    | Some tileset_image ->
      let jug_tileset_img = Tiled.Tileset.image tileset_image in

      let make_stub width tile_x tile_y =
        (* TODO maybe just pass x/y into Sprite.build_ functions and do the scaling in there *)
        let x, y = Tiled.Room.tile_coords json_room ~tile_x ~tile_y in
        Some
          (Sprite.build_texture_from_image ~scale:Config.scale.room jug_tileset_img
             (Some { w = json_room.tile_w *. width; h = json_room.tile_h; pos = { x; y } }))
      in

      let make_fragment name tile_x tile_y y_offset w h : entity =
        let x', y' = Tiled.Room.tile_coords json_room ~tile_x ~tile_y in
        let x, y = (x', y' +. y_offset) in
        let texture =
          Sprite.build_texture_from_image ~scale:Config.scale.room jug_tileset_img (Some { pos = { x; y }; w; h })
        in
        let sprite =
          Sprite.create
            (fmt "fragment sprite %s %0.1f" name y_offset)
            texture
            { pos = { x; y }; w = w *. Config.scale.room; h = h *. Config.scale.room }
        in
        let entity =
          Entity.create_for_sprite sprite ~inanimate:true { pos = Zero.vector (); w = sprite.dest.w; h = sprite.dest.h }
        in
        entity
      in

      let make_jug (config : jug_config) : int * jug_fragments =
        let fragments : entity list =
          let tile_x = config.tile_x in
          let tile_y =
            (* always render a stub, even if it is an empty tile image *)
            config.h + 1
          in
          match List.assoc_opt "../tilesets/jugs.json" tilesets_by_path with
          | None -> failwith "no jugs tileset"
          | Some tileset ->
            let get_coll_wh id : float * float =
              (* id is (firstgid + idx) *)
              match Tiled.get_object_collision tileset.json jug_firstgid id with
              | None -> failwithf "expected one collision rect object, got 0 for gid %d" id
              | Some coll_rect -> (coll_rect.w, coll_rect.h)
            in
            let build_fragment (collision_idx : int) (collision : Json_t.collision) : entity option =
              let w, h = get_coll_wh (jug_firstgid + collision.id) in
              (* this check only supports the left column of wide jugs *)
              if collision.id mod tileset.json.columns = tile_x then (
                let y = (collision_idx |> Int.to_float) *. tileset.json.tile_h in
                Some (make_fragment config.jug_name tile_x tile_y y w h))
              else
                None
            in
            let in_this_column (c : Json_t.collision) = c.id mod tileset.json.columns = tile_x in
            let collisions = List.filter in_this_column tileset.json.collisions in
            let fragments = List.mapi build_fragment collisions in
            (* TODO seems like there is probably a better way to do List.filter_mapi, but this works *)
            fragments |> List.filter Option.is_some |> List.map Option.get
        in
        if List.length fragments = 0 then
          failwithf "found 0 fragments for jug %s with tile_x %d - configure these with Tiled collision editor"
            config.jug_name config.tile_x;
        (* x is the 0-indexed tile coordinate on the jugs.png image, but we need to add jug_firstgid to adjust the cache key *)
        (config.tile_x + jug_firstgid, { stub = make_stub (config.w |> Int.to_float) config.tile_x config.h; fragments })
      in

      let metadata : jug_config list =
        let configs : Json_t.jug_metadata_file =
          let full_path = "../config/jugs.json" in
          Tiled.read_whole_file full_path |> Json_j.jug_metadata_file_of_string
        in
        let build (metadata : Json_t.jug_metadata) : jug_config =
          { jug_name = metadata.name; tile_x = metadata.x; w = metadata.w; h = metadata.h }
        in
        List.map build configs
      in

      { jug_fragments_by_gid = List.map make_jug metadata; tilesets_by_path }
  in

  let create_camera_bounds (room : json_room) =
    let w = (room.w_in_tiles |> Int.to_float) *. room.tile_w *. Config.scale.room in
    let h = (room.h_in_tiles |> Int.to_float) *. room.tile_h *. Config.scale.room in
    {
      min = { x = Config.window.center_x; y = Config.window.center_y };
      max = { x = w -. Config.window.center_x; y = h -. Config.window.center_y };
    }
  in
  let tint =
    match area_id with
    | FORGOTTEN_CLASSROOMS -> Raylib.Color.create 107 157 255 255
    | INFECTED_CLASSROOMS ->
      (* TODO maybe should be more red than orange *)
      Raylib.Color.create 107 60 40 255
    | BASEMENT -> Raylib.Color.create 50 20 50 255
    | TRAMPOLINEPATH -> Raylib.Color.create 50 220 50 255
    | CITY_OF_CHAIRS -> Raylib.Color.create 180 50 180 255
    (* TODO not sure what colors these should be *)
    | AC_REPAIR_ANNEX -> Raylib.Color.create 50 50 50 255
    | COMPUTER_WING -> Raylib.Color.create 50 50 50 255
    | MEOW_MEOW_BEENZ -> Raylib.Color.create 50 50 50 255
    | FINAL -> failwithf "area_id not configured yet: %s" (Show.area_id area_id)
  in
  let area = { id = area_id; tint } in
  {
    area;
    id = room_id;
    json = json_room;
    progress = room_progress;
    idx_configs = !idx_configs;
    camera_bounds = create_camera_bounds json_room;
    exits;
    triggers =
      { camera = !camera_triggers; lore = !lore_triggers; cutscene = !cutscene_triggers; shadows = !shadow_triggers };
    layers = tile_layers;
    enemies = List.map (fun (e : enemy) -> (e.id, e)) enemies;
    npcs;
    cache;
  }
