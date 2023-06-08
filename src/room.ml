open Types

[@@@ocaml.warning "-26-27-32"]

let get_pickup_indicators (room_progress : Json_t.room_progress) (texture : texture) (triggers : (string * rect) list) :
    sprite list =
  let show_obj ((name, dest') : string * rect) : sprite option =
    if List.mem name room_progress.finished_interactions then
      None
    else (
      let dest =
        let child_w, child_h =
          let src = get_src texture in
          (src.w *. Config.scale.room, src.h *. Config.scale.room)
        in
        let parent_w, parent_h = (dest'.w, dest'.h) in
        let parent_x, parent_y = (dest'.pos.x, dest'.pos.y) in
        {
          pos = { x = parent_x +. ((parent_w -. child_w) /. 2.); y = parent_y +. ((parent_h -. child_h) /. 2.) };
          w = child_w;
          h = child_h;
        }
      in
      Some (Sprite.create "indicator" texture dest))
  in
  List.filter_map show_obj triggers

let update_pickup_indicators (state : state) (game : game) =
  game.room.pickup_indicators <-
    get_pickup_indicators game.room.progress state.global.textures.pickup_indicator game.room.triggers.item_pickups

let save_progress (game : game) =
  let room_uuid = Tiled.Room.get_uuid game.room in
  game.progress <- Utils.assoc_replace room_uuid game.room.progress game.progress

type room_params = {
  file_name : string;
  progress : (string * Json_t.room_progress) list;
  exits : rect list;
  enemy_configs : (enemy_id * Json_t.enemy_config) list;
  npc_configs : (npc_id * Json_t.npc_config) list;
  pickup_indicator_texture : texture;
  lever_texture : texture;
}

let init (params : room_params) : room =
  (* TODO sometimes this function gets called when area/room kinds are already known, so this lookup is redundant *)
  let (area_id, room_id) : area_id * room_id = Tiled.parse_room_filename "init_room" params.file_name in
  let room_key = Tiled.Room.get_uuid' area_id room_id in
  let new_room_progress () : Json_t.room_progress =
    (* room_progress gets created here, but isn't saved into state.progress.rooms until the room is being unloaded at an exit *)
    { finished_interactions = []; revealed_shadow_layers = []; removed_idxs_by_layer = [] }
  in
  let room_progress =
    match List.assoc_opt room_key params.progress with
    | None -> new_room_progress ()
    | Some rp -> rp
  in
  let parse_room (path : string) : Json_t.room =
    let full_path = fmt "../assets/tiled/rooms/%s" path in
    (* TODO cache these instead of reloading the tileset between every room *)
    File.read full_path |> Json_j.room_of_string
  in

  let json_room = parse_room (fmt "%s.json" params.file_name) in
  let idx_configs : (int * idx_config) list ref = ref [] in
  let camera_triggers : (string * rect) list ref = ref [] in
  let lever_triggers : (string * sprite) list ref = ref [] in
  let shadow_triggers : (string * rect) list ref = ref [] in
  let lore_triggers : (string * rect) list ref = ref [] in
  let pickup_triggers : (string * rect) list ref = ref [] in
  let cutscene_triggers : (string * rect) list ref = ref [] in
  let enemy_rects : (enemy_id * rect) list ref = ref [] in
  let npc_rects : (npc_id * rect * bool) list ref = ref [] in
  let get_object_rects (jl : Json_t.layer) =
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
        Tiled.Room.tile_idx json_room (coll_rect.x, coll_rect.y)
      in
      let add_idx_config config = idx_configs := (tile_idx (), config) :: !idx_configs in
      match name_prefix with
      | "camera" -> camera_triggers := get_object_rect ~floor:true name coll_rect :: !camera_triggers
      | "lever" ->
        let direction, _ = Utils.separate name '-' in
        if not @@ List.mem direction [ "up"; "down" ] then (* TODO horizontal levers *)
          failwithf "unsupported lever direction: %s" direction;
        let lever_sprite () : sprite =
          let shape =
            make_shape
              [
                (* TODO move to config *)
                { x = 12.; y = 0. };
                { x = 34.; y = 0. };
                { x = 34.; y = 87. };
                { x = 12.; y = 87. };
              ]
          in
          {
            ident = fmt "Sprite %s" name;
            dest = get_object_rect ~floor:true name coll_rect |> snd;
            texture = params.lever_texture;
            collision = Some (SHAPE shape);
            facing_right = true;
          }
        in
        lever_triggers := (name, lever_sprite ()) :: !lever_triggers
      | "door-health" ->
        let door_health =
          (* this uses the object height, eg most breakable walls take 4 hits to destroy, so the door-health rects are 4 pixels high *)
          coll_rect.h |> Float.to_int
        in
        add_idx_config (DOOR_HITS door_health)
      | "purple-pen" -> add_idx_config (PURPLE_PEN coll_rect.name)
      | "hide" -> shadow_triggers := get_object_rect name coll_rect :: !shadow_triggers
      | "info"
      | "health" ->
        lore_triggers := get_object_rect coll_rect.name coll_rect :: !lore_triggers
      | "ability"
      | "weapon"
      | "dreamer" ->
        pickup_triggers := get_object_rect coll_rect.name coll_rect :: !pickup_triggers
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
    let is_jug (ts : Json_t.tileset_source) =
      String.length ts.source > 10 && Str.last_chars ts.source 10 = "/jugs.json"
    in
    match List.find_opt is_jug json_room.tileset_sources with
    | Some tileset_source -> tileset_source.firstgid
    | None -> (* if a room doesn't use the jugs tileset, it never needs to use the cache *) 0
  in

  let enemies : enemy list =
    Enemy.create_from_rects !enemy_rects room_progress.finished_interactions params.enemy_configs
  in
  let npcs : npc list = Npc.create_from_rects !npc_rects params.npc_configs in

  let tilesets_by_path = Tiled.load_tilesets json_room in

  let tile_layers =
    let layers = ref [] in
    let layer_names = ref [] in
    let build_tile_layer (json_layer : Json_t.layer) =
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
              | "lever-doors" -> [ "collides"; "permanently_removable" ]
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
                  pogoable = has "pogo";
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
            fragments |> Utils.filter_somes
        in
        if List.length fragments = 0 then
          failwithf "found 0 fragments for jug %s with tile_x %d - configure these with Tiled collision editor"
            config.jug_name config.tile_x;
        (* x is the 0-indexed tile coordinate on the jugs.png image, but we need to add jug_firstgid to adjust the cache key *)
        (config.tile_x + jug_firstgid, { stub = make_stub (config.w |> Int.to_float) config.tile_x config.h; fragments })
      in

      let metadata : jug_config list =
        let configs : Json_t.jug_metadata_file = File.read_config "jugs" Json_j.jug_metadata_file_of_string in
        let build (metadata : Json_t.jug_metadata) : jug_config =
          { jug_name = metadata.name; tile_x = metadata.x; w = metadata.w; h = metadata.h }
        in
        List.map build configs
      in

      { jug_fragments_by_gid = List.map make_jug metadata; tilesets_by_path }
  in

  let create_camera_bounds (room : Json_t.room) =
    let w = (room.w_in_tiles |> Int.to_float) *. room.tile_w *. Config.scale.room in
    let h = (room.h_in_tiles |> Int.to_float) *. room.tile_h *. Config.scale.room in
    {
      min = { x = Config.window.center_x; y = Config.window.center_y };
      max = { x = w -. Config.window.center_x; y = h -. Config.window.center_y };
    }
  in
  let area =
    let tint =
      match area_id with
      | FORGOTTEN_CLASSROOMS -> Raylib.Color.create 107 157 255 255
      | CITY_OF_CHAIRS -> Raylib.Color.create 107 107 205 255
      | INFECTED_CLASSROOMS -> Raylib.Color.create 107 60 40 255
      | TRAMPOLINEPATH -> Raylib.Color.create 50 220 50 255
      | BASEMENT -> Raylib.Color.create 50 20 50 255
      | MEOW_MEOW_BEENZ -> Raylib.Color.create 221 221 140 255
      | COMPUTER_WING -> Raylib.Color.create 63 93 57 255
      | AC_REPAIR_ANNEX -> Raylib.Color.create 83 129 129 255
      | _ -> failwithf "area_id not configured yet: %s" (Show.area_id area_id)
    in
    {
      id = area_id;
      tint;
      bg_color = Raylib.Color.create (Raylib.Color.r tint / 7) (Raylib.Color.g tint / 7) (Raylib.Color.b tint / 7) 255;
    }
  in
  {
    area;
    id = room_id;
    json = json_room;
    progress = room_progress;
    idx_configs = !idx_configs;
    camera_bounds = create_camera_bounds json_room;
    exits = params.exits;
    triggers =
      {
        camera = !camera_triggers;
        lore = !lore_triggers;
        item_pickups = !pickup_triggers;
        cutscene = !cutscene_triggers;
        shadows = !shadow_triggers;
        levers = !lever_triggers;
      };
    layers = tile_layers;
    enemies = List.map (fun (e : enemy) -> (e.id, e)) enemies;
    npcs;
    pickup_indicators = get_pickup_indicators room_progress params.pickup_indicator_texture !pickup_triggers;
    cache;
  }

let handle_transitions (state : state) (game : game) =
  let get_global_pos (current : vector) (room_location : room_location) : vector =
    { x = current.x +. room_location.global_x; y = current.y +. room_location.global_y }
  in
  let get_local_pos (global : vector) (room_id : room_id) (world : world) : vector =
    let room_location = List.assoc room_id world in
    { x = global.x -. room_location.global_x; y = global.y -. room_location.global_y }
  in
  let colliding exit_rect =
    (* TODO this might not be working sometimes with the new collision detection *)
    Collision.with_entity game.ghost.entity exit_rect
  in
  match List.find_map colliding game.room.exits with
  | None -> false
  | Some collision ->
    if collision.rect.w < 10. || collision.rect.h < 10. then
      (* don't trigger the exit immediately when the ghost hits the edge of the screen *)
      false
    else (
      let cr = collision.rect in
      let current_room_location = List.assoc game.room.id state.world in
      let ghost_pos = game.ghost.entity.dest.pos in
      let global_ghost_pos = get_global_pos ghost_pos current_room_location in
      let (path, target_room_id) : string * room_id =
        let global_x, global_y =
          ( cr.pos.x +. (cr.w /. 2.) +. current_room_location.global_x,
            cr.pos.y +. (cr.h /. 2.) +. current_room_location.global_y )
        in
        Tiled.Room.locate state.world global_x global_y
      in
      let room_location = List.assoc target_room_id state.world in
      let exits = Tiled.Room.get_exits room_location in
      let start_pos' = get_local_pos global_ghost_pos target_room_id state.world in
      let start_pos : vector =
        (* fixes ghost.facing_right, and adjusts the ghost to be further from the edge of screen
           - TODO I think this is broken when exiting a room below (falling)
           - the ghost can pretty easily fall through the floor at the mama-mahogany cutscene that happens right after room transition
        *)
        match collision.direction with
        | LEFT ->
          game.ghost.entity.sprite.facing_right <- true;
          { start_pos' with x = start_pos'.x +. game.ghost.entity.dest.w }
        | RIGHT ->
          game.ghost.entity.sprite.facing_right <- false;
          { start_pos' with x = start_pos'.x -. game.ghost.entity.dest.w }
        | UP -> { start_pos' with y = start_pos'.y +. game.ghost.entity.dest.h }
        | DOWN -> { start_pos' with y = start_pos'.y -. game.ghost.entity.dest.h }
      in

      save_progress game;

      let new_room =
        init
          {
            file_name = path;
            progress = game.progress;
            exits;
            enemy_configs = state.global.enemy_configs;
            npc_configs = state.global.npc_configs;
            pickup_indicator_texture = state.global.textures.pickup_indicator;
            lever_texture = state.global.textures.door_lever;
          }
      in
      game.ghost.entity.current_floor <- None;
      game.ghost.current.wall <- None;
      game.ghost.spawned_vengeful_spirits <- [];
      game.ghost.entity.dest.pos <- start_pos;
      (* all rooms are using the same tilesets now, but still unload them here (and re-load them
         in load_room) every time because the tilesets could be in a different order per room
         - not really sure about ^this comment, I don't know if different tileset order would break the
           tile lookup code now, so just unload/reload to be safe ¯\_(ツ)_/¯
      *)
      (* TODO probably need to unload things like enemy textures *)
      Tiled.Room.unload_tilesets game.room;
      game.room <- new_room;
      game.room.layers <- Tiled.Room.get_layer_tile_groups game.room game.room.progress.removed_idxs_by_layer;
      state.camera.raylib <- Tiled.create_camera_at (Raylib.Vector2.create start_pos.x start_pos.y) 0.;
      true)
