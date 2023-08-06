open Types

[@@@ocaml.warning "-26-27-32"]

let get_pickup_indicators
    (room_progress : Json_t.room_progress)
    (texture : texture)
    (triggers : trigger list) : sprite list =
  let show_obj (trigger : trigger) : sprite option =
    if List.mem trigger.full_name room_progress.finished_interactions then
      None
    else (
      let dest =
        let child_w, child_h =
          let src = get_src texture in
          (src.w *. Config.scale.room, src.h *. Config.scale.room)
        in
        let parent_w, parent_h = (trigger.dest.w, trigger.dest.h) in
        let parent_x, parent_y = (trigger.dest.pos.x, trigger.dest.pos.y) in
        {
          pos =
            {
              x = parent_x +. ((parent_w -. child_w) /. 2.);
              y = parent_y +. ((parent_h -. child_h) /. 2.);
            };
          w = child_w;
          h = child_h;
        }
      in
      Some (Sprite.create "indicator" texture dest))
  in
  List.filter_map show_obj triggers

let update_pickup_indicators (state : state) (game : game) =
  game.room.pickup_indicators <-
    get_pickup_indicators game.room.progress state.global.textures.pickup_indicator
      game.room.triggers.item_pickups

let save_progress (game : game) =
  let room_uuid = Tiled.Room.get_filename game.room in
  game.progress <- Utils.replace_assoc room_uuid game.room.progress game.progress

type room_params = {
  file_name : string;
  progress : (string * Json_t.room_progress) list;
  exits : rect list;
  enemy_configs : (enemy_id * Json_t.enemy_config) list;
  npc_configs : (npc_id * Json_t.npc_config) list;
  pickup_indicator_texture : texture;
  lever_texture : texture;
  respawn_pos : vector;
  platforms : (string * texture) list;
}

let init (params : room_params) : room =
  (* TODO sometimes this function gets called when area/room kinds are already known, so this lookup is redundant *)
  let (area_id, room_id) : area_id * room_id =
    Tiled.parse_room_filename "init_room" params.file_name
  in
  let room_key = Tiled.Room.get_filename' area_id room_id in
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
  let platforms : platform list ref = ref [] in
  let floors : rect list ref = ref [] in
  let platform_spikes : (int * rect) list ref = ref [] in
  let conveyor_belts : (rect * float) list ref = ref [] in
  let spikes : rect list ref = ref [] in
  let acid : rect list ref = ref [] in
  let hazards : rect list ref = ref [] in
  let idx_configs : (int * idx_config) list ref = ref [] in
  let camera_triggers : trigger list ref = ref [] in
  let lever_triggers : (sprite * int * trigger) list ref = ref [] in
  let shadow_triggers : trigger list ref = ref [] in
  let lore_triggers : trigger list ref = ref [] in
  let d_nail_triggers : trigger list ref = ref [] in
  let item_pickup_triggers : trigger list ref = ref [] in
  let cutscene_triggers : trigger list ref = ref [] in
  let respawn_triggers : (vector * trigger) list ref = ref [] in
  let enemy_rects : (enemy_id * rect) list ref = ref [] in
  let npc_rects : (npc_id * rect * bool) list ref = ref [] in
  let floor_rect rect =
    {
      pos = { x = rect.pos.x |> Float.floor; y = rect.pos.y |> Float.floor };
      w = rect.w |> Float.floor;
      h = rect.h |> Float.floor;
    }
  in

  let tilesets_by_path = Tiled.load_tilesets json_room in

  let get_object_rects (jl : Json_t.layer) =
    let get_object_rect ?(floor = false) ?(hidden = false) name (coll_rect : Json_t.coll_rect) =
      let rect = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      if floor then
        (name, floor_rect rect)
      else if hidden then
        (name, { rect with pos = { x = -1. *. rect.pos.x; y = -1. *. rect.pos.y } })
      else
        (name, rect)
    in

    let make_floor idx (coll_rect : Json_t.coll_rect) =
      let dest = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      floors := dest :: !floors
    in

    let make_spikes idx (coll_rect : Json_t.coll_rect) =
      let dest = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      spikes := dest :: !spikes
    in

    let make_hazard idx (coll_rect : Json_t.coll_rect) =
      let dest = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      hazards := dest :: !hazards
    in

    let make_acid idx (coll_rect : Json_t.coll_rect) =
      let dest = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      acid := dest :: !acid
    in

    let make_conveyor_belt idx (coll_rect : Json_t.coll_rect) =
      let dest = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      conveyor_belts := (dest, coll_rect.name |> float_of_string) :: !conveyor_belts
    in

    let make_platform idx (coll_rect : Json_t.coll_rect) =
      (* CLEANUP duplicated *)
      let texture_name, texture, platform_kind =
        Tiled.Room.look_up_platform json_room params.platforms coll_rect.gid
      in
      let dest = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      let sprite : sprite =
        {
          ident = fmt "%s_%d" texture_name idx;
          dest = floor_rect dest;
          texture;
          facing_right = true;
          (* this could be Some DEST, but the rects are being checked directly like other floors *)
          collision = None;
        }
      in
      (match platform_kind with
      | Some (ROTATABLE _) ->
        let dest =
          Tiled.scale_rect (coll_rect.x -. 1.)
            (coll_rect.y +. (coll_rect.h /. 2.))
            (coll_rect.w +. 2.) (coll_rect.h *. 0.55)
        in
        platform_spikes := (idx, dest) :: !platform_spikes
      | _ -> ());
      platforms :=
        { id = idx; original_x = coll_rect.x *. Config.scale.room; sprite; kind = platform_kind }
        :: !platforms
    in

    let categorize_trigger (coll_rect : Json_t.coll_rect) =
      let name_prefix', name_suffix = Utils.split_at_first ':' coll_rect.name in
      let (blocking_interaction, name_prefix) : string option * string =
        Utils.split_at_first_opt '|' name_prefix'
      in

      let get_object_trigger ?(floor = false) ?(hidden = false) ?(label = None) kind : trigger =
        let rect = Tiled.scale_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
        let dest : rect =
          if floor then
            (* these were preventing the pixels in the background from being distorted
               TODO see if that still happens
            *)
            {
              pos = { x = rect.pos.x |> Float.floor; y = rect.pos.y |> Float.floor };
              w = rect.w |> Float.floor;
              h = rect.h |> Float.floor;
            }
          else if hidden then
            { rect with pos = { x = -1. *. rect.pos.x; y = -1. *. rect.pos.y } }
          else
            rect
        in
        {
          full_name = coll_rect.name;
          name_prefix;
          name_suffix;
          kind;
          dest;
          label;
          blocking_interaction;
        }
      in

      let tile_idx () =
        (* this fn finds the tile_idx that the trigger object's top-left corner is in, so trigger objects that are
           used like this (purple-pen, door-health) don't need to be placed exactly at that tile's coordinates *)
        Tiled.Room.tile_idx json_room (coll_rect.x, coll_rect.y)
      in
      let add_idx_config config = idx_configs := (tile_idx (), config) :: !idx_configs in

      let parse_warp_target name : warp_target =
        let room_name, coords = Utils.split_at_first '@' name in
        let pos = Tiled.Room.dest_from_coords' json_room coords in
        { room_name; pos }
      in

      match name_prefix with
      | "camera" ->
        let x, y = Utils.split_at_first '-' name_suffix in
        camera_triggers := get_object_trigger ~floor:true (CAMERA (x, y)) :: !camera_triggers
      | "lever" ->
        let direction', door_coords' = Utils.split_at_first '-' name_suffix in
        let direction, transformation_bits =
          match direction' with
          | "up" -> (UP, 0)
          | "down" -> (DOWN, 2)
          | "left"
          | "right" ->
            failwithf "horizontal levers aren't supported" direction'
          | _ -> failwithf "unknown direction '%s' in get_transformation_bits" direction'
        in
        let x', y' = Utils.split_at_first ',' door_coords' in
        let door_tile_idx =
          Tiled.Tile.tile_idx_from_coords ~width:json_room.w_in_tiles
            (x' |> float_of_string, y' |> float_of_string)
        in
        let lever_sprite : sprite =
          let shape =
            make_shape
              [
                (* TODO move to config *)
                { x = 10.; y = 0. };
                { x = 28.; y = 0. };
                { x = 28.; y = 83. };
                { x = 10.; y = 83. };
              ]
          in
          {
            ident = fmt "Sprite %s" name_suffix;
            dest =
              Sprite.make_dest
                (coll_rect.x *. Config.scale.room)
                (coll_rect.y *. Config.scale.room)
                params.lever_texture;
            texture = params.lever_texture;
            collision = Some (SHAPE shape);
            facing_right = true;
          }
        in
        lever_triggers :=
          ( lever_sprite,
            transformation_bits,
            {
              full_name = coll_rect.name;
              name_prefix;
              name_suffix;
              kind = LEVER { direction; door_tile_idx };
              dest = lever_sprite.dest;
              label = None;
              blocking_interaction = None;
            } )
          :: !lever_triggers
      | "door-health" ->
        let door_health =
          (* this uses the object height, eg most breakable walls take 4 hits to destroy, so the door-health rects are 4 pixels high *)
          coll_rect.h |> Float.to_int
        in
        add_idx_config (DOOR_HITS door_health)
      | "purple-pen" -> add_idx_config (PURPLE_PEN name_suffix)
      | "hide" -> shadow_triggers := get_object_trigger SHADOW :: !shadow_triggers
      | "warp" ->
        let target = parse_warp_target name_suffix in
        lore_triggers := get_object_trigger ~label:(Some "Enter") (WARP target) :: !lore_triggers
      | "info" -> lore_triggers := get_object_trigger ~label:(Some "Read") INFO :: !lore_triggers
      | "health" ->
        lore_triggers := get_object_trigger ~label:(Some "Read") HEALTH :: !lore_triggers
      | "d-nail-item" -> d_nail_triggers := get_object_trigger D_NAIL :: !item_pickup_triggers
      | "ability"
      | "weapon"
      | "dreamer" ->
        item_pickup_triggers :=
          get_object_trigger ~label:(Some "Pick up") ITEM :: !item_pickup_triggers
      | "npc" ->
        (* TODO this should have "Talk" interaction_label *)
        let npc_id, dest =
          get_object_rect
            (Npc.parse_name (fmt "Tiled rect npc_%s" name_suffix) name_suffix)
            coll_rect
        in
        npc_rects := (npc_id, dest, true) :: !npc_rects
      | "mirrored-npc" ->
        let npc_id, dest =
          get_object_rect
            (Npc.parse_name (fmt "Tiled rect hidden-npc_%s" name_suffix) name_suffix)
            coll_rect
        in
        npc_rects := (npc_id, dest, false) :: !npc_rects
      | "hidden-npc" ->
        let npc_id, dest =
          get_object_rect ~hidden:true
            (Npc.parse_name (fmt "Tiled rect hidden-npc_%s" name_suffix) name_suffix)
            coll_rect
        in
        npc_rects := (npc_id, dest, true) :: !npc_rects
      | "enemy" ->
        enemy_rects :=
          get_object_rect
            (Enemy.parse_name (fmt "Tiled rect enemy_%s" name_suffix) name_suffix)
            coll_rect
          :: !enemy_rects
      | "hidden-enemy" ->
        enemy_rects :=
          get_object_rect ~hidden:true
            (Enemy.parse_name (fmt "Tiled rect hidden-enemy_%s" name_suffix) name_suffix)
            coll_rect
          :: !enemy_rects
      | "respawn" ->
        (* TODO try using connected objects instead of parsing coordinates from the name *)
        let respawn_pos = Tiled.Room.dest_from_coords' json_room name_suffix in
        respawn_triggers := (respawn_pos, get_object_trigger RESPAWN) :: !respawn_triggers
      | "door-warp" ->
        let target = parse_warp_target name_suffix in
        cutscene_triggers := get_object_trigger (WARP target) :: !cutscene_triggers
      | "cutscene" -> cutscene_triggers := get_object_trigger CUTSCENE :: !cutscene_triggers
      | _ ->
        failwithf
          "init_room invalid interaction name '%s' - needs to start with 'camera_', 'health_', \
           'cutscene_', etc."
          coll_rect.name
    in
    match jl with
    | `OBJECT_LAYER json -> (
      match json.name with
      | "spikes" -> List.iteri make_spikes json.objects
      | "acid" -> List.iteri make_acid json.objects
      | "hazard" -> List.iteri make_hazard json.objects
      | "floors" -> List.iteri make_floor json.objects
      | "platforms" -> List.iteri make_platform json.objects
      | "conveyor-belts" -> List.iteri make_conveyor_belt json.objects
      | "triggers" -> List.iter categorize_trigger json.objects
      | "world-map-labels" -> ( (* only used for generating world map *) )
      | json_name -> failwithf "init_room bad object layer name: '%s'" json_name)
    | `IMAGE_LAYER _
    | `TILE_LAYER _ ->
      ()
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

  let tile_layers =
    let layers = ref [] in
    let layer_names = ref [] in
    let build_tile_layer (json_layer : Json_t.layer) =
      match json_layer with
      | `IMAGE_LAYER _
      | `OBJECT_LAYER _ ->
        ()
      | `TILE_LAYER json ->
        let add_new_layer () =
          (* TODO use this for:
             - vending machines in teacher's lounge
             - scootenanny chair
             - keys unlocking doors
             - could do something like slightly alter the corkboard when the health has already been increased
          *)
          let (layer_name', hidden) : string * bool =
            match Utils.split_at_first_opt '|' json.name with
            | Some interaction_name, layer_name -> (
              let finished name = List.mem name room_progress.finished_interactions in
              match String.get interaction_name 0 with
              | '!' -> (layer_name, not (finished (Str.string_after interaction_name 1)))
              | _ -> (layer_name, finished interaction_name))
            | _ ->
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
                  ( Str.first_chars full_name number_idx,
                    int_of_string (Str.matched_string full_name) )
                with
                | Not_found -> (full_name, 1)
              in
              (name, number)
            in
            let configs =
              match layer_name with
              | "monkey-block" -> [ "collides"; "monkey"; "permanently_removable" ]
              | "floors" -> [ "fg" ]
              (* this is used for floors that are removed, like the breaking floor in King's Pass
                 and the bridge in c-dash room *)
              | "temporary-floors" -> [ "collides" ]
              | "benches" -> [ "collides" ]
              (* these layers are still needed to render the tiles, but collisions are checked based on objects now *)
              | "hazard" -> [ "fg" ]
              (* FIXME-5 fix acid/water collisions *)
              | "acid" -> [ "animated"; "hazard" ]
              | "water" -> [ "animated"; "water"; "fg" ]
              (* TODO probably add "conveyor-belt" tiles that are animated like acid
                 - or maybe just rename "acid"->"animated" layer and use it for conveyor belts too
              *)
              | "boss-doors" -> [ "collides" ]
              | "lever-doors" -> [ "collides"; "permanently_removable" ]
              | "doors" -> [ "collides"; "destroyable"; "permanently_removable" ]
              | "bg"
              | "bg-iso"
              | "bg-iso-lava"
              | "bg-iso-walls"
              | "bg-far" ->
                [ "bg" ]
              | "fg"
              | "shadow" ->
                (* TODO maybe move this validation somewhere else *)
                (* if not (List.mem 0 json.data) then
                 *   print
                 *     "\n\n\n\n\
                 *     \ -------------------- BAD SHADOW LAYER in room %s -------------------- \n\n\n\n"
                 *     params.file_name; *)
                [ "fg" ]
              | "close-fg" -> [ "fg"; "shaded" ]
              | "fg-jugs" -> [ "fg"; "destroyable"; "pogoable" ]
              | "bg-jugs" -> [ "bg"; "destroyable"; "pogoable" ]
              | unknown -> failwithf "unknown layer name '%s' in room %s" unknown params.file_name
            in

            let build_config config_parts =
              let has name = List.mem name config_parts in
              let depth_configs = ref 0 in
              if has "bg" then incr depth_configs;
              if has "fg" then incr depth_configs;
              if has "hazard" then incr depth_configs;
              if has "collides" then incr depth_configs;
              if !depth_configs <> 1 then
                failwithf
                  (* TODO make this a variant and track it separately from the other config stuff *)
                  "bad layer config for %s: needs to have exactly one of 'bg', 'fg', 'hazard', or \
                   'collides'"
                  layer_name
              else
                {
                  render = { bg = has "bg"; fg = has "fg" };
                  collides_with_ghost = has "collides";
                  hazard = has "hazard";
                  pogoable = has "pogoable";
                  destroyable = has "destroyable";
                  permanently_removable = has "permanently_removable";
                  shaded = has "shaded";
                  animated = has "animated";
                  monkey = has "monkey";
                  water = has "water";
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
              name = layer_name';
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
          failwithf "already parsed layer '%s' - append numbers to disambiguate, eg %s2" json.name
            json.name
        else
          layer_names := json.name :: !layer_names;
        let reference_layer () = String.starts_with ~prefix:"ref:" json.name in
        let auto_layer () = String.starts_with ~prefix:"auto:" json.name in
        if json.name = "world-map" || reference_layer () || auto_layer () then
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
    | Some tileset ->
      let jug_tileset_img = Tiled.Tileset.image tileset in

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
          Sprite.build_texture_from_image ~scale:Config.scale.room jug_tileset_img
            (Some { pos = { x; y }; w; h })
        in
        let sprite =
          Sprite.create
            (fmt "fragment sprite %s %0.1f" name y_offset)
            texture
            { pos = { x; y }; w = w *. Config.scale.room; h = h *. Config.scale.room }
        in
        let entity =
          Entity.create_for_sprite sprite ~inanimate:true
            { pos = Zero.vector (); w = sprite.dest.w; h = sprite.dest.h }
        in
        entity
      in

      (* FIXME-3 update for new tile size
         - width will definitely be wrong - maybe update the configs, but maybe just multiply by 2 here
         - fragments may be messed up
      *)
      let make_jug (config : jug_config) : int * jug_fragments =
        let fragments : entity list =
          let tile_x = config.tile_x in
          let tile_y =
            (* always render a stub, even if it is an empty tile image *)
            config.h + 1
          in
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
          failwithf
            "found 0 fragments for jug %s with tile_x %d - configure these with Tiled collision \
             editor"
            config.jug_name config.tile_x;
        (* x is the 0-indexed tile coordinate on the jugs.png image, but we need to add jug_firstgid to adjust the cache key *)
        ( config.tile_x + jug_firstgid,
          { stub = make_stub (config.w |> Int.to_float) config.tile_x config.h; fragments } )
      in

      let metadata : jug_config list =
        let configs : Json_t.jug_metadata_file =
          File.read_config "jugs" Json_j.jug_metadata_file_of_string
        in
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
      | OUTLANDS -> (* FIXME  *) Raylib.Color.create 121 121 40 255
      | COMPUTER_WING -> Raylib.Color.create 63 93 57 255
      | AC_REPAIR_ANNEX -> Raylib.Color.create 83 129 129 255
      | VENTWAYS -> Raylib.Color.create 129 129 129 255
      | LIBRARY -> Raylib.Color.create 2 89 2 255
      | FINAL -> failwithf "area_id not configured yet: %s" (Show.area_id area_id)
    in
    {
      id = area_id;
      tint;
      bg_color =
        Raylib.Color.create
          (Raylib.Color.r tint / 7)
          (Raylib.Color.g tint / 7)
          (Raylib.Color.b tint / 7)
          255;
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
    respawn_pos = clone_vector params.respawn_pos;
    platforms = !platforms;
    floors = !floors;
    triggers =
      {
        camera = !camera_triggers;
        cutscene = !cutscene_triggers;
        d_nail = !d_nail_triggers;
        item_pickups = !item_pickup_triggers;
        levers = !lever_triggers;
        lore = !lore_triggers;
        respawn = !respawn_triggers;
        shadows = !shadow_triggers;
      };
    spikes = !spikes;
    platform_spikes = !platform_spikes;
    conveyor_belts = !conveyor_belts;
    acid = !acid;
    hazards = !hazards;
    layers = tile_layers;
    enemies = List.map (fun (e : enemy) -> (e.id, e)) enemies;
    npcs;
    loose_projectiles = [];
    pickup_indicators =
      get_pickup_indicators room_progress params.pickup_indicator_texture !item_pickup_triggers;
    interaction_label = None;
    cache;
  }

let change_current_room
    (state : state)
    (game : game)
    (room_location : room_location)
    (ghost_start_pos : vector) =
  let exits = Tiled.Room.get_exits room_location in
  save_progress game;

  let new_room =
    init
      {
        file_name = room_location.filename;
        progress = game.progress;
        exits;
        enemy_configs = state.global.enemy_configs;
        npc_configs = state.global.npc_configs;
        pickup_indicator_texture = state.global.textures.pickup_indicator;
        lever_texture = state.global.textures.door_lever;
        respawn_pos = ghost_start_pos;
        platforms = state.global.textures.platforms;
      }
  in
  game.ghost.entity.current_floor <- None;
  game.ghost.current.wall <- None;
  game.ghost.spawned_vengeful_spirits <- [];
  (* FIXME-8 seems like handle_transitions is finding the correct room, but spawning at the wrong location
     - problem is only when global_x is in a certain range
     - transition right of kp doesn't work
     - neither does transition right to resting grounds
     - but, transition right into blue lake _does_ work, even though it is further to the right than both of those
     - maybe it is area-based, because transitioning right out of blue lake into trampolinepath does break
     -- seems like this is it - moved (infected) resting grounds to one of the forgotten rooms and it broke there
  *)
  tmp "got ghost_start_pos: %s" (Show.vector ghost_start_pos);
  game.ghost.entity.dest.pos <- ghost_start_pos;
  (* game.ghost.entity.dest.pos <- { x = ghost_start_pos.x *. 2.; y = ghost_start_pos.y *. 2.}; *)
  (* all rooms are using the same tilesets now, but still unload them here (and re-load them
     in load_room) every time because the tilesets could be in a different order per room
     - not really sure about ^this comment, I don't know if different tileset order would break the
       tile lookup code now, so just unload/reload to be safe ¯\_(ツ)_/¯
  *)
  (* TODO probably need to unload things like enemy textures *)
  Tiled.Room.unload_tilesets game.room;
  game.room <- new_room;
  game.room.layers <-
    Tiled.Room.get_layer_tile_groups game.room game.room.progress.removed_idxs_by_layer;
  state.camera.update_instantly <- true;
  state.camera.raylib <-
    Tiled.create_camera_at (Raylib.Vector2.create ghost_start_pos.x ghost_start_pos.y) 0.

let get_global_pos (current_pos : vector) (room_location : room_location) : vector =
  { x = current_pos.x +. room_location.global_x; y = current_pos.y +. room_location.global_y }

let handle_transitions (state : state) (game : game) =
  let get_local_pos (global : vector) (room_id : room_id) (world : world) : vector =
    let room_location = List.assoc room_id world in
    tmp "handling transition with room_location: w %f, h %f, global x %f, y %f" room_location.w
      room_location.h room_location.global_x room_location.global_y;
    tmp "global x %f, y %f" global.x global.y;

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
      let global_ghost_pos = get_global_pos game.ghost.entity.dest.pos current_room_location in
      let target_room_id, room_location =
        let global_x, global_y =
          ( cr.pos.x +. (cr.w /. 2.) +. current_room_location.global_x,
            cr.pos.y +. (cr.h /. 2.) +. current_room_location.global_y )
        in
        Tiled.Room.locate_by_coords state.world global_x global_y
      in
      let start_pos' = get_local_pos global_ghost_pos target_room_id state.world in
      tmp "got start_pos': %s" (Show.vector start_pos');
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

      change_current_room state game room_location start_pos;
      true)
