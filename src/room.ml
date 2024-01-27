open Utils
open Types

let scale_room_rect x y w h = scale_rect Config.scale.room { pos = { x; y }; w; h }
let in_teachers_archives (room : room) = room.id = LIB_A || room.id = LIB_B
let show_ghost_on_map (room : room) = not (in_teachers_archives room || room.id = VENT_HUB)

let get_filename_from_ids area_id room_id : string =
  fmt "%s_%s" (Show.area_id area_id) (Show.room_id_filename room_id)

let get_filename (room : room) : string = get_filename_from_ids room.area.id room.id

(* room.progress keeps track of the current (unsaved) progress for the room, and gets saved
   into game.progress on room transition or on game save *)
let save_progress_to_game (game : game) =
  let room_uuid = get_filename game.room in
  game.progress.by_room <- List.replace_assoc room_uuid game.room.progress game.progress.by_room

let init (params : room_params) : room =
  (* TODO sometimes this function gets called when area/room kinds are already known, so this
     lookup is redundant *)
  let (area_id, room_id) : area_id * room_id =
    Tiled.parse_room_filename "Room.init" params.file_name
  in
  let room_key = get_filename_from_ids area_id room_id in
  let new_room_progress () : Json_t.room_progress =
    {
      finished_interactions = [];
      revealed_shadow_layers = [];
      removed_tile_idxs = [];
      removed_platform_ids = [];
    }
  in
  let room_progress =
    match String.Map.find_opt room_key params.progress_by_room with
    | None -> new_room_progress ()
    | Some rp -> clone_room_progress rp
  in
  let parse_room (path : string) : Json_t.room =
    let full_path = File.make_assets_path [ "tiled"; "rooms"; path ] in
    (* TODO cache these instead of reloading the tileset between every room *)
    File.read full_path |> Json_j.room_of_string
  in

  let json_room = parse_room (fmt "%s.json" params.file_name) in
  let platforms : platform list ref = ref [] in
  let floors : rect list ref = ref [] in
  let platform_spikes : (string * rect) list ref = ref [] in
  let conveyor_belts : (rect * float) list ref = ref [] in
  let spikes : rect list ref = ref [] in
  let hazards : rect list ref = ref [] in
  let idx_configs : (int * idx_config) list ref = ref [] in
  let camera_triggers : trigger list ref = ref [] in
  let lever_triggers : lever list ref = ref [] in
  let shadow_triggers : trigger list ref = ref [] in
  let lore_triggers : trigger list ref = ref [] in
  let d_nail_triggers : trigger list ref = ref [] in
  let cutscene_triggers : trigger list ref = ref [] in
  let respawn_triggers : respawn_trigger list ref = ref [] in
  let targets : (int * vector) list ref = ref [] in
  let enemy_rects : (enemy_id * rect) list ref = ref [] in
  let boss_area : rect option ref = ref None in
  let npc_rects : (npc_id * rect * bool) list ref = ref [] in

  let tilesets_by_path = Tiled.load_tilesets json_room in

  let get_object_rects (json_layer : Json_t.layer) =
    let get_object_rect ?(hidden = false) (coll_rect : Json_t.coll_rect) =
      let rect = scale_room_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      if hidden then
        { rect with pos = { x = -1. *. rect.pos.x; y = -1. *. rect.pos.y } }
      else
        rect
    in

    let add_object_rect (rects : 'a list ref) (coll_rect : Json_t.coll_rect) =
      let dest = scale_room_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      rects := dest :: !rects
    in

    let make_conveyor_belt idx (coll_rect : Json_t.coll_rect) =
      let dest = scale_room_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      conveyor_belts :=
        (dest, (coll_rect.name |> float_of_string) *. Config.window.scale) :: !conveyor_belts
    in

    let make_platform idx (coll_rect : Json_t.coll_rect) =
      let texture_name, texture, platform_kind =
        Tiled.JsonRoom.look_up_platform json_room params.platforms coll_rect.gid
      in
      let dest = scale_room_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      let sprite : sprite =
        {
          ident = fmt "%s_%d" texture_name idx;
          dest;
          texture;
          facing_right = true;
          (* this could be Some DEST, but the rects are being checked directly like other floors *)
          collision = None;
        }
      in
      let platform_id = fmt "%d,%d" (coll_rect.x |> Float.to_int) (coll_rect.y |> Float.to_int) in
      (match platform_kind with
      | Some (ROTATABLE _) ->
        let spikes_dest =
          scale_room_rect (coll_rect.x -. 1.)
            (coll_rect.y +. (coll_rect.h /. 2.))
            (coll_rect.w +. 2.) (coll_rect.h *. 0.55)
        in
        platform_spikes := (platform_id, spikes_dest) :: !platform_spikes
      | None
      | Some (LOCKED_DOOR _)
      | Some (TEMPORARY _)
      | Some (DISAPPEARABLE _) ->
        ());
      if not (List.mem platform_id room_progress.removed_platform_ids) then
        platforms :=
          {
            id = platform_id;
            original_x = coll_rect.x *. Config.scale.room;
            sprite;
            kind = platform_kind;
          }
          :: !platforms
    in

    let collect_targets (coll_rect : Json_t.coll_rect) =
      let rect = scale_room_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in
      match coll_rect.name with
      | "target" -> targets := (coll_rect.id, { x = coll_rect.x; y = coll_rect.y }) :: !targets
      | _ -> ()
    in

    let categorize_trigger (coll_rect : Json_t.coll_rect) =
      let split s =
        match String.split_at_first_opt ':' s with
        | None, _ -> (s, "")
        | Some prefix, rest -> (prefix, rest)
      in
      let name_prefix', name_suffix = split coll_rect.name in
      let (blocking_interaction, name_prefix) : string option * string =
        String.split_at_first_opt '|' name_prefix'
      in
      let rect = scale_room_rect coll_rect.x coll_rect.y coll_rect.w coll_rect.h in

      let get_followup_trigger trigger_name kind : trigger =
        let name_prefix, name_suffix = split trigger_name in
        {
          full_name = trigger_name;
          name_prefix;
          name_suffix;
          kind;
          dest = rect;
          label = None;
          blocking_interaction;
        }
      in

      let get_object_trigger ?(hidden = false) ?(label = None) kind : trigger =
        let dest : rect =
          if hidden then
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
        Tiled.JsonRoom.tile_idx json_room (coll_rect.x, coll_rect.y)
      in
      let add_idx_config config = idx_configs := (tile_idx (), config) :: !idx_configs in

      let parse_warp_target name : warp_target =
        let room_name, coords = String.split_at_first '@' name in
        let target = coords |> Tiled.JsonRoom.coords_to_dest json_room in
        { room_name; target }
      in

      let get_target_ids (coll_rect : Json_t.coll_rect) =
        List.Non_empty.map
          (fun (target : Json_t.connected_object) -> target.id)
          (coll_rect.targets |> List.to_ne_list)
      in

      match name_prefix with
      | "camera" ->
        let x, y = String.split_at_first ',' name_suffix in
        camera_triggers := get_object_trigger (CAMERA (x, y)) :: !camera_triggers
      | "lever" ->
        (* y_offset is to align the lever with the floor, so the trigger rect
           doesn't need to be the exact size of the lever
        *)
        let direction, transformation, y_offset =
          match name_suffix with
          | "up" ->
            let lever_h = (get_src params.lever_texture).h in
            (UP, 0, lever_h -. coll_rect.h)
          | "down" -> (DOWN, 2, 0.)
          | "left"
          | "right" ->
            failwithf "horizontal levers aren't supported (%s)" name_suffix
          | _ -> failwithf "got 'lever:%s', needs to be lever:up or lever:down" name_suffix
        in
        let target_ids : int ne_list = get_target_ids coll_rect in
        let door_tile_idxs : int ne_list =
          let to_tile_idx ((target_id, target_pos) : int * vector) =
            let to_idx pos =
              pos
              |> Tiled.Tile.pos_to_coords
              |> Tiled.Tile.coords_to_idx ~width:json_room.w_in_tiles
            in
            if List.Non_empty.mem target_id target_ids then
              Some (target_pos |> to_idx)
            else
              None
          in
          List.filter_map to_tile_idx !targets |> List.to_ne_list
        in
        let lever_sprite : sprite =
          let dest =
            Sprite.make_dest ~scale:Config.scale.room
              (coll_rect.x *. Config.scale.room)
              ((coll_rect.y -. y_offset) *. Config.scale.room)
              params.lever_texture
          in
          {
            ident = fmt "Sprite %s" name_suffix;
            dest;
            texture = params.lever_texture;
            collision = Some (SHAPE Config.lever_shape);
            facing_right = true;
          }
        in
        lever_triggers :=
          {
            sprite = lever_sprite;
            transformation;
            door_tile_idxs;
            trigger =
              {
                full_name = coll_rect.name;
                name_prefix;
                name_suffix;
                kind = LEVER;
                dest = lever_sprite.dest;
                label = None;
                blocking_interaction = None;
              };
          }
          :: !lever_triggers
      | "door-health" ->
        let door_health = name_suffix |> String.to_int in
        add_idx_config (DOOR_HITS door_health)
      | "purple-pen" ->
        let suffix', followup_trigger =
          match String.split_at_first_opt '+' name_suffix with
          | None, _ -> (name_suffix, None)
          | Some suffix', new_trigger_name ->
            (suffix', Some (get_followup_trigger new_trigger_name FOLLOWUP))
        in
        add_idx_config (PURPLE_PEN (suffix', followup_trigger))
      | "hide" -> shadow_triggers := get_object_trigger SHADOW :: !shadow_triggers
      | "warp" ->
        let target = parse_warp_target name_suffix in
        (* TODO "lore_triggers" probably isn't a good name for this since warping isn't lore
           - maybe "interactable_triggers" (since now some triggers aren't interactable)
        *)
        lore_triggers := get_object_trigger ~label:(Some "Enter") (WARP target) :: !lore_triggers
      | "info" -> lore_triggers := get_object_trigger ~label:(Some "Read") INFO :: !lore_triggers
      | "talk" -> lore_triggers := get_object_trigger ~label:(Some "Talk") INFO :: !lore_triggers
      | "npc" ->
        npc_rects :=
          ( Npc.parse_name (fmt "Tiled rect npc:%s" name_suffix) name_suffix,
            get_object_rect coll_rect,
            true )
          :: !npc_rects
      | "mirrored-npc" ->
        npc_rects :=
          ( Npc.parse_name (fmt "Tiled rect mirrored-npc:%s" name_suffix) name_suffix,
            get_object_rect coll_rect,
            false )
          :: !npc_rects
      | "hidden-npc" ->
        npc_rects :=
          ( Npc.parse_name (fmt "Tiled rect hidden-npc:%s" name_suffix) name_suffix,
            get_object_rect ~hidden:true coll_rect,
            true )
          :: !npc_rects
      | "enemy" ->
        enemy_rects :=
          ( Enemy.parse_name (fmt "Tiled rect enemy:%s" name_suffix) name_suffix,
            get_object_rect coll_rect )
          :: !enemy_rects
      | "hidden-enemy" ->
        enemy_rects :=
          ( Enemy.parse_name (fmt "Tiled rect hidden-enemy:%s" name_suffix) name_suffix,
            get_object_rect ~hidden:true coll_rect )
          :: !enemy_rects
      | "boss-area" -> boss_area := Some (get_object_rect coll_rect)
      | "respawn" ->
        let target_ids : int ne_list = get_target_ids coll_rect in
        let targets : vector ne_list =
          List.filter_map
            (fun ((target_id, target_pos) : int * vector) ->
              if List.Non_empty.mem target_id target_ids then
                Some
                  { x = target_pos.x *. Config.scale.room; y = target_pos.y *. Config.scale.room }
              else
                None)
            !targets
          |> List.to_ne_list
        in
        respawn_triggers := { trigger = get_object_trigger RESPAWN; targets } :: !respawn_triggers
      | "target" ->
        (* targets were already processed in collect_targets *)
        ()
      | "door-warp" ->
        let target = parse_warp_target name_suffix in
        cutscene_triggers := get_object_trigger (WARP target) :: !cutscene_triggers
      | "cutscene" -> cutscene_triggers := get_object_trigger CUTSCENE :: !cutscene_triggers
      | _ ->
        failwithf "Room.init invalid interaction name '%s', prefix '%s'" coll_rect.name name_prefix
    in

    match json_layer with
    | `OBJECT_LAYER json -> (
      match json.name with
      | "spikes" -> List.iter (add_object_rect spikes) json.objects
      | "hazard" -> List.iter (add_object_rect hazards) json.objects
      | "floors" -> List.iter (add_object_rect floors) json.objects
      | "platforms" -> List.iteri make_platform json.objects
      | "conveyor-belts" -> List.iteri make_conveyor_belt json.objects
      | "respawns"
      | "triggers" ->
        (* using separate layers for "cameras" and "respawns" so the "triggers" layer is
           less cluttered *)
        List.iter collect_targets json.objects;
        List.iter categorize_trigger json.objects
      | "cameras" -> List.iter categorize_trigger json.objects
      | json_name -> failwithf "Room.init bad object layer name: '%s'" json_name)
    | `IMAGE_LAYER _
    | `TILE_LAYER _ ->
      ()
  in
  (* TODO could add some validation that all object layer names are unique *)
  List.iter get_object_rects json_room.layers;

  (* need this for on_destroy cache keys *)
  let jug_firstgid =
    let is_jug (ts : Json_t.tileset_source) =
      String.length ts.source > 10 && Str.last_chars ts.source 9 = "jugs.json"
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
          let (layer_name', hidden) : string * bool =
            match String.split_at_first_opt '|' json.name with
            | Some interaction_name, layer_name -> (
              let finished name =
                Interactions.cutscene_finished (params.progress_by_room |> String.Map.to_list) name
              in
              match interaction_name.[0] with
              | '!' -> (layer_name, finished (Str.string_after interaction_name 1))
              | _ -> (layer_name, not (finished interaction_name)))
            | _ ->
              let prefix = "hidden-" in
              if String.starts_with ~prefix json.name then
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
                    Str.matched_string full_name |> String.to_int )
                with
                | Not_found -> (full_name, 1)
              in
              (name, number)
            in
            let configs =
              match layer_name with
              | "floors" -> [ "fg" ]
              (* this is used for floors that are removed, like the bridge in c-dash room *)
              | "temporary-floors" -> [ "collides" ]
              | "benches" -> [ "collides" ]
              (* these layers are still needed to render the tiles, but collisions are checked based on objects now *)
              | "hazard" -> [ "fg" ]
              | "acid" -> [ "animated"; "hazard" ]
              | "water" -> [ "animated"; "fg" ]
              | "boss-doors" -> [ "collides" ]
              | "lever-doors" -> [ "collides"; "permanently_removable" ]
              | "doors" ->
                (* this is also used for purple pen jars
                   TODO maybe add a "jars" layer that has the same config
                *)
                [ "collides"; "destroyable"; "permanently_removable" ]
              | "bg"
              | "bg-iso"
              | "bg-iso-lava"
              | "bg-iso-walls"
              | "bg-far" ->
                [ "bg" ]
              | "fg"
              | "shadow" ->
                [ "fg" ]
              | "close-fg" -> [ "fg"; "shaded" ]
              | "fg-flowers" -> [ "fg"; "destroyable"; "silent" ]
              | "bg-flowers" -> [ "bg"; "destroyable"; "silent" ]
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
                  render = { bg = has "bg"; fg = has "fg" || has "hazard" };
                  animated = has "animated";
                  collides_with_ghost = has "collides";
                  destroyable = has "destroyable";
                  hazard = has "hazard";
                  permanently_removable = has "permanently_removable";
                  pogoable = has "pogoable";
                  shaded = has "shaded";
                  silent = has "silent";
                }
            in
            build_config configs
          in

          let destroyed_tiles =
            if String.ends_with ~suffix:"doors" json.name then
              room_progress.removed_tile_idxs
            else
              []
          in

          let data =
            (* padding zeros on right/bottom sides of grid prevents errors because  *)
            let matrix : int Matrix.t = Matrix.make json.data json.w in
            let append_zero row = Array.append row [| 0 |] in
            let padded_rows : int Matrix.t = Array.map append_zero matrix in
            let zero_row : int Matrix.t =
              [| Array.init (Array.length padded_rows.(0)) (fun _ -> 0) |]
            in
            Array.append padded_rows zero_row
          in

          layers :=
            {
              json;
              name = layer_name';
              data;
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
          failwithf "already parsed layer '%s' for room %s - append numbers to disambiguate, eg %s2"
            json.name params.file_name json.name
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
    (* this isn't using make_path because tilesets_by_path keys come from Tiled exported files *)
    match List.assoc_opt "../tilesets/jugs.json" tilesets_by_path with
    | None ->
      {
        jug_fragments_by_gid = Int.Map.empty;
        tilesets_by_path = tilesets_by_path |> List.to_string_map;
      }
    | Some tileset ->
      (* all of the `*. 2.` or `+ 2` in this function is here because jugs were originally
         written for a 24x24 tileset *)
      let make_jug (config : jug_config) : int * jug_fragments =
        let make_stub width tile_x tile_y =
          (* TODO maybe just pass x/y into Sprite.build_ functions and do the scaling in there *)
          let stub_pos = (tile_x, tile_y) |> Tiled.Tile.coords_to_pos in
          let stub_h =
            (* two tiles for some big stubs, but most are only one tile high *)
            json_room.tile_h *. 2.
          in
          Some
            (Sprite.build_texture_from_image tileset.image
               (Some { w = json_room.tile_w *. width; h = stub_h; pos = stub_pos }))
        in
        let fragments : entity list =
          let tile_x = config.tile_x in
          let tile_y =
            (* always render a stub, even if it is an empty tile image *)
            config.h + 2
          in
          let get_coll_wh id : float * float =
            (* id is (firstgid + idx) *)
            match Tiled.get_object_collision tileset.json jug_firstgid id with
            | None -> failwithf "expected one collision rect object, got 0 for gid %d" id
            | Some coll_rect -> (coll_rect.w, coll_rect.h)
          in
          let in_this_column (c : Json_t.collision) = c.id mod tileset.json.columns = tile_x in
          let build_fragment (collision_idx : int) (collision : Json_t.collision) : entity option =
            let make_fragment name y_offset w h : entity =
              let fragment_pos' = (tile_x, tile_y) |> Tiled.Tile.coords_to_pos in
              let fragment_pos = { fragment_pos' with y = fragment_pos'.y +. y_offset } in
              let texture =
                Sprite.build_texture_from_image tileset.image (Some { pos = fragment_pos; w; h })
              in
              let sprite =
                Sprite.create
                  (fmt "fragment sprite %s %0.1f" name y_offset)
                  texture
                  { pos = fragment_pos; w = w *. Config.scale.room; h = h *. Config.scale.room }
              in
              let entity =
                Entity.create_for_sprite sprite ~inanimate:true
                  { pos = Zero.vector (); w = sprite.dest.w; h = sprite.dest.h }
              in
              entity
            in
            let w, h = get_coll_wh (jug_firstgid + collision.id) in
            (* this check only supports the left column of wide jugs *)
            if in_this_column collision then (
              (* collision.id mod tileset.json.columns = tile_x then ( *)
              let fragment_y = (collision_idx |> Int.to_float) *. tileset.json.tile_h *. 2. in
              Some (make_fragment config.jug_name fragment_y w h))
            else
              None
          in
          let collisions = List.filter in_this_column tileset.json.collisions in
          let fragments = List.mapi build_fragment collisions in
          fragments |> List.filter_somes
        in

        if List.length fragments = 0 then
          failwithf
            "found 0 fragments for jug '%s' with tile_x %d - configure these with Tiled collision \
             editor"
            config.jug_name config.tile_x;
        (* x is the 0-indexed tile coordinate on the jugs.png image, but we need to add jug_firstgid to adjust the cache key *)
        let stub_y = config.h in
        ( config.tile_x + jug_firstgid,
          { stub = make_stub (config.w |> Int.to_float) config.tile_x stub_y; fragments } )
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

      {
        jug_fragments_by_gid = List.map make_jug metadata |> List.to_int_map;
        tilesets_by_path = tilesets_by_path |> List.to_string_map;
      }
  in

  let create_camera_bounds (room : Json_t.room) =
    let w = (room.w_in_tiles |> Int.to_float) *. room.tile_w *. Config.scale.room in
    let h = (room.h_in_tiles |> Int.to_float) *. room.tile_h *. Config.scale.room in
    {
      min = { x = Config.window.center.x; y = Config.window.center.y };
      max = { x = w -. Config.window.center.x; y = h -. Config.window.center.y };
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
      | OUTLANDS -> Raylib.Color.create 121 121 40 255
      | COMPUTER_WING -> Raylib.Color.create 63 93 57 255
      | AC_REPAIR_ANNEX -> Raylib.Color.create 83 129 129 255
      | VENTWAYS -> Raylib.Color.create 129 129 129 255
      | LIBRARY -> Raylib.Color.create 162 89 82 255
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

  let raindrops =
    match area.id with
    | CITY_OF_CHAIRS ->
      let texture = params.raindrop_texture in
      let w, h =
        ( Config.other.raindrop_scale *. (Raylib.Texture.width texture.image |> Int.to_float),
          Config.other.raindrop_scale *. (Raylib.Texture.height texture.image |> Int.to_float) )
      in
      [
        Sprite.create "raindrop 1" texture { pos = { x = 0.; y = -.h }; w; h };
        Sprite.create "raindrop 2" texture { pos = { x = w; y = -.h }; w; h };
        Sprite.create "raindrop 3" texture { pos = { x = 0.; y = 0. }; w; h };
        Sprite.create "raindrop 4" texture { pos = { x = w; y = 0. }; w; h };
        Sprite.create "raindrop 5" texture { pos = { x = 0.; y = h }; w; h };
        Sprite.create "raindrop 6" texture { pos = { x = w; y = h }; w; h };
      ]
    | AC_REPAIR_ANNEX
    | BASEMENT
    | COMPUTER_WING
    | FORGOTTEN_CLASSROOMS
    | INFECTED_CLASSROOMS
    | MEOW_MEOW_BEENZ
    | OUTLANDS
    | TRAMPOLINEPATH
    | VENTWAYS
    | LIBRARY ->
      []
  in

  {
    area;
    id = room_id;
    json = json_room;
    progress = room_progress;
    idx_configs = !idx_configs;
    camera_bounds = create_camera_bounds json_room;
    exits = params.exits;
    respawn = { in_trigger_now = false; target = clone_vector params.respawn_pos };
    platforms = !platforms;
    floors = !floors;
    triggers =
      {
        camera = !camera_triggers;
        cutscene = !cutscene_triggers;
        d_nail = !d_nail_triggers;
        levers = !lever_triggers;
        lore = !lore_triggers;
        respawns = !respawn_triggers;
        shadows = !shadow_triggers;
      };
    spikes = !spikes;
    platform_spikes = !platform_spikes |> List.to_string_map;
    conveyor_belts = !conveyor_belts;
    hazards = !hazards;
    layers = tile_layers;
    enemies;
    boss_area = !boss_area;
    npcs;
    loose_projectiles = [];
    interaction_label = None;
    cache;
    raindrops;
  }

let reset_tile_groups (room : room) =
  let get_layer_tile_groups ?(debug = false) (room : room) (removed_door_idxs : int list) :
      layer list =
    let get_rectangle_tile_groups (json_layer : Json_t.tile_layer) (layer_name : string) :
        tile_group list =
      let tile_w, tile_h = (room.json.tile_w, room.json.tile_h) in
      let tile_groups : tile_group list ref =
        (* keeps track of gid indexes too so they can be not rendered after they are destroyed *)
        ref []
      in
      let available_idxs : bool ref array = Array.make (List.length json_layer.data) (ref true) in
      let removed_idxs =
        if String.ends_with ~suffix:"doors" layer_name then
          removed_door_idxs
        else
          []
      in
      let partition_rects idx tile_gid =
        if not (List.mem idx removed_idxs) then (
          let x, y =
            Tiled.Tile.dest_xy json_layer.offset_x json_layer.offset_y tile_w tile_h idx
              json_layer.w
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
                List.map
                  (fun idx -> Tiled.Tile.raw_gid (List.nth json_layer.data idx))
                  (!idxs |> List.uniq)
                |> List.uniq
              in
              let keys = Int.Map.to_list room.cache.jug_fragments_by_gid |> List.map fst in
              match List.find_opt (fun raw_gid -> List.mem raw_gid keys) all_raw_gids with
              | None -> (None, [])
              | Some raw_gid -> (
                match Int.Map.find_opt raw_gid room.cache.jug_fragments_by_gid with
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
              transformation_bits = Tiled.Tile.transformation_bits tile_gid;
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
          || layer.config.destroyable
          || layer.config.hazard
          || layer.name = "water")
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
  in
  room.layers <- get_layer_tile_groups room room.progress.removed_tile_idxs

let change_current_room
    (state : state)
    (game : game)
    (room_location : room_location)
    (ghost_start_pos : vector) =
  let exits = Tiled.JsonRoom.get_exits room_location in
  save_progress_to_game game;

  let new_room : room =
    init
      {
        file_name = room_location.filename;
        progress_by_room = game.progress.by_room |> List.to_string_map;
        exits;
        enemy_configs = state.global.enemy_configs;
        npc_configs = state.global.npc_configs;
        pickup_indicator_texture = state.global.textures.pickup_indicator;
        lever_texture = state.global.textures.door_lever;
        raindrop_texture = state.global.textures.raindrop;
        respawn_pos = ghost_start_pos;
        platforms = state.global.textures.platforms;
      }
  in
  let hide_party_ghosts () =
    let hide_party_ghost (party_ghost : party_ghost) = Entity.hide party_ghost.ghost.entity in
    List.iter hide_party_ghost game.party
  in
  let unload_tilesets (room : room) : unit =
    let unload_tileset _path tileset = Raylib.unload_texture tileset.image in
    String.Map.iter unload_tileset room.cache.tilesets_by_path
  in
  let get_music area_id =
    List.find (fun (area_music : area_music) -> List.mem area_id area_music.areas) state.area_musics
  in
  let current_area_music = get_music game.room.area.id in
  let target_area_id, _ = Tiled.parse_room_filename "room transition" room_location.filename in
  let target_area_music = get_music target_area_id in
  if current_area_music.music.name <> target_area_music.music.name then (
    Audio.reset_music target_area_music.music;
    game.music <- target_area_music);

  if game.room.area.id <> new_room.area.id then (
    let visited_areas =
      let get_area_id (room_name, _) = String.maybe_trim_after '_' room_name in
      List.map get_area_id game.progress.by_room |> List.uniq
    in
    let first_visit_to_area = not (List.mem (Show.area_id new_room.area.id) visited_areas) in
    let text : Interaction.non_blocking_text =
      {
        content = Show.area_id_corner_text new_room.area.id;
        visible = Interaction.make_UNTIL 3.5 state.frame.time;
        scale = 1.;
      }
    in
    if first_visit_to_area then
      game.interaction.floating_text <- Some { text with scale = 3. }
    else
      game.interaction.corner_text <- Some text);

  game.room_changed_last_frame <- true;
  game.player.ghost.entity.current_floor <- None;
  game.player.current.wall <- None;
  game.player.spawned_vengeful_spirits <- [];
  game.player.ghost.entity.dest.pos <- ghost_start_pos;
  game.player.current.can_dash <- true;
  game.player.current.can_flap <- true;
  (match game.mode with
  | CLASSIC -> ()
  | DEMO
  | STEEL_SOLE ->
    game.player.soul.current <- game.player.soul.max);
  hide_party_ghosts ();
  (* all rooms are using the same tilesets now, but still unload them here (and re-load them
     in load_room) every time because the tilesets could be in a different order per room
     - not really sure about ^this comment, I don't know if different tileset order would break the
       tile lookup code now, so just unload/reload to be safe ¯\_(ツ)_/¯
  *)
  (* TODO probably need to unload things like enemy textures *)
  unload_tilesets game.room;
  game.room <- new_room;
  reset_tile_groups game.room;
  state.camera.update_instantly <- true;
  state.camera.raylib <-
    Tiled.create_camera_at
      (Raylib.Vector2.create ghost_start_pos.x ghost_start_pos.y)
      0. Config.window.center.x Config.window.center.y

let get_global_pos (current_pos : vector) (room_location : room_location) : vector =
  { x = current_pos.x +. room_location.global_x; y = current_pos.y +. room_location.global_y }

let handle_transitions (state : state) (game : game) =
  let colliding exit_rect =
    match Collision.with_entity game.player.ghost.entity exit_rect with
    | None -> None
    | Some c -> Some (exit_rect, c)
  in
  match List.find_map colliding game.room.exits with
  | None -> false
  | Some (exit_rect, collision) ->
    let overlap_w, overlap_h =
      match collision.collided_from with
      (* multiply by 2 to get the whole width using distance from center *)
      | UP -> (0., 2. *. abs_float (collision.center.y -. exit_rect.pos.y))
      | DOWN -> (0., 2. *. abs_float (exit_rect.pos.y +. exit_rect.h -. collision.center.y))
      | LEFT -> (2. *. abs_float (collision.center.x -. exit_rect.pos.x), 0.)
      | RIGHT -> (2. *. abs_float (exit_rect.pos.x +. exit_rect.w -. collision.center.x), 0.)
    in
    if overlap_w < game.player.ghost.entity.dest.w && overlap_h < game.player.ghost.entity.dest.h
    then
      false
    else (
      let current_room_location = List.assoc game.room.id state.world in
      let global_ghost_pos =
        get_global_pos game.player.ghost.entity.dest.pos current_room_location
      in
      let target_room_id, room_location =
        let global_x, global_y =
          ( collision.center.x +. current_room_location.global_x,
            collision.center.y +. current_room_location.global_y )
        in
        Tiled.JsonRoom.locate_by_coords state.world global_x global_y
      in
      let get_local_pos (global : vector) (room_id : room_id) (world : world) : vector =
        let room_location = List.assoc room_id world in
        { x = global.x -. room_location.global_x; y = global.y -. room_location.global_y }
      in
      let start_pos' = get_local_pos global_ghost_pos target_room_id state.world in
      let start_pos : vector =
        (* fixes ghost.facing_right, and adjusts the ghost to be further from the edge of screen *)
        match collision.collided_from with
        | LEFT ->
          game.player.ghost.entity.sprite.facing_right <- true;
          { start_pos' with x = start_pos'.x +. game.player.ghost.entity.dest.w }
        | RIGHT ->
          game.player.ghost.entity.sprite.facing_right <- false;
          { start_pos' with x = start_pos'.x -. game.player.ghost.entity.dest.w }
        | UP
        | DOWN ->
          start_pos'
      in
      change_current_room state game room_location start_pos;
      true)
