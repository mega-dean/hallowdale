(* Auto-generated from "json.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type global_map = Json_t.global_map = {
  file_name: string;
  x: int;
  y: int;
  h_in_pixels: int;
  w_in_pixels: int
}

type world = Json_t.world = { global_maps: global_map list }

type color = Json_t.color = { r: int; g: int; b: int; a: int }

type weapon = Json_t.weapon = {
  tint: color;
  pickup_text: string;
  damage: int;
  scale_x: float;
  scale_y: float;
  text_color: string;
  swing_speed: float
}

type weapons_file = Json_t.weapons_file

type tileset_source = Json_t.tileset_source = {
  firstgid: int;
  source: string
}

type connected_object = Json_t.connected_object = { id: int }

type coll_rect = Json_t.coll_rect = {
  gid: int;
  id: int;
  name: string;
  x: float;
  y: float;
  h: float;
  w: float;
  targets: connected_object list
}

type object_group = Json_t.object_group = { objects: coll_rect list }

type collision = Json_t.collision = { id: int; objectgroup: object_group }

type tileset = Json_t.tileset = {
  name: string;
  tile_h: float;
  tile_w: float;
  source: string;
  columns: int;
  collisions: collision list;
  tile_count: int
}

type tile_layer = Json_t.tile_layer = {
  name: string;
  data: int list;
  h: int;
  w: int;
  parallax_x: float;
  parallax_y: float;
  offset_x: float;
  offset_y: float
}

type texture_config = Json_t.texture_config = {
  count: int;
  duration: float;
  x_offset: float;
  y_offset: float
}

type texture_configs = Json_t.texture_configs

type steel_sole_progress = Json_t.steel_sole_progress = {
  mutable dunks: int;
  mutable c_dashes: int
}

type room_progress = Json_t.room_progress = {
  mutable removed_tile_idxs: int list;
  mutable removed_platform_ids: string list;
  mutable finished_interactions: string list;
  mutable revealed_shadow_layers: string list
}

type ghost_abilities = Json_t.ghost_abilities = {
  mutable crystal_heart: bool;
  mutable dream_nail: bool;
  mutable ismas_tear: bool;
  mutable mantis_claw: bool;
  mutable monarch_wings: bool;
  mutable mothwing_cloak: bool;
  mutable shade_cloak: bool;
  mutable vengeful_spirit: bool;
  mutable shade_soul: bool;
  mutable desolate_dive: bool;
  mutable descending_dark: bool;
  mutable howling_wraiths: bool;
  mutable abyss_shriek: bool;
  mutable quick_focus: bool;
  mutable soul_catcher_bonus: int;
  mutable dream_wielder: bool;
  mutable deep_focus: bool;
  mutable shaman_stone: bool;
  mutable spell_twister: bool
}

type game_progress = Json_t.game_progress = {
  mutable frame_idx: int;
  steel_sole: steel_sole_progress;
  mutable by_room: (string * room_progress) list;
  mutable purple_pens_found: (int * string) list;
  mutable keys_found: string list;
  mutable dreamer_items_found: int;
  mutable last_upgrade_claimed: int
}

type save_file = Json_t.save_file = {
  ghost_id: string;
  game_mode: string;
  ghost_x: float;
  ghost_y: float;
  room_name: string;
  respawn_x: float;
  respawn_y: float;
  ghosts_in_party: string list;
  abilities: ghost_abilities;
  weapons: string list;
  current_weapon: string;
  progress: game_progress;
  max_health: int;
  max_soul: int
}

type object_layer = Json_t.object_layer = {
  name: string;
  objects: coll_rect list
}

type image_layer = Json_t.image_layer = { name: string; image: string }

type layer = Json_t.layer

type room = Json_t.room = {
  tile_h: float;
  tile_w: float;
  h_in_tiles: int;
  w_in_tiles: int;
  layers: layer list;
  tileset_sources: tileset_source list
}

type npc_config = Json_t.npc_config = {
  w: int;
  h: int;
  texture_configs: (string * texture_config) list
}

type npcs_file = Json_t.npcs_file = {
  npcs: (string * npc_config) list;
  shared_textures: (string * texture_config) list
}

type lore_file = Json_t.lore_file

type keybinds_file = Json_t.keybinds_file

type jug_metadata = Json_t.jug_metadata = {
  name: string;
  x: int;
  w: int;
  h: int
}

type jug_metadata_file = Json_t.jug_metadata_file

type ghost_action = Json_t.ghost_action = {
  duration: float;
  cooldown: float;
  input_buffer: float;
  collision_shape: (float * float) list
}

type ghosts_file = Json_t.ghosts_file = {
  body_textures: texture_configs;
  shared_textures: texture_configs;
  actions: (string * ghost_action) list
}

type enemy_dream_nail_config = Json_t.enemy_dream_nail_config = {
  recoil_vx: float;
  vulnerable: bool
}

type enemy_config = Json_t.enemy_config = {
  w: int;
  h: int;
  health: int;
  kind: string;
  scale: float;
  damage: int;
  gravity_multiplier: float;
  death_gravity_multiplier: float;
  dream_nail: enemy_dream_nail_config;
  attrs: (string * float) list;
  unscaled_attrs: (string * float) list;
  texture_configs: texture_configs;
  can_take_damage: bool;
  can_recoil: bool
}

type enemies_file = Json_t.enemies_file = {
  enemies: (string * enemy_config) list;
  shared_textures: (string * texture_config) list
}

let write_global_map : _ -> global_map -> _ = (
  fun ob (x : global_map) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"fileName\":";
    (
      Yojson.Safe.write_string
    )
      ob x.file_name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"x\":";
    (
      Yojson.Safe.write_int
    )
      ob x.x;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"y\":";
    (
      Yojson.Safe.write_int
    )
      ob x.y;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"height\":";
    (
      Yojson.Safe.write_int
    )
      ob x.h_in_pixels;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"width\":";
    (
      Yojson.Safe.write_int
    )
      ob x.w_in_pixels;
    Buffer.add_char ob '}';
)
let string_of_global_map ?(len = 1024) x =
  let ob = Buffer.create len in
  write_global_map ob x;
  Buffer.contents ob
let read_global_map = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_file_name = ref (None) in
    let field_x = ref (None) in
    let field_y = ref (None) in
    let field_h_in_pixels = ref (None) in
    let field_w_in_pixels = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 1 -> (
                match String.unsafe_get s pos with
                  | 'x' -> (
                      1
                    )
                  | 'y' -> (
                      2
                    )
                  | _ -> (
                      -1
                    )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                  4
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                  3
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'N' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_file_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 2 ->
            field_y := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 3 ->
            field_h_in_pixels := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 4 ->
            field_w_in_pixels := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 1 -> (
                  match String.unsafe_get s pos with
                    | 'x' -> (
                        1
                      )
                    | 'y' -> (
                        2
                      )
                    | _ -> (
                        -1
                      )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'N' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_file_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 2 ->
              field_y := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 3 ->
              field_h_in_pixels := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 4 ->
              field_w_in_pixels := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            file_name = (match !field_file_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "file_name");
            x = (match !field_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x");
            y = (match !field_y with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "y");
            h_in_pixels = (match !field_h_in_pixels with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "h_in_pixels");
            w_in_pixels = (match !field_w_in_pixels with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "w_in_pixels");
          }
         : global_map)
      )
)
let global_map_of_string s =
  read_global_map (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__global_map_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_global_map
  )
)
let string_of__global_map_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__global_map_list ob x;
  Buffer.contents ob
let read__global_map_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_global_map
  )
)
let _global_map_list_of_string s =
  read__global_map_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_world : _ -> world -> _ = (
  fun ob (x : world) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"maps\":";
    (
      write__global_map_list
    )
      ob x.global_maps;
    Buffer.add_char ob '}';
)
let string_of_world ?(len = 1024) x =
  let ob = Buffer.create len in
  write_world ob x;
  Buffer.contents ob
let read_world = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_global_maps = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 4 && String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 's' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_global_maps := (
              Some (
                (
                  read__global_map_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 4 && String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 's' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_global_maps := (
                Some (
                  (
                    read__global_map_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            global_maps = (match !field_global_maps with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "global_maps");
          }
         : world)
      )
)
let world_of_string s =
  read_world (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_color : _ -> color -> _ = (
  fun ob (x : color) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"r\":";
    (
      Yojson.Safe.write_int
    )
      ob x.r;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"g\":";
    (
      Yojson.Safe.write_int
    )
      ob x.g;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"b\":";
    (
      Yojson.Safe.write_int
    )
      ob x.b;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"a\":";
    (
      Yojson.Safe.write_int
    )
      ob x.a;
    Buffer.add_char ob '}';
)
let string_of_color ?(len = 1024) x =
  let ob = Buffer.create len in
  write_color ob x;
  Buffer.contents ob
let read_color = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_r = ref (None) in
    let field_g = ref (None) in
    let field_b = ref (None) in
    let field_a = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 1 then (
            match String.unsafe_get s pos with
              | 'a' -> (
                  3
                )
              | 'b' -> (
                  2
                )
              | 'g' -> (
                  1
                )
              | 'r' -> (
                  0
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_r := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_g := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 2 ->
            field_b := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 3 ->
            field_a := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 1 then (
              match String.unsafe_get s pos with
                | 'a' -> (
                    3
                  )
                | 'b' -> (
                    2
                  )
                | 'g' -> (
                    1
                  )
                | 'r' -> (
                    0
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_r := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_g := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 2 ->
              field_b := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 3 ->
              field_a := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            r = (match !field_r with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "r");
            g = (match !field_g with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "g");
            b = (match !field_b with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b");
            a = (match !field_a with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "a");
          }
         : color)
      )
)
let color_of_string s =
  read_color (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_weapon : _ -> weapon -> _ = (
  fun ob (x : weapon) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"tint\":";
    (
      write_color
    )
      ob x.tint;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"pickup_text\":";
    (
      Yojson.Safe.write_string
    )
      ob x.pickup_text;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"damage\":";
    (
      Yojson.Safe.write_int
    )
      ob x.damage;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"scale_x\":";
    (
      Yojson.Safe.write_float
    )
      ob x.scale_x;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"scale_y\":";
    (
      Yojson.Safe.write_float
    )
      ob x.scale_y;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"text_color\":";
    (
      Yojson.Safe.write_string
    )
      ob x.text_color;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"swing_speed\":";
    (
      Yojson.Safe.write_float
    )
      ob x.swing_speed;
    Buffer.add_char ob '}';
)
let string_of_weapon ?(len = 1024) x =
  let ob = Buffer.create len in
  write_weapon ob x;
  Buffer.contents ob
let read_weapon = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_tint = ref (None) in
    let field_pickup_text = ref (None) in
    let field_damage = ref (None) in
    let field_scale_x = ref (None) in
    let field_scale_y = ref (None) in
    let field_text_color = ref (None) in
    let field_swing_speed = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'e' then (
                  2
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' then (
                  match String.unsafe_get s (pos+6) with
                    | 'x' -> (
                        3
                      )
                    | 'y' -> (
                        4
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'r' then (
                  5
                )
                else (
                  -1
                )
              )
            | 11 -> (
                match String.unsafe_get s pos with
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 't' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'w' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 'd' then (
                        6
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_tint := (
              Some (
                (
                  read_color
                ) p lb
              )
            );
          | 1 ->
            field_pickup_text := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 2 ->
            field_damage := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 3 ->
            field_scale_x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 4 ->
            field_scale_y := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 5 ->
            field_text_color := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 6 ->
            field_swing_speed := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'e' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' then (
                    match String.unsafe_get s (pos+6) with
                      | 'x' -> (
                          3
                        )
                      | 'y' -> (
                          4
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'r' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  match String.unsafe_get s pos with
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 't' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'w' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 'd' then (
                          6
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_tint := (
                Some (
                  (
                    read_color
                  ) p lb
                )
              );
            | 1 ->
              field_pickup_text := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 2 ->
              field_damage := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 3 ->
              field_scale_x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 4 ->
              field_scale_y := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 5 ->
              field_text_color := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 6 ->
              field_swing_speed := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            tint = (match !field_tint with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "tint");
            pickup_text = (match !field_pickup_text with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "pickup_text");
            damage = (match !field_damage with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "damage");
            scale_x = (match !field_scale_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "scale_x");
            scale_y = (match !field_scale_y with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "scale_y");
            text_color = (match !field_text_color with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "text_color");
            swing_speed = (match !field_swing_speed with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "swing_speed");
          }
         : weapon)
      )
)
let weapon_of_string s =
  read_weapon (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_weapon_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        write_weapon
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_weapon_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_weapon_list ob x;
  Buffer.contents ob
let read__string_weapon_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              read_weapon
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _string_weapon_list_of_string s =
  read__string_weapon_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_weapons_file = (
  write__string_weapon_list
)
let string_of_weapons_file ?(len = 1024) x =
  let ob = Buffer.create len in
  write_weapons_file ob x;
  Buffer.contents ob
let read_weapons_file = (
  read__string_weapon_list
)
let weapons_file_of_string s =
  read_weapons_file (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tileset_source : _ -> tileset_source -> _ = (
  fun ob (x : tileset_source) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"firstgid\":";
    (
      Yojson.Safe.write_int
    )
      ob x.firstgid;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"source\":";
    (
      Yojson.Safe.write_string
    )
      ob x.source;
    Buffer.add_char ob '}';
)
let string_of_tileset_source ?(len = 1024) x =
  let ob = Buffer.create len in
  write_tileset_source ob x;
  Buffer.contents ob
let read_tileset_source = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_firstgid = ref (None) in
    let field_source = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 6 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'e' then (
                  1
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'd' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_firstgid := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_source := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 6 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'e' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'd' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_firstgid := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_source := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            firstgid = (match !field_firstgid with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "firstgid");
            source = (match !field_source with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "source");
          }
         : tileset_source)
      )
)
let tileset_source_of_string s =
  read_tileset_source (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_connected_object : _ -> connected_object -> _ = (
  fun ob (x : connected_object) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      Yojson.Safe.write_int
    )
      ob x.id;
    Buffer.add_char ob '}';
)
let string_of_connected_object ?(len = 1024) x =
  let ob = Buffer.create len in
  write_connected_object ob x;
  Buffer.contents ob
let read_connected_object = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_id = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_id := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_id := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            id = (match !field_id with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "id");
          }
         : connected_object)
      )
)
let connected_object_of_string s =
  read_connected_object (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__connected_object_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_connected_object
  )
)
let string_of__connected_object_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__connected_object_list ob x;
  Buffer.contents ob
let read__connected_object_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_connected_object
  )
)
let _connected_object_list_of_string s =
  read__connected_object_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_coll_rect : _ -> coll_rect -> _ = (
  fun ob (x : coll_rect) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if x.gid <> 0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"gid\":";
      (
        Yojson.Safe.write_int
      )
        ob x.gid;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"id\":";
    (
      Yojson.Safe.write_int
    )
      ob x.id;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"x\":";
    (
      Yojson.Safe.write_float
    )
      ob x.x;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"y\":";
    (
      Yojson.Safe.write_float
    )
      ob x.y;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"height\":";
    (
      Yojson.Safe.write_float
    )
      ob x.h;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"width\":";
    (
      Yojson.Safe.write_float
    )
      ob x.w;
    if x.targets <> [] then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"properties\":";
      (
        write__connected_object_list
      )
        ob x.targets;
    );
    Buffer.add_char ob '}';
)
let string_of_coll_rect ?(len = 1024) x =
  let ob = Buffer.create len in
  write_coll_rect ob x;
  Buffer.contents ob
let read_coll_rect = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_gid = ref (0) in
    let field_id = ref (None) in
    let field_name = ref (None) in
    let field_x = ref (None) in
    let field_y = ref (None) in
    let field_h = ref (None) in
    let field_w = ref (None) in
    let field_targets = ref ([]) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 1 -> (
                match String.unsafe_get s pos with
                  | 'x' -> (
                      3
                    )
                  | 'y' -> (
                      4
                    )
                  | _ -> (
                      -1
                    )
              )
            | 2 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'd' then (
                  1
                )
                else (
                  -1
                )
              )
            | 3 -> (
                if String.unsafe_get s pos = 'g' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' then (
                  0
                )
                else (
                  -1
                )
              )
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  2
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                  6
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                  5
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                  7
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_gid := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 1 ->
            field_id := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 2 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 3 ->
            field_x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 4 ->
            field_y := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 5 ->
            field_h := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 6 ->
            field_w := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 7 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_targets := (
                (
                  read__connected_object_list
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 1 -> (
                  match String.unsafe_get s pos with
                    | 'x' -> (
                        3
                      )
                    | 'y' -> (
                        4
                      )
                    | _ -> (
                        -1
                      )
                )
              | 2 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 3 -> (
                  if String.unsafe_get s pos = 'g' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                    6
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                    7
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_gid := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 1 ->
              field_id := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 2 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 3 ->
              field_x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 4 ->
              field_y := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 5 ->
              field_h := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 6 ->
              field_w := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 7 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_targets := (
                  (
                    read__connected_object_list
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            gid = !field_gid;
            id = (match !field_id with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "id");
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            x = (match !field_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x");
            y = (match !field_y with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "y");
            h = (match !field_h with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "h");
            w = (match !field_w with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "w");
            targets = !field_targets;
          }
         : coll_rect)
      )
)
let coll_rect_of_string s =
  read_coll_rect (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__coll_rect_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_coll_rect
  )
)
let string_of__coll_rect_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__coll_rect_list ob x;
  Buffer.contents ob
let read__coll_rect_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_coll_rect
  )
)
let _coll_rect_list_of_string s =
  read__coll_rect_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_object_group : _ -> object_group -> _ = (
  fun ob (x : object_group) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"objects\":";
    (
      write__coll_rect_list
    )
      ob x.objects;
    Buffer.add_char ob '}';
)
let string_of_object_group ?(len = 1024) x =
  let ob = Buffer.create len in
  write_object_group ob x;
  Buffer.contents ob
let read_object_group = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_objects = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 7 && String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'j' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 's' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_objects := (
              Some (
                (
                  read__coll_rect_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 7 && String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'j' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 's' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_objects := (
                Some (
                  (
                    read__coll_rect_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            objects = (match !field_objects with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "objects");
          }
         : object_group)
      )
)
let object_group_of_string s =
  read_object_group (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_collision : _ -> collision -> _ = (
  fun ob (x : collision) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"id\":";
    (
      Yojson.Safe.write_int
    )
      ob x.id;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"objectgroup\":";
    (
      write_object_group
    )
      ob x.objectgroup;
    Buffer.add_char ob '}';
)
let string_of_collision ?(len = 1024) x =
  let ob = Buffer.create len in
  write_collision ob x;
  Buffer.contents ob
let read_collision = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_id = ref (None) in
    let field_objectgroup = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 2 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'd' then (
                  0
                )
                else (
                  -1
                )
              )
            | 11 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'j' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'p' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_id := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_objectgroup := (
              Some (
                (
                  read_object_group
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 2 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'd' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'j' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'p' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_id := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_objectgroup := (
                Some (
                  (
                    read_object_group
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            id = (match !field_id with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "id");
            objectgroup = (match !field_objectgroup with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "objectgroup");
          }
         : collision)
      )
)
let collision_of_string s =
  read_collision (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__collision_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_collision
  )
)
let string_of__collision_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__collision_list ob x;
  Buffer.contents ob
let read__collision_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_collision
  )
)
let _collision_list_of_string s =
  read__collision_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tileset : _ -> tileset -> _ = (
  fun ob (x : tileset) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"tileheight\":";
    (
      Yojson.Safe.write_float
    )
      ob x.tile_h;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"tilewidth\":";
    (
      Yojson.Safe.write_float
    )
      ob x.tile_w;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"image\":";
    (
      Yojson.Safe.write_string
    )
      ob x.source;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"columns\":";
    (
      Yojson.Safe.write_int
    )
      ob x.columns;
    if x.collisions <> [] then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"tiles\":";
      (
        write__collision_list
      )
        ob x.collisions;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"tilecount\":";
    (
      Yojson.Safe.write_int
    )
      ob x.tile_count;
    Buffer.add_char ob '}';
)
let string_of_tileset ?(len = 1024) x =
  let ob = Buffer.create len in
  write_tileset ob x;
  Buffer.contents ob
let read_tileset = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (None) in
    let field_tile_h = ref (None) in
    let field_tile_w = ref (None) in
    let field_source = ref (None) in
    let field_columns = ref (None) in
    let field_collisions = ref ([]) in
    let field_tile_count = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                match String.unsafe_get s pos with
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' then (
                        5
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 's' then (
                  4
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' then (
                  match String.unsafe_get s (pos+4) with
                    | 'c' -> (
                        if String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' then (
                          6
                        )
                        else (
                          -1
                        )
                      )
                    | 'w' -> (
                        if String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'h' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'g' && String.unsafe_get s (pos+8) = 'h' && String.unsafe_get s (pos+9) = 't' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_tile_h := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 2 ->
            field_tile_w := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 3 ->
            field_source := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 4 ->
            field_columns := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_collisions := (
                (
                  read__collision_list
                ) p lb
              );
            )
          | 6 ->
            field_tile_count := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  match String.unsafe_get s pos with
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' then (
                          5
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 's' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' then (
                    match String.unsafe_get s (pos+4) with
                      | 'c' -> (
                          if String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' then (
                            6
                          )
                          else (
                            -1
                          )
                        )
                      | 'w' -> (
                          if String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'h' then (
                            2
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'g' && String.unsafe_get s (pos+8) = 'h' && String.unsafe_get s (pos+9) = 't' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_tile_h := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 2 ->
              field_tile_w := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 3 ->
              field_source := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 4 ->
              field_columns := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_collisions := (
                  (
                    read__collision_list
                  ) p lb
                );
              )
            | 6 ->
              field_tile_count := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            tile_h = (match !field_tile_h with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "tile_h");
            tile_w = (match !field_tile_w with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "tile_w");
            source = (match !field_source with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "source");
            columns = (match !field_columns with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "columns");
            collisions = !field_collisions;
            tile_count = (match !field_tile_count with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "tile_count");
          }
         : tileset)
      )
)
let tileset_of_string s =
  read_tileset (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_list = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_int
  )
)
let string_of__int_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_list ob x;
  Buffer.contents ob
let read__int_list = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_int
  )
)
let _int_list_of_string s =
  read__int_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tile_layer : _ -> tile_layer -> _ = (
  fun ob (x : tile_layer) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"data\":";
    (
      write__int_list
    )
      ob x.data;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"height\":";
    (
      Yojson.Safe.write_int
    )
      ob x.h;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"width\":";
    (
      Yojson.Safe.write_int
    )
      ob x.w;
    if x.parallax_x <> 1.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"parallaxx\":";
      (
        Yojson.Safe.write_float
      )
        ob x.parallax_x;
    );
    if x.parallax_y <> 1.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"parallaxy\":";
      (
        Yojson.Safe.write_float
      )
        ob x.parallax_y;
    );
    if x.offset_x <> 0.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"offsetx\":";
      (
        Yojson.Safe.write_float
      )
        ob x.offset_x;
    );
    if x.offset_y <> 0.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"offsety\":";
      (
        Yojson.Safe.write_float
      )
        ob x.offset_y;
    );
    Buffer.add_char ob '}';
)
let string_of_tile_layer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_tile_layer ob x;
  Buffer.contents ob
let read_tile_layer = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (None) in
    let field_data = ref (None) in
    let field_h = ref (None) in
    let field_w = ref (None) in
    let field_parallax_x = ref (1.0) in
    let field_parallax_y = ref (1.0) in
    let field_offset_x = ref (0.0) in
    let field_offset_y = ref (0.0) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                match String.unsafe_get s pos with
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 'n' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                  3
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                  2
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'f' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 't' then (
                  match String.unsafe_get s (pos+6) with
                    | 'x' -> (
                        6
                      )
                    | 'y' -> (
                        7
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'x' then (
                  match String.unsafe_get s (pos+8) with
                    | 'x' -> (
                        4
                      )
                    | 'y' -> (
                        5
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_data := (
              Some (
                (
                  read__int_list
                ) p lb
              )
            );
          | 2 ->
            field_h := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 3 ->
            field_w := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_parallax_x := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_parallax_y := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 6 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_offset_x := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 7 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_offset_y := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 4 -> (
                  match String.unsafe_get s pos with
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'n' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'f' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 't' then (
                    match String.unsafe_get s (pos+6) with
                      | 'x' -> (
                          6
                        )
                      | 'y' -> (
                          7
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'x' then (
                    match String.unsafe_get s (pos+8) with
                      | 'x' -> (
                          4
                        )
                      | 'y' -> (
                          5
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_data := (
                Some (
                  (
                    read__int_list
                  ) p lb
                )
              );
            | 2 ->
              field_h := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 3 ->
              field_w := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_parallax_x := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_parallax_y := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 6 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_offset_x := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 7 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_offset_y := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            data = (match !field_data with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "data");
            h = (match !field_h with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "h");
            w = (match !field_w with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "w");
            parallax_x = !field_parallax_x;
            parallax_y = !field_parallax_y;
            offset_x = !field_offset_x;
            offset_y = !field_offset_y;
          }
         : tile_layer)
      )
)
let tile_layer_of_string s =
  read_tile_layer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_texture_config : _ -> texture_config -> _ = (
  fun ob (x : texture_config) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if x.count <> 1 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"count\":";
      (
        Yojson.Safe.write_int
      )
        ob x.count;
    );
    if x.duration <> 0.066666 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"duration\":";
      (
        Yojson.Safe.write_float
      )
        ob x.duration;
    );
    if x.x_offset <> 0.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"x_offset\":";
      (
        Yojson.Safe.write_float
      )
        ob x.x_offset;
    );
    if x.y_offset <> 0.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"y_offset\":";
      (
        Yojson.Safe.write_float
      )
        ob x.y_offset;
    );
    Buffer.add_char ob '}';
)
let string_of_texture_config ?(len = 1024) x =
  let ob = Buffer.create len in
  write_texture_config ob x;
  Buffer.contents ob
let read_texture_config = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_count = ref (1) in
    let field_duration = ref (0.066666) in
    let field_x_offset = ref (0.0) in
    let field_y_offset = ref (0.0) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | 8 -> (
                match String.unsafe_get s pos with
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 'x' -> (
                      if String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'f' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 't' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 'y' -> (
                      if String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'f' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 't' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_count := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_duration := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_x_offset := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_y_offset := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  match String.unsafe_get s pos with
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'x' -> (
                        if String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'f' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 't' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'y' -> (
                        if String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'f' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 't' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_count := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_duration := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_x_offset := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_y_offset := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            count = !field_count;
            duration = !field_duration;
            x_offset = !field_x_offset;
            y_offset = !field_y_offset;
          }
         : texture_config)
      )
)
let texture_config_of_string s =
  read_texture_config (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_texture_config_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        write_texture_config
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_texture_config_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_texture_config_list ob x;
  Buffer.contents ob
let read__string_texture_config_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              read_texture_config
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _string_texture_config_list_of_string s =
  read__string_texture_config_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_texture_configs = (
  write__string_texture_config_list
)
let string_of_texture_configs ?(len = 1024) x =
  let ob = Buffer.create len in
  write_texture_configs ob x;
  Buffer.contents ob
let read_texture_configs = (
  read__string_texture_config_list
)
let texture_configs_of_string s =
  read_texture_configs (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_steel_sole_progress : _ -> steel_sole_progress -> _ = (
  fun ob (x : steel_sole_progress) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"dunks\":";
    (
      Yojson.Safe.write_int
    )
      ob x.dunks;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"c_dashes\":";
    (
      Yojson.Safe.write_int
    )
      ob x.c_dashes;
    Buffer.add_char ob '}';
)
let string_of_steel_sole_progress ?(len = 1024) x =
  let ob = Buffer.create len in
  write_steel_sole_progress ob x;
  Buffer.contents ob
let read_steel_sole_progress = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_dunks = ref (None) in
    let field_c_dashes = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_dunks := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_c_dashes := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_dunks := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_c_dashes := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            dunks = (match !field_dunks with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "dunks");
            c_dashes = (match !field_c_dashes with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "c_dashes");
          }
         : steel_sole_progress)
      )
)
let steel_sole_progress_of_string s =
  read_steel_sole_progress (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_list = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__string_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_list ob x;
  Buffer.contents ob
let read__string_list = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _string_list_of_string s =
  read__string_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_room_progress : _ -> room_progress -> _ = (
  fun ob (x : room_progress) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"removed_tile_idxs\":";
    (
      write__int_list
    )
      ob x.removed_tile_idxs;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"removed_platform_ids\":";
    (
      write__string_list
    )
      ob x.removed_platform_ids;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"finished_interactions\":";
    (
      write__string_list
    )
      ob x.finished_interactions;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"revealed_shadow_layers\":";
    (
      write__string_list
    )
      ob x.revealed_shadow_layers;
    Buffer.add_char ob '}';
)
let string_of_room_progress ?(len = 1024) x =
  let ob = Buffer.create len in
  write_room_progress ob x;
  Buffer.contents ob
let read_room_progress = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_removed_tile_idxs = ref (None) in
    let field_removed_platform_ids = ref (None) in
    let field_finished_interactions = ref (None) in
    let field_revealed_shadow_layers = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 17 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'd' && String.unsafe_get s (pos+15) = 'x' && String.unsafe_get s (pos+16) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 20 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'p' && String.unsafe_get s (pos+9) = 'l' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = 'm' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'd' && String.unsafe_get s (pos+19) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | 21 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'r' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'c' && String.unsafe_get s (pos+16) = 't' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 's' then (
                  2
                )
                else (
                  -1
                )
              )
            | 22 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'v' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'w' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = 'a' && String.unsafe_get s (pos+18) = 'y' && String.unsafe_get s (pos+19) = 'e' && String.unsafe_get s (pos+20) = 'r' && String.unsafe_get s (pos+21) = 's' then (
                  3
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_removed_tile_idxs := (
              Some (
                (
                  read__int_list
                ) p lb
              )
            );
          | 1 ->
            field_removed_platform_ids := (
              Some (
                (
                  read__string_list
                ) p lb
              )
            );
          | 2 ->
            field_finished_interactions := (
              Some (
                (
                  read__string_list
                ) p lb
              )
            );
          | 3 ->
            field_revealed_shadow_layers := (
              Some (
                (
                  read__string_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 17 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'd' && String.unsafe_get s (pos+15) = 'x' && String.unsafe_get s (pos+16) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 20 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'p' && String.unsafe_get s (pos+9) = 'l' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = 'm' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'd' && String.unsafe_get s (pos+19) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 21 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'r' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'c' && String.unsafe_get s (pos+16) = 't' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 's' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 22 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'v' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'w' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = 'a' && String.unsafe_get s (pos+18) = 'y' && String.unsafe_get s (pos+19) = 'e' && String.unsafe_get s (pos+20) = 'r' && String.unsafe_get s (pos+21) = 's' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_removed_tile_idxs := (
                Some (
                  (
                    read__int_list
                  ) p lb
                )
              );
            | 1 ->
              field_removed_platform_ids := (
                Some (
                  (
                    read__string_list
                  ) p lb
                )
              );
            | 2 ->
              field_finished_interactions := (
                Some (
                  (
                    read__string_list
                  ) p lb
                )
              );
            | 3 ->
              field_revealed_shadow_layers := (
                Some (
                  (
                    read__string_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            removed_tile_idxs = (match !field_removed_tile_idxs with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "removed_tile_idxs");
            removed_platform_ids = (match !field_removed_platform_ids with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "removed_platform_ids");
            finished_interactions = (match !field_finished_interactions with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "finished_interactions");
            revealed_shadow_layers = (match !field_revealed_shadow_layers with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "revealed_shadow_layers");
          }
         : room_progress)
      )
)
let room_progress_of_string s =
  read_room_progress (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_ghost_abilities : _ -> ghost_abilities -> _ = (
  fun ob (x : ghost_abilities) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"crystal_heart\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.crystal_heart;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"dream_nail\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.dream_nail;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"ismas_tear\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.ismas_tear;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"mantis_claw\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.mantis_claw;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"monarch_wings\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.monarch_wings;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"mothwing_cloak\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.mothwing_cloak;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"shade_cloak\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.shade_cloak;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"vengeful_spirit\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.vengeful_spirit;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"shade_soul\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.shade_soul;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"desolate_dive\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.desolate_dive;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"descending_dark\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.descending_dark;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"howling_wraiths\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.howling_wraiths;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"abyss_shriek\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.abyss_shriek;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"quick_focus\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.quick_focus;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"soul_catcher_bonus\":";
    (
      Yojson.Safe.write_int
    )
      ob x.soul_catcher_bonus;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"dream_wielder\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.dream_wielder;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"deep_focus\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.deep_focus;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"shaman_stone\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.shaman_stone;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"spell_twister\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.spell_twister;
    Buffer.add_char ob '}';
)
let string_of_ghost_abilities ?(len = 1024) x =
  let ob = Buffer.create len in
  write_ghost_abilities ob x;
  Buffer.contents ob
let read_ghost_abilities = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_crystal_heart = ref (None) in
    let field_dream_nail = ref (None) in
    let field_ismas_tear = ref (None) in
    let field_mantis_claw = ref (None) in
    let field_monarch_wings = ref (None) in
    let field_mothwing_cloak = ref (None) in
    let field_shade_cloak = ref (None) in
    let field_vengeful_spirit = ref (None) in
    let field_shade_soul = ref (None) in
    let field_desolate_dive = ref (None) in
    let field_descending_dark = ref (None) in
    let field_howling_wraiths = ref (None) in
    let field_abyss_shriek = ref (None) in
    let field_quick_focus = ref (None) in
    let field_soul_catcher_bonus = ref (None) in
    let field_dream_wielder = ref (None) in
    let field_deep_focus = ref (None) in
    let field_shaman_stone = ref (None) in
    let field_spell_twister = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 10 -> (
                match String.unsafe_get s pos with
                  | 'd' -> (
                      match String.unsafe_get s (pos+1) with
                        | 'e' -> (
                            if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 's' then (
                              16
                            )
                            else (
                              -1
                            )
                          )
                        | 'r' -> (
                            if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'l' then (
                              1
                            )
                            else (
                              -1
                            )
                          )
                        | _ -> (
                            -1
                          )
                    )
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'd' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 'l' then (
                        8
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 11 -> (
                match String.unsafe_get s pos with
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'w' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | 'q' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'k' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'f' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 's' then (
                        13
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'd' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'k' then (
                        6
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 12 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'h' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'k' then (
                        12
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 'e' then (
                        17
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 13 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'h' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 't' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 'd' -> (
                      match String.unsafe_get s (pos+1) with
                        | 'e' -> (
                            if String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'v' && String.unsafe_get s (pos+12) = 'e' then (
                              9
                            )
                            else (
                              -1
                            )
                          )
                        | 'r' -> (
                            if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'w' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'l' && String.unsafe_get s (pos+10) = 'd' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'r' then (
                              15
                            )
                            else (
                              -1
                            )
                          )
                        | _ -> (
                            -1
                          )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'h' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'w' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 'g' && String.unsafe_get s (pos+12) = 's' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'p' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'r' then (
                        18
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 14 -> (
                if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = 'w' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'g' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'k' then (
                  5
                )
                else (
                  -1
                )
              )
            | 15 -> (
                match String.unsafe_get s pos with
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'd' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'r' && String.unsafe_get s (pos+14) = 'k' then (
                        10
                      )
                      else (
                        -1
                      )
                    )
                  | 'h' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'w' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'w' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = 'h' && String.unsafe_get s (pos+14) = 's' then (
                        11
                      )
                      else (
                        -1
                      )
                    )
                  | 'v' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 't' then (
                        7
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 18 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'h' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'b' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'n' && String.unsafe_get s (pos+16) = 'u' && String.unsafe_get s (pos+17) = 's' then (
                  14
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_crystal_heart := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 1 ->
            field_dream_nail := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 2 ->
            field_ismas_tear := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 3 ->
            field_mantis_claw := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 4 ->
            field_monarch_wings := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 5 ->
            field_mothwing_cloak := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 6 ->
            field_shade_cloak := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 7 ->
            field_vengeful_spirit := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 8 ->
            field_shade_soul := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 9 ->
            field_desolate_dive := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 10 ->
            field_descending_dark := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 11 ->
            field_howling_wraiths := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 12 ->
            field_abyss_shriek := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 13 ->
            field_quick_focus := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 14 ->
            field_soul_catcher_bonus := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 15 ->
            field_dream_wielder := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 16 ->
            field_deep_focus := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 17 ->
            field_shaman_stone := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 18 ->
            field_spell_twister := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 10 -> (
                  match String.unsafe_get s pos with
                    | 'd' -> (
                        match String.unsafe_get s (pos+1) with
                          | 'e' -> (
                              if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 's' then (
                                16
                              )
                              else (
                                -1
                              )
                            )
                          | 'r' -> (
                              if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'l' then (
                                1
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'd' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 'l' then (
                          8
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 11 -> (
                  match String.unsafe_get s pos with
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'w' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 'q' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'k' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'f' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 's' then (
                          13
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'd' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'k' then (
                          6
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 12 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'h' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'k' then (
                          12
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 'e' then (
                          17
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 13 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'h' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 't' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 'd' -> (
                        match String.unsafe_get s (pos+1) with
                          | 'e' -> (
                              if String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'v' && String.unsafe_get s (pos+12) = 'e' then (
                                9
                              )
                              else (
                                -1
                              )
                            )
                          | 'r' -> (
                              if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'w' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'l' && String.unsafe_get s (pos+10) = 'd' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'r' then (
                                15
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'h' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'w' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 'g' && String.unsafe_get s (pos+12) = 's' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'p' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'r' then (
                          18
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 14 -> (
                  if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = 'w' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'g' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'k' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  match String.unsafe_get s pos with
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'd' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'r' && String.unsafe_get s (pos+14) = 'k' then (
                          10
                        )
                        else (
                          -1
                        )
                      )
                    | 'h' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'w' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'w' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = 'h' && String.unsafe_get s (pos+14) = 's' then (
                          11
                        )
                        else (
                          -1
                        )
                      )
                    | 'v' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 't' then (
                          7
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 18 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'h' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'b' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'n' && String.unsafe_get s (pos+16) = 'u' && String.unsafe_get s (pos+17) = 's' then (
                    14
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_crystal_heart := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 1 ->
              field_dream_nail := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 2 ->
              field_ismas_tear := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 3 ->
              field_mantis_claw := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 4 ->
              field_monarch_wings := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 5 ->
              field_mothwing_cloak := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 6 ->
              field_shade_cloak := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 7 ->
              field_vengeful_spirit := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 8 ->
              field_shade_soul := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 9 ->
              field_desolate_dive := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 10 ->
              field_descending_dark := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 11 ->
              field_howling_wraiths := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 12 ->
              field_abyss_shriek := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 13 ->
              field_quick_focus := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 14 ->
              field_soul_catcher_bonus := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 15 ->
              field_dream_wielder := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 16 ->
              field_deep_focus := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 17 ->
              field_shaman_stone := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 18 ->
              field_spell_twister := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            crystal_heart = (match !field_crystal_heart with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "crystal_heart");
            dream_nail = (match !field_dream_nail with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "dream_nail");
            ismas_tear = (match !field_ismas_tear with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "ismas_tear");
            mantis_claw = (match !field_mantis_claw with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "mantis_claw");
            monarch_wings = (match !field_monarch_wings with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "monarch_wings");
            mothwing_cloak = (match !field_mothwing_cloak with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "mothwing_cloak");
            shade_cloak = (match !field_shade_cloak with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "shade_cloak");
            vengeful_spirit = (match !field_vengeful_spirit with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "vengeful_spirit");
            shade_soul = (match !field_shade_soul with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "shade_soul");
            desolate_dive = (match !field_desolate_dive with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "desolate_dive");
            descending_dark = (match !field_descending_dark with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "descending_dark");
            howling_wraiths = (match !field_howling_wraiths with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "howling_wraiths");
            abyss_shriek = (match !field_abyss_shriek with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "abyss_shriek");
            quick_focus = (match !field_quick_focus with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "quick_focus");
            soul_catcher_bonus = (match !field_soul_catcher_bonus with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "soul_catcher_bonus");
            dream_wielder = (match !field_dream_wielder with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "dream_wielder");
            deep_focus = (match !field_deep_focus with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "deep_focus");
            shaman_stone = (match !field_shaman_stone with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "shaman_stone");
            spell_twister = (match !field_spell_twister with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "spell_twister");
          }
         : ghost_abilities)
      )
)
let ghost_abilities_of_string s =
  read_ghost_abilities (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_room_progress_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        write_room_progress
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_room_progress_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_room_progress_list ob x;
  Buffer.contents ob
let read__string_room_progress_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              read_room_progress
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _string_room_progress_list_of_string s =
  read__string_room_progress_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_string_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_int
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__int_string_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_string_list ob x;
  Buffer.contents ob
let read__int_string_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_int
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _int_string_list_of_string s =
  read__int_string_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_game_progress : _ -> game_progress -> _ = (
  fun ob (x : game_progress) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"frame_idx\":";
    (
      Yojson.Safe.write_int
    )
      ob x.frame_idx;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"steel_sole\":";
    (
      write_steel_sole_progress
    )
      ob x.steel_sole;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"by_room\":";
    (
      write__string_room_progress_list
    )
      ob x.by_room;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"purple_pens_found\":";
    (
      write__int_string_list
    )
      ob x.purple_pens_found;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"keys_found\":";
    (
      write__string_list
    )
      ob x.keys_found;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"dreamer_items_found\":";
    (
      Yojson.Safe.write_int
    )
      ob x.dreamer_items_found;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"last_upgrade_claimed\":";
    (
      Yojson.Safe.write_int
    )
      ob x.last_upgrade_claimed;
    Buffer.add_char ob '}';
)
let string_of_game_progress ?(len = 1024) x =
  let ob = Buffer.create len in
  write_game_progress ob x;
  Buffer.contents ob
let read_game_progress = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_frame_idx = ref (None) in
    let field_steel_sole = ref (None) in
    let field_by_room = ref (None) in
    let field_purple_pens_found = ref (None) in
    let field_keys_found = ref (None) in
    let field_dreamer_items_found = ref (None) in
    let field_last_upgrade_claimed = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 7 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'm' then (
                  2
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = 'x' then (
                  0
                )
                else (
                  -1
                )
              )
            | 10 -> (
                match String.unsafe_get s pos with
                  | 'k' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'e' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 17 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'u' && String.unsafe_get s (pos+15) = 'n' && String.unsafe_get s (pos+16) = 'd' then (
                  3
                )
                else (
                  -1
                )
              )
            | 19 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'f' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'u' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'd' then (
                  5
                )
                else (
                  -1
                )
              )
            | 20 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'g' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'd' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'c' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'm' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'd' then (
                  6
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_frame_idx := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_steel_sole := (
              Some (
                (
                  read_steel_sole_progress
                ) p lb
              )
            );
          | 2 ->
            field_by_room := (
              Some (
                (
                  read__string_room_progress_list
                ) p lb
              )
            );
          | 3 ->
            field_purple_pens_found := (
              Some (
                (
                  read__int_string_list
                ) p lb
              )
            );
          | 4 ->
            field_keys_found := (
              Some (
                (
                  read__string_list
                ) p lb
              )
            );
          | 5 ->
            field_dreamer_items_found := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 6 ->
            field_last_upgrade_claimed := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 7 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'm' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = 'x' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  match String.unsafe_get s pos with
                    | 'k' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'e' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 17 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'u' && String.unsafe_get s (pos+15) = 'n' && String.unsafe_get s (pos+16) = 'd' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 19 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'f' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'u' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'd' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 20 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'g' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'd' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'c' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'm' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'd' then (
                    6
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_frame_idx := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_steel_sole := (
                Some (
                  (
                    read_steel_sole_progress
                  ) p lb
                )
              );
            | 2 ->
              field_by_room := (
                Some (
                  (
                    read__string_room_progress_list
                  ) p lb
                )
              );
            | 3 ->
              field_purple_pens_found := (
                Some (
                  (
                    read__int_string_list
                  ) p lb
                )
              );
            | 4 ->
              field_keys_found := (
                Some (
                  (
                    read__string_list
                  ) p lb
                )
              );
            | 5 ->
              field_dreamer_items_found := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 6 ->
              field_last_upgrade_claimed := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            frame_idx = (match !field_frame_idx with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "frame_idx");
            steel_sole = (match !field_steel_sole with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "steel_sole");
            by_room = (match !field_by_room with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "by_room");
            purple_pens_found = (match !field_purple_pens_found with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "purple_pens_found");
            keys_found = (match !field_keys_found with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "keys_found");
            dreamer_items_found = (match !field_dreamer_items_found with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "dreamer_items_found");
            last_upgrade_claimed = (match !field_last_upgrade_claimed with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "last_upgrade_claimed");
          }
         : game_progress)
      )
)
let game_progress_of_string s =
  read_game_progress (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_save_file : _ -> save_file -> _ = (
  fun ob (x : save_file) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"ghost_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.ghost_id;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"game_mode\":";
    (
      Yojson.Safe.write_string
    )
      ob x.game_mode;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"ghost_x\":";
    (
      Yojson.Safe.write_float
    )
      ob x.ghost_x;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"ghost_y\":";
    (
      Yojson.Safe.write_float
    )
      ob x.ghost_y;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"room_name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.room_name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"respawn_x\":";
    (
      Yojson.Safe.write_float
    )
      ob x.respawn_x;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"respawn_y\":";
    (
      Yojson.Safe.write_float
    )
      ob x.respawn_y;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"ghosts_in_party\":";
    (
      write__string_list
    )
      ob x.ghosts_in_party;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"abilities\":";
    (
      write_ghost_abilities
    )
      ob x.abilities;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"weapons\":";
    (
      write__string_list
    )
      ob x.weapons;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"current_weapon\":";
    (
      Yojson.Safe.write_string
    )
      ob x.current_weapon;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"progress\":";
    (
      write_game_progress
    )
      ob x.progress;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"max_health\":";
    (
      Yojson.Safe.write_int
    )
      ob x.max_health;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"max_soul\":";
    (
      Yojson.Safe.write_int
    )
      ob x.max_soul;
    Buffer.add_char ob '}';
)
let string_of_save_file ?(len = 1024) x =
  let ob = Buffer.create len in
  write_save_file ob x;
  Buffer.contents ob
let read_save_file = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_ghost_id = ref (None) in
    let field_game_mode = ref (None) in
    let field_ghost_x = ref (None) in
    let field_ghost_y = ref (None) in
    let field_room_name = ref (None) in
    let field_respawn_x = ref (None) in
    let field_respawn_y = ref (None) in
    let field_ghosts_in_party = ref (None) in
    let field_abilities = ref (None) in
    let field_weapons = ref (None) in
    let field_current_weapon = ref (None) in
    let field_progress = ref (None) in
    let field_max_health = ref (None) in
    let field_max_soul = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 7 -> (
                match String.unsafe_get s pos with
                  | 'g' -> (
                      if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = '_' then (
                        match String.unsafe_get s (pos+6) with
                          | 'x' -> (
                              2
                            )
                          | 'y' -> (
                              3
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'w' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 's' then (
                        9
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 8 -> (
                match String.unsafe_get s pos with
                  | 'g' -> (
                      if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'd' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'l' then (
                        13
                      )
                      else (
                        -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 's' then (
                        11
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 9 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 's' then (
                        8
                      )
                      else (
                        -1
                      )
                    )
                  | 'g' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = 'e' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 'r' -> (
                      match String.unsafe_get s (pos+1) with
                        | 'e' -> (
                            if String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'w' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = '_' then (
                              match String.unsafe_get s (pos+8) with
                                | 'x' -> (
                                    5
                                  )
                                | 'y' -> (
                                    6
                                  )
                                | _ -> (
                                    -1
                                  )
                            )
                            else (
                              -1
                            )
                          )
                        | 'o' -> (
                            if String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' then (
                              4
                            )
                            else (
                              -1
                            )
                          )
                        | _ -> (
                            -1
                          )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'h' then (
                  12
                )
                else (
                  -1
                )
              )
            | 14 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'w' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'p' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'n' then (
                  10
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 'g' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' then (
                  7
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_ghost_id := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_game_mode := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 2 ->
            field_ghost_x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 3 ->
            field_ghost_y := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 4 ->
            field_room_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 5 ->
            field_respawn_x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 6 ->
            field_respawn_y := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 7 ->
            field_ghosts_in_party := (
              Some (
                (
                  read__string_list
                ) p lb
              )
            );
          | 8 ->
            field_abilities := (
              Some (
                (
                  read_ghost_abilities
                ) p lb
              )
            );
          | 9 ->
            field_weapons := (
              Some (
                (
                  read__string_list
                ) p lb
              )
            );
          | 10 ->
            field_current_weapon := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 11 ->
            field_progress := (
              Some (
                (
                  read_game_progress
                ) p lb
              )
            );
          | 12 ->
            field_max_health := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 13 ->
            field_max_soul := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 7 -> (
                  match String.unsafe_get s pos with
                    | 'g' -> (
                        if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = '_' then (
                          match String.unsafe_get s (pos+6) with
                            | 'x' -> (
                                2
                              )
                            | 'y' -> (
                                3
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'w' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 's' then (
                          9
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 8 -> (
                  match String.unsafe_get s pos with
                    | 'g' -> (
                        if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'd' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'l' then (
                          13
                        )
                        else (
                          -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 's' then (
                          11
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 9 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 's' then (
                          8
                        )
                        else (
                          -1
                        )
                      )
                    | 'g' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = 'e' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        match String.unsafe_get s (pos+1) with
                          | 'e' -> (
                              if String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'w' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = '_' then (
                                match String.unsafe_get s (pos+8) with
                                  | 'x' -> (
                                      5
                                    )
                                  | 'y' -> (
                                      6
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                              else (
                                -1
                              )
                            )
                          | 'o' -> (
                              if String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' then (
                                4
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'h' then (
                    12
                  )
                  else (
                    -1
                  )
                )
              | 14 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'w' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'p' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'n' then (
                    10
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 'g' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' then (
                    7
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_ghost_id := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_game_mode := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 2 ->
              field_ghost_x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 3 ->
              field_ghost_y := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 4 ->
              field_room_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 5 ->
              field_respawn_x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 6 ->
              field_respawn_y := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 7 ->
              field_ghosts_in_party := (
                Some (
                  (
                    read__string_list
                  ) p lb
                )
              );
            | 8 ->
              field_abilities := (
                Some (
                  (
                    read_ghost_abilities
                  ) p lb
                )
              );
            | 9 ->
              field_weapons := (
                Some (
                  (
                    read__string_list
                  ) p lb
                )
              );
            | 10 ->
              field_current_weapon := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 11 ->
              field_progress := (
                Some (
                  (
                    read_game_progress
                  ) p lb
                )
              );
            | 12 ->
              field_max_health := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 13 ->
              field_max_soul := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            ghost_id = (match !field_ghost_id with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "ghost_id");
            game_mode = (match !field_game_mode with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "game_mode");
            ghost_x = (match !field_ghost_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "ghost_x");
            ghost_y = (match !field_ghost_y with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "ghost_y");
            room_name = (match !field_room_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "room_name");
            respawn_x = (match !field_respawn_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "respawn_x");
            respawn_y = (match !field_respawn_y with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "respawn_y");
            ghosts_in_party = (match !field_ghosts_in_party with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "ghosts_in_party");
            abilities = (match !field_abilities with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "abilities");
            weapons = (match !field_weapons with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "weapons");
            current_weapon = (match !field_current_weapon with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "current_weapon");
            progress = (match !field_progress with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "progress");
            max_health = (match !field_max_health with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "max_health");
            max_soul = (match !field_max_soul with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "max_soul");
          }
         : save_file)
      )
)
let save_file_of_string s =
  read_save_file (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_object_layer : _ -> object_layer -> _ = (
  fun ob (x : object_layer) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"objects\":";
    (
      write__coll_rect_list
    )
      ob x.objects;
    Buffer.add_char ob '}';
)
let string_of_object_layer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_object_layer ob x;
  Buffer.contents ob
let read_object_layer = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (None) in
    let field_objects = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'j' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_objects := (
              Some (
                (
                  read__coll_rect_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = 'j' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_objects := (
                Some (
                  (
                    read__coll_rect_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            objects = (match !field_objects with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "objects");
          }
         : object_layer)
      )
)
let object_layer_of_string s =
  read_object_layer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_image_layer : _ -> image_layer -> _ = (
  fun ob (x : image_layer) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"image\":";
    (
      Yojson.Safe.write_string
    )
      ob x.image;
    Buffer.add_char ob '}';
)
let string_of_image_layer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_image_layer ob x;
  Buffer.contents ob
let read_image_layer = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (None) in
    let field_image = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_image := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_image := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            image = (match !field_image with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "image");
          }
         : image_layer)
      )
)
let image_layer_of_string s =
  read_image_layer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_layer = (
  Atdgen_runtime.Oj_run.write_with_adapter Atdgen_runtime.Json_adapter.Type_field.restore (
    fun ob x ->
      match x with
        | `TILE_LAYER x ->
          Buffer.add_string ob "<\"tilelayer\":";
          (
            write_tile_layer
          ) ob x;
          Buffer.add_char ob '>'
        | `IMAGE_LAYER x ->
          Buffer.add_string ob "<\"imagelayer\":";
          (
            write_image_layer
          ) ob x;
          Buffer.add_char ob '>'
        | `OBJECT_LAYER x ->
          Buffer.add_string ob "<\"objectgroup\":";
          (
            write_object_layer
          ) ob x;
          Buffer.add_char ob '>'
  )
)
let string_of_layer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_layer ob x;
  Buffer.contents ob
let read_layer = (
  Atdgen_runtime.Oj_run.read_with_adapter Atdgen_runtime.Json_adapter.Type_field.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "tilelayer" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_tile_layer
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `TILE_LAYER x
              | "imagelayer" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_image_layer
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `IMAGE_LAYER x
              | "objectgroup" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_object_layer
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `OBJECT_LAYER x
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "tilelayer" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_tile_layer
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `TILE_LAYER x
              | "imagelayer" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_image_layer
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `IMAGE_LAYER x
              | "objectgroup" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_object_layer
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `OBJECT_LAYER x
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let layer_of_string s =
  read_layer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__tileset_source_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_tileset_source
  )
)
let string_of__tileset_source_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__tileset_source_list ob x;
  Buffer.contents ob
let read__tileset_source_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_tileset_source
  )
)
let _tileset_source_list_of_string s =
  read__tileset_source_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__layer_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_layer
  )
)
let string_of__layer_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__layer_list ob x;
  Buffer.contents ob
let read__layer_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_layer
  )
)
let _layer_list_of_string s =
  read__layer_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_room : _ -> room -> _ = (
  fun ob (x : room) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"tileheight\":";
    (
      Yojson.Safe.write_float
    )
      ob x.tile_h;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"tilewidth\":";
    (
      Yojson.Safe.write_float
    )
      ob x.tile_w;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"height\":";
    (
      Yojson.Safe.write_int
    )
      ob x.h_in_tiles;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"width\":";
    (
      Yojson.Safe.write_int
    )
      ob x.w_in_tiles;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"layers\":";
    (
      write__layer_list
    )
      ob x.layers;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"tilesets\":";
    (
      write__tileset_source_list
    )
      ob x.tileset_sources;
    Buffer.add_char ob '}';
)
let string_of_room ?(len = 1024) x =
  let ob = Buffer.create len in
  write_room ob x;
  Buffer.contents ob
let read_room = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_tile_h = ref (None) in
    let field_tile_w = ref (None) in
    let field_h_in_tiles = ref (None) in
    let field_w_in_tiles = ref (None) in
    let field_layers = ref (None) in
    let field_tileset_sources = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                  3
                )
                else (
                  -1
                )
              )
            | 6 -> (
                match String.unsafe_get s pos with
                  | 'h' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 'l' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 's' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 8 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 's' then (
                  5
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'w' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'h' then (
                  1
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'g' && String.unsafe_get s (pos+8) = 'h' && String.unsafe_get s (pos+9) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_tile_h := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 1 ->
            field_tile_w := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 2 ->
            field_h_in_tiles := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 3 ->
            field_w_in_tiles := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 4 ->
            field_layers := (
              Some (
                (
                  read__layer_list
                ) p lb
              )
            );
          | 5 ->
            field_tileset_sources := (
              Some (
                (
                  read__tileset_source_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  match String.unsafe_get s pos with
                    | 'h' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'l' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 's' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 's' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'w' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'h' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'g' && String.unsafe_get s (pos+8) = 'h' && String.unsafe_get s (pos+9) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_tile_h := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 1 ->
              field_tile_w := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 2 ->
              field_h_in_tiles := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 3 ->
              field_w_in_tiles := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 4 ->
              field_layers := (
                Some (
                  (
                    read__layer_list
                  ) p lb
                )
              );
            | 5 ->
              field_tileset_sources := (
                Some (
                  (
                    read__tileset_source_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            tile_h = (match !field_tile_h with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "tile_h");
            tile_w = (match !field_tile_w with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "tile_w");
            h_in_tiles = (match !field_h_in_tiles with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "h_in_tiles");
            w_in_tiles = (match !field_w_in_tiles with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "w_in_tiles");
            layers = (match !field_layers with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "layers");
            tileset_sources = (match !field_tileset_sources with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "tileset_sources");
          }
         : room)
      )
)
let room_of_string s =
  read_room (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_npc_config : _ -> npc_config -> _ = (
  fun ob (x : npc_config) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"w\":";
    (
      Yojson.Safe.write_int
    )
      ob x.w;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"h\":";
    (
      Yojson.Safe.write_int
    )
      ob x.h;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"texture_configs\":";
    (
      write__string_texture_config_list
    )
      ob x.texture_configs;
    Buffer.add_char ob '}';
)
let string_of_npc_config ?(len = 1024) x =
  let ob = Buffer.create len in
  write_npc_config ob x;
  Buffer.contents ob
let read_npc_config = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_w = ref (None) in
    let field_h = ref (None) in
    let field_texture_configs = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 1 -> (
                match String.unsafe_get s pos with
                  | 'h' -> (
                      1
                    )
                  | 'w' -> (
                      0
                    )
                  | _ -> (
                      -1
                    )
              )
            | 15 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 's' then (
                  2
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_w := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_h := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 2 ->
            field_texture_configs := (
              Some (
                (
                  read__string_texture_config_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 1 -> (
                  match String.unsafe_get s pos with
                    | 'h' -> (
                        1
                      )
                    | 'w' -> (
                        0
                      )
                    | _ -> (
                        -1
                      )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 's' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_w := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_h := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 2 ->
              field_texture_configs := (
                Some (
                  (
                    read__string_texture_config_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            w = (match !field_w with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "w");
            h = (match !field_h with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "h");
            texture_configs = (match !field_texture_configs with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "texture_configs");
          }
         : npc_config)
      )
)
let npc_config_of_string s =
  read_npc_config (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_npc_config_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        write_npc_config
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_npc_config_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_npc_config_list ob x;
  Buffer.contents ob
let read__string_npc_config_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              read_npc_config
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _string_npc_config_list_of_string s =
  read__string_npc_config_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_npcs_file : _ -> npcs_file -> _ = (
  fun ob (x : npcs_file) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"npcs\":";
    (
      write__string_npc_config_list
    )
      ob x.npcs;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"shared_textures\":";
    (
      write__string_texture_config_list
    )
      ob x.shared_textures;
    Buffer.add_char ob '}';
)
let string_of_npcs_file ?(len = 1024) x =
  let ob = Buffer.create len in
  write_npcs_file ob x;
  Buffer.contents ob
let read_npcs_file = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_npcs = ref (None) in
    let field_shared_textures = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'p' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_npcs := (
              Some (
                (
                  read__string_npc_config_list
                ) p lb
              )
            );
          | 1 ->
            field_shared_textures := (
              Some (
                (
                  read__string_texture_config_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'p' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_npcs := (
                Some (
                  (
                    read__string_npc_config_list
                  ) p lb
                )
              );
            | 1 ->
              field_shared_textures := (
                Some (
                  (
                    read__string_texture_config_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            npcs = (match !field_npcs with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "npcs");
            shared_textures = (match !field_shared_textures with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "shared_textures");
          }
         : npcs_file)
      )
)
let npcs_file_of_string s =
  read_npcs_file (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_string_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_string_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_string_list ob x;
  Buffer.contents ob
let read__string_string_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _string_string_list_of_string s =
  read__string_string_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_lore_file = (
  write__string_string_list
)
let string_of_lore_file ?(len = 1024) x =
  let ob = Buffer.create len in
  write_lore_file ob x;
  Buffer.contents ob
let read_lore_file = (
  read__string_string_list
)
let lore_file_of_string s =
  read_lore_file (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_keybinds_file = (
  write__string_string_list
)
let string_of_keybinds_file ?(len = 1024) x =
  let ob = Buffer.create len in
  write_keybinds_file ob x;
  Buffer.contents ob
let read_keybinds_file = (
  read__string_string_list
)
let keybinds_file_of_string s =
  read_keybinds_file (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_jug_metadata : _ -> jug_metadata -> _ = (
  fun ob (x : jug_metadata) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"x\":";
    (
      Yojson.Safe.write_int
    )
      ob x.x;
    if x.w <> 2 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"w\":";
      (
        Yojson.Safe.write_int
      )
        ob x.w;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"h\":";
    (
      Yojson.Safe.write_int
    )
      ob x.h;
    Buffer.add_char ob '}';
)
let string_of_jug_metadata ?(len = 1024) x =
  let ob = Buffer.create len in
  write_jug_metadata ob x;
  Buffer.contents ob
let read_jug_metadata = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (None) in
    let field_x = ref (None) in
    let field_w = ref (2) in
    let field_h = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 1 -> (
                match String.unsafe_get s pos with
                  | 'h' -> (
                      3
                    )
                  | 'w' -> (
                      2
                    )
                  | 'x' -> (
                      1
                    )
                  | _ -> (
                      -1
                    )
              )
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_w := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 3 ->
            field_h := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 1 -> (
                  match String.unsafe_get s pos with
                    | 'h' -> (
                        3
                      )
                    | 'w' -> (
                        2
                      )
                    | 'x' -> (
                        1
                      )
                    | _ -> (
                        -1
                      )
                )
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_w := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 3 ->
              field_h := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            x = (match !field_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x");
            w = !field_w;
            h = (match !field_h with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "h");
          }
         : jug_metadata)
      )
)
let jug_metadata_of_string s =
  read_jug_metadata (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__jug_metadata_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_jug_metadata
  )
)
let string_of__jug_metadata_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__jug_metadata_list ob x;
  Buffer.contents ob
let read__jug_metadata_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_jug_metadata
  )
)
let _jug_metadata_list_of_string s =
  read__jug_metadata_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_jug_metadata_file = (
  write__jug_metadata_list
)
let string_of_jug_metadata_file ?(len = 1024) x =
  let ob = Buffer.create len in
  write_jug_metadata_file ob x;
  Buffer.contents ob
let read_jug_metadata_file = (
  read__jug_metadata_list
)
let jug_metadata_file_of_string s =
  read_jug_metadata_file (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__float_float_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_float
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        Yojson.Safe.write_float
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__float_float_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__float_float_list ob x;
  Buffer.contents ob
let read__float_float_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_number
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_number
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _float_float_list_of_string s =
  read__float_float_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_ghost_action : _ -> ghost_action -> _ = (
  fun ob (x : ghost_action) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if x.duration <> 0.066666 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"duration\":";
      (
        Yojson.Safe.write_float
      )
        ob x.duration;
    );
    if x.cooldown <> 0.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"cooldown\":";
      (
        Yojson.Safe.write_float
      )
        ob x.cooldown;
    );
    if x.input_buffer <> 0.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"input_buffer\":";
      (
        Yojson.Safe.write_float
      )
        ob x.input_buffer;
    );
    if x.collision_shape <> [] then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"collision_shape\":";
      (
        write__float_float_list
      )
        ob x.collision_shape;
    );
    Buffer.add_char ob '}';
)
let string_of_ghost_action ?(len = 1024) x =
  let ob = Buffer.create len in
  write_ghost_action ob x;
  Buffer.contents ob
let read_ghost_action = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_duration = ref (0.066666) in
    let field_cooldown = ref (0.0) in
    let field_input_buffer = ref (0.0) in
    let field_collision_shape = ref ([]) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 8 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'w' && String.unsafe_get s (pos+7) = 'n' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'f' && String.unsafe_get s (pos+9) = 'f' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' then (
                  2
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 'h' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'p' && String.unsafe_get s (pos+14) = 'e' then (
                  3
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_duration := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_cooldown := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_input_buffer := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_collision_shape := (
                (
                  read__float_float_list
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 8 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'w' && String.unsafe_get s (pos+7) = 'n' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'f' && String.unsafe_get s (pos+9) = 'f' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 'h' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'p' && String.unsafe_get s (pos+14) = 'e' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_duration := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_cooldown := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_input_buffer := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_collision_shape := (
                  (
                    read__float_float_list
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            duration = !field_duration;
            cooldown = !field_cooldown;
            input_buffer = !field_input_buffer;
            collision_shape = !field_collision_shape;
          }
         : ghost_action)
      )
)
let ghost_action_of_string s =
  read_ghost_action (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_ghost_action_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        write_ghost_action
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_ghost_action_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_ghost_action_list ob x;
  Buffer.contents ob
let read__string_ghost_action_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              read_ghost_action
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _string_ghost_action_list_of_string s =
  read__string_ghost_action_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_ghosts_file : _ -> ghosts_file -> _ = (
  fun ob (x : ghosts_file) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"body_textures\":";
    (
      write_texture_configs
    )
      ob x.body_textures;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"shared_textures\":";
    (
      write_texture_configs
    )
      ob x.shared_textures;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"actions\":";
    (
      write__string_ghost_action_list
    )
      ob x.actions;
    Buffer.add_char ob '}';
)
let string_of_ghosts_file ?(len = 1024) x =
  let ob = Buffer.create len in
  write_ghosts_file ob x;
  Buffer.contents ob
let read_ghosts_file = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_body_textures = ref (None) in
    let field_shared_textures = ref (None) in
    let field_actions = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 7 -> (
                if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 's' then (
                  2
                )
                else (
                  -1
                )
              )
            | 13 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'y' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'x' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_body_textures := (
              Some (
                (
                  read_texture_configs
                ) p lb
              )
            );
          | 1 ->
            field_shared_textures := (
              Some (
                (
                  read_texture_configs
                ) p lb
              )
            );
          | 2 ->
            field_actions := (
              Some (
                (
                  read__string_ghost_action_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 7 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 's' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 13 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'y' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'x' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_body_textures := (
                Some (
                  (
                    read_texture_configs
                  ) p lb
                )
              );
            | 1 ->
              field_shared_textures := (
                Some (
                  (
                    read_texture_configs
                  ) p lb
                )
              );
            | 2 ->
              field_actions := (
                Some (
                  (
                    read__string_ghost_action_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            body_textures = (match !field_body_textures with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "body_textures");
            shared_textures = (match !field_shared_textures with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "shared_textures");
            actions = (match !field_actions with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "actions");
          }
         : ghosts_file)
      )
)
let ghosts_file_of_string s =
  read_ghosts_file (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_enemy_dream_nail_config : _ -> enemy_dream_nail_config -> _ = (
  fun ob (x : enemy_dream_nail_config) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if x.recoil_vx <> 1800.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"recoil_vx\":";
      (
        Yojson.Safe.write_float
      )
        ob x.recoil_vx;
    );
    if x.vulnerable <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"vulnerable\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.vulnerable;
    );
    Buffer.add_char ob '}';
)
let string_of_enemy_dream_nail_config ?(len = 1024) x =
  let ob = Buffer.create len in
  write_enemy_dream_nail_config ob x;
  Buffer.contents ob
let read_enemy_dream_nail_config = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_recoil_vx = ref (1800.0) in
    let field_vulnerable = ref (true) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 9 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'v' && String.unsafe_get s (pos+8) = 'x' then (
                  0
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'b' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'e' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_recoil_vx := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_vulnerable := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 9 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'v' && String.unsafe_get s (pos+8) = 'x' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'b' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'e' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_recoil_vx := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_vulnerable := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            recoil_vx = !field_recoil_vx;
            vulnerable = !field_vulnerable;
          }
         : enemy_dream_nail_config)
      )
)
let enemy_dream_nail_config_of_string s =
  read_enemy_dream_nail_config (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_float_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        Yojson.Safe.write_float
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_float_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_float_list ob x;
  Buffer.contents ob
let read__string_float_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_number
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _string_float_list_of_string s =
  read__string_float_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_enemy_config : _ -> enemy_config -> _ = (
  fun ob (x : enemy_config) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"w\":";
    (
      Yojson.Safe.write_int
    )
      ob x.w;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"h\":";
    (
      Yojson.Safe.write_int
    )
      ob x.h;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"health\":";
    (
      Yojson.Safe.write_int
    )
      ob x.health;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"kind\":";
    (
      Yojson.Safe.write_string
    )
      ob x.kind;
    if x.scale <> 1.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"scale\":";
      (
        Yojson.Safe.write_float
      )
        ob x.scale;
    );
    if x.damage <> 1 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"damage\":";
      (
        Yojson.Safe.write_int
      )
        ob x.damage;
    );
    if x.gravity_multiplier <> 1.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"gravity_multiplier\":";
      (
        Yojson.Safe.write_float
      )
        ob x.gravity_multiplier;
    );
    if x.death_gravity_multiplier <> 1.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"death_gravity_multiplier\":";
      (
        Yojson.Safe.write_float
      )
        ob x.death_gravity_multiplier;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"dream_nail\":";
    (
      write_enemy_dream_nail_config
    )
      ob x.dream_nail;
    if x.attrs <> [] then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"attrs\":";
      (
        write__string_float_list
      )
        ob x.attrs;
    );
    if x.unscaled_attrs <> [] then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"unscaled_attrs\":";
      (
        write__string_float_list
      )
        ob x.unscaled_attrs;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"texture_configs\":";
    (
      write_texture_configs
    )
      ob x.texture_configs;
    if x.can_take_damage <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"can_take_damage\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.can_take_damage;
    );
    if x.can_recoil <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"can_recoil\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.can_recoil;
    );
    Buffer.add_char ob '}';
)
let string_of_enemy_config ?(len = 1024) x =
  let ob = Buffer.create len in
  write_enemy_config ob x;
  Buffer.contents ob
let read_enemy_config = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_w = ref (None) in
    let field_h = ref (None) in
    let field_health = ref (None) in
    let field_kind = ref (None) in
    let field_scale = ref (1.0) in
    let field_damage = ref (1) in
    let field_gravity_multiplier = ref (1.0) in
    let field_death_gravity_multiplier = ref (1.0) in
    let field_dream_nail = ref (None) in
    let field_attrs = ref ([]) in
    let field_unscaled_attrs = ref ([]) in
    let field_texture_configs = ref (None) in
    let field_can_take_damage = ref (true) in
    let field_can_recoil = ref (true) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 1 -> (
                match String.unsafe_get s pos with
                  | 'h' -> (
                      1
                    )
                  | 'w' -> (
                      0
                    )
                  | _ -> (
                      -1
                    )
              )
            | 4 -> (
                if String.unsafe_get s pos = 'k' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'd' then (
                  3
                )
                else (
                  -1
                )
              )
            | 5 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 's' then (
                        9
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 6 -> (
                match String.unsafe_get s pos with
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'e' then (
                        5
                      )
                      else (
                        -1
                      )
                    )
                  | 'h' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'h' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 10 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'l' then (
                        13
                      )
                      else (
                        -1
                      )
                    )
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'l' then (
                        8
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 14 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 's' then (
                  10
                )
                else (
                  -1
                )
              )
            | 15 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'k' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 'e' then (
                        12
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 's' then (
                        11
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 18 -> (
                if String.unsafe_get s pos = 'g' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'y' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'p' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'i' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 'r' then (
                  6
                )
                else (
                  -1
                )
              )
            | 24 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'y' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'm' && String.unsafe_get s (pos+15) = 'u' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = 't' && String.unsafe_get s (pos+18) = 'i' && String.unsafe_get s (pos+19) = 'p' && String.unsafe_get s (pos+20) = 'l' && String.unsafe_get s (pos+21) = 'i' && String.unsafe_get s (pos+22) = 'e' && String.unsafe_get s (pos+23) = 'r' then (
                  7
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_w := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_h := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 2 ->
            field_health := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 3 ->
            field_kind := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_scale := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_damage := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 6 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_gravity_multiplier := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 7 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_death_gravity_multiplier := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 8 ->
            field_dream_nail := (
              Some (
                (
                  read_enemy_dream_nail_config
                ) p lb
              )
            );
          | 9 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_attrs := (
                (
                  read__string_float_list
                ) p lb
              );
            )
          | 10 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_unscaled_attrs := (
                (
                  read__string_float_list
                ) p lb
              );
            )
          | 11 ->
            field_texture_configs := (
              Some (
                (
                  read_texture_configs
                ) p lb
              )
            );
          | 12 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_can_take_damage := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 13 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_can_recoil := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 1 -> (
                  match String.unsafe_get s pos with
                    | 'h' -> (
                        1
                      )
                    | 'w' -> (
                        0
                      )
                    | _ -> (
                        -1
                      )
                )
              | 4 -> (
                  if String.unsafe_get s pos = 'k' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'd' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 's' then (
                          9
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 6 -> (
                  match String.unsafe_get s pos with
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'e' then (
                          5
                        )
                        else (
                          -1
                        )
                      )
                    | 'h' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'h' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 10 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'l' then (
                          13
                        )
                        else (
                          -1
                        )
                      )
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'l' then (
                          8
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 14 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'd' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 's' then (
                    10
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'k' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 'e' then (
                          12
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 's' then (
                          11
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 18 -> (
                  if String.unsafe_get s pos = 'g' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'y' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'p' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'i' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 'r' then (
                    6
                  )
                  else (
                    -1
                  )
                )
              | 24 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'y' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'm' && String.unsafe_get s (pos+15) = 'u' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = 't' && String.unsafe_get s (pos+18) = 'i' && String.unsafe_get s (pos+19) = 'p' && String.unsafe_get s (pos+20) = 'l' && String.unsafe_get s (pos+21) = 'i' && String.unsafe_get s (pos+22) = 'e' && String.unsafe_get s (pos+23) = 'r' then (
                    7
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_w := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_h := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 2 ->
              field_health := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 3 ->
              field_kind := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_scale := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_damage := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 6 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_gravity_multiplier := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 7 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_death_gravity_multiplier := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 8 ->
              field_dream_nail := (
                Some (
                  (
                    read_enemy_dream_nail_config
                  ) p lb
                )
              );
            | 9 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_attrs := (
                  (
                    read__string_float_list
                  ) p lb
                );
              )
            | 10 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_unscaled_attrs := (
                  (
                    read__string_float_list
                  ) p lb
                );
              )
            | 11 ->
              field_texture_configs := (
                Some (
                  (
                    read_texture_configs
                  ) p lb
                )
              );
            | 12 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_can_take_damage := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 13 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_can_recoil := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            w = (match !field_w with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "w");
            h = (match !field_h with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "h");
            health = (match !field_health with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "health");
            kind = (match !field_kind with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "kind");
            scale = !field_scale;
            damage = !field_damage;
            gravity_multiplier = !field_gravity_multiplier;
            death_gravity_multiplier = !field_death_gravity_multiplier;
            dream_nail = (match !field_dream_nail with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "dream_nail");
            attrs = !field_attrs;
            unscaled_attrs = !field_unscaled_attrs;
            texture_configs = (match !field_texture_configs with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "texture_configs");
            can_take_damage = !field_can_take_damage;
            can_recoil = !field_can_recoil;
          }
         : enemy_config)
      )
)
let enemy_config_of_string s =
  read_enemy_config (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_enemy_config_list = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        write_enemy_config
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_enemy_config_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_enemy_config_list ob x;
  Buffer.contents ob
let read__string_enemy_config_list = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Atdgen_runtime.Oj_run.read_string
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              read_enemy_config
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _string_enemy_config_list_of_string s =
  read__string_enemy_config_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_enemies_file : _ -> enemies_file -> _ = (
  fun ob (x : enemies_file) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"enemies\":";
    (
      write__string_enemy_config_list
    )
      ob x.enemies;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"shared_textures\":";
    (
      write__string_texture_config_list
    )
      ob x.shared_textures;
    Buffer.add_char ob '}';
)
let string_of_enemies_file ?(len = 1024) x =
  let ob = Buffer.create len in
  write_enemies_file ob x;
  Buffer.contents ob
let read_enemies_file = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_enemies = ref (None) in
    let field_shared_textures = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 7 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_enemies := (
              Some (
                (
                  read__string_enemy_config_list
                ) p lb
              )
            );
          | 1 ->
            field_shared_textures := (
              Some (
                (
                  read__string_texture_config_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 7 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_enemies := (
                Some (
                  (
                    read__string_enemy_config_list
                  ) p lb
                )
              );
            | 1 ->
              field_shared_textures := (
                Some (
                  (
                    read__string_texture_config_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            enemies = (match !field_enemies with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "enemies");
            shared_textures = (match !field_shared_textures with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "shared_textures");
          }
         : enemies_file)
      )
)
let enemies_file_of_string s =
  read_enemies_file (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
