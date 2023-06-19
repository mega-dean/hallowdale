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
  swing_speed: float
}

type weapons_file = Json_t.weapons_file

type tileset_source = Json_t.tileset_source = {
  firstgid: int;
  source: string
}

type coll_rect = Json_t.coll_rect = {
  name: string;
  x: float;
  y: float;
  h: float;
  w: float
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
  x_offset: int;
  y_offset: int
}

type room_progress = Json_t.room_progress = {
  mutable removed_idxs_by_layer: (string * int list) list;
  mutable finished_interactions: string list;
  mutable revealed_shadow_layers: string list
}

type ghost_abilities = Json_t.ghost_abilities = {
  mutable crystal_heart: bool;
  mutable mantis_claw: bool;
  mutable monarch_wings: bool;
  mutable mothwing_cloak: bool;
  mutable vengeful_spirit: bool;
  mutable desolate_dive: bool;
  mutable howling_wraiths: bool
}

type save_file = Json_t.save_file = {
  ghost_id: string;
  ghost_x: float;
  ghost_y: float;
  respawn_x: float;
  respawn_y: float;
  ghosts_in_party: string list;
  room_name: string;
  abilities: ghost_abilities;
  weapons: string list;
  current_weapon: string;
  progress: (string * room_progress) list
}

type object_layer = Json_t.object_layer = {
  name: string;
  objects: coll_rect list
}

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

type ghost_texture_configs = Json_t.ghost_texture_configs

type ghost_action = Json_t.ghost_action = {
  duration: float;
  cooldown: float;
  input_buffer: float;
  collision_shape: (float * float) list
}

type ghosts_file = Json_t.ghosts_file = {
  individual_textures: (string * ghost_texture_configs) list;
  shared_textures: (string * texture_config) list;
  actions: (string * ghost_action) list
}

type enemy_config = Json_t.enemy_config = {
  health: int;
  w: int;
  h: int;
  kind: string;
  props: (string * float) list;
  texture_configs: (string * texture_config) list
}

type enemies_file = Json_t.enemies_file = {
  enemies: (string * enemy_config) list;
  shared_textures: (string * texture_config) list
}

val write_global_map :
  Buffer.t -> global_map -> unit
  (** Output a JSON value of type {!type:global_map}. *)

val string_of_global_map :
  ?len:int -> global_map -> string
  (** Serialize a value of type {!type:global_map}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_global_map :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> global_map
  (** Input JSON data of type {!type:global_map}. *)

val global_map_of_string :
  string -> global_map
  (** Deserialize JSON data of type {!type:global_map}. *)

val write_world :
  Buffer.t -> world -> unit
  (** Output a JSON value of type {!type:world}. *)

val string_of_world :
  ?len:int -> world -> string
  (** Serialize a value of type {!type:world}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_world :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> world
  (** Input JSON data of type {!type:world}. *)

val world_of_string :
  string -> world
  (** Deserialize JSON data of type {!type:world}. *)

val write_color :
  Buffer.t -> color -> unit
  (** Output a JSON value of type {!type:color}. *)

val string_of_color :
  ?len:int -> color -> string
  (** Serialize a value of type {!type:color}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_color :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> color
  (** Input JSON data of type {!type:color}. *)

val color_of_string :
  string -> color
  (** Deserialize JSON data of type {!type:color}. *)

val write_weapon :
  Buffer.t -> weapon -> unit
  (** Output a JSON value of type {!type:weapon}. *)

val string_of_weapon :
  ?len:int -> weapon -> string
  (** Serialize a value of type {!type:weapon}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_weapon :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> weapon
  (** Input JSON data of type {!type:weapon}. *)

val weapon_of_string :
  string -> weapon
  (** Deserialize JSON data of type {!type:weapon}. *)

val write_weapons_file :
  Buffer.t -> weapons_file -> unit
  (** Output a JSON value of type {!type:weapons_file}. *)

val string_of_weapons_file :
  ?len:int -> weapons_file -> string
  (** Serialize a value of type {!type:weapons_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_weapons_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> weapons_file
  (** Input JSON data of type {!type:weapons_file}. *)

val weapons_file_of_string :
  string -> weapons_file
  (** Deserialize JSON data of type {!type:weapons_file}. *)

val write_tileset_source :
  Buffer.t -> tileset_source -> unit
  (** Output a JSON value of type {!type:tileset_source}. *)

val string_of_tileset_source :
  ?len:int -> tileset_source -> string
  (** Serialize a value of type {!type:tileset_source}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tileset_source :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tileset_source
  (** Input JSON data of type {!type:tileset_source}. *)

val tileset_source_of_string :
  string -> tileset_source
  (** Deserialize JSON data of type {!type:tileset_source}. *)

val write_coll_rect :
  Buffer.t -> coll_rect -> unit
  (** Output a JSON value of type {!type:coll_rect}. *)

val string_of_coll_rect :
  ?len:int -> coll_rect -> string
  (** Serialize a value of type {!type:coll_rect}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_coll_rect :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> coll_rect
  (** Input JSON data of type {!type:coll_rect}. *)

val coll_rect_of_string :
  string -> coll_rect
  (** Deserialize JSON data of type {!type:coll_rect}. *)

val write_object_group :
  Buffer.t -> object_group -> unit
  (** Output a JSON value of type {!type:object_group}. *)

val string_of_object_group :
  ?len:int -> object_group -> string
  (** Serialize a value of type {!type:object_group}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_object_group :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> object_group
  (** Input JSON data of type {!type:object_group}. *)

val object_group_of_string :
  string -> object_group
  (** Deserialize JSON data of type {!type:object_group}. *)

val write_collision :
  Buffer.t -> collision -> unit
  (** Output a JSON value of type {!type:collision}. *)

val string_of_collision :
  ?len:int -> collision -> string
  (** Serialize a value of type {!type:collision}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_collision :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> collision
  (** Input JSON data of type {!type:collision}. *)

val collision_of_string :
  string -> collision
  (** Deserialize JSON data of type {!type:collision}. *)

val write_tileset :
  Buffer.t -> tileset -> unit
  (** Output a JSON value of type {!type:tileset}. *)

val string_of_tileset :
  ?len:int -> tileset -> string
  (** Serialize a value of type {!type:tileset}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tileset :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tileset
  (** Input JSON data of type {!type:tileset}. *)

val tileset_of_string :
  string -> tileset
  (** Deserialize JSON data of type {!type:tileset}. *)

val write_tile_layer :
  Buffer.t -> tile_layer -> unit
  (** Output a JSON value of type {!type:tile_layer}. *)

val string_of_tile_layer :
  ?len:int -> tile_layer -> string
  (** Serialize a value of type {!type:tile_layer}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tile_layer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tile_layer
  (** Input JSON data of type {!type:tile_layer}. *)

val tile_layer_of_string :
  string -> tile_layer
  (** Deserialize JSON data of type {!type:tile_layer}. *)

val write_texture_config :
  Buffer.t -> texture_config -> unit
  (** Output a JSON value of type {!type:texture_config}. *)

val string_of_texture_config :
  ?len:int -> texture_config -> string
  (** Serialize a value of type {!type:texture_config}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_texture_config :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> texture_config
  (** Input JSON data of type {!type:texture_config}. *)

val texture_config_of_string :
  string -> texture_config
  (** Deserialize JSON data of type {!type:texture_config}. *)

val write_room_progress :
  Buffer.t -> room_progress -> unit
  (** Output a JSON value of type {!type:room_progress}. *)

val string_of_room_progress :
  ?len:int -> room_progress -> string
  (** Serialize a value of type {!type:room_progress}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_room_progress :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> room_progress
  (** Input JSON data of type {!type:room_progress}. *)

val room_progress_of_string :
  string -> room_progress
  (** Deserialize JSON data of type {!type:room_progress}. *)

val write_ghost_abilities :
  Buffer.t -> ghost_abilities -> unit
  (** Output a JSON value of type {!type:ghost_abilities}. *)

val string_of_ghost_abilities :
  ?len:int -> ghost_abilities -> string
  (** Serialize a value of type {!type:ghost_abilities}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ghost_abilities :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ghost_abilities
  (** Input JSON data of type {!type:ghost_abilities}. *)

val ghost_abilities_of_string :
  string -> ghost_abilities
  (** Deserialize JSON data of type {!type:ghost_abilities}. *)

val write_save_file :
  Buffer.t -> save_file -> unit
  (** Output a JSON value of type {!type:save_file}. *)

val string_of_save_file :
  ?len:int -> save_file -> string
  (** Serialize a value of type {!type:save_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_save_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> save_file
  (** Input JSON data of type {!type:save_file}. *)

val save_file_of_string :
  string -> save_file
  (** Deserialize JSON data of type {!type:save_file}. *)

val write_object_layer :
  Buffer.t -> object_layer -> unit
  (** Output a JSON value of type {!type:object_layer}. *)

val string_of_object_layer :
  ?len:int -> object_layer -> string
  (** Serialize a value of type {!type:object_layer}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_object_layer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> object_layer
  (** Input JSON data of type {!type:object_layer}. *)

val object_layer_of_string :
  string -> object_layer
  (** Deserialize JSON data of type {!type:object_layer}. *)

val write_layer :
  Buffer.t -> layer -> unit
  (** Output a JSON value of type {!type:layer}. *)

val string_of_layer :
  ?len:int -> layer -> string
  (** Serialize a value of type {!type:layer}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_layer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> layer
  (** Input JSON data of type {!type:layer}. *)

val layer_of_string :
  string -> layer
  (** Deserialize JSON data of type {!type:layer}. *)

val write_room :
  Buffer.t -> room -> unit
  (** Output a JSON value of type {!type:room}. *)

val string_of_room :
  ?len:int -> room -> string
  (** Serialize a value of type {!type:room}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_room :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> room
  (** Input JSON data of type {!type:room}. *)

val room_of_string :
  string -> room
  (** Deserialize JSON data of type {!type:room}. *)

val write_npc_config :
  Buffer.t -> npc_config -> unit
  (** Output a JSON value of type {!type:npc_config}. *)

val string_of_npc_config :
  ?len:int -> npc_config -> string
  (** Serialize a value of type {!type:npc_config}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_npc_config :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> npc_config
  (** Input JSON data of type {!type:npc_config}. *)

val npc_config_of_string :
  string -> npc_config
  (** Deserialize JSON data of type {!type:npc_config}. *)

val write_npcs_file :
  Buffer.t -> npcs_file -> unit
  (** Output a JSON value of type {!type:npcs_file}. *)

val string_of_npcs_file :
  ?len:int -> npcs_file -> string
  (** Serialize a value of type {!type:npcs_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_npcs_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> npcs_file
  (** Input JSON data of type {!type:npcs_file}. *)

val npcs_file_of_string :
  string -> npcs_file
  (** Deserialize JSON data of type {!type:npcs_file}. *)

val write_lore_file :
  Buffer.t -> lore_file -> unit
  (** Output a JSON value of type {!type:lore_file}. *)

val string_of_lore_file :
  ?len:int -> lore_file -> string
  (** Serialize a value of type {!type:lore_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_lore_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> lore_file
  (** Input JSON data of type {!type:lore_file}. *)

val lore_file_of_string :
  string -> lore_file
  (** Deserialize JSON data of type {!type:lore_file}. *)

val write_keybinds_file :
  Buffer.t -> keybinds_file -> unit
  (** Output a JSON value of type {!type:keybinds_file}. *)

val string_of_keybinds_file :
  ?len:int -> keybinds_file -> string
  (** Serialize a value of type {!type:keybinds_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_keybinds_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> keybinds_file
  (** Input JSON data of type {!type:keybinds_file}. *)

val keybinds_file_of_string :
  string -> keybinds_file
  (** Deserialize JSON data of type {!type:keybinds_file}. *)

val write_jug_metadata :
  Buffer.t -> jug_metadata -> unit
  (** Output a JSON value of type {!type:jug_metadata}. *)

val string_of_jug_metadata :
  ?len:int -> jug_metadata -> string
  (** Serialize a value of type {!type:jug_metadata}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_jug_metadata :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> jug_metadata
  (** Input JSON data of type {!type:jug_metadata}. *)

val jug_metadata_of_string :
  string -> jug_metadata
  (** Deserialize JSON data of type {!type:jug_metadata}. *)

val write_jug_metadata_file :
  Buffer.t -> jug_metadata_file -> unit
  (** Output a JSON value of type {!type:jug_metadata_file}. *)

val string_of_jug_metadata_file :
  ?len:int -> jug_metadata_file -> string
  (** Serialize a value of type {!type:jug_metadata_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_jug_metadata_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> jug_metadata_file
  (** Input JSON data of type {!type:jug_metadata_file}. *)

val jug_metadata_file_of_string :
  string -> jug_metadata_file
  (** Deserialize JSON data of type {!type:jug_metadata_file}. *)

val write_ghost_texture_configs :
  Buffer.t -> ghost_texture_configs -> unit
  (** Output a JSON value of type {!type:ghost_texture_configs}. *)

val string_of_ghost_texture_configs :
  ?len:int -> ghost_texture_configs -> string
  (** Serialize a value of type {!type:ghost_texture_configs}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ghost_texture_configs :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ghost_texture_configs
  (** Input JSON data of type {!type:ghost_texture_configs}. *)

val ghost_texture_configs_of_string :
  string -> ghost_texture_configs
  (** Deserialize JSON data of type {!type:ghost_texture_configs}. *)

val write_ghost_action :
  Buffer.t -> ghost_action -> unit
  (** Output a JSON value of type {!type:ghost_action}. *)

val string_of_ghost_action :
  ?len:int -> ghost_action -> string
  (** Serialize a value of type {!type:ghost_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ghost_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ghost_action
  (** Input JSON data of type {!type:ghost_action}. *)

val ghost_action_of_string :
  string -> ghost_action
  (** Deserialize JSON data of type {!type:ghost_action}. *)

val write_ghosts_file :
  Buffer.t -> ghosts_file -> unit
  (** Output a JSON value of type {!type:ghosts_file}. *)

val string_of_ghosts_file :
  ?len:int -> ghosts_file -> string
  (** Serialize a value of type {!type:ghosts_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ghosts_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ghosts_file
  (** Input JSON data of type {!type:ghosts_file}. *)

val ghosts_file_of_string :
  string -> ghosts_file
  (** Deserialize JSON data of type {!type:ghosts_file}. *)

val write_enemy_config :
  Buffer.t -> enemy_config -> unit
  (** Output a JSON value of type {!type:enemy_config}. *)

val string_of_enemy_config :
  ?len:int -> enemy_config -> string
  (** Serialize a value of type {!type:enemy_config}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_enemy_config :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> enemy_config
  (** Input JSON data of type {!type:enemy_config}. *)

val enemy_config_of_string :
  string -> enemy_config
  (** Deserialize JSON data of type {!type:enemy_config}. *)

val write_enemies_file :
  Buffer.t -> enemies_file -> unit
  (** Output a JSON value of type {!type:enemies_file}. *)

val string_of_enemies_file :
  ?len:int -> enemies_file -> string
  (** Serialize a value of type {!type:enemies_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_enemies_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> enemies_file
  (** Input JSON data of type {!type:enemies_file}. *)

val enemies_file_of_string :
  string -> enemies_file
  (** Deserialize JSON data of type {!type:enemies_file}. *)

