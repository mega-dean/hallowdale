(* Auto-generated from "json.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type global_map = {
  file_name: string;
  x: int;
  y: int;
  h_in_pixels: int;
  w_in_pixels: int
}

type world = { global_maps: global_map list }

type color = { r: int; g: int; b: int; a: int }

type weapon = {
  tint: color;
  pickup_text: string;
  damage: int;
  scale_x: float;
  scale_y: float;
  swing_speed: float
}

type weapons_file = (string * weapon) list

type tileset_source = { firstgid: int; source: string }

type coll_rect = {
  gid: int;
  name: string;
  x: float;
  y: float;
  h: float;
  w: float
}

type object_group = { objects: coll_rect list }

type collision = { id: int; objectgroup: object_group }

type tileset = {
  name: string;
  tile_h: float;
  tile_w: float;
  source: string;
  columns: int;
  collisions: collision list;
  tile_count: int
}

type tile_layer = {
  name: string;
  data: int list;
  h: int;
  w: int;
  parallax_x: float;
  parallax_y: float;
  offset_x: float;
  offset_y: float
}

type texture_config = {
  count: int;
  duration: float;
  x_offset: int;
  y_offset: int
}

type room_progress = {
  mutable removed_idxs_by_layer: (string * int list) list;
  mutable finished_interactions: string list;
  mutable revealed_shadow_layers: string list
}

type ghost_abilities = {
  mutable crystal_heart: bool;
  mutable mantis_claw: bool;
  mutable monarch_wings: bool;
  mutable mothwing_cloak: bool;
  mutable shade_cloak: bool;
  mutable ismas_tear: bool;
  mutable vengeful_spirit: bool;
  mutable desolate_dive: bool;
  mutable howling_wraiths: bool
}

type save_file = {
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

type object_layer = { name: string; objects: coll_rect list }

type image_layer = { name: string; image: string }

type layer = [
    `TILE_LAYER of tile_layer
  | `IMAGE_LAYER of image_layer
  | `OBJECT_LAYER of object_layer
]

type room = {
  tile_h: float;
  tile_w: float;
  h_in_tiles: int;
  w_in_tiles: int;
  layers: layer list;
  tileset_sources: tileset_source list
}

type npc_config = {
  w: int;
  h: int;
  texture_configs: (string * texture_config) list
}

type npcs_file = {
  npcs: (string * npc_config) list;
  shared_textures: (string * texture_config) list
}

type lore_file = (string * string) list

type keybinds_file = (string * string) list

type jug_metadata = { name: string; x: int; w: int; h: int }

type jug_metadata_file = jug_metadata list

type ghost_texture_configs = (string * texture_config) list

type ghost_action = {
  duration: float;
  cooldown: float;
  input_buffer: float;
  collision_shape: (float * float) list
}

type ghosts_file = {
  individual_textures: (string * ghost_texture_configs) list;
  shared_textures: (string * texture_config) list;
  actions: (string * ghost_action) list
}

type enemy_dream_nail_config = {
  dialogues: string list;
  recoil_vx: float;
  vulnerable: bool
}

type enemy_config = {
  w: int;
  h: int;
  health: int;
  kind: string;
  damage: int;
  gravity_multiplier: float;
  death_gravity_multiplier: float;
  can_take_damage: bool;
  dream_nail: enemy_dream_nail_config;
  props: (string * float) list;
  texture_configs: (string * texture_config) list
}

type enemies_file = {
  enemies: (string * enemy_config) list;
  shared_textures: (string * texture_config) list
}
