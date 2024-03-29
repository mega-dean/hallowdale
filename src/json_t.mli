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
  text_color: string;
  swing_speed: float
}

type weapons_file = (string * weapon) list

type tileset_source = { firstgid: int; source: string }

type connected_object = { id: int }

type coll_rect = {
  gid: int;
  id: int;
  name: string;
  x: float;
  y: float;
  h: float;
  w: float;
  targets: connected_object list
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
  x_offset: float;
  y_offset: float
}

type texture_configs = (string * texture_config) list

type steel_sole_progress = { mutable dunks: int; mutable c_dashes: int }

type room_progress = {
  mutable removed_tile_idxs: int list;
  mutable removed_platform_ids: string list;
  mutable finished_interactions: string list;
  mutable revealed_shadow_layers: string list
}

type ghost_abilities = {
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

type game_progress = {
  mutable frame_idx: int;
  steel_sole: steel_sole_progress;
  mutable by_room: (string * room_progress) list;
  mutable purple_pens_found: (int * string) list;
  mutable keys_found: string list;
  mutable dreamer_items_found: int;
  mutable last_upgrade_claimed: int
}

type save_file = {
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

type ghost_action = {
  duration: float;
  cooldown: float;
  input_buffer: float;
  collision_shape: (float * float) list
}

type ghosts_file = {
  body_textures: texture_configs;
  shared_textures: texture_configs;
  actions: (string * ghost_action) list
}

type enemy_dream_nail_config = { recoil_vx: float; vulnerable: bool }

type enemy_config = {
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

type enemies_file = {
  enemies: (string * enemy_config) list;
  shared_textures: (string * texture_config) list
}
