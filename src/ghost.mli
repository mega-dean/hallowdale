open Types

val parse_name : string -> ghost_id
val read_config : unit -> ghosts_file
val available_ghost_ids : (ghost_id * party_ghost) list -> ghost_id list
val maybe_begin_interaction : state -> game -> trigger -> unit
val find_trigger_collision' : ghost -> trigger list -> trigger option
val get_spell_sprite : ghost -> (sprite * time) option
val get_damage : ghost -> damage_kind -> int
val past_cooldown : ?debug:bool -> ghost_action -> float -> bool
val start_action : ?debug:bool -> state -> game -> ghost_action_kind -> unit
val swap_current_ghost : state -> game -> ?swap_pos:bool -> ghost_id -> unit
val equip_weapon : ghost -> string -> unit
val make_party : ?in_party:bool -> ghost -> party_ghost
val get_invincibility_kind : state -> game -> invincibility_kind option
val is_vulnerable : state -> game -> bool
val handle_debug_keys : game -> state -> state
val update : game -> state -> state
val load_shared_textures : (string * texture_config) list -> ghost_shared_textures

val init :
  ghost_id ->
  texture ->
  ghost_head_textures ->
  (string * ghost_action_config) list ->
  vector ->
  Json_t.save_file ->
  (string * Json_t.weapon) list ->
  ghost_shared_textures ->
  ghost

val init_party : ghost_id -> ghost_head_textures -> texture -> vector -> bool -> party_ghost
