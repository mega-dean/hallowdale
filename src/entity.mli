open Types

val above_floor : entity -> rect -> bool
val recoil_backwards : entity -> recoil -> unit
val freeze : entity -> unit
val unfreeze : entity -> unit
val hide : entity -> unit
val unhide : entity -> unit
val unhide_at : entity -> vector -> unit
val hidden : entity -> bool
val set_facing : direction -> entity -> unit
val apply_v : ?debug:string option -> float -> entity -> unit
val get_bench_collisions : room -> entity -> (collision * rect) list
val get_conveyor_belt_collision : room -> entity -> (collision * rect * vector) option
val get_floor_collisions : room -> entity -> (collision * rect) list
val get_water_collisions : room -> entity -> (collision * rect) list
val get_acid_collisions : room -> entity -> (collision * rect) list
val get_damage_collisions : room -> entity -> (collision * rect) list * (collision * rect) list
val get_loose_projectile_collisions : room -> entity -> (collision * projectile) list
val update_pos : ?debug:string option -> room -> entity -> float -> unit
val maybe_unset_current_floor : entity -> room -> unit

val update_enemy_pos :
  ?debug:string option -> ?gravity_multiplier':float option -> room -> entity -> float -> bool

val get_child_pos' : rect -> bool -> relative_position -> float -> float -> vector
val get_child_pos : entity -> relative_position -> float -> float -> vector
val on_ground : entity -> bool
val descending : entity -> bool
val is_on_screen : entity -> bool
val get_center : entity -> vector
val set_facing_right : ?allow_vertical:bool -> entity -> direction -> unit
val update_vx : entity -> float -> unit
val walk : entity -> direction -> unit
val adjust_sprite_dest : entity -> unit
val update_sprite_texture : entity -> texture -> unit
val clone : entity -> entity

val create :
  string ->
  ?scale:float ->
  ?inanimate:bool ->
  ?gravity_multiplier:float ->
  ?v:vector ->
  ?facing_right:bool ->
  ?collision:collision_shape option ->
  texture ->
  rect ->
  entity

val create_for_sprite :
  sprite -> ?inanimate:bool -> ?gravity_multiplier:float -> ?v:vector -> rect -> entity

val to_texture_config : asset_dir -> string -> string * Json_t.texture_config -> texture_config

val create_from_textures :
  ?collision:collision_shape option ->
  ?gravity_multiplier:float ->
  texture_config list ->
  (string * texture) list ->
  rect ->
  entity * (string * texture) list

val create_from_texture_configs :
  ?collision:collision_shape option ->
  ?gravity_multiplier:float ->
  texture_config list ->
  rect ->
  entity * (string * texture) list
