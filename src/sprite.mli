open Types

val make_dest : float -> float -> texture -> rect
val reset_texture : texture -> unit
val advance_animation : float -> texture -> sprite -> unit
val advance_or_despawn : float -> texture -> sprite -> sprite option

val build_texture_from_config :
  ?scale:float -> ?particle:bool -> ?once:bool -> texture_config -> texture

val build_texture_from_path :
  ?scale:float -> ?particle:bool -> ?once:bool -> texture_path -> texture

val build_texture_from_image : ?scale:float -> ?particle:bool -> image -> rect option -> texture
val clone : sprite -> sprite

val create :
  string -> texture -> ?facing_right:bool -> ?collision:collision_shape option -> rect -> sprite

val spawn_particle :
  ?facing_right:bool ->
  ?collision:collision_shape option ->
  string ->
  texture ->
  rect ->
  float ->
  sprite
