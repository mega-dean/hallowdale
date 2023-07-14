open Types

val is_dead : enemy -> bool
val is_alive : enemy -> bool
val parse_name : string -> string -> enemy_id
val last_damage : enemy -> time
val set_pose : enemy -> string -> unit
val start_action : enemy -> string -> float -> (string * float) list -> unit
val log_action : enemy -> string -> float -> unit
val took_damage_at : enemy -> damage_kind -> time
val maybe_take_damage : state -> enemy -> time -> damage_kind -> int -> collision -> bool
val choose_behavior : enemy -> state -> game -> unit

val create_from_rects :
  (enemy_id * rect) list -> string list -> (enemy_id * Json_t.enemy_config) list -> enemy list
