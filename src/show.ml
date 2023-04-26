open Types

let vector (v : vector) : string = fmt "(%0.1f, %0.1f)" v.x v.y
let rect (r : rect) : string = fmt "[w: %0.1f, h: %0.1f, at (%0.1f, %0.1f)]" r.w r.h r.pos.x r.pos.y
let int_list xs = List.map string_of_int xs
let sprite_name (s : sprite) = s.ident
let entity_name (e : entity) = sprite_name e.sprite

let animation_src (anim_src : animation_src) =
  match anim_src with
  | STILL r -> fmt "STILL (at %s)" (rect r)
  | PARTICLE a -> fmt "PARTICLE (frame %d of %d)" a.current_idx (List.length a.frames)
  | ANIMATED a -> fmt "ANIMATED (frame %d of %d)" a.current_idx (List.length a.frames)

let direction (d : direction) : string =
  match d with
  | UP -> "up"
  | DOWN -> "down"
  | LEFT -> "left"
  | RIGHT -> "right"

let ghost_id id =
  match id with
  | ABED -> "ABED"
  | ANNIE -> "ANNIE"
  | BRITTA -> "BRITTA"
  | JEFF -> "JEFF"
  | TROY -> "TROY"

let ghost_pose pose =
  match pose with
  | AIRBORNE f -> fmt (if f >= 0. then "DESCENDING(%f)" else "ASCENDING(%f)") f
  | ATTACKING d -> fmt "ATTACKING (%s)" (direction d)
  | CASTING -> "CASTING"
  | CRAWLING -> "CRAWLING"
  | DASHING -> "DASHING"
  | DIVING -> "DIVING"
  | FLAPPING -> "FLAPPING"
  | FOCUSING -> "FOCUSING"
  | IDLE -> "IDLE"
  | JUMPING -> "JUMPING"
  | LANDING _r -> "LANDING"
  | READING -> "READING"
  | TAKING_DAMAGE d -> fmt "TAKING_DAMAGE (%s)" (direction d)
  | WALKING d -> fmt "WALKING (%s)" (direction d)
  | WALL_JUMPING -> "WALL_JUMPING"
  | WALL_SLIDING _r -> "WALL_SLIDING"

let ghost_location (g : ghost) = print "ghost at %0.1f, %0.1f" g.entity.sprite.dest.pos.x g.entity.sprite.dest.pos.y
let ghost_name (g : ghost) = fmt "ghost:%s" (entity_name g.entity)

let area_id id =
  match id with
  | AC_REPAIR_ANNEX -> "ac-repair"
  | BASEMENT -> "basement"
  | CITY_OF_CHAIRS -> "city"
  | COMPUTER_WING -> "computer"
  | FINAL -> "final"
  | FORGOTTEN_CLASSROOMS -> "forgotten"
  | INFECTED_CLASSROOMS -> "infected"
  | MEOW_MEOW_BEENZ -> "beenz"
  | TRAMPOLINEPATH -> "trampoline"

let room_id id =
  match id with
  | AC_DREAMER -> "dreamer"
  | AC_A -> "a"
  | AC_B -> "b"
  | AC_C -> "c"
  | AC_D -> "d"
  | AC_E -> "e"
  | AC_F -> "f"
  | AC_G -> "g"
  | BASE_ABYSS_CLIMB -> "abyss-climb"
  | BASE_A -> "a"
  | BASE_B -> "b"
  | BASE_C -> "c"
  | BASE_D -> "d"
  | BASE_E -> "e"
  | BASE_F -> "f"
  | BOSS -> "boss"
  | CC_A -> "a"
  | CC_B -> "b"
  | CC_C -> "c"
  | CC_D -> "d"
  | CC_E -> "e"
  | CW_DREAMER -> "dreamer"
  | CW_A -> "a"
  | CW_B -> "b"
  | CW_C -> "c"
  | CW_D -> "d"
  | CW_E -> "e"
  | CW_F -> "f"
  | FC_CAFETERIA -> "cafeteria"
  | FC_DEANS_PASS -> "deans-pass"
  | FC_STAIRWELL -> "stairwell"
  | FC_A -> "a"
  | FC_B -> "b"
  | FC_C -> "c"
  | FC_D -> "d"
  | FC_E -> "e"
  | FC_F -> "f"
  | FC_G -> "g"
  | FC_H -> "h"
  | FC_I -> "i"
  | IC_TEACHERS_LOUNGE -> "teachers-lounge"
  | IC_A -> "a"
  | IC_B -> "b"
  | IC_C -> "c"
  | IC_D -> "d"
  | IC_E -> "e"
  | IC_F -> "f"
  | IC_G -> "g"
  | IC_H -> "h"
  | IC_I -> "i"
  | MMB_1_A -> "one-entrance"
  | MMB_2_A -> "two-a"
  | MMB_2_B -> "two-b"
  | MMB_2_C -> "two-c"
  | MMB_3_A -> "three-hallway"
  | MMB_3_COLO -> "three-colo"
  | MMB_3_ELEVATOR -> "three-elevator"
  | MMB_4_A -> "four-stairwell"
  | MMB_4_COLO -> "four-colo"
  | MMB_5_LOUNGE -> "lounge"
  | MMB_5_PALACE_GROUNDS -> "grounds"
  | MMB_OUTLANDS_A -> "outlands-a"
  | MMB_OUTLANDS_B -> "outlands-b"
  | MMB_OUTLANDS_C -> "outlands-c"
  | TP_DREAMER -> "dreamer"
  | TP_A -> "a"
  | TP_B -> "b"
  | TP_C -> "c"
  | TP_D -> "d"
  | TP_E -> "e"
  | TP_F -> "f"
  | TP_G -> "g"

let damage_kind (d : damage_kind) : string =
  match d with
  | NAIL -> "NAIL"
  | VENGEFUL_SPIRIT -> "VENGEFUL_SPIRIT"

let time (t : time) = fmt "%f" t.at

let jug_config (config : jug_config) : string =
  fmt "%s === w %d, h %d, tile x %d" config.jug_name config.w config.h config.tile_x

let json_collision (collision : Json_t.collision) = fmt "id %d" collision.id
let debug_change_ability name b = print "%s: %b -> %b" name b (not b)
let npc_name (npc : npc) = fmt "npc(%s)" (entity_name npc.entity)

let enemy_id (e : enemy_id) : string =
  match e with
  | DUNCAN -> "DUNCAN"
  | LOCKER_BOY -> "LOCKER_BOY"
  | PENGUIN -> "PENGUIN"

let npc_id (n : npc_id) : string =
  match n with
  | ANNIES_BOOBS -> "ANNIES_BOOBS"
  | CHANG -> "CHANG"
  | GARRETT -> "GARRETT"
  | HICKEY -> "HICKEY"
  | LEONARD -> "LEONARD"
  | NEIL -> "NEIL"
  | SHIRLEY -> "SHIRLEY"
  | VICKI -> "VICKI"

let enemy_name (enemy : enemy) = fmt "enemy(%s)" (entity_name enemy.entity)
let enemy (enemy : enemy) = fmt "%s %s" (enemy_id enemy.id) (enemy_name enemy)
