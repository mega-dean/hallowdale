open Types

let line (line : line) = fmt "[%0.1fx + %0.1fy + %0.1f = 0.]" line.a line.b line.c

let line_mx_b (line : line) : string =
  let slope = -1. *. (line.a /. line.b) in
  let y_intercept = -1. *. (line.c /. line.b) in
  fmt "y = %0.1fx + %0.1f" slope y_intercept

let vector (v : vector) : string = fmt "(%0.1f, %0.1f)" v.x v.y
let rect (r : rect) : string = fmt "[w: %0.1f, h: %0.1f, at (%0.1f, %0.1f)]" r.w r.h r.pos.x r.pos.y
let int_list xs = List.map string_of_int xs
let sprite_name (s : sprite) = s.ident
let entity_name (e : entity) = sprite_name e.sprite
let shape (shape : shape) = fmt "shape with %d edges" (List.length shape.edges)
let shape_points (shape : shape) = fmt "shape with points: %s" (List.map vector (get_points shape) |> join)
let shape_lines (shape : shape) = fmt "shape with lines:\n%s" (List.map line_mx_b (get_lines shape) |> join ~sep:"\n")

let animation_src (anim_src : animation_src) =
  match anim_src with
  | STILL r -> fmt "STILL (at %s)" (rect r)
  | PARTICLE a -> fmt "PARTICLE (frame %d of %d)" (a.frame_idx + 1) (List.length a.frames)
  | LOOPED a -> fmt "LOOPED (frame %d of %d)" (a.frame_idx + 1) (List.length a.frames)

let asset_dir (asset_dir : asset_dir) =
  match asset_dir with
  | GHOSTS -> "ghosts"
  | NPCS -> "npcs"
  | ENEMIES -> "enemies"

let direction (d : direction) : string =
  match d with
  | UP -> "up"
  | DOWN -> "down"
  | LEFT -> "left"
  | RIGHT -> "right"

let attack_direction (d : direction option) : string =
  match d with
  | None -> "not attacking"
  | Some UP -> "up"
  | Some DOWN -> "down"
  | Some LEFT -> "left"
  | Some RIGHT -> "right"

let ghost_id id =
  match id with
  | ABED -> "ABED"
  | ANNIE -> "ANNIE"
  | BRITTA -> "BRITTA"
  | JEFF -> "JEFF"
  | TROY -> "TROY"

let spell_kind (kind : spell_kind) =
  match kind with
  | VENGEFUL_SPIRIT -> "VENGEFUL_SPIRIT"
  | DESOLATE_DIVE -> "DESOLATE_DIVE"
  | HOWLING_WRAITHS -> "HOWLING_WRAITHS"

let ghost_action_kind id =
  match id with
  | TAKE_DAMAGE d -> fmt "TAKE_DAMAGE (%s)" (direction d)
  | ATTACK d -> fmt "ATTACK (%s)" (direction d)
  | CAST spell -> fmt "CAST (%s)" (spell_kind spell)
  | DIVE_COOLDOWN -> "DIVE_COOLDOWN"
  | DASH -> "DASH"
  | FOCUS -> "FOCUS"
  | JUMP -> "JUMP"
  | FLAP -> "FLAP"
  | WALL_KICK -> "WALL_KICK"

let ghost_pose pose =
  match pose with
  | PERFORMING action_kind -> fmt "PERFORMING %s" (ghost_action_kind action_kind)
  | AIRBORNE f -> fmt (if f >= 0. then "DESCENDING(%f)" else "ASCENDING(%f)") f
  | CRAWLING -> "CRAWLING"
  | IDLE -> "IDLE"
  | LANDING _r -> "LANDING"
  | READING -> "READING"
  | WALKING d -> fmt "WALKING (%s)" (direction d)
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
  | DESOLATE_DIVE -> "DESOLATE_DIVE"
  | DESOLATE_DIVE_SHOCKWAVE -> "DESOLATE_DIVE_SHOCKWAVE"
  | HOWLING_WRAITHS -> "HOWLING_WRAITHS"

let ghost_child_kind (d : ghost_child_kind) : string =
  match d with
  | DIVE -> "DIVE"
  | DIVE_COOLDOWN -> "DIVE_COOLDOWN"
  | FOCUS -> "FOCUS"
  | NAIL _ -> "NAIL"
  | WRAITHS -> "WRAITHS"

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

let main_menu_choice (choice : main_menu_choice) =
  match choice with
  | START_GAME -> "START_GAME"
  | QUIT -> "QUIT"

let save_files_choice (choice : save_files_choice) =
  match choice with
  | SLOT_1 -> "SLOT_1"
  | SLOT_2 -> "SLOT_2"
  | SLOT_3 -> "SLOT_3"
  | SLOT_4 -> "SLOT_4"
  | BACK -> "BACK"

let pause_menu_choice (choice : pause_menu_choice) =
  match choice with
  | CONTINUE -> "CONTINUE"
  | CHANGE_WEAPON -> "CHANGE_WEAPON"
  | QUIT_TO_MAIN_MENU -> "QUIT_TO_MAIN_MENU"

let change_weapon_menu_choice (choice : change_weapon_menu_choice) =
  match choice with
  | EQUIP_WEAPON weapon_name -> fmt "EQUIP WEAPON %s" weapon_name
  | BACK -> "BACK"

let menu_choice (choice : menu_choice) =
  match choice with
  | MAIN_MENU choice -> main_menu_choice choice
  | SAVE_FILES choice -> save_files_choice choice
  | PAUSE_MENU' choice -> pause_menu_choice choice
  | CHANGE_WEAPON_MENU choice -> change_weapon_menu_choice choice
