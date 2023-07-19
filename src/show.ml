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

let shape_lines (shape : shape) =
  fmt "shape with lines:\n%s" (List.map line_mx_b (get_lines shape) |> join ~sep:"\n")

let shape_points shape : string =
  List.map fst shape.edges
  |> List.map (fun v -> fmt "{ x = %f; y = %f };" v.x v.y)
  |> join ~sep:"\n"

let direction (d : direction) : string =
  match d with
  | UP -> "up"
  | DOWN -> "down"
  | LEFT -> "left"
  | RIGHT -> "right"

let trigger_kind (kind : trigger_kind) =
  match kind with
  | CAMERA (x, y) -> fmt "CAMERA_%s-%s" x y
  | LEVER lever -> fmt "LEVER (%s, %d)" (direction lever.direction) lever.door_tile_idx
  | INFO -> "INFO"
  | HEALTH -> "HEALTH"
  | ITEM -> "ITEM"
  | SHADOW -> "SHADOW"
  | WARP target -> fmt "WARP to %s at (%0.2f, %0.2f)" target.room_name target.pos.x target.pos.y
  | CUTSCENE -> "CUTSCENE"
  | RESPAWN -> "RESPAWN"
  | PURPLE_PEN -> "PURPLE_PEN"
  | BOSS_KILLED -> "BOSS_KILLED"
  | D_NAIL -> "D_NAIL"

let animation_src (anim_src : animation_src) =
  match anim_src with
  | STILL r -> fmt "STILL (at %s)" (rect r)
  | PARTICLE a -> fmt "PARTICLE (frame %d of %d)" (a.frame_idx + 1) (List.length a.frames)
  | LOOPED a -> fmt "LOOPED (frame %d of %d)" (a.frame_idx + 1) (List.length a.frames)
  | ONCE a -> fmt "ONCE (frame %d of %d)" (a.frame_idx + 1) (List.length a.frames)

let asset_dir (asset_dir : asset_dir) =
  match asset_dir with
  | GHOSTS -> "ghosts"
  | NPCS -> "npcs"
  | ENEMIES -> "enemies"
  | TILED -> "tiled"

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

let ghost_action (action : ghost_action) =
  fmt "started: %f, blocked_until: %f, doing_until: %f" action.started.at action.blocked_until.at
    action.doing_until.at

let ghost_action_config (config : ghost_action_config) =
  fmt "duration: %f, cooldown: %f, input_buffer: %f" config.duration.seconds config.cooldown.seconds
    config.input_buffer.seconds

let ghost_action_kind id =
  match id with
  | TAKE_DAMAGE (damage, direction') -> fmt "TAKE_DAMAGE (%d, %s)" damage (direction direction')
  | TAKE_DAMAGE_AND_RESPAWN -> "TAKE_DAMAGE_AND_RESPAWN"
  | ATTACK d -> fmt "ATTACK (%s)" (direction d)
  | CAST spell -> fmt "CAST (%s)" (spell_kind spell)
  | DREAM_NAIL -> "DREAM_NAIL"
  | DIVE_COOLDOWN -> "DIVE_COOLDOWN"
  | C_DASH_COOLDOWN -> "C_DASH_COOLDOWN"
  | C_DASH_WALL_COOLDOWN -> "C_DASH_WALL_COOLDOWN"
  | C_DASH_CHARGE -> "C_DASH_CHARGE"
  | C_DASH -> "C_DASH"
  | SHADE_DASH -> "SHADE_DASH"
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
  | SWIMMING _ -> "SWIMMING"

let ghost_location (g : ghost) =
  print "ghost at %0.1f, %0.1f" g.entity.sprite.dest.pos.x g.entity.sprite.dest.pos.y

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
  | LIBRARY -> "library"
  | VENTWAYS -> "ventways"

let room_id id =
  match id with
  | AC_DREAMER -> "AC_dreamer"
  | AC_A -> "AC_a"
  | AC_B -> "AC_b"
  | AC_C -> "AC_c"
  | AC_D -> "AC_d"
  | AC_E -> "AC_e"
  | AC_F -> "AC_f"
  | AC_G -> "AC_g"
  | BASE_ABYSS_CLIMB -> "BASE_abyss-climb"
  | BASE_A -> "BASE_a"
  | BASE_B -> "BASE_b"
  | BASE_C -> "BASE_c"
  | BASE_D -> "BASE_d"
  | BASE_E -> "BASE_e"
  | BASE_F -> "BASE_f"
  | BOSS -> "boss"
  | CC_A -> "CC_a"
  | CC_B -> "CC_b"
  | CC_C -> "CC_c"
  | CC_D -> "CC_d"
  | CC_E -> "CC_e"
  | CW_DREAMER -> "CW_dreamer"
  | CW_A -> "CW_a"
  | CW_B -> "CW_b"
  | CW_C -> "CW_c"
  | CW_D -> "CW_d"
  | CW_E -> "CW_e"
  | CW_F -> "CW_f"
  | FC_CAFETERIA -> "FC_cafeteria"
  | FC_DEANS_PASS -> "FC_deans-pass"
  | FC_STAIRWELL -> "FC_stairwell"
  | FC_A -> "FC_a"
  | FC_B -> "FC_b"
  | FC_C -> "FC_c"
  | FC_D -> "FC_d"
  | FC_E -> "FC_e"
  | FC_F -> "FC_f"
  | FC_G -> "FC_g"
  | FC_H -> "FC_h"
  | FC_I -> "FC_i"
  | IC_TEACHERS_LOUNGE -> "IC_teachers-lounge"
  | IC_A -> "IC_a"
  | IC_B -> "IC_b"
  | IC_C -> "IC_c"
  | IC_D -> "IC_d"
  | IC_E -> "IC_e"
  | IC_F -> "IC_f"
  | IC_G -> "IC_g"
  | IC_H -> "IC_h"
  | IC_I -> "IC_i"
  | LIB_A -> "LIB_a"
  | LIB_B -> "LIB_b"
  | LIB_C -> "LIB_c"
  | LIB_D -> "LIB_d"
  | LIB_E -> "LIB_e"
  | MMB_1_A -> "MMB_one-entrance"
  | MMB_2_A -> "MMB_2-a"
  | MMB_2_B -> "MMB_2-b"
  | MMB_2_C -> "MMB_2-c"
  | MMB_3_A -> "MMB_3-hallway"
  | MMB_3_COLO -> "MMB_3-colo"
  | MMB_3_ELEVATOR -> "MMB_3-elevator"
  | MMB_4_A -> "MMB_4-stairwell"
  | MMB_4_COLO -> "MMB_4-colo"
  | MMB_5_LOUNGE -> "MMB_lounge"
  | MMB_5_PALACE_GROUNDS -> "MMB_grounds"
  | MMB_OUTLANDS_A -> "MMB_outlands-a"
  | MMB_OUTLANDS_B -> "MMB_outlands-b"
  | MMB_OUTLANDS_C -> "MMB_outlands-c"
  | TP_DREAMER -> "TP_dreamer"
  | TP_A -> "TP_a"
  | TP_B -> "TP_b"
  | TP_C -> "TP_c"
  | TP_D -> "TP_d"
  | TP_E -> "TP_e"
  | TP_F -> "TP_f"
  | TP_G -> "TP_g"
  | VENT_HUB -> "VENT_hub"

let room_id_filename id =
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
  | LIB_A -> "a"
  | LIB_B -> "b"
  | LIB_C -> "c"
  | LIB_D -> "d"
  | LIB_E -> "e"
  | MMB_1_A -> "one-entrance"
  | MMB_2_A -> "2-a"
  | MMB_2_B -> "2-b"
  | MMB_2_C -> "2-c"
  | MMB_3_A -> "3-hallway"
  | MMB_3_COLO -> "3-colo"
  | MMB_3_ELEVATOR -> "3-elevator"
  | MMB_4_A -> "4-stairwell"
  | MMB_4_COLO -> "4-colo"
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
  | VENT_HUB -> "hub"

let damage_kind (d : damage_kind) : string =
  match d with
  | NAIL -> "NAIL"
  | DREAM_NAIL -> "DREAM_NAIL"
  | VENGEFUL_SPIRIT -> "VENGEFUL_SPIRIT"
  | DESOLATE_DIVE -> "DESOLATE_DIVE"
  | DESOLATE_DIVE_SHOCKWAVE -> "DESOLATE_DIVE_SHOCKWAVE"
  | HOWLING_WRAITHS -> "HOWLING_WRAITHS"

let ghost_child_kind (d : ghost_child_kind) : string =
  match d with
  | DIVE -> "DIVE"
  | DREAM_NAIL -> "DREAM_NAIL"
  | C_DASH_CHARGE_CRYSTALS -> "C_DASH_CHARGE_CRYSTALS"
  | C_DASH_WALL_CHARGE_CRYSTALS -> "C_DASH_WALL_CHARGE_CRYSTALS"
  | C_DASH_WHOOSH -> "C_DASH_WHOOSH"
  | SHADE_DASH_SPARKLES -> "SHADE_DASH_SPARKLES"
  | DIVE_COOLDOWN -> "DIVE_COOLDOWN"
  | FOCUS -> "FOCUS"
  | NAIL _ -> "NAIL"
  | WRAITHS -> "WRAITHS"

let time (t : time) = fmt "%f" t.at

let tile_group (tile_group : tile_group) : string =
  fmt "[%s]" (int_list tile_group.tile_idxs |> join)

let layer_tile_groups (layer : layer) : string =
  fmt "tile_groups:\n%s" (List.map tile_group layer.tile_groups |> join ~sep:"\n")

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
  | FISH -> "FISH"
  | FROG -> "FROG"
  | ELECTRICITY -> "ELECTRICITY"

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
  | START_GAME -> "Start Game"
  | QUIT -> "Quit"

let save_files_choice (choice : save_files_choice) =
  match choice with
  | SLOT_1 -> "1:"
  | SLOT_2 -> "2:"
  | SLOT_3 -> "3:"
  | SLOT_4 -> "4:"
  | BACK -> "Back"

let pause_menu_choice (choice : pause_menu_choice) =
  match choice with
  | CONTINUE -> "Continue"
  | CHANGE_GHOST -> "Change Ghost"
  | CHANGE_WEAPON -> "Change Weapon"
  | QUIT_TO_MAIN_MENU -> "Save and Quit"

let change_weapon_menu_choice (game : game option) (choice : change_weapon_menu_choice) =
  match game with
  | None -> failwith "need to have a game to show weapons menu"
  | Some game' -> (
    match choice with
    | EQUIP_WEAPON weapon_name ->
      if game'.ghost.current_weapon.name = weapon_name then
        fmt "{{blue}} %s" weapon_name
      else
        weapon_name
    | BACK -> "Back")

let menu_ghost_id (game : game option) id =
  match game with
  | None -> failwith "need to have a game to show weapons menu"
  | Some game' -> (
    let show name =
      if id = game'.ghost.id then
        fmt "{{blue}} %s" name
      else
        name
    in
    match id with
    | ABED -> show "Abed"
    | ANNIE -> show "Annie"
    | BRITTA -> show "Britta"
    | JEFF -> show "Jeff"
    | TROY -> show "Troy")

let change_ghost_menu_choice (game : game option) (choice : change_ghost_menu_choice) =
  match choice with
  | USE_GHOST id -> menu_ghost_id game id
  | BACK -> "Back"

let menu_choice (game : game option) (choice : menu_choice) =
  match choice with
  | MAIN_MENU choice -> main_menu_choice choice
  | SAVE_FILES choice -> save_files_choice choice
  | PAUSE_MENU choice -> pause_menu_choice choice
  | CHANGE_WEAPON_MENU choice -> change_weapon_menu_choice game choice
  | CHANGE_GHOST_MENU choice -> change_ghost_menu_choice game choice
