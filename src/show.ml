open Utils
open Types

let option (to_str : 'a -> string) (v : 'a option) =
  match v with
  | None -> "None"
  | Some a -> a |> to_str

let line (line : line) = fmt "[%0.1fx + %0.1fy + %0.1f = 0.]" line.a line.b line.c

let line_mx_b (line : line) : string =
  let slope = -1. *. (line.a /. line.b) in
  let y_intercept = -1. *. (line.c /. line.b) in
  fmt "y = %0.1fx + %0.1f" slope y_intercept

let vector (v : vector) : string = fmt "(%0.1f, %0.1f)" v.x v.y
let rect (r : rect) : string = fmt "[w: %0.1f, h: %0.1f, at (%0.1f, %0.1f)]" r.w r.h r.pos.x r.pos.y

let coll_rect (r : Json_t.coll_rect) : string =
  fmt "[w: %0.1f, h: %0.1f, at (%0.1f, %0.1f)]" r.w r.h r.x r.y

let int_list xs = List.map string_of_int xs
let sprite (s : sprite) = s.ident
let entity (e : entity) = sprite e.sprite
let shape (shape : shape) = fmt "shape with %d edges" (List.length shape.edges)

let shape_lines (shape : shape) =
  fmt "shape with lines:\n%s" (List.map line_mx_b (get_lines shape) |> String.join_lines)

let shape_points shape : string = List.map fst shape.edges |> List.map vector |> String.join_lines

let x_alignment (x : x_alignment) : string =
  match x with
  | IN_FRONT facing_right -> fmt "IN_FRONT (%b)" facing_right
  | LEFT_INSIDE -> "LEFT_INSIDE"
  | RIGHT_INSIDE -> "RIGHT_INSIDE"
  | LEFT_OUTSIDE -> "LEFT_OUTSIDE"
  | RIGHT_OUTSIDE -> "RIGHT_OUTSIDE"
  | CENTER -> "CENTER"
  | RANDOM -> "RANDOM"

let y_alignment (y : y_alignment) : string =
  match y with
  | TOP_INSIDE -> "TOP_INSIDE"
  | BOTTOM_INSIDE -> "BOTTOM_INSIDE"
  | TOP_OUTSIDE -> "TOP_OUTSIDE"
  | BOTTOM_OUTSIDE -> "BOTTOM_OUTSIDE"
  | CENTER -> "CENTER"
  | RANDOM -> "RANDOM"

let relative_pos ((x, y) : relative_position) : string =
  fmt "(%s, %s)" (x_alignment x) (y_alignment y)

let recoil (recoil : recoil) : string =
  fmt "speed: %f, time_left: %f, reset_v: %b" recoil.speed recoil.time_left.seconds recoil.reset_v

let disappearable_state (state : disappearable_state) =
  match state with
  | VISIBLE -> "VISIBLE"
  | TOUCHED f -> fmt "TOUCHED %f" f
  | INVISIBLE f -> fmt "INVISIBLE %f" f

let rotatable_state (state : rotatable_state) =
  match state with
  | UPRIGHT -> "UPRIGHT"
  | TOUCHED f -> fmt "TOUCHED %f" f
  | ROTATING_NOW -> "ROTATING_NOW"
  | UPSIDE_DOWN f -> fmt "UPSIDE_DOWN %f" f

let platform_kind (kind : platform_kind) =
  match kind with
  | TEMPORARY state -> fmt "TEMPORARY (%s)" (disappearable_state state)
  | DISAPPEARABLE state -> fmt "DISAPPEARABLE (%s)" (disappearable_state state)
  | ROTATABLE state -> fmt "ROTATABLE (%s)" (rotatable_state state)
  | LOCKED_DOOR (key, state) -> fmt "LOCKED_DOOR (%s, %s)" key (disappearable_state state)

let platform_kind_opt (kind : platform_kind option) =
  match kind with
  | None -> "None"
  | Some kind -> platform_kind kind

let trigger_kind (kind : trigger_kind) =
  match kind with
  | BOSS_FIGHT autosave_pos -> fmt "BOSS_FIGHT (%s)" (vector autosave_pos)
  | BOSS_KILLED -> "BOSS_KILLED"
  | CAMERA (x, y) -> fmt "CAMERA_%s-%s" x y
  | CUTSCENE -> "CUTSCENE"
  | D_NAIL -> "D_NAIL"
  | FOLLOWUP -> "FOLLOWUP"
  | INFO -> "INFO"
  | LEVER -> "LEVER"
  | PURPLE_PEN -> "PURPLE_PEN"
  | REFLECT -> "REFLECT"
  | RESPAWN -> "RESPAWN"
  | SHADOW -> "SHADOW"
  | WARP warp -> fmt "WARP to %s at (%0.2f, %0.2f)" warp.room_name warp.target.x warp.target.y

let animation_src (anim_src : animation_src) =
  match anim_src with
  | STILL r -> fmt "STILL (%s)" (rect r)
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
  | Some d -> fmt "attacking %s" (Controls.show_direction d)

let ghost_id id =
  match id with
  | ABED -> "ABED"
  | ANNIE -> "ANNIE"
  | BRITTA -> "BRITTA"
  | JEFF -> "JEFF"
  | LAVA_BRITTA -> "LAVA_BRITTA"
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
  | ATTACK d -> fmt "ATTACK (%s)" (Controls.show_direction d)
  | CAST spell -> fmt "CAST (%s)" (spell_kind spell)
  | C_DASH -> "C_DASH"
  | C_DASH_CHARGE -> "C_DASH_CHARGE"
  | C_DASH_COOLDOWN -> "C_DASH_COOLDOWN"
  | C_DASH_WALL_COOLDOWN -> "C_DASH_WALL_COOLDOWN"
  | DASH -> "DASH"
  | DIE -> "DIE"
  | DIVE_HOP -> "DIVE_HOP"
  | DIVE_COOLDOWN -> "DIVE_COOLDOWN"
  | DREAM_NAIL -> "DREAM_NAIL"
  | FLAP -> "FLAP"
  | FOCUS -> "FOCUS"
  | HARDFALL -> "HARDFALL"
  | JUMP -> "JUMP"
  | SHADE_DASH -> "SHADE_DASH"
  | TAKE_DAMAGE (damage, direction') ->
    fmt "TAKE_DAMAGE (%d, %s)" damage (Controls.show_direction direction')
  | TAKE_DAMAGE_AND_RESPAWN -> "TAKE_DAMAGE_AND_RESPAWN"
  | WALL_KICK -> "WALL_KICK"

let ghost_pose pose =
  match pose with
  | AIRBORNE f -> fmt (if f >= 0. then "DESCENDING(%f)" else "ASCENDING(%f)") f
  | CRAWLING -> "CRAWLING"
  | IDLE -> "IDLE"
  | PERFORMING action_kind -> fmt "PERFORMING %s" (ghost_action_kind action_kind)
  | READING -> "READING"
  | SWIMMING _ -> "SWIMMING"
  | WALKING d -> fmt "WALKING (%s)" (Controls.show_direction d)
  | WALL_SLIDING _r -> "WALL_SLIDING"

let ghost_location (g : player) =
  print "ghost at %0.1f, %0.1f" g.ghost.entity.sprite.dest.pos.x g.ghost.entity.sprite.dest.pos.y

let ghost_name (g : player) = fmt "ghost:%s" (entity g.ghost.entity)

let area_id id =
  match id with
  | AC_REPAIR_ANNEX -> "ac-repair"
  | BASEMENT -> "basement"
  | CITY_OF_CHAIRS -> "city"
  | COMPUTER_WING -> "computer"
  | FORGOTTEN_CLASSROOMS -> "forgotten"
  | INFECTED_CLASSROOMS -> "infected"
  | LIBRARY -> "library"
  | MEOW_MEOW_BEENZ -> "beenz"
  | OUTLANDS -> "outlands"
  | TRAMPOLINEPATH -> "trampoline"
  | VENTWAYS -> "ventways"

let area_id_corner_text id =
  match id with
  | AC_REPAIR_ANNEX -> "Air Conditioning Repair Annex"
  | BASEMENT -> "Basement"
  | CITY_OF_CHAIRS -> "City of Chairs"
  | COMPUTER_WING -> "Computer Wing"
  | FORGOTTEN_CLASSROOMS -> "Forgotten Classrooms"
  | INFECTED_CLASSROOMS -> "Infected Classrooms"
  | LIBRARY -> "Library"
  | MEOW_MEOW_BEENZ -> "MeowMeowBeenz Beta Test"
  | OUTLANDS -> "The Outlands"
  | TRAMPOLINEPATH -> "Trampolinepath"
  | VENTWAYS -> "Ventways"

let room_id id =
  match id with
  | AC_B -> "AC_b"
  | AC_C -> "AC_c"
  | AC_D -> "AC_d"
  | AC_E -> "AC_e"
  | AC_F -> "AC_f"
  | BASE_A -> "BASE_a"
  | BASE_B -> "BASE_b"
  | BASE_C -> "BASE_c"
  | BASE_D -> "BASE_d"
  | BASE_E -> "BASE_e"
  | BASE_F -> "BASE_f"
  | CITY_A -> "CITY_a"
  | CITY_B -> "CITY_b"
  | CITY_D -> "CITY_d"
  | CITY_E -> "CITY_e"
  | CPU_A -> "CPU_a"
  | CPU_B -> "CPU_b"
  | CPU_C -> "CPU_c"
  | CPU_F -> "CPU_f"
  | CPU_G -> "CPU_g"
  | CPU_H -> "CPU_h"
  | CPU_I -> "CPU_i"
  | CPU_K -> "CPU_k"
  | CPU_M -> "CPU_m"
  | FORG_TEST -> "FORG_test"
  | FORG_A -> "FORG_a"
  | FORG_B -> "FORG_b"
  | FORG_C -> "FORG_c"
  | FORG_D -> "FORG_d"
  | FORG_DEANS_PASS -> "FORG_deans-pass"
  | FORG_E -> "FORG_e"
  | FORG_F -> "FORG_f"
  | FORG_G -> "FORG_g"
  | FORG_H -> "FORG_h"
  | INF_A -> "INF_a"
  | INF_B -> "INF_b"
  | INF_C -> "INF_c"
  | INF_D -> "INF_d"
  | LIB_A -> "LIB_a"
  | LIB_B -> "LIB_b"
  | LIB_C -> "LIB_c"
  | LIB_D -> "LIB_d"
  | LIB_E -> "LIB_e"
  | LIB_F -> "LIB_f"
  | LIB_G -> "LIB_g"
  | LIB_H -> "LIB_h"
  | LIB_I -> "LIB_i"
  | MMB_A -> "MMB_a"
  | OUT_A -> "OUT_a"
  | OUT_B -> "OUT_b"
  | OUT_C -> "OUT_c"
  | OUT_D -> "OUT_d"
  | OUT_E -> "OUT_e"
  | TRAMP_A -> "TRAMP_a"
  | TRAMP_B -> "TRAMP_b"
  | TRAMP_C -> "TRAMP_c"
  | TRAMP_D -> "TRAMP_d"
  | TRAMP_E -> "TRAMP_e"
  | TRAMP_F -> "TRAMP_f"
  | TRAMP_G -> "TRAMP_g"
  | TRAMP_H -> "TRAMP_h"
  | VENT_HUB -> "VENT_hub"

let room_id_filename id =
  match id with
  | AC_B -> "b"
  | AC_C -> "c"
  | AC_D -> "d"
  | AC_E -> "e"
  | AC_F -> "f"
  | BASE_A -> "a"
  | BASE_B -> "b"
  | BASE_C -> "c"
  | BASE_D -> "d"
  | BASE_E -> "e"
  | BASE_F -> "f"
  | CITY_A -> "a"
  | CITY_B -> "b"
  | CITY_D -> "d"
  | CITY_E -> "e"
  | CPU_A -> "a"
  | CPU_B -> "b"
  | CPU_C -> "c"
  | CPU_F -> "f"
  | CPU_G -> "g"
  | CPU_H -> "h"
  | CPU_I -> "i"
  | CPU_K -> "k"
  | CPU_M -> "m"
  | FORG_TEST -> "test"
  | FORG_A -> "a"
  | FORG_B -> "b"
  | FORG_C -> "c"
  | FORG_D -> "d"
  | FORG_DEANS_PASS -> "deans-pass"
  | FORG_E -> "e"
  | FORG_F -> "f"
  | FORG_G -> "g"
  | FORG_H -> "h"
  | INF_A -> "a"
  | INF_B -> "b"
  | INF_C -> "c"
  | INF_D -> "d"
  | LIB_A -> "a"
  | LIB_B -> "b"
  | LIB_C -> "c"
  | LIB_D -> "d"
  | LIB_E -> "e"
  | LIB_F -> "f"
  | LIB_G -> "g"
  | LIB_H -> "h"
  | LIB_I -> "i"
  | MMB_A -> "a"
  | OUT_A -> "a"
  | OUT_B -> "b"
  | OUT_C -> "c"
  | OUT_D -> "d"
  | OUT_E -> "e"
  | TRAMP_A -> "a"
  | TRAMP_B -> "b"
  | TRAMP_C -> "c"
  | TRAMP_D -> "d"
  | TRAMP_E -> "e"
  | TRAMP_F -> "f"
  | TRAMP_G -> "g"
  | TRAMP_H -> "h"
  | VENT_HUB -> "hub"

let ghost_child_kind (d : ghost_child_kind) : string =
  match d with
  | C_DASH_CHARGE_CRYSTALS -> "C_DASH_CHARGE_CRYSTALS"
  | C_DASH_WALL_CHARGE_CRYSTALS -> "C_DASH_WALL_CHARGE_CRYSTALS"
  | C_DASH_WHOOSH -> "C_DASH_WHOOSH"
  | DIVE -> "DIVE"
  | DIVE_COOLDOWN -> "DIVE_COOLDOWN"
  | DREAM_NAIL -> "DREAM_NAIL"
  | FOCUS -> "FOCUS"
  | NAIL _ -> "NAIL"
  | SHADE_DASH_SPARKLES -> "SHADE_DASH_SPARKLES"
  | WRAITHS -> "WRAITHS"

let time (t : time) = fmt "%f" t.at

let tile_group (tile_group : tile_group) : string =
  fmt "[%s]" (int_list tile_group.tile_idxs |> String.join)

let layer_tile_groups (layer : layer) : string =
  fmt "tile_groups:\n%s" (List.map tile_group layer.tile_groups |> String.join_lines)

let jug_config (config : jug_config) : string =
  fmt "%s === w %d, h %d, tile x %d" config.jug_name config.w config.h config.tile_x

let json_collision (collision : Json_t.collision) = fmt "id %d" collision.id
let debug_change_ability name b = print "%s: %b -> %b" name b (not b)

let damage_kind (d : damage_kind) : string =
  match d with
  | DESOLATE_DIVE -> "DESOLATE_DIVE"
  | DESOLATE_DIVE_SHOCKWAVE -> "DESOLATE_DIVE_SHOCKWAVE"
  | DREAM_NAIL -> "DREAM_NAIL"
  | HOWLING_WRAITHS -> "HOWLING_WRAITHS"
  | NAIL -> "NAIL"
  | VENGEFUL_SPIRIT -> "VENGEFUL_SPIRIT"

let enemy_id (e : enemy_id) : string =
  match e with
  | BAT -> "BAT"
  | BIRD -> "BIRD"
  | BORCHERT -> "BORCHERT"
  | BUDDY -> "BUDDY"
  | DEAN -> "DEAN"
  | DUNCAN -> "DUNCAN"
  | ELECTRICITY -> "ELECTRICITY"
  | FISH -> "FISH"
  | FLYING_HIPPIE -> "FLYING_HIPPIE"
  | FLYING_HIPPIE_2 -> "FLYING_HIPPIE_2"
  | FROG -> "FROG"
  | HICKEY -> "HICKEY"
  | HIPPIE -> "HIPPIE"
  | JOSHUA -> "JOSHUA"
  | LAVA_BRITTA -> "LAVA_BRITTA"
  | LAVA_BRITTA_2 -> "LAVA_BRITTA_2"
  | LOCKER_BOY -> "LOCKER_BOY"
  | LUIS_GUZMAN -> "LUIS_GUZMAN"
  | MANICORN -> "MANICORN"
  | MANICORN_2 -> "MANICORN_2"
  | MANICORN_3 -> "MANICORN_3"
  | PENGUIN -> "PENGUIN"
  | VICE_DEAN_LAYBOURNE -> "VICE_DEAN_LAYBOURNE"

let enemy_name (enemy : enemy) = fmt "enemy(%s)" (entity enemy.entity)
let enemy (enemy : enemy) = fmt "%s %s" (enemy_id enemy.id) (enemy_name enemy)

let enemy_action (action : enemy_action) : string =
  match action with
  | PERFORMED action_name -> fmt "PERFORMED %s" action_name
  | TOOK_DAMAGE kind -> fmt "TOOK_DAMAGE %s" (damage_kind kind)

let npc_name (npc : npc) = fmt "npc(%s)" (entity npc.entity)

let npc_id (n : npc_id) : string =
  match n with
  | ANNIES_BOOBS -> "ANNIES_BOOBS"
  | BLACKSMITH_WIFE -> "BLACKSMITH_WIFE"
  | CHANG -> "CHANG"
  | FRANKIE -> "FRANKIE"
  | GARRETT -> "GARRETT"
  | HILDA -> "HILDA"
  | HUMAN_BEING -> "HUMAN_BEING"
  | JERRY -> "JERRY"
  | LEONARD -> "LEONARD"
  | NEIL -> "NEIL"
  | POTTERY_TEACHER -> "POTTERY_TEACHER"
  | SHIRLEY -> "SHIRLEY"
  | TROY_AND_ABED_IN_A_BUBBLE -> "TROY_AND_ABED_IN_A_BUBBLE"
  | VICKI -> "VICKI"

let game_mode (game_mode : game_mode) : string =
  match game_mode with
  | CLASSIC -> "Classic"
  | STEEL_SOLE -> "Steel Sole"
  | DEMO -> "Demo"

let save_slot idx save_slot =
  let continue =
    match save_slot.file.game_mode with
    | "Classic" -> "Continue"
    | "Steel Sole" -> "Continue (Steel Sole)"
    | "Demo" -> "Continue (Demo)"
    | _ -> failwith "bad game mode"
  in
  fmt "file %d: %s" (idx + 1) (if save_slot.new_game then "New Game" else continue)

let menu_choice ?(save_slots = []) state (game_opt : game option) (choice : menu_choice) =
  let save_files_choice (choice : save_files_choice) =
    match choice with
    | START_SLOT n -> save_slot (n - 1) (List.nth save_slots (n - 1))
    | DELETE_SAVE_FILE n -> fmt "Delete save file %d" n
    | BACK -> "Back"
  in
  let confirm_delete_choice (choice : confirm_delete_choice) =
    match choice with
    | CONFIRM_DELETE n -> fmt "Delete file %d" n
    | CANCEL -> "Cancel"
  in
  let game_mode_choice (game_mode_choice : select_game_mode_choice) : string =
    match game_mode_choice with
    | USE_MODE (mode, _, _) -> fmt "Start %s" (game_mode mode)
    | BACK -> "Back"
  in
  let change_ghost_menu_choice (choice : change_ghost_menu_choice) =
    let menu_ghost_id id =
      match game_opt with
      | None -> failwith "need to have a game to show weapons menu"
      | Some game' -> (
        let show name =
          if id = game'.player.ghost.id then
            fmt "{{blue}} %s" name
          else
            name
        in
        match id with
        | ABED -> show "Abed"
        | ANNIE -> show "Annie"
        | BRITTA
        | LAVA_BRITTA ->
          show "Britta"
        | JEFF -> show "Jeff"
        | TROY -> show "Troy")
    in
    match choice with
    | USE_GHOST id -> menu_ghost_id id
    | BACK -> "Back"
  in
  let change_weapon_menu_choice (choice : change_weapon_menu_choice) =
    match game_opt with
    | None -> failwith "need to have a game to show weapons menu"
    | Some game' -> (
      match choice with
      | EQUIP_WEAPON weapon_name ->
        if game'.player.current_weapon.name = weapon_name then
          fmt "{{blue}} %s" weapon_name
        else
          weapon_name
      | BACK -> "Back")
  in
  let change_setting _setting choice =
    match choice with
    (* this looks pretty terrible, but it works for now *)
    | INCREASE -> "louder"
    | DECREASE -> "quieter"
    | BACK -> "Back"
  in
  let main_menu_choice (choice : main_menu_choice) =
    match choice with
    | START_GAME -> "Start Game"
    | QUIT -> "Quit"
  in
  let pause_menu_choice (choice : pause_menu_choice) =
    match choice with
    | CONTINUE -> "Continue"
    | CHANGE_GHOST -> "Change Ghost"
    | CHANGE_WEAPON -> "Change Weapon"
    | PROGRESS -> "Progress"
    | SETTINGS -> "Settings"
    | SAVE -> "Save"
    | QUIT_TO_MAIN_MENU -> "Save and Quit"
  in
  let settings_menu_choice (choice : settings_menu_choice) =
    match choice with
    | MUSIC -> "Music"
    | SOUND_EFFECTS -> "Sound Effects"
    | REBIND_CONTROLS -> "Rebind Controls"
    | BACK -> "Back"
  in
  let rebind_menu_choice (choice : rebind_menu_choice) =
    match choice with
    | KEYBOARD -> "Keyboard"
    | GAMEPAD -> "Gamepad"
    | BACK -> "Back"
  in
  let rebind_game_action_choice ~keyboard (choice : rebind_action_menu_choice) =
    match choice with
    | REBIND_ACTION action -> (
      let binding =
        if keyboard then
          Controls.get_key state.controls action |> Controls.show_key
        else
          Controls.get_button state.controls action |> Controls.show_button
      in
      let show_control s =
        let color =
          match state.rebinding_action with
          | None -> "{{white}}"
          | Some (_, action') -> if action = action' then "{{blue}}" else "{{white}}"
        in
        fmt "%s: %s %s" s color binding
      in
      match action with
      (* actions *)
      | CAST -> show_control "Cast"
      | C_DASH -> show_control "C-dash"
      | DASH -> show_control "Dash"
      | D_NAIL -> show_control "Dream Nail"
      | FOCUS -> show_control "Focus"
      | INTERACT -> show_control "Interact"
      | JUMP -> show_control "Jump"
      | NAIL -> show_control "Attack"
      | PAUSE -> show_control "Pause"
      | OPEN_MAP -> show_control "Open Map"
      (* directions *)
      | ARROW direction' -> show_control (Controls.show_direction direction')
      (* debug *)
      | DEBUG_1 -> "DEBUG_1"
      | DEBUG_2 -> "DEBUG_2"
      | DEBUG_3 -> "DEBUG_3"
      | DEBUG_4 -> "DEBUG_4"
      | DEBUG_5 -> "DEBUG_5"
      | DEBUG_UP -> "DEBUG_UP"
      | DEBUG_DOWN -> "DEBUG_DOWN"
      | DEBUG_LEFT -> "DEBUG_LEFT"
      | DEBUG_RIGHT -> "DEBUG_RIGHT")
    | RESET_BINDINGS -> "Reset to default bindings"
    | BACK -> "Back"
  in
  match choice with
  | MAIN_MENU choice -> main_menu_choice choice
  | CONFIRM_DELETE_MENU choice -> confirm_delete_choice choice
  | SAVE_FILES_MENU choice -> save_files_choice choice
  | PAUSE_MENU choice -> pause_menu_choice choice
  | SELECT_GAME_MODE choice -> game_mode_choice choice
  | CHANGE_WEAPON_MENU choice -> change_weapon_menu_choice choice
  | CHANGE_GHOST_MENU choice -> change_ghost_menu_choice choice
  | SETTINGS_MENU choice -> settings_menu_choice choice
  | REBIND_CONTROLS_MENU choice -> rebind_menu_choice choice
  | REBIND_KEYBOARD_MENU choice -> rebind_game_action_choice ~keyboard:true choice
  | REBIND_GAMEPAD_MENU choice -> rebind_game_action_choice ~keyboard:false choice
  | CHANGE_AUDIO_SETTING (setting, choice) -> change_setting setting choice

let text_config (config : Interaction.text_config) : string =
  fmt "padding : %s;\nmargin_x : %f;\nmargin_y_top : %f;\nmargin_y_bottom : %f;\ncentered : %b;\n"
    (vector config.padding) config.margin_x config.margin_y_top config.margin_y_bottom
    config.centered

let interaction_config (config : Interaction.config) : string =
  fmt "remove_nail: %b, autosave_pos: %s" config.remove_nail (option vector config.autosave_pos)

let reward (reward : Interaction.reward) : string =
  match reward with
  | Interaction.INCREASE_MAX_SOUL -> "increased maximum {{blue}} LIFE VAPOR."
  | Interaction.ABILITY (name, desc) -> fmt "{{blue}} %s: {{white}} %s" name desc

let projectile_despawn (despawn : projectile_despawn) : string =
  match despawn with
  | TIME_LEFT _ -> "TIME_LEFT"
  | DETONATE (_, _) -> "DETONATE"
  | BOSS_AREA_X (_, _) -> "BOSS_AREA_X"
  | BOSS_AREA_Y (_, _) -> "BOSS_AREA_Y"
  | UNTIL_FLOOR_COLLISION -> "UNTIL_FLOOR_COLLISION"
  | UNTIL_ENEMY_DEATH -> "UNTIL_ENEMY_DEATH"
