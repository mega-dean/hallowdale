open Utils

let raylib_Rect_to_rect (r : Raylib.Rectangle.t) : rect =
  Raylib.Rectangle.{ w = width r; h = height r; pos = { x = x r; y = y r } }

let rect_to_Rect (r : rect) : Raylib.Rectangle.t = Raylib.Rectangle.create r.pos.x r.pos.y r.w r.h

type direction =
  | UP
  | DOWN
  | LEFT
  | RIGHT

let opposite_of direction =
  match direction with
  | UP -> DOWN
  | DOWN -> UP
  | LEFT -> RIGHT
  | RIGHT -> LEFT

type bounds = {
  min : vector;
  max : vector;
}

type time = { mutable at : float }
type duration = { seconds : float }
type color = Raylib.Color.t

module Zero = struct
  (* create a new vector/rect/time each invocation in case they are updated later *)
  let vector () : vector = { x = 0.; y = 0. }
  let rect () : rect = { pos = vector (); w = 0.; h = 0. }
  let time () : time = { at = 0. }

  (* these won't ever be updated later, so don't need to create a new one each time *)
  let raylib_vector : Raylib.Vector2.t = Raylib.Vector2.create 0. 0.
end

(* the raw image file that a texture can source from *)
type image = Raylib.Texture.t

let load_tiled_asset path = Raylib.load_texture (File.make_assets_path [ "tiled"; path ])

let load_image ?(debug = false) path : image =
  let full_path = File.make_assets_path [ path ] in
  if Sys.file_exists full_path then
    Raylib.load_texture full_path
  else
    failwithf "file doesn't exist: load_image %s" full_path

type animation_frame = {
  src : rect;
  duration : duration;
}

(* a list of animation_frames, and info about the currently-rendered frame
   - every frame in the animation has to have the same offsets from the
     collision rect (texture.coll_offsets')
*)
type animation = {
  frames : animation_frame list;
  mutable frame_idx : int;
  mutable time_started : time;
}

type animation_src =
  | STILL of rect
  | LOOPED of animation
  | PARTICLE of animation
  | (* this is used for c-dash crystals, which animate once and then stay on-screen *)
    ONCE of animation

let get_frame (a : animation) : animation_frame =
  List.nth a.frames (a.frame_idx mod List.length a.frames)

let make_single_frame ~w ~h : rect = { w; h; pos = { x = 0.; y = 0. } }

(* TODO add OTHER for world-map, ability-outlines *)
type asset_dir =
  | GHOSTS
  | ENEMIES
  | NPCS
  | TILED

type texture_path = {
  asset_dir : asset_dir;
  (* this is either the npc name or the ghost name, capitalized like a variant name
     eg. BRITTA or LOCKER_BOY *)
  character_name : string;
  (* this is the name of the specific pose being loaded, corresponding to a .png file *)
  pose_name : string;
}

(* used to load textures to populate npc_texture_cache or ghost_textures *)
type texture_config = {
  path : texture_path;
  (* these values all come from the texture config in json *)
  x_offset : float;
  y_offset : float;
  count : int;
  duration : duration; (* TODO collision_shape configs should probably go here *)
}

(* an image that has a collision box and that might be animated *)
type texture = {
  (* TODO maybe just make this file_path *)
  ident : string;
  image : image;
  animation_src : animation_src;
  coll_offset : vector;
}

let animation_loop_duration (texture : texture) : float =
  match texture.animation_src with
  | STILL _ -> failwith "can't get animation_loop_duration for STILL"
  | ONCE animation
  | PARTICLE animation
  | LOOPED animation ->
    (get_frame animation).duration.seconds *. (List.length animation.frames |> Int.to_float)

let get_src (texture : texture) : rect =
  match texture.animation_src with
  | STILL frame_src -> frame_src
  | ONCE animation
  | PARTICLE animation
  | LOOPED animation ->
    (get_frame animation).src

(* scale arg needs to be something that is scaled by window size *)
let get_scaled_texture_size scale (texture : texture) =
  let src = get_src texture in
  (src.w *. scale, src.h *. scale)

(* knockback in a single direction *)
type recoil = {
  speed : float;
  mutable time_left : duration;
  reset_v : bool;
}

(* ax + by + c = 0 *)
type line = {
  a : float;
  b : float;
  c : float;
}

let horizontal (line : line) = line.a = 0.
let vertical (line : line) = line.b = 0.

type shape = { edges : (vector * line) list }

type collision_shape =
  | DEST
  | SHAPE of shape

let make_shape (points : vector list) : shape =
  if List.length points < 3 then
    failwith "can't make a shape with < 3 points";
  let make_line_from_points (p1 : vector) (p2 : vector) : line =
    if p1.x = p2.x then
      { a = 1.; b = 0.; c = -.p1.x }
    else (
      let slope = (p1.y -. p2.y) /. (p1.x -. p2.x) in
      let y_intercept = p1.y -. (slope *. p1.x) in
      { a = -.slope; b = 1.; c = -.y_intercept })
  in
  let get_point_and_line point_idx point =
    let next_point = List.nth points ((point_idx + 1) mod List.length points) in
    let line = make_line_from_points point next_point in
    (point, line)
  in
  { edges = List.mapi get_point_and_line points }

let get_points (shape : shape) = List.map fst shape.edges
let get_lines (shape : shape) = List.map snd shape.edges

let shape_of_rect (rect : rect) : shape =
  make_shape
    [
      { x = rect.pos.x; y = rect.pos.y };
      { x = rect.pos.x +. rect.w; y = rect.pos.y };
      { x = rect.pos.x +. rect.w; y = rect.pos.y +. rect.h };
      { x = rect.pos.x; y = rect.pos.y +. rect.h };
    ]

(* a texture that is rendered at a specific location:
   - for sprites that belong to entities, changes are applied to entity.dest.p and mirrored to sprite.dest
   - sprites that don't belong to entities don't change position each frame (eg tiles)
*)
type sprite = {
  ident : string;
  mutable texture : texture;
  mutable dest : rect;
  collision : collision_shape option;
  mutable facing_right : bool;
}

let align_shape_with_parent (dest : rect) (facing_right : bool) (shape : shape) : shape =
  let adjust_point point_idx (point : vector) : vector =
    let x =
      if facing_right then
        dest.pos.x +. point.x
      else
        dest.pos.x +. dest.w -. point.x
    in
    let y = dest.pos.y +. point.y in
    { x; y }
  in
  let adjusted_points : vector list = List.mapi adjust_point (get_points shape) in
  make_shape adjusted_points

let align_shape_with_parent_sprite (sprite : sprite) (shape : shape) : shape =
  align_shape_with_parent sprite.dest sprite.facing_right shape

let get_sprite_collision_shape (sprite : sprite) =
  match sprite.collision with
  | None -> failwith "get_collision_shape for a shape with no collision"
  | Some DEST -> shape_of_rect sprite.dest
  | Some (SHAPE shape) -> align_shape_with_parent_sprite sprite shape

type disappearable_state =
  | VISIBLE
  | TOUCHED of float
  | INVISIBLE of float

type rotatable_state =
  | UPRIGHT
  | TOUCHED of float
  | ROTATING_NOW
  | UPSIDE_DOWN of float

(* conveyor belts don't need to be platforms because they don't change state, they just
   have a constant effect on the ghost when colliding *)
type platform_kind =
  (* this is only used for the floor in kp that disappears permanently *)
  | TEMPORARY of disappearable_state
  | DISAPPEARABLE of disappearable_state
  | ROTATABLE of rotatable_state
  (* string is key name *)
  | LOCKED_DOOR of string * disappearable_state

type platform = {
  (* this is used to keep track of the associated spikes (which are tracked separately in room.platform_spikes) *)
  id : string;
  (* this is used for resetting position after shaking *)
  original_x : float;
  mutable kind : platform_kind option;
  sprite : sprite;
}

type entity_config = {
  bounce : float;
  (* only using the negative here because "animate" sounds more like a verb than an adjective *)
  inanimate : bool;
  gravity_multiplier : float;
}

(* a sprite with physical/movement properties *)
type entity = {
  sprite : sprite;
  config : entity_config;
  dest : rect;
  mutable v : vector;
  mutable frozen : bool;
  mutable x_recoil : recoil option;
  mutable y_recoil : recoil option;
  (* vector is usually 0, but is nonzero for conveyor belts *)
  mutable current_floor : (rect * vector) option;
  mutable current_platforms : platform list;
}

let get_entity_collision_shape (entity : entity) =
  match entity.sprite.collision with
  | None -> shape_of_rect entity.dest
  | Some _ -> get_sprite_collision_shape entity.sprite

type collision = {
  (* .center is roughly where the collision occurred, only used for drawing damage_sprite *)
  center : vector;
  other_rect : rect;
  collided_from : direction;
}

type spell_kind =
  | VENGEFUL_SPIRIT
  | DESOLATE_DIVE
  | HOWLING_WRAITHS

type ghost_action_kind =
  | FLAP
  | WALL_KICK
  | JUMP
  | DIE
  | TAKE_DAMAGE_AND_RESPAWN
  | TAKE_DAMAGE of int * direction
  | CAST of spell_kind
  | DIVE_COOLDOWN
  | HARDFALL
  | SHADE_DASH
  | DASH
  | (* TODO maybe do this like CAST, ie C_DASH of [CHARGE,DASH,COOLDOWN] *)
    C_DASH_CHARGE
  | C_DASH
  | C_DASH_COOLDOWN
  | C_DASH_WALL_COOLDOWN
  | DREAM_NAIL
    (* dream_nail doesn't need a direction because it can't upslash/downslash, and it can use sprite.facing_right *)
  | ATTACK of direction
  | FOCUS

(* the ghost's state that can be mapped to a texture and rendered *)
type ghost_pose =
  | PERFORMING of ghost_action_kind
  | AIRBORNE of float (* new_vy *)
  | CRAWLING
  | IDLE
  | READING
  | WALKING of direction
  | WALL_SLIDING of rect
  | SWIMMING of rect

type ghost_id =
  | ABED
  | ANNIE
  | BRITTA
  | JEFF
  | TROY

type health = {
  mutable current : int;
  mutable max : int;
}

type camera_subject =
  | GHOST
  | FIXED of vector

type npc_id =
  | CHANG
  | HICKEY
  | ANNIES_BOOBS
  | NEIL
  | SHIRLEY
  | LEONARD
  | VICKI
  | GARRETT
  | JERRY
  | BLACKSMITH_WIFE
  | HILDA
  | FRANKIE
  | HUMAN_BEING
  | POTTERY_TEACHER

type enemy_id =
  (* enemies *)
  | FISH
  | FROG
  | ELECTRICITY
  | PENGUIN
  | HIPPIE
  | FLYING_HIPPIE
  | FLYING_HIPPIE_2
  | BIRD
  | BAT
  | MANICORN
  | MANICORN_2
  | MANICORN_3
  (* bosses *)
  | DUNCAN
  | LOCKER_BOY
  | JOSHUA
  | VICE_DEAN_LAYBOURNE
  | LUIS_GUZMAN
  | BORCHERT
  | DEAN
  | BUDDY
  | LAVA_BRITTA
  | LAVA_BRITTA_2

type weapon = {
  name : string;
  tint : color;
  scale_x : float;
  scale_y : float;
  cooldown_scale : float;
}

type main_menu_choice =
  | START_GAME
  | QUIT

type game_mode =
  | CLASSIC
  | STEEL_SOLE
  | DEMO

type select_game_mode_choice =
  | USE_MODE of game_mode * Json_t.save_file * int
  | BACK

type save_files_choice =
  | START_SLOT of int
  | DELETE_SAVE_FILE of int
  | BACK

type confirm_delete_choice =
  | CONFIRM_DELETE of int
  | CANCEL

type pause_menu_choice =
  | CONTINUE
  | CHANGE_GHOST
  | CHANGE_WEAPON
  | PROGRESS
  | SETTINGS
  | SAVE
  | QUIT_TO_MAIN_MENU

type change_weapon_menu_choice =
  | EQUIP_WEAPON of string
  | BACK

type change_ghost_menu_choice =
  | USE_GHOST of ghost_id
  | BACK

type settings_menu_choice =
  | MUSIC
  | SOUND_EFFECTS
  | BACK

type change_setting_choice =
  | INCREASE
  | DECREASE
  | BACK

type menu_choice =
  | PAUSE_MENU of pause_menu_choice
  | CHANGE_WEAPON_MENU of change_weapon_menu_choice
  | CHANGE_GHOST_MENU of change_ghost_menu_choice
  | SETTINGS_MENU of settings_menu_choice
  | CHANGE_AUDIO_SETTING of (settings_menu_choice * change_setting_choice)
  | MAIN_MENU of main_menu_choice
  | SELECT_GAME_MODE of select_game_mode_choice
  | SAVE_FILES_MENU of save_files_choice
  | CONFIRM_DELETE_MENU of confirm_delete_choice

type menu = {
  choices : menu_choice list;
  mutable current_choice_idx : int;
}

type save_slot = {
  file : Json_t.save_file;
  new_game : bool;
}

type warp_target = {
  room_name : string;
  target : vector;
}

type trigger_kind =
  | CAMERA of string * string
  | LEVER
  | INFO
  | FOLLOWUP
  | SHADOW
  | WARP of warp_target
  | BOSS_FIGHT of vector
  | CUTSCENE
  | RESPAWN
  | PURPLE_PEN
  | BOSS_KILLED
  | D_NAIL
  | REFLECT

type trigger = {
  kind : trigger_kind;
  full_name : string;
  name_prefix : string;
  name_suffix : string;
  dest : rect;
  label : string option;
  blocking_interaction : string option;
}

(* this is for things that aren't created from Tiled objects in the triggers layer *)
let make_stub_trigger kind name_prefix name_suffix : trigger =
  {
    full_name = fmt "%s:%s" name_prefix name_suffix;
    name_prefix;
    name_suffix;
    kind;
    dest = Zero.rect ();
    label = None;
    blocking_interaction = None;
  }

type camera_motion =
  | LINEAR of float
  | SMOOTH of float * float

type timer = {
  mutable left : duration;
  total : duration;
}

let make_timer seconds = { left = { seconds }; total = { seconds } }

type screen_fade = {
  target_alpha : int;
  mutable timer : timer option;
  show_ghost : bool;
}

module Interaction = struct
  type options = {
    remove_nail : bool;
    autosave_pos : vector option;
  }

  type general_step =
    | INITIALIZE_INTERACTIONS of options
    | CONCLUDE_INTERACTIONS of trigger
    | SET_SCREEN_FADE of screen_fade
    | CLEAR_SCREEN_FADE
    | SHAKE_SCREEN of float
    | DEBUG
    | WAIT of float
    | WARP of trigger_kind
    | DOOR_WARP of trigger_kind
    | SPAWN_VENGEFUL_SPIRIT of direction * int * int
    (* text *)
    | TEXT of string list
    | FLOATING_TEXT of string * float
    | FOCUS_ABILITY_TEXT of string list * rect * string list
    | ABILITY_TEXT of rect * string list
    (* TODO maybe add OFFSET_DIALOGUE that takes params for where to draw the text box *)
    | DIALOGUE of string * string
    | PURPLE_PEN_TEXT of string
    (* camera *)
    | SET_FIXED_CAMERA of int * int
    | SET_GHOST_CAMERA
    | SET_CAMERA_MOTION of camera_motion
    | SET_IGNORE_CAMERA_TRIGGERS of bool
    (* layers *)
    | HIDE_LAYER of string
    | UNHIDE_LAYER of string

  type entity_step =
    | UNSET_FLOOR
    | SET_FACING of direction
    | WAIT_UNTIL_LANDED of bool
    | HIDE
    | UNHIDE
    | (* TODO add another param facing_right : bool *)
      UNHIDE_AT of int * int * float * float
    | FREEZE
    | UNFREEZE
    | MOVE_TO of (int * int)
    | SET_VX of float
    | SET_VY of float

  type item_kind =
    | WEAPON of string
    | ABILITY of string
    | DREAMER of string * string
    | KEY of string

  type party_ghost_step =
    | SET_POSE of ghost_pose
    | WALK_TO of int
    | ADD_TO_PARTY
    | REMOVE_FROM_PARTY
    | JUMP of direction * float
    | ENTITY of entity_step
    | MAKE_CURRENT_GHOST

  type reward =
    | INCREASE_MAX_SOUL
    | ABILITY of string * string

  type ghost_step =
    | FILL_LIFE_VAPOR
    | CLAIM_REWARD of int * reward
    | INCREASE_HEALTH_TEXT of string
    | (* this isn't using (PARTY (SET_POSE ...)) because it uses the real Ghost.set_pose
         instead of the simpler one for party_ghosts during interactions
      *)
      SET_POSE of ghost_pose
    | ADD_ITEM of item_kind
    | ENTITY of entity_step
    | PARTY of party_ghost_step

  type enemy_step =
    | WALK_TO of int
    (* this is only used for DUNCAN *)
    | DEAD_WALK_TO of int
    | SET_POSE of string
    | START_ACTION of string
    | ENTITY of entity_step

  type npc_step =
    | SET_POSE of string
    | WALK_TO of int
    | ENTITY of entity_step

  type step =
    | STEP of general_step
    | CURRENT_GHOST of ghost_step
    | PARTY_GHOST of ghost_id * party_ghost_step
    | ENEMY of enemy_id * enemy_step
    | NTH_ENEMY of int * enemy_id * enemy_step
    | NPC of npc_id * npc_step

  type text_config = {
    padding : vector;
    margin_x : float;
    margin_y_top : float;
    margin_y_bottom : float;
    cursor_padding : float;
    mutable centered : bool;
  }

  type ability_text = {
    (* these top_paragraphs are only non-empty for focus-info for now *)
    top_paragraphs : string list;
    outline_src : rect;
    bottom_paragraphs : string list;
  }

  type text_kind =
    | PLAIN of string list
    | ABILITY of ability_text
    | (* this is a separate variant because the focus dialogue is the only one with top_paragraphs *)
      FOCUS_ABILITY of ability_text
    | DIALOGUE of string * string
    | MENU of menu * save_slot list option

  type non_blocking_text_visible =
    (* duration, end_time *)
    | UNTIL of float * time
    | PAUSE_MENU_OPEN

  let make_UNTIL duration time = UNTIL (duration, { at = time +. duration })

  (* this is text that only shows temporarily and doesn't start an interaction, like
     corner_text and floating_text
  *)
  type non_blocking_text = {
    content : string;
    visible : non_blocking_text_visible;
    (* scale should always be 1.0 for corner text *)
    scale : float;
  }

  type t = {
    mutable steps : step list;
    mutable text : text_kind option;
    mutable speaker_name : string option;
    mutable use_dashes_in_archives : bool option;
    (* this is used for "Game Saved" when sitting on benches *)
    mutable corner_text : non_blocking_text option;
    (* this is for text boxes that should show up on the screen without blocking gameplay,
       like dream-nail thoughts and some interactions *)
    mutable floating_text : non_blocking_text option;
  }
end

type enemy_on_killed = {
  interaction_name : string option;
  (* this means there are multiple enemies that all need to die before the interaction starts
     - this is pretty specific and will mostly be false, but it will at least be used for Sisters of Battle and Watcher Knights *)
  multiple_enemies : bool;
}

type damage_kind =
  | NAIL
  | DREAM_NAIL
  | VENGEFUL_SPIRIT
  | DESOLATE_DIVE
  | DESOLATE_DIVE_SHOCKWAVE
  | HOWLING_WRAITHS
[@@deriving ord]

type enemy_action =
  | PERFORMED of string
  | TOOK_DAMAGE of damage_kind
[@@deriving ord]

module Enemy_action' = struct
  type t = enemy_action

  let compare = compare_enemy_action
end

module Enemy_action = struct
  include Enemy_action'
  module Map = Map.Make (Enemy_action')
end

type projectile_update_v =
  | WAVY
  | HOMING of float

type projectile_despawn =
  | TIME_LEFT of duration
  | DETONATE of duration * projectile list
  | BOSS_AREA_X of float * float
  | BOSS_AREA_Y of float * float
  | UNTIL_FLOOR_COLLISION
  | UNTIL_ENEMY_DEATH

(* TODO add despawn_animation option so projectiles don't just disappear *)
and projectile = {
  entity : entity;
  (* TODO maybe make this a list of projectile_despawns *)
  mutable despawn : projectile_despawn;
  update_v : projectile_update_v option;
  spawned : time;
  pogoable : bool;
  collide_with_floors : bool;
  damage : int;
  draw_on_top : bool;
  (* float is offset in radians, bool is clockwise *)
  orbiting : (float * bool * enemy) option;
}

and enemy_status = {
  mutable check_damage_collisions : bool;
  mutable active : bool;
  (* status.props are values representing the current state of the enemy *)
  mutable props : float String.Map.t;
}

and enemy_kind =
  | ENEMY
  | BOSS
  | MULTI_BOSS

and enemy = {
  id : enemy_id;
  kind : enemy_kind;
  status : enemy_status;
  level : int;
  entity : entity;
  damage : int;
  initial_pos : vector;
  health : health;
  mutable history : time Enemy_action.Map.t;
  (* TODO maybe keep track of last_non_idle_performed, so enemies are less likely to
     repeat the same attack several times
     - probably can get this from history already though
  *)
  mutable last_performed : (string * time) option;
  (* keep track of this so they only have to be checked once per frame
     (during State.update_enemies, and reused in Enemy.choose_behavior) *)
  mutable floor_collisions_this_frame : collision list;
  mutable projectiles : projectile list;
  mutable damage_sprites : sprite list;
  textures : texture String.Map.t;
  (* TODO maybe move these into global_cache since they will be the same for every enemy of
     the same type *)
  (* attrs are immutable properties of the enemy, defined in enemies.json *)
  attrs : float String.Map.t;
  json : Json_t.enemy_config;
  on_killed : enemy_on_killed;
}

type npc = {
  id : npc_id;
  entity : entity;
  textures : texture String.Map.t;
}

let get_npc_texture (npc : npc) (texture_name : string) : texture =
  match String.Map.find_opt texture_name npc.textures with
  | None -> failwithf "could not find texture '%s' for npc %s" texture_name npc.entity.sprite.ident
  | Some v -> v

type slash = {
  sprite : sprite;
  direction : direction;
  collision : collision_shape;
}

let get_slash_shape (slash : slash) : shape =
  match slash.collision with
  | SHAPE slash_shape -> align_shape_with_parent_sprite slash.sprite slash_shape
  | _ -> failwith "slash should have SHAPE collision"

type soul = {
  mutable current : int;
  max : int;
  mutable at_focus_start : int;
  mutable health_at_focus_start : int;
  mutable last_decremented : time;
}

type ghost_action_config = {
  duration : duration;
  cooldown : duration;
  input_buffer : duration;
}

(* a pose that happens at a specific time *)
type ghost_action = {
  config : ghost_action_config;
  mutable started : time;
  mutable blocked_until : time;
  mutable doing_until : time;
}

(* this keeps track of the last time the ghost performed these actions *)
type ghost_action_history = {
  cast_vs : ghost_action;
  (* cast_dive ends when landing on a floor, not based on duration *)
  cast_dive : ghost_action;
  dive_cooldown : ghost_action;
  cast_wraiths : ghost_action;
  dash : ghost_action;
  shade_dash : ghost_action;
  charge_c_dash : ghost_action;
  (* c_dash ends when hitting a wall or pressing a button, not based on duration *)
  c_dash : ghost_action;
  c_dash_cooldown : ghost_action;
  c_dash_wall_cooldown : ghost_action;
  flap : ghost_action;
  jump : ghost_action;
  wall_kick : ghost_action;
  take_damage : ghost_action;
  take_damage_and_respawn : ghost_action;
  die : ghost_action;
  (* checking is_doing for nail/focus uses the ghost.child sprite, not
     the duration/doing_until/blocked_until like the other actions
  *)
  nail : ghost_action;
  dream_nail : ghost_action;
  focus : ghost_action;
  hardfall : ghost_action;
}

(* - instead of keeping track of the current ghost_pose in state, we just update the sprite texture whenever
   a new pose is set, so it can still render the latest pose
   - this data structure tracks the variant arguments that need to be checked/re-set in future frames
*)
type ghost_status = {
  (* TODO just move this to entity and get rid of this type, since some enemies will need this, and the others can leave it always None *)
  mutable wall : rect option;
  (* these fields are here for things that can't use Ghost.is_doing to check their status
     with config.duration (like normal actions can):
     - dive ends when hitting the floor
     - c-dash ends when hitting a wall, or when pressing a button to end it
     - c-dash charge ends when a button is released
  *)
  mutable water : rect option;
  mutable is_diving : bool;
  mutable is_c_dashing : bool;
  mutable is_charging_c_dash : bool;
  (* this is here because hazard damage needs to run start_action twice *)
  mutable is_taking_hazard_damage : bool;
  mutable can_dash : bool;
  mutable can_flap : bool;
}

type frame_input = {
  mutable pressed : bool;
  mutable down : bool;
  mutable released : bool;
  mutable down_since : time option;
}

(* this is updated every frame based on which keys are pressed *)
type frame_inputs = {
  (* directions *)
  up : frame_input;
  down : frame_input;
  left : frame_input;
  right : frame_input;
  (* actions *)
  cast : frame_input;
  c_dash : frame_input;
  dream_nail : frame_input;
  dash : frame_input;
  focus : frame_input;
  jump : frame_input;
  nail : frame_input;
  pause : frame_input;
  open_map : frame_input;
  interact : frame_input;
}

(* Examples:
   x = LEFT_INSIDE, y = CENTER ->
   pppppppppppppppp
   p              p
   ccccc          p
   c   c          p
   c   c          p
   ccccc          p
   p              p
   pppppppppppppppp

   x = CENTER, y = BOTTOM_OUTSIDE ->
   pppppppppppppppp
   p              p
   p              p
   p              p
   p              p
   p              p
   p              p
   pppppccccccppppp
        c    c
        cccccc

   x = IN_FRONT, y = CENTER ->
   (p is facing right)
   ppppppppppp
   p         p
   p    ccccccccccc
   p    c         c
   p    ccccccccccc
   p         p
   ppppppppppp
*)
type x_alignment =
  | IN_FRONT of bool
  | LEFT_INSIDE
  | RIGHT_INSIDE
  | LEFT_OUTSIDE
  | RIGHT_OUTSIDE
  | CENTER
  | RANDOM

type y_alignment =
  | TOP_INSIDE
  | BOTTOM_INSIDE
  | TOP_OUTSIDE
  | BOTTOM_OUTSIDE
  | CENTER
  | RANDOM

type relative_position = x_alignment * y_alignment

let align_x (x_alignment : x_alignment) parent_dest child_w : float =
  let left_inside = parent_dest.pos.x in
  let right_inside = parent_dest.pos.x +. parent_dest.w -. child_w in
  match x_alignment with
  | IN_FRONT facing_right ->
    if facing_right then
      rect_center_x parent_dest
    else
      rect_center_x parent_dest -. child_w
  | LEFT_INSIDE -> left_inside
  | RIGHT_INSIDE -> right_inside
  | LEFT_OUTSIDE -> parent_dest.pos.x -. child_w
  | RIGHT_OUTSIDE -> parent_dest.pos.x +. parent_dest.w
  | CENTER -> parent_dest.pos.x +. ((parent_dest.w -. child_w) /. 2.)
  | RANDOM -> left_inside +. Random.float right_inside

let align_y (y_alignment : y_alignment) parent_dest child_h : float =
  let top_inside = parent_dest.pos.y in
  let bottom_inside = parent_dest.pos.y +. parent_dest.h -. child_h in
  match y_alignment with
  | TOP_INSIDE -> top_inside
  | BOTTOM_INSIDE -> bottom_inside
  | TOP_OUTSIDE -> parent_dest.pos.y -. child_h
  | BOTTOM_OUTSIDE -> parent_dest.pos.y +. parent_dest.h
  | CENTER -> parent_dest.pos.y +. ((parent_dest.h -. child_h) /. 2.)
  | RANDOM -> top_inside +. Random.float bottom_inside

let align ((x_alignment, y_alignment) : relative_position) parent_dest child_w child_h : vector =
  { x = align_x x_alignment parent_dest child_w; y = align_y y_alignment parent_dest child_h }

(* this aligns enemy/npc to (CENTER, BOTTOM_INSIDE), and handles hidden entities *)
let align_to_bottom parent_dest child_w child_h =
  if parent_dest.pos.y > 0. then
    align (CENTER, BOTTOM_INSIDE) parent_dest child_w child_h
  else (
    let parent_dest' =
      { parent_dest with pos = { x = -.parent_dest.pos.x; y = -.parent_dest.pos.y } }
    in
    let pos' = align (CENTER, BOTTOM_INSIDE) parent_dest' child_w child_h in
    { x = -.pos'.x; y = -.pos'.y })

(* ghost will only have one NAIL child at a time, so slashes don't really need to be comparable *)
let compare_slash a b = 0

(* - used for things that are "attached" to the ghost, ie their position depends on ghost
   - so spawned_vengeful_spirits are not children, but dive and shreik are
   - some things are purely visual (FOCUS)
   - some will have collisions, like NAIL, DIVE maybe, C_DASH_WHOOSH
*)
type ghost_child_kind =
  | NAIL of slash
  | DREAM_NAIL
  | C_DASH_CHARGE_CRYSTALS
  | C_DASH_WALL_CHARGE_CRYSTALS
  | C_DASH_WHOOSH
  | SHADE_DASH_SPARKLES
  (* TODO | DASH_WHOOSH *)
  | WRAITHS
  | DIVE
  | DIVE_COOLDOWN
  | FOCUS
[@@deriving ord]

module Ghost_child_kind' = struct
  type t = ghost_child_kind

  let compare a b = compare_ghost_child_kind a b
end

module Ghost_child_kind = struct
  include Ghost_child_kind'
  module Map = Map.Make (Ghost_child_kind')
end

type ghost_child = {
  relative_pos : relative_position;
  sprite : sprite;
  in_front : bool;
}

type invincibility_kind =
  (* TODO add something like | NO_FLASHING *)
  | DIVE_IFRAMES
  | TAKING_HAZARD_DAMAGE
  | TOOK_DAMAGE
  | SHADE_CLOAK

type ghosts_file = {
  body_textures : texture_config String.Map.t;
  actions : ghost_action_config String.Map.t;
  shared_textures : texture_config String.Map.t;
}

type ghost_body_textures = {
  cast : texture;
  crawl : texture;
  dash : texture;
  dive : texture;
  fall : texture;
  flap : texture;
  focus : texture;
  idle : texture;
  jump : texture;
  nail : texture;
  read : texture;
  take_damage : texture;
  walk : texture;
  wall_slide : texture;
}

type ghost_head_textures = {
  look_down : texture;
  look_up : texture;
  idle : texture;
  read : texture;
  take_damage : texture;
  walk : texture;
  wall_slide : texture;
}

(* non-ghost-specific textures that will be needed for any ghost *)
type ghost_shared_textures = {
  (* TODO add separate dream_nail texture / collision shape *)
  slash : texture;
  upslash : texture;
  downslash : texture;
  shine : texture;
  health : texture;
  vengeful_spirit : texture;
  (* - shade-soul needs a separate texture because it is bigger than vengeful spirit
     - d-dark and shriek use the same size, but different tint
  *)
  shade_soul : texture;
  desolate_dive : texture;
  dive_shockwave : texture;
  howling_wraiths : texture;
  energon_pod_base : texture;
  energon_pod_1 : texture;
  energon_pod_2 : texture;
  energon_pod_3 : texture;
  energon_pod_4 : texture;
  focus_sparkles : texture;
  c_dash_crystals : texture;
  (* TODO this is temporary, eventually combine it with c_dash_crystals and rotate *)
  c_dash_wall_crystals : texture;
  (* TODO these might be temporary, could try to just reuse the last frame of c_dash_crystals *)
  c_dash_crystals_full : texture;
  c_dash_wall_crystals_full : texture;
  c_dash_whoosh : texture;
  shade_cloak_sparkles : texture;
}

type ghost = {
  mutable id : ghost_id;
  mutable head : texture;
  head_textures : ghost_head_textures;
  (* the entity.sprite.texture is the ghost body *)
  entity : entity;
  mutable hardfall_timer : time option;
}

(* this is for the ghosts that are not being controlled right now *)
type party_ghost = {
  ghost : ghost;
  mutable in_party : bool;
}

type player = {
  mutable current : ghost_status;
  shared_textures : ghost_shared_textures;
  history : ghost_action_history;
  mutable ghost : ghost;
  mutable current_weapon : weapon;
  mutable weapons : Json_t.weapon String.Map.t;
  mutable abilities : Json_t.ghost_abilities;
  mutable health : health;
  mutable soul : soul;
  mutable children : ghost_child Ghost_child_kind.Map.t;
  mutable spawned_vengeful_spirits : projectile list;
}

(* a cache for image/tiles that have been loaded for a tileset *)
type tileset = {
  json : Json_t.tileset;
  image : image;
  (* all these textures use tileset.image with a different src rect *)
  (* expected to be non-empty *)
  tiles : texture array;
}

type door_health = {
  mutable hits : int;
  mutable last_hit_at : float;
}

(* a rectangle of tiles that is grouped into a single collision, eg. floors, jugs, doors *)
type tile_group = {
  dest : rect;
  transformation_bits : int;
  stub_sprite : sprite option;
  (* these are fragments that are spawned on destroy
     - the fragments that have already been spawned are in layer.spawned_fragments
  *)
  fragments : entity list;
  tile_idxs : int list; (* used for tracking destroyed tiles *)
  door_health : door_health option;
}

(* seems redundant to have both of these, but it's used to distinguish between things that are in the same plane as the ghost (ie parallax 1)
   - the "same plane" is tracked in "collides_with_ghost", since they should never be overlapping
   - jugs are in the same plane as the ghost but don't collide (except pogos), so they will still have fg or bg set
   - there is validation to make sure that only one of "fg" or "bg" is configured
*)
type layer_render_config = {
  bg : bool;
  fg : bool;
}

type layer_config = {
  render : layer_render_config;
  animated : bool;
  collides_with_ghost : bool;
  destroyable : bool;
  hazard : bool;
  permanently_removable : bool;
  pogoable : bool;
  shaded : bool;
  silent : bool;
}

(* a tilelayer in Tiled *)
type layer = {
  name : string;
  json : Json_t.tile_layer;
  data : int array array;
  config : layer_config;
  mutable hidden : bool;
  mutable tile_groups : tile_group list;
  mutable destroyed_tiles : int list;
  mutable spawned_fragments : entity list;
  (* int is transformation bits *)
  mutable spawned_stub_sprites : (sprite * int) list;
}

type area_id =
  | AC_REPAIR_ANNEX
  | BASEMENT
  | CITY_OF_CHAIRS
  | COMPUTER_WING
  | FORGOTTEN_CLASSROOMS
  | INFECTED_CLASSROOMS
  | MEOW_MEOW_BEENZ
  | OUTLANDS
  | TRAMPOLINEPATH
  | VENTWAYS
  | LIBRARY

type music = {
  name : string;
  t : Raylib.Music.t;
  loop_start : time;
  loop_end : time;
}

type area_music = {
  music : music;
  areas : area_id list;
}

type area = {
  id : area_id;
  tint : Raylib.Color.t;
  bg_color : Raylib.Color.t;
}

(* it seems weird to have the area_id embedded in the name, but it's for room names that are shared *)
type room_id =
  (* AC_REPAIR_ANNEX *)
  | AC_B
  | AC_C
  | AC_D
  | AC_E
  | AC_F
  (* BASEMENT *)
  | BASE_A
  | BASE_B
  | BASE_C
  | BASE_D
  | BASE_E
  | BASE_F
  (* CITY_OF_CHAIRS *)
  | CITY_A
  | CITY_B
  | CITY_D
  | CITY_E
  (* COMPUTER_WING *)
  | CPU_A
  | CPU_B
  | CPU_C
  | CPU_F
  | CPU_G
  | CPU_H
  | CPU_I
  | CPU_K
  | CPU_M
  (* FORGOTTEN_CLASSROOMS *)
  | (* just for testing *) FORG_TEST
  | FORG_DEANS_PASS
  | FORG_A
  | FORG_B
  | FORG_C
  | FORG_D
  | FORG_E
  | FORG_F
  | FORG_G
  | FORG_H
  (* INFECTED_CLASSROOMS *)
  | INF_A
  | INF_B
  | INF_C
  | INF_D
  (* MEOW_MEOW_BEENZ *)
  | MMB_A
  (* OUTLANDS *)
  | OUT_A
  | OUT_B
  | OUT_C
  | OUT_D
  | OUT_E
  (* TRAMPOLINEPATH *)
  | TRAMP_A
  | TRAMP_B
  | TRAMP_C
  | TRAMP_D
  | TRAMP_E
  | TRAMP_F
  | TRAMP_G
  | TRAMP_H
  (* VENTWAYS *)
  | VENT_HUB
  (* LIBRARY *)
  | LIB_A
  | LIB_B
  | LIB_C
  | LIB_D
  | LIB_E
  | LIB_F
  | LIB_G
  | LIB_H
  | LIB_I

type jug_fragments = {
  stub : texture option;
  fragments : entity list;
}

type jug_config = {
  jug_name : string;
  tile_x : int;
  w : int;
  h : int;
}

type room_cache = {
  jug_fragments_by_gid : jug_fragments Int.Map.t;
  tilesets_by_path : tileset String.Map.t;
}

type lever = {
  sprite : sprite;
  trigger : trigger;
  transformation : int;
  door_tile_idxs : int ne_list;
}

(* respawn.pos only gets updated to the nearest respawn target whenever the ghost enters
   a respawn trigger, not every frame
*)
type respawn = {
  mutable in_trigger_now : bool;
  mutable target : vector;
}

type respawn_trigger = {
  trigger : trigger;
  targets : vector ne_list;
}

type triggers = {
  camera : trigger list;
  boss_fight : trigger list;
  cutscene : trigger list;
  d_nail : trigger list;
  levers : lever list;
  (* this is used for any infinitely-repeatable interactions, like reading lore or warping *)
  lore : trigger list;
  respawns : respawn_trigger list;
  shadows : trigger list;
  reflect : trigger list;
}

type camera_state = {
  current_pos : vector;
  subject : vector;
  room_bounds : bounds;
}

type idx_config =
  | PURPLE_PEN of string * trigger option
  | DOOR_HITS of int

type room_params = {
  file_name : string;
  progress_by_room : Json_t.room_progress String.Map.t;
  exits : rect list;
  enemy_configs : (enemy_id * Json_t.enemy_config) list;
  npc_configs : (npc_id * Json_t.npc_config) list;
  pickup_indicator_texture : texture;
  lever_texture : texture;
  raindrop_texture : texture;
  respawn_pos : vector;
  platforms : texture String.Map.t;
}

type room = {
  id : room_id;
  area : area;
  json : Json_t.room;
  progress : Json_t.room_progress;
  mutable idx_configs : (int * idx_config) list;
  camera_bounds : bounds;
  cache : room_cache;
  enemies : enemy list;
  boss_area : rect option;
  exits : rect list;
  mutable respawn : respawn;
  mutable npcs : npc list;
  mutable layers : layer list;
  mutable interaction_label : (string * rect) option;
  (* this is for projectiles that are spawned by enemies after they die *)
  mutable loose_projectiles : projectile list;
  (* everything below is built from object layers *)
  triggers : triggers;
  floors : rect list;
  platforms : platform list;
  (* key is the platform.id, which is just the platform's coordinates as a string *)
  platform_spikes : rect String.Map.t;
  spikes : rect list;
  conveyor_belts : (rect * float) list;
  (* "hazards" are non-pogoable, like thorns in greenpath or crystals in c-dash *)
  hazards : rect list;
  (* these are only used in City of Chairs *)
  raindrops : sprite list;
}

(* these are all in pixels, scaled by Config.scale.room *)
type room_location = {
  filename : string;
  global_x : float;
  global_y : float;
  w : float;
  h : float;
}

type texture_cache = {
  (* TODO this causes every damage sprite to share the same texture
     - multiple sprites can still be rendered at separate dests, but they animations are synced
     - so creating a new damage sprite while one is on the screen will cause it to reset at the beginning
     - in practice this doesn't matter because the animation is so short, but it might cause problems for other cached textures

     - this will cause problems for rotating platforms
     - maybe just write a clone_texture function to spawn a new one
  *)
  damage : texture;
  ability_outlines : texture;
  pickup_indicator : texture;
  main_menu : texture;
  door_lever : texture;
  door_lever_struck : texture;
  platforms : texture String.Map.t;
  (* this is the only animated platform for now, maybe want a separate animated_platforms if more are added *)
  rotating_platform : texture;
  skybox : texture;
  raindrop : texture;
  ghost_bodies : ghost_body_textures;
  world_map : texture;
}

(* these are all things that are eager-loaded from json config files *)
type global_cache = {
  textures : texture_cache;
  lore : string String.Map.t;
  honda_quotes : string list;
  weapons : Json_t.weapon String.Map.t;
  (* TODO collision_shapes : (string * shape) list; *)
  enemy_configs : (enemy_id * Json_t.enemy_config) list;
  npc_configs : (npc_id * Json_t.npc_config) list;
  (* TODO could add sound_id instead of using strings *)
  sounds : Raylib.Sound.t String.Map.t;
}

type debug = {
  mutable enabled : bool;
  mutable show_frame_inputs : bool;
  mutable rects : (color * rect) list;
  mutable paused : bool;
  mutable safe_ss : bool;
}

type game = {
  mode : game_mode;
  mutable player : player;
  (* this should include a party_ghost for the currently-controlled ghost, so it should
     always be a list of all five ghosts
  *)
  mutable party : party_ghost list;
  mutable room : room;
  mutable room_changed_last_frame : bool;
  mutable in_boss_fight : bool;
  (* this is only used for the orb reflection *)
  mutable reflection_x : float option;
  mutable music : area_music;
  interaction : Interaction.t;
  (* - game.progress keeps track of the the current progress, and gets saved to save_file.progress
     when the game is saved
     - when the player dies, game.progress is reset to the save_file.progress
  *)
  progress : Json_t.game_progress;
  mutable save_file : Json_t.save_file;
  save_file_slot : int;
}

type camera = {
  mutable raylib : Raylib.Camera2D.t;
  mutable subject : camera_subject;
  mutable shake : float;
  (* the camera smoothly follows the subject, but sometimes (room transitions) it should be
     updated immediately
  *)
  mutable update_instantly : bool;
  mutable motion : camera_motion;
}

type game_context =
  | MAIN_MENU of menu * save_slot list
  | SAVE_FILES of menu * save_slot list
  | IN_PROGRESS of game
  | DIED of game

type frame_info = {
  mutable idx : int;
  mutable dt : float;
  mutable time : float;
  mutable timeout : int;
}

type world = (room_id * room_location) list

type settings = {
  mutable music_volume : float;
  mutable sound_effects_volume : float;
}

type world_map = {
  black_rects : rect list;
  ghost_pos : vector;
}

type pause_menu =
  | MENU of menu
  | WORLD_MAP of world_map
  | PROGRESS

type state = {
  mutable game_context : game_context;
  world : world;
  mutable screen_fade : screen_fade option;
  mutable camera : camera;
  mutable ignore_camera_triggers : bool;
  mutable frame : frame_info;
  mutable save_pos : vector option;
  mutable pause_menu : pause_menu option;
  frame_inputs : frame_inputs;
  mutable debug : debug;
  (* these are all configs that are eager-loaded from json on startup *)
  global : global_cache;
  menu_music : music;
  area_musics : area_music list;
  settings : settings;
}

let clone_vector (v : vector) : vector = { x = v.x; y = v.y }
let clone_rect (r : rect) : rect = { pos = clone_vector r.pos; w = r.w; h = r.h }
let clone_time (t : time) : time = { at = t.at }
let clone_sprite (sprite : sprite) : sprite = { sprite with dest = clone_rect sprite.dest }

let clone_entity (entity : entity) : entity =
  {
    entity with
    sprite = clone_sprite entity.sprite;
    dest = clone_rect entity.dest;
    v = clone_vector entity.v;
  }

let clone_room_progress (room_progress : Json_t.room_progress) : Json_t.room_progress =
  {
    removed_tile_idxs = room_progress.removed_tile_idxs;
    removed_platform_ids = room_progress.removed_platform_ids;
    finished_interactions = room_progress.finished_interactions;
    revealed_shadow_layers = room_progress.revealed_shadow_layers;
  }

let clone_progress_by_room (progress_by_room : (string * Json_t.room_progress) list) :
    (string * Json_t.room_progress) list =
  List.map (fun (name, progress) -> (name, clone_room_progress progress)) progress_by_room

let clone_game_progress (game_progress : Json_t.game_progress) : Json_t.game_progress =
  {
    by_room = clone_progress_by_room game_progress.by_room;
    keys_found = game_progress.keys_found;
    purple_pens_found = game_progress.purple_pens_found;
    dreamer_items_found = game_progress.dreamer_items_found;
    frame_idx = game_progress.frame_idx;
    steel_sole =
      { dunks = game_progress.steel_sole.dunks; c_dashes = game_progress.steel_sole.c_dashes };
    last_upgrade_claimed = game_progress.last_upgrade_claimed;
  }

let clone_abilities (abilities : Json_t.ghost_abilities) : Json_t.ghost_abilities =
  {
    (* movement *)
    crystal_heart = abilities.crystal_heart;
    dream_nail = abilities.dream_nail;
    ismas_tear = abilities.ismas_tear;
    mantis_claw = abilities.mantis_claw;
    monarch_wings = abilities.monarch_wings;
    mothwing_cloak = abilities.mothwing_cloak;
    shade_cloak = abilities.shade_cloak;
    (* spells *)
    vengeful_spirit = abilities.vengeful_spirit;
    shade_soul = abilities.shade_soul;
    desolate_dive = abilities.desolate_dive;
    descending_dark = abilities.descending_dark;
    howling_wraiths = abilities.howling_wraiths;
    abyss_shriek = abilities.abyss_shriek;
    quick_focus = abilities.quick_focus;
    soul_catcher_bonus = abilities.soul_catcher_bonus;
    dream_wielder = abilities.dream_wielder;
    deep_focus = abilities.deep_focus;
    shaman_stone = abilities.shaman_stone;
    spell_twister = abilities.spell_twister;
  }
