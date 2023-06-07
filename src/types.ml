[@@@ocaml.warning "-26-27-32"]

module Utils = struct
  let range (n : int) : int list = List.init n (fun x -> x)
  let rangef (n : int) : float list = List.init n (fun x -> x) |> List.map Int.to_float
  let bound_int (min : int) (n : int) (max : int) : int = Int.max min (Int.min n max)
  let bound (min : float) (n : float) (max : float) : float = Float.max min (Float.min n max)

  let uniq xs =
    let res = ref [] in
    let add_x x =
      if not (List.mem x !res) then
        res := x :: !res
    in
    List.iter add_x xs;
    !res

  let assoc_replace (k : 'a) (v : 'b) (xs : ('a * 'b) list) : ('a * 'b) list =
    match List.assoc_opt k xs with
    | None -> (k, v) :: xs
    | Some _ -> (k, v) :: List.remove_assoc k xs

  (* returns the strings before and after the first occurrence of char c:
     separate "a.b.c.d" '.' => "a", "b.c.d"
  *)
  let separate str c : string * string =
    let separator_idx =
      match String.index_from_opt str 0 c with
      | None -> failwith (Printf.sprintf "Utils.separate ---- no separator %c in string %s" c str)
      | Some idx -> idx
    in
    (Str.string_before str separator_idx, Str.string_after str (separator_idx + 1))

  let find_idx x xs =
    let matches ((_i, x') : int * 'a) : bool = x = x' in
    match List.find_opt matches (List.mapi (fun i x -> (i, x)) xs) with
    | None -> failwith "find_idx"
    | Some (idx, _) -> idx

  let filter_somes xs =
    (* TODO seems like there is probably a better way to do List.filter_mapi, but this works *)
    xs |> List.filter Option.is_some |> List.map Option.get
end

module StrSet = Set.Make (String)

type str_set = StrSet.t

let fmt s = Printf.sprintf s
let print fmtstr = Printf.ksprintf print_endline fmtstr
let tmp fmtstr = Printf.ksprintf print_endline fmtstr
let itmp fmtstr = Printf.ifprintf print_endline fmtstr
let failwithf f = Printf.ksprintf failwith f
let join ?(sep = ", ") strs = String.concat sep strs

(* TODO maybe add type hdirection = LEFT | RIGHT for things that only can be horizontal *)
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

type vector = {
  mutable x : float;
  mutable y : float;
}

type rect = {
  mutable pos : vector;
  mutable w : float;
  mutable h : float;
}

let get_center (rect : rect) = { x = rect.pos.x +. (rect.w /. 2.); y = rect.pos.y +. (rect.h /. 2.) }

type bounds = {
  min : vector;
  max : vector;
}

type time = { mutable at : float }
type duration = { seconds : float (* in seconds *) }
type color = Raylib.Color.t

module Zero = struct
  let vector () : vector = { x = 0.; y = 0. }
  let rect () : rect = { pos = vector (); w = 0.; h = 0. }
  let time () : time = { at = 0. }
end

(* the raw image file that a texture can source from *)
type image = Raylib.Texture.t

let load_tiled_asset path = Raylib.load_texture (fmt "../assets/tiled/%s" path)

let load_image path : image =
  let full_path = fmt "../assets/%s.png" path in
  if Sys.file_exists full_path then
    Raylib.load_texture full_path
  else
    failwithf "file doesn't exist: load_image %s" full_path

type animation_frame = {
  src : rect;
  duration : duration;
}

(* a list of animation_frames, and info about the currently-rendered frame
   - every frame in the animation has to have the same offsets from the collision rect (texture.coll_offsets')
*)
type animation = {
  frames : animation_frame list;
  mutable frame_idx : int;
  mutable frame_started : time;
}

type animation_src =
  | STILL of rect
  | LOOPED of animation
  | PARTICLE of animation
  | ONCE of animation

let get_frame (a : animation) : animation_frame = List.nth a.frames (a.frame_idx mod List.length a.frames)
let make_single_frame ~w ~h : rect = { w; h; pos = { x = 0.; y = 0. } }

type asset_dir =
  | GHOSTS
  | ENEMIES
  | NPCS

(* used to load textures to populate npc_texture_cache or ghost_textures *)
type texture_config = {
  asset_dir : asset_dir;
  (* this is either the npc name or the ghost name, capitalized like a variant name, eg. BRITTA or LOCKER_BOY *)
  character_name : string;
  (* this is the name of the specific pose being loaded, corresponding to a .png file *)
  pose_name : string;
  (* these values all come from the texture config in json *)
  x_offset : float;
  y_offset : float;
  count : int;
  duration : duration; (* TODO collision_shape configs should probably go here *)
}

(* an image that has a collision box and that might be animated
   - most textures are associated with an entity that has it's own dest rect, so coll_offset is
   only really used for tiles/tile_groups, which have some weird rules about collision rects:
   -- left == right so it has to be symmetrical
   -- bottom == 0
*)
type texture = {
  (* TODO maybe just make this file_path *)
  ident : string;
  image : image;
  animation_src : animation_src;
  coll_offset : vector;
}

let animation_loop_duration (t : texture) : float =
  match t.animation_src with
  | STILL _ -> failwith "can't get animation_loop_duration for STILL"
  | ONCE animation
  | PARTICLE animation
  | LOOPED animation ->
    (get_frame animation).duration.seconds *. (List.length animation.frames |> Int.to_float)

let get_src (t : texture) : rect =
  match t.animation_src with
  | STILL frame_src -> frame_src
  | ONCE (animation)
  | PARTICLE animation
  | LOOPED animation ->
    (get_frame animation).src

let get_scaled_texture_size ?(scale = Config.scale.room) (texture : texture) =
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

type shape = { edges : (vector * line) list }

type collision_shape =
  | DEST
  | SHAPE of shape

let make_shape (points : vector list) : shape =
  if List.length points < 3 then
    failwith "can't make a shape with < 3 points";
  let make_line_from_points (p1 : vector) (p2 : vector) : line =
    if p1.x = p2.x then
      { a = 1.; b = 0.; c = -1. *. p1.x }
    else (
      let slope = (p1.y -. p2.y) /. (p1.x -. p2.x) in
      let y_intercept = p1.y -. (slope *. p1.x) in
      { a = -1. *. slope; b = 1.; c = -1. *. y_intercept })
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

let get_collision_shape (sprite : sprite) =
  match sprite.collision with
  | None -> failwith "get_collision_shape for a shape with no collision"
  | Some DEST -> shape_of_rect sprite.dest
  | Some (SHAPE shape) -> align_shape_with_parent_sprite sprite shape

type entity_config = {
  bounce : float;
  (* only using the negative here because "animate" sounds more like a verb than an adjective *)
  inanimate : bool;
}

(* a sprite with physical/movement properties *)
type entity = {
  sprite : sprite;
  config : entity_config;
  dest : rect;
  mutable v : vector;
  mutable update_pos : bool;
  mutable current_floor : rect option;
  mutable x_recoil : recoil option;
  mutable y_recoil : recoil option;
}

let of_Rect (r : Raylib.Rectangle.t) : rect = Raylib.Rectangle.{ w = width r; h = height r; pos = { x = x r; y = y r } }
let to_Rect (r : rect) : Raylib.Rectangle.t = Raylib.Rectangle.create r.pos.x r.pos.y r.w r.h

(* A rect representing the overlap between a source entity and a target entity, and the direction
   that the source entity (probably) collided from
*)
type collision = {
  rect : rect;
  direction : direction;
}

(* non-ghost-specific textures that will be needed for any ghost *)
type shared_textures = {
  slash : texture;
  upslash : texture;
  downslash : texture;
  shine : texture;
  health : texture;
  vengeful_cushion : texture;
  desolate_dive : texture;
  dive_shockwave : texture;
  howling_wraiths : texture;
  energon_pod : texture;
  focus_sparkles : texture;
}

type spell_kind =
  | VENGEFUL_SPIRIT
  | DESOLATE_DIVE
  | HOWLING_WRAITHS

type ghost_action_kind =
  | FLAP
  | WALL_KICK
  | JUMP
  | TAKE_DAMAGE of direction
  | CAST of spell_kind
  | DIVE_COOLDOWN
  | DASH
  | ATTACK of direction
  | FOCUS

(* the ghost's state that can be mapped to a texture and rendered *)
type ghost_pose =
  | PERFORMING of ghost_action_kind
  | AIRBORNE of float (* new_vy *)
  | CRAWLING
  | IDLE
  | LANDING of rect
  | READING
  | WALKING of direction
  | WALL_SLIDING of rect

(* TODO maybe this should be (ghost_pose * texture) list now, but we need to match against ghost_pose anyway to apply side-effects *)
type ghost_textures = {
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

type ghost_id =
  | ABED
  | ANNIE
  | BRITTA
  | JEFF
  | TROY

(* when an enemy is initialized, this cache is filled with a texture for each image file in their directory *)
type npc_texture_cache = (string * texture) list

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

type enemy_id =
  | PENGUIN
  | DUNCAN
  | LOCKER_BOY

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

type save_files_choice =
  | SLOT_1
  | SLOT_2
  | SLOT_3
  | SLOT_4
  (* TODO add buttons for DELETE_SLOT_N *)
  | BACK

type pause_menu_choice =
  | CONTINUE
  | CHANGE_GHOST
  | CHANGE_WEAPON
  | QUIT_TO_MAIN_MENU

type change_weapon_menu_choice =
  | EQUIP_WEAPON of string
  | BACK

type change_ghost_menu_choice =
  | USE_GHOST of ghost_id
  | BACK

type menu_choice =
  | PAUSE_MENU of pause_menu_choice
  | CHANGE_WEAPON_MENU of change_weapon_menu_choice
  | CHANGE_GHOST_MENU of change_ghost_menu_choice
  | MAIN_MENU of main_menu_choice
  | SAVE_FILES of save_files_choice

type menu = {
  choices : menu_choice list;
  mutable current_choice_idx : int;
}

(* bool is true for new games *)
type save_slots = {
  slot_1 : Json_t.save_file * bool;
  slot_2 : Json_t.save_file * bool;
  slot_3 : Json_t.save_file * bool;
  slot_4 : Json_t.save_file * bool;
}

module Interaction = struct
  type general_step =
    | INITIALIZE_INTERACTIONS of bool
    | FADE_SCREEN_OUT
    | FADE_SCREEN_IN
    | WAIT of float
    | SPAWN_VENGEFUL_SPIRIT of direction * int * int
    (* text *)
    | TEXT of string list
    | FOCUS_ABILITY_TEXT of string list * rect * string list
    | ABILITY_TEXT of rect * string list
    | DIALOGUE of string * string
    | PURPLE_PEN_TEXT of string list
    | PUSH_RECT of float * float * float * float
    (* | POP_RECT *)
    (* camera *)
    | SET_FIXED_CAMERA of int * int
    | SET_GHOST_CAMERA
    (* layers *)
    | SWAP_HIDDEN_LAYER of string
    | HIDE_LAYER of string
    | UNHIDE_LAYER of string
    | HIDE_BOSS_DOORS
    | UNHIDE_BOSS_DOORS

  type entity_step =
    | SET_FACING of direction
    | HIDE
    | UNHIDE
    | UNHIDE_AT of int * int * float * float
    | FREEZE
    | UNFREEZE

  type item_kind =
    | WEAPON of string
    | ABILITY of string
    | DREAMER of string * string

  type ghost_step =
    | SET_POSE of ghost_pose
    | WALK_TO of int
    | FILL_LIFE_VAPOR
    | ADD_ITEM of item_kind
    | INCREASE_HEALTH_TEXT of bool * string
    | ADD_TO_PARTY
    | REMOVE_FROM_PARTY
    | JUMP of direction * float
    | ENTITY of entity_step
    | MAKE_CURRENT_GHOST

  type enemy_step =
    | WALK_TO of int
    | SET_VX of float
    | SET_POSE of string
    | SET_ACTION of string
    | ENTITY of entity_step

  type npc_step =
    | SET_POSE of string
    | WALK_TO of int
    | ENTITY of entity_step

  type step =
    | STEP of general_step
    | CURRENT_GHOST of ghost_step
    | GHOST of ghost_id * ghost_step
    | ENEMY of enemy_id * enemy_step
    | NPC of npc_id * npc_step

  type text_config = {
    padding_x : int;
    padding_y : int;
    margin_x : int;
    margin_y : int;
    margin_y_bottom : int;
    outline_offset_y : int;
    (* TODO
       - move this to type text
       - center text for ADD_WEAPON
       - maybe rename this type to text_box_config
    *)
    centered : bool;
  }

  type ability_text = {
    (* these top_paragraphs are only non-empty for focus-info for now *)
    top_paragraphs : string list;
    outline_src : rect;
    bottom_paragraphs : string list;
  }

  type text = {
    content : string list;
    increases_health : bool;
  }

  type text_kind =
    | PLAIN of text
    (* kinda weird for this to be a separate variant but use the same underlying type
       - the alternative is to just use ABILITY_TEXT everywhere, and have top_paragraphs almost always be empty
    *)
    | FOCUS_ABILITY of ability_text
    | ABILITY of ability_text
    | DIALOGUE of string * text
    | MENU of menu * save_slots option

  type t = {
    mutable name : string option;
    mutable steps : step list;
    mutable text : text_kind option;
    mutable speaker_name : string option;
    (* these are only used for revealing the opening poem for new games *)
    mutable black_rects : rect list;
  }
end

(* TODO drop geo
   - or maybe this should be boss_on_killed, and geo should be handled separately
*)
type enemy_on_killed = {
  interaction_name : string option;
  (* this means there are multiple enemies that all need to die before the interaction starts
     - this is pretty specific and will mostly be false, but it will at least be used for Sisters of Battle and Watcher Knights *)
  multiple_enemies : bool;
}

type damage_kind =
  | NAIL
  | VENGEFUL_SPIRIT
  | DESOLATE_DIVE
  | DESOLATE_DIVE_SHOCKWAVE
  | HOWLING_WRAITHS

type enemy_action =
  | PERFORMED of string
  | TOOK_DAMAGE of damage_kind

(* things that enemies use to decide what to do next
   - these could be optional values since not all enemies need all this info
*)
type enemy_behavior_params = {
  ghost_pos : vector;
  room_bounds : bounds;
  time : float;
}

type projectile_duration =
  | TIME_LEFT of duration
  | X_BOUNDS of float * float

type projectile = {
  entity : entity;
  mutable despawn : projectile_duration;
  spawned : time;
  pogoable : bool;
}

(* TODO probably don't need a separate type for this anymore *)
type enemy_status = {
  mutable check_damage_collisions : bool;
  mutable choose_behavior : bool;
}

type enemy_kind =
  | ENEMY
  | BOSS
  | MULTI_BOSS

type enemy = {
  id : enemy_id;
  kind : enemy_kind;
  status : enemy_status;
  entity : entity;
  mutable health : health;
  mutable history : (enemy_action * time) list;
  mutable props : (string * float) list;
  mutable spawned_projectiles : projectile list;
  mutable damage_sprites : sprite list;
  textures : (string * texture) list;
  json : Json_t.enemy_config;
  on_killed : enemy_on_killed;
  choose_behavior : self:enemy -> enemy_behavior_params -> unit;
}

type npc = {
  id : npc_id;
  entity : entity;
  textures : (string * texture) list;
}

let get_npc_texture (npc : npc) (texture_name : string) : texture =
  match List.assoc_opt texture_name npc.textures with
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
  mutable max : int;
  mutable at_focus_start : int;
  mutable health_at_focus_start : int;
  mutable last_decremented : time;
}

type ghost_action_config = {
  (* TODO add frame_duration
     - this might be used inconsistently right now, probably need to consolidate usages
  *)
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
  (* cast_dive uses "landing on a floor" *)
  cast_dive : ghost_action;
  dive_cooldown : ghost_action;
  cast_wraiths : ghost_action;
  dash : ghost_action;
  flap : ghost_action;
  jump : ghost_action;
  wall_kick : ghost_action;
  take_damage : ghost_action;
  (* checking is_doing for nail/focus uses the ghost.child sprite, not
     the duration/doing_until/blocked_until like the other actions
  *)
  nail : ghost_action;
  focus : ghost_action;
}

(* - instead of keeping track of the current ghost_pose in state, we just update the sprite texture whenever
   a new pose is set, so it can still render the latest pose
   - this data structure tracks the variant arguments that need to be checked/re-set in future frames
*)
type current_status = {
  (* TODO just move this to entity and get rid of this type, since some enemies will need this, and the others can leave it always None *)
  mutable wall : rect option;
  (* can't remove this:
     - `is_doing (CAST DIVE)` doesn't work because the dive config.duration is very large
     - dive doesn't use config.duration because it ends when hitting the floor
     - so this field is just updated whenever a dive starts/ends
  *)
  mutable is_diving : bool;
}

type frame_input = {
  mutable pressed : bool;
  mutable down : bool;
  mutable released : bool;
  mutable down_since : time option;
}

(* - this is updated every frame based on which keys are pressed, and should be used for inputs that:
   -- need to be buffered
   -- need socd (only left-right currently)
   - other inputs (eg. INTERACT) are checked directly with key_pressed
*)
type frame_inputs = {
  (* directions *)
  up : frame_input;
  down : frame_input;
  left : frame_input;
  right : frame_input;
  (* actions *)
  cast : frame_input;
  d_nail : frame_input;
  dash : frame_input;
  focus : frame_input;
  jump : frame_input;
  nail : frame_input;
  pause : frame_input;
}

type x_alignment =
  | LEFT
  | RIGHT
  | CENTER

type y_alignment =
  | TOP
  | BOTTOM
  | CENTER

type relative_position =
  | IN_FRONT
  | BEHIND
  | ALIGNED of x_alignment * y_alignment

(* - used for things that are "attached" to the ghost, ie their position depends on ghost
   - so spawned_vengeful_spirits are not children, but dive and shreik are
   - only one of these things should be rendered on the screen at a time
   - some things are purely visual (FOCUS)
   - some will have collisions, like NAIL, DIVE maybe, C_DASH_WHOOSH
*)
type ghost_child_kind =
  | NAIL of slash
  (* | DASH_WHOOSH of sprite *)
  (* | C_DASH_WHOOSH of sprite *)
  | WRAITHS
  | DIVE
  | DIVE_COOLDOWN
  | FOCUS

type ghost_child = {
  kind : ghost_child_kind;
  relative_pos : relative_position;
  sprite : sprite;
}

type invincibility_kind =
  (* TODO add something like | NO_FLASHING *)
  | DIVE_IFRAMES
  | TOOK_DAMAGE

(* this is very similar to Json_t.ghosts_file, but it eg. parses ghost names into ghost_id for the key of .textures *)
type ghosts_file = {
  textures : (ghost_id * texture_config list) list;
  actions : (string * ghost_action_config) list;
  shared_textures : (string * texture_config) list;
}

type ghost = {
  entity : entity;
  current : current_status;
  mutable textures : ghost_textures;
  (* TODO probably don't need .shared_textures on every ghost *)
  shared_textures : shared_textures;
  history : ghost_action_history;
  mutable abilities : Json_t.ghost_abilities;
  mutable current_weapon : weapon;
  mutable weapons : (string * Json_t.weapon) list;
  mutable in_party : bool;
  mutable id : ghost_id;
  mutable child : ghost_child option;
  mutable health : health;
  mutable soul : soul;
  mutable can_dash : bool;
  mutable can_flap : bool;
  mutable spawned_vengeful_spirits : projectile list;
}

(* a cache for image/tiles that have been loaded for a tileset *)
type tileset = {
  json : Json_t.tileset;
  (* TODO this doesn't really need to be an array of textures, since the image is
     always the same and the animation is always STILL. This is just needed to parse
     the collision rects per-tile, so maybe this could be a `coll_offsets list`
  *)
  (* expected to be non-empty *)
  tiles : texture array;
}

type door_health = {
  mutable hits : int;
  mutable last_hit_at : float;
}

(* - a rectangle of tiles that is grouped into a single collision
   -  eg floors, jugs, doors
*)
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
   - jugs are in the same plane as the ghost but don't collide, so they will still have fg or bg set
   - there is validation to make sure that only one of "fg", "bg", or "collides" is configured
*)
type layer_render_config = {
  bg : bool;
  fg : bool;
}

type layer_config = {
  render : layer_render_config;
  collides_with_ghost : bool;
  damages_ghost : bool;
  pogoable : bool;
  destroyable : bool;
  permanently_removable : bool;
  shaded : bool;
}

(* a tilelayer in Tiled *)
type layer = {
  name : string;
  json : Json_t.tile_layer;
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
  | TRAMPOLINEPATH
  | FINAL (* only used for final boss fight *)

type area = {
  id : area_id;
  tint : Raylib.Color.t;
  bg_color : Raylib.Color.t;
}

(* it seems weird to have the area_id embedded in the name, but it's for room names that are shared *)
type room_id =
  (* AC_REPAIR_ANNEX *)
  | AC_DREAMER
  | AC_A
  | AC_B
  | AC_C
  | AC_D
  | AC_E
  | AC_F
  | AC_G
  (* BASEMENT *)
  | BASE_ABYSS_CLIMB
  | BASE_A
  | BASE_B
  | BASE_C
  | BASE_D
  | BASE_E
  | BASE_F
  (* CITY_OF_CHAIRS *)
  | CC_A
  | CC_B
  | CC_C
  | CC_D
  | CC_E
  (* COMPUTER_WING *)
  | CW_DREAMER
  | CW_A
  | CW_B
  | CW_C
  | CW_D
  | CW_E
  | CW_F
  (* FORGOTTEN_CLASSROOMS *)
  | FC_DEANS_PASS
  | FC_CAFETERIA
  | FC_STAIRWELL
  | FC_A
  | FC_B
  | FC_C
  | FC_D
  | FC_E
  | FC_F
  | FC_G
  | FC_H
  | FC_I
  (* INFECTED_CLASSROOMS *)
  | IC_TEACHERS_LOUNGE
  | IC_A
  | IC_B
  | IC_C
  | IC_D
  | IC_E
  | IC_F
  | IC_G
  | IC_H
  | IC_I
  (* MEOW_MEOW_BEENZ *)
  | MMB_1_A
  | MMB_2_A
  | MMB_2_B
  | MMB_2_C
  | MMB_3_ELEVATOR
  | MMB_3_A
  | MMB_3_COLO
  | MMB_4_A
  | MMB_4_COLO
  | MMB_5_LOUNGE
  | MMB_5_PALACE_GROUNDS
  | MMB_OUTLANDS_A
  | MMB_OUTLANDS_B
  | MMB_OUTLANDS_C
  (* TRAMPOLINEPATH *)
  | TP_DREAMER
  | TP_A
  | TP_B
  | TP_C
  | TP_D
  | TP_E
  | TP_F
  | TP_G
  (* FINAL *)
  | BOSS

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
  jug_fragments_by_gid : (int * jug_fragments) list;
  tilesets_by_path : (string * tileset) list;
}

type triggers = {
  camera : (string * rect) list;
  lore : (string * rect) list;
  item_pickups : (string * rect) list;
  shadows : (string * rect) list;
  cutscene : (string * rect) list;
  levers : (string * sprite) list;
}

type camera_state = {
  current_pos : vector;
  subject : vector;
  room_bounds : bounds;
}

type idx_config =
  | PURPLE_PEN of string
  | DOOR_HITS of int
(* | DOOR_LEVER of int *)

(* TODO add current_interaction : string to handle dying during a boss-fight
   - unset on death
   - on duncan-killed, move current_interaction into finished_interactions
*)
type room = {
  id : room_id;
  area : area;
  json : Json_t.room;
  progress : Json_t.room_progress;
  mutable idx_configs : (int * idx_config) list;
  camera_bounds : bounds;
  cache : room_cache;
  triggers : triggers;
  enemies : (enemy_id * enemy) list;
  exits : rect list;
  mutable npcs : npc list;
  mutable layers : layer list;
  mutable pickup_indicators : sprite list;
}

(* these are all in pixels, scaled by Config.scale.room *)
type room_location = {
  filename : string;
  global_x : float;
  global_y : float;
  w : float;
  h : float;
}

type world = (room_id * room_location) list

type texture_cache = {
  (* TODO this causes every damage sprite to share the same texture
     - multiple sprites can still be rendered at separate dests, but they animations are synced
     - so creating a new damage sprite while one is on the screen will cause it to reset at the beginning
     - in practice this doesn't matter because the animation is so short, but it might cause problems for other cached textures
  *)
  damage : texture;
  ability_outlines : texture;
  pickup_indicator : texture;
  main_menu : texture;
  door_lever : texture;
  door_lever_struck : texture;
}

(* these are all things that are eager-loaded from json config files *)
type global_cache = {
  textures : texture_cache;
  lore : (string * string) list;
  weapons : (string * Json_t.weapon) list;
  (* TODO collision_shapes : (string * shape) list; *)
  enemy_configs : (enemy_id * Json_t.enemy_config) list;
  npc_configs : (npc_id * Json_t.npc_config) list;
}

type frame_info = {
  mutable idx : int;
  mutable dt : float;
  mutable time : float;
}

type debug = {
  mutable enabled : bool;
  mutable rects : (color * rect) list;
}

type game = {
  mutable ghost : ghost;
  mutable ghosts : (ghost_id * ghost) list;
  mutable room : room;
  interaction : Interaction.t;
  (* string is room uuid *)
  mutable progress : (string * Json_t.room_progress) list;
  mutable save_file_slot : int;
}

type camera = {
  mutable raylib : Raylib.Camera2D.t;
  mutable subject : camera_subject;
  mutable shake : float;
}

type game_context =
  | MAIN_MENU of menu * save_slots
  | SAVE_FILES of menu * save_slots
  | IN_PROGRESS of game

type state = {
  mutable game_context : game_context;
  world : world;
  mutable screen_fade : int option; (* out of 255 *)
  mutable camera : camera;
  mutable frame : frame_info;
  mutable pause_menu : menu option;
  frame_inputs : frame_inputs;
  mutable debug : debug;
  (* these are all configs that are eager-loaded from json on startup *)
  global : global_cache;
}

let clone_vector (v : vector) : vector = { x = v.x; y = v.y }
let clone_rect (r : rect) : rect = { pos = clone_vector r.pos; w = r.w; h = r.h }
let clone_time (t : time) : time = { at = t.at }
