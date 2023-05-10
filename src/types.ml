[@@@ocaml.warning "-26-27-32"]

module Utils = struct
  let range (n : int) : int list = List.init n (fun x -> x)
  let rangef (n : int) : float list = List.init n (fun x -> x) |> List.map Int.to_float
  let boundi (min : int) (n : int) (max : int) : int = Int.max min (Int.min n max)
  let boundi_below (min : int) (n : int) : int = Int.max min n
  let boundi_above (max : int) (n : int) : int = Int.min max n

  let bound ?(debug = false) (min : float) (n : float) (max : float) : float =
    if debug then
      print_endline (Printf.sprintf "bound %0.1f between (%0.1f, %0.1f)" n min max);
    Float.max min (Float.min n max)

  let bound_below (min : float) (n : float) : float = Float.max min n
  let bound_above (max : float) (n : float) : float = Float.min max n

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
  duration : duration;
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
  | PARTICLE animation
  | LOOPED animation ->
    (get_frame animation).duration.seconds *. (List.length animation.frames |> Int.to_float)

let get_src (t : texture) : rect =
  match t.animation_src with
  | STILL frame_src -> frame_src
  | PARTICLE animation
  | LOOPED animation ->
    (get_frame animation).src

let get_scaled_texture_size ?(scale = Config.scale.room) (t : texture) =
  let src = get_src t in
  (src.w *. scale, src.h *. scale)

(* knockback in a single direction *)
type recoil = {
  speed : float;
  mutable time_left : duration;
  reset_v : bool;
}

(* a texture that is rendered at a specific location:
   - for sprites that belong to entities, changes are applied to entity.dest.p and mirrored to sprite.dest
   - sprites that don't belong to entities don't change position each frame (eg tiles)
*)
type sprite = {
  ident : string;
  mutable texture : texture;
  mutable dest : rect;
  mutable facing_right : bool;
}

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

(* TODO move to collision.ml *)
(* direction here means "the direction that the entity is (probably) colliding from" *)
let collision_between (e : entity) (r2 : rect) : collision option =
  let cr : rect = Raylib.get_collision_rec (to_Rect e.dest) (to_Rect r2) |> of_Rect in
  let feq a b =
    (* - Float.equal is too precise
       - 0.0001 was also too precise (for CAFETERIA-sized rooms)
    *)
    abs_float (a -. b) < 0.0001
  in
  let no_collision = cr.w < 0.1 || cr.h < 0.1 in
  (* let no_collision = feq cr.w 0. || feq cr.h 0. in *)
  if no_collision then
    None
  else
    Some
      {
        rect = cr;
        direction =
          (let up = feq r2.pos.y cr.pos.y in
           let down = feq (r2.pos.y +. r2.h) (cr.pos.y +. cr.h) in
           let left = feq r2.pos.x cr.pos.x in
           let right = feq (r2.pos.x +. r2.w) (cr.pos.x +. cr.w) in
           match (up, down, left, right) with
           (* one side *)
           | true, false, false, false -> UP
           | false, true, false, false -> DOWN
           | false, false, true, false -> LEFT
           | false, false, false, true -> RIGHT
           (* top-bottom / left-right *)
           | true, true, false, false ->
             if e.v.y >= 0. then
               UP
             else
               DOWN
           | false, false, true, true -> if e.v.x >= 0. then LEFT else RIGHT
           (* corners *)
           (* TODO this is too "slippery": falling too fast causes top collisions to be considered side collisions
              - can probably fix this by considering position of entity at start of frame (before applying v)
              - this is much more noticeable at 60fps
           *)
           | false, true, true, false -> if cr.h < cr.w then DOWN else LEFT
           | true, false, true, false ->
             if cr.h < cr.w then
               UP
             else
               LEFT
           | false, true, false, true -> if cr.h < cr.w then DOWN else RIGHT
           | true, false, false, true ->
             if cr.h < cr.w then
               UP
             else
               RIGHT
           (* three sides *)
           | false, true, true, true -> DOWN
           | true, false, true, true -> UP
           | true, true, false, true -> RIGHT
           | true, true, true, false -> LEFT
           (* ghost covers entire collision *)
           | true, true, true, true
           (* ghost is entirely inside collision *)
           | false, false, false, false -> (
             match (e.v.x > 0., e.v.y > 0., e.v.x > e.v.y) with
             | true, true, true -> LEFT
             | true, true, false -> UP
             | true, false, true -> LEFT
             | true, false, false -> DOWN
             | false, true, true -> RIGHT
             | false, true, false -> UP
             | false, false, true -> RIGHT
             | false, false, false -> DOWN));
      }

(* non-ghost-specific textures that will be needed for any ghost *)
type shared_textures = {
  slash : texture;
  upslash : texture;
  downslash : texture;
  shine : texture;
  health : texture;
  vengeful_cushion : texture;
  energon_pod : texture;
  focus_sparkles : texture;
}

(* the ghost's state that can be mapped to a texture and rendered *)
type ghost_pose =
  | AIRBORNE of float (* new_vy *)
  | ATTACKING of direction
  | (* TODO either needs to be CASTING of spell_type, or make separate variants for dive/shreik *)
    CASTING
  | CRAWLING
  | DASHING
  | DIVING
  | FLAPPING
  | FOCUSING
  | IDLE
  | JUMPING
  | LANDING of rect
  | READING
  | TAKING_DAMAGE of direction
  | WALKING of direction
  | WALL_JUMPING
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

  type ghost_step =
    | SET_POSE of ghost_pose
    | WALK_TO of int
    | FILL_LIFE_VAPOR
    | ADD_WEAPON of string
    | ADD_ABILITY of string
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
    text_box_width : int;
    padding_x : int;
    padding_y : int;
    margin_x : int;
    margin_y : int;
    margin_y_bottom : int;
    outline_offset_y : int;
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

  type t = {
    mutable name : string option;
    mutable steps : step list;
    mutable text : text_kind option;
    mutable speaker_name : string option;
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
  (* TODO add collision_shape instead of using entire sprite.dest *)
  sprite : sprite;
  direction : direction;
}

let slash_collision_between (slash : slash) (r2 : rect) : collision option =
  let cr : rect =
    (* TODO use slash.collision instead of sprite.dest *)
    Raylib.get_collision_rec (to_Rect slash.sprite.dest) (to_Rect r2) |> of_Rect
  in
  let no_collision = cr.w < 0.1 || cr.h < 0.1 in
  if no_collision then
    None
  else
    Some { rect = cr; direction = opposite_of slash.direction }

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
type ghost_actions = {
  cast : ghost_action;
  dash : ghost_action;
  double_jump : ghost_action;
  jump : ghost_action;
  wall_jump : ghost_action;
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
type pose_status = {
  (* TODO just move this to entity and get rid of this type, since some enemies will need this, and the others can leave it always None *)
  mutable wall : rect option;
}

type frame_input = {
  mutable pressed : bool;
  mutable down : bool;
  mutable released : bool;
  mutable down_since : time option;
}

(* - this is updated every frame based on which keys are pressed, and should be used for inputs that:
   -- need to be buffered
   -- need to be checked with is_doing
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
}

(* spells and abilities *)
type abilities = {
  (* movement *)
  mutable crystal_heart : bool;
  mutable mantis_claw : bool;
  mutable monarch_wings : bool;
  mutable mothwing_cloak : bool;
  (* spells *)
  mutable vengeful_spirit : bool;
  mutable desolate_dive : bool;
  mutable howling_wraiths : bool;
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
  (* | DIVE of sprite *)
  | FOCUS of sprite

type ghost_child = {
  kind : ghost_child_kind;
  relative_pos : relative_position;
}

type weapon = {
  name : string;
  tint : color;
  scale_x : float;
  scale_y : float;
  cooldown_scale : float;
}

type ghost = {
  entity : entity;
  current : pose_status;
  mutable textures : ghost_textures;
  shared_textures : shared_textures;
  actions : ghost_actions;
  mutable abilities : abilities;
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

type json_world = Json_t.world
type json_room = Json_t.room
type json_tileset = Json_t.tileset
type json_tileset_source = Json_t.tileset_source
type json_tile_layer = Json_t.tile_layer
type json_object_layer = Json_t.object_layer
type json_layer = Json_t.layer
type json_enemy_config = Json_t.enemy_config
type json_npc_config = Json_t.npc_config

(* a cache for image/tiles that have been loaded for a tileset *)
type tileset = {
  json : json_tileset;
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
  json : json_tile_layer;
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
  shadows : (string * rect) list;
  cutscene : (string * rect) list;
}

type camera_state = {
  current_pos : vector;
  subject : vector;
  room_bounds : bounds;
}

type idx_config =
  | PURPLE_PEN of string
  | DOOR_HITS of int

(* TODO levers to unlock doors - use a new trigger type *)
(* TODO add current_interaction : string to handle dying during a boss-fight
   - unset on death
   - on duncan-killed, move current_interaction into finished_interactions
*)
(* this progress is permanently saved so eg. removed_idxs_by_layer is for doors that are permanently removed, but
   not for jugs (which respawn when the room is re-entered, and are tracked by layer.tile_groups / .destroyed_tiles)
*)
type room_progress = {
  mutable removed_idxs_by_layer : (string * int list) list;
  mutable finished_interactions : string list;
  mutable revealed_shadow_layers : string list;
}

type room = {
  id : room_id;
  area : area;
  json : json_room;
  progress : room_progress;
  mutable idx_configs : (int * idx_config) list;
  camera_bounds : bounds;
  cache : room_cache;
  triggers : triggers;
  enemies : (enemy_id * enemy) list;
  exits : rect list;
  mutable npcs : npc list;
  mutable layers : layer list;
}

(* TODO
   type camera = {
     mutable raylib : Raylib.Camera2D.t;
     mutable subject : camera_subject;
     mutable shake : int;
   }
*)

type progress = {
  (* string is room uuid *)
  mutable rooms : (string * room_progress) list;
  mutable global : string list;
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
}

(* these are all things that are eager-loaded from json config files *)
type global_cache = {
  textures : texture_cache;
  lore : (string * string) list;
  weapons : (string * Json_t.weapon) list;
  enemy_configs : (enemy_id * json_enemy_config) list;
  npc_configs : (npc_id * json_npc_config) list;
}

type frame_info = {
  mutable idx : int;
  mutable dt : float;
  mutable time : float;
}

type ghost_cache = {
  annie : ghost;
  abed : ghost;
  britta : ghost;
  jeff : ghost;
  troy : ghost;
}

type debug = {
  mutable enabled : bool;
  mutable rects : (color * rect) list;
}

type state = {
  mutable ghost : ghost;
  mutable ghosts : (ghost_id * ghost) list;
  mutable room : room;
  world : world;
  mutable screen_faded : bool;
  interaction : Interaction.t;
  progress : progress;
  mutable camera : Raylib.Camera2D.t;
  mutable camera_subject : camera_subject;
  mutable shake : float;
  mutable frame : frame_info;
  frame_inputs : frame_inputs;
  mutable debug : debug;
  (* these are all configs that are eager-loaded from json on startup *)
  global : global_cache;
}

let clone_vector (v : vector) : vector = { x = v.x; y = v.y }
let clone_rect (r : rect) : rect = { pos = clone_vector r.pos; w = r.w; h = r.h }
let clone_time (t : time) : time = { at = t.at }
