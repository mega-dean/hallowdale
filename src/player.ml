open Utils
open Types
open Controls

let parse_name name : ghost_id =
  match name with
  | "ABED" -> ABED
  | "ANNIE" -> ANNIE
  | "BRITTA" -> BRITTA
  | "JEFF" -> JEFF
  | "TROY" -> TROY
  | _ -> failwithf "bad ghost name '%s'" name

let read_config () : ghosts_file =
  let ghost_file : Json_t.ghosts_file = File.read_config "ghosts" Json_j.ghosts_file_of_string in
  let parse_texture_config character_name ((pose_name, config) : string * Json_t.texture_config) :
      texture_config =
    {
      path = { asset_dir = GHOSTS; character_name; pose_name };
      count = config.count;
      duration = { seconds = config.duration };
      x_offset = config.x_offset;
      y_offset = config.y_offset;
    }
  in
  let parse_ghost_texture
      ((ghost_id_str, ghost_poses) : string * (string * Json_t.texture_config) list) :
      ghost_id * texture_config list =
    let texture_configs : texture_config list =
      List.map (parse_texture_config ghost_id_str) ghost_poses
    in
    (parse_name ghost_id_str, texture_configs)
  in
  let load_textures name textures =
    let parse_texture_config' (s, tc) : string * texture_config =
      (s, parse_texture_config name (s, tc))
    in
    List.map parse_texture_config' textures
  in
  let shared_textures : (string * texture_config) list =
    load_textures "shared" ghost_file.shared_textures
  in
  let body_textures : (string * texture_config) list =
    load_textures "body" ghost_file.body_textures
  in
  let parse_ghost_action ((name, json_ghost_action) : string * Json_t.ghost_action) =
    ( name,
      {
        duration = { seconds = json_ghost_action.duration };
        cooldown = { seconds = json_ghost_action.cooldown };
        input_buffer = { seconds = json_ghost_action.input_buffer };
      } )
  in
  let actions : (string * ghost_action_config) list =
    List.map parse_ghost_action ghost_file.actions
  in
  {
    actions = actions |> List.to_string_map;
    body_textures = body_textures |> List.to_string_map;
    shared_textures = shared_textures |> List.to_string_map;
  }

let ghost_ids_in_party ghosts : ghost_id list =
  ghosts |> List.filter_map (fun g -> if g.in_party then Some g.ghost.id else None)

let maybe_begin_interactions
    (state : state)
    (game : game)
    ?(followup : trigger option = None)
    (trigger : trigger) =
  if List.length game.interaction.steps = 0 then (
    let begin_interactions ?(autosave_pos = None) () =
      game.interaction.floating_text <- None;
      game.interaction.steps <- Interactions.get_steps state game ~autosave_pos ~followup trigger
    in
    let begin_cutscene_interaction ?(autosave_pos = None) name =
      (* these are only viewable once *)
      if not (List.mem name game.room.progress.finished_interactions) then
        begin_interactions ~autosave_pos ()
    in
    let strip_blocking_interaction (full_name : string) : string =
      String.maybe_trim_before '|' full_name
    in
    let name = strip_blocking_interaction trigger.full_name in
    match trigger.kind with
    | REFLECT -> game.reflection_x <- Some trigger.dest.pos.x
    | WARP _
    | PURPLE_PEN
    | INFO ->
      (* these have no side effects and can be repeated
         - purple pens aren't repeatable because destroying them removes the trigger
      *)
      begin_interactions ()
    | BOSS_FIGHT autosave_pos -> (
      match game.mode with
      | DEMO
      | STEEL_SOLE ->
        ()
      | CLASSIC -> begin_cutscene_interaction ~autosave_pos:(Some autosave_pos) name)
    | D_NAIL
    | BOSS_KILLED
    | CUTSCENE -> (
      match game.mode with
      | DEMO
      | STEEL_SOLE ->
        ()
      | CLASSIC -> begin_cutscene_interaction name)
    | CAMERA _
    | FOLLOWUP
    | LEVER
    | SHADOW
    | RESPAWN ->
      failwithf "cannot begin interaction with kind %s" (Show.trigger_kind trigger.kind))

let maybe_begin_interaction (state : state) (game : game) trigger =
  maybe_begin_interactions state game trigger

let find_trigger_collision' player (xs : 'a list) get_trigger_dest : 'a option =
  let colliding x =
    Option.is_some (Collision.with_entity player.ghost.entity (get_trigger_dest x))
  in
  List.find_opt colliding xs

let find_trigger_collision player (triggers : trigger list) =
  find_trigger_collision' player triggers (fun t -> t.dest)

let find_respawn_trigger_collision player (triggers : respawn_trigger list) : respawn_trigger option
    =
  find_trigger_collision' player triggers (fun rt -> rt.trigger.dest)

let pogo (player : player) =
  match player.ghost.entity.y_recoil with
  | Some _ -> ()
  | None ->
    player.ghost.entity.current_floor <- None;
    player.current.can_dash <- true;
    player.current.can_flap <- true;
    player.ghost.entity.y_recoil <-
      Some
        {
          speed = -.Config.ghost.recoil_speed;
          time_left = { seconds = Config.ghost.pogo_recoil_time };
          reset_v = true;
        }

let make_slash
    (player : player)
    (direction : direction)
    relative_pos
    (sprite : sprite)
    (frame_time : float) : slash =
  let textures = player.shared_textures in
  let vertical_slash = ref true in
  let slash_texture : texture =
    match direction with
    | LEFT
    | RIGHT ->
      vertical_slash := false;
      textures.slash
    | UP -> textures.upslash
    | DOWN -> textures.downslash
  in
  let count =
    match slash_texture.animation_src with
    | STILL _
    | ONCE _
    | LOOPED _ ->
      failwith "invalid slash texture animation_src - should be PARTICLE"
    | PARTICLE animation -> List.length animation.frames
  in
  let src_w = Raylib.Texture.width slash_texture.image / count |> Int.to_float in
  let src_h = Raylib.Texture.height slash_texture.image |> Int.to_float in
  let dest_w', dest_h' = (src_w *. Config.scale.slash, src_h *. Config.scale.slash) in
  let dest_w, dest_h =
    if !vertical_slash then
      (dest_w', dest_h' *. player.current_weapon.scale_y)
    else
      (dest_w' *. player.current_weapon.scale_x, dest_h')
  in
  let dest =
    let pos = Entity.get_child_pos player.ghost.entity relative_pos dest_w dest_h in
    { pos; h = dest_h; w = dest_w }
  in

  (* TODO move these to config *)
  let sideways_points =
    [
      { x = 0. *. player.current_weapon.scale_x; y = 0. };
      { x = 111. *. player.current_weapon.scale_x; y = 8. };
      { x = 158. *. player.current_weapon.scale_x; y = 16. };
      { x = 184. *. player.current_weapon.scale_x; y = 26. };
      { x = 196. *. player.current_weapon.scale_x; y = 37. };
      { x = 195. *. player.current_weapon.scale_x; y = 49. };
      { x = 178. *. player.current_weapon.scale_x; y = 64. };
      { x = 136. *. player.current_weapon.scale_x; y = 77. };
      { x = 60. *. player.current_weapon.scale_x; y = 86. };
      { x = 1. *. player.current_weapon.scale_x; y = 87. };
    ]
  in
  let up_points =
    [
      { x = 0.; y = 189. *. player.current_weapon.scale_y };
      { x = 5.; y = 110. *. player.current_weapon.scale_y };
      { x = 15.; y = 63. *. player.current_weapon.scale_y };
      { x = 30.; y = 25. *. player.current_weapon.scale_y };
      { x = 45.; y = 8. *. player.current_weapon.scale_y };
      { x = 55.; y = 0. *. player.current_weapon.scale_y };
      { x = 75.; y = 1. *. player.current_weapon.scale_y };
      { x = 90.; y = 14. *. player.current_weapon.scale_y };
      { x = 110.; y = 46. *. player.current_weapon.scale_y };
      { x = 120.; y = 86. *. player.current_weapon.scale_y };
      { x = 130.; y = 189. *. player.current_weapon.scale_y };
    ]
  in
  let down_points =
    List.map
      (fun (point : vector) ->
        { point with y = (189. -. point.y) *. player.current_weapon.scale_y })
      up_points
  in
  let make_scaled_shape ?(scale = Config.scale.slash) points =
    let scale_point point = { x = point.x *. scale; y = point.y *. scale } in
    let scaled_points = List.map scale_point points in
    make_shape scaled_points
  in
  let sprite =
    (* slash will have a sprite with None .collision because there is a (non-optional) .collision on slash itself *)
    Sprite.spawn_particle "slash" slash_texture ~facing_right:sprite.facing_right dest frame_time
  in
  let collision =
    match direction with
    | DOWN -> SHAPE (make_scaled_shape down_points)
    | UP -> SHAPE (make_scaled_shape up_points)
    | _ -> SHAPE (make_scaled_shape sideways_points)
  in
  { sprite; direction; collision }

exception ShortCircuit

let find_child fn (children : ghost_child Ghost_child_kind.Map.t) : 'a option =
  let r = ref None in
  try
    Ghost_child_kind.Map.iter
      (fun child_kind child ->
        match fn (child_kind, child) with
        | Some res ->
          r := Some res;
          raise ShortCircuit
        | None -> ())
      children;
    None
  with
  | ShortCircuit -> !r

let get_spell_sprite (player : player) : (sprite * time) option =
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) : (sprite * time) option =
    match child_kind with
    | WRAITHS -> Some (child.sprite, player.history.cast_wraiths.started)
    | DIVE -> Some (child.sprite, player.history.cast_dive.started)
    | DIVE_COOLDOWN -> Some (child.sprite, player.history.dive_cooldown.started)
    | _ -> None
  in
  find_child get_sprite player.children

let get_current_slash (player : player) : slash option =
  let get_slash ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | NAIL slash -> Some slash
    | _ -> None
  in
  find_child get_slash player.children

let get_dream_nail (player : player) : rect option =
  let get_rect ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | DREAM_NAIL -> Some child.sprite.dest
    | _ -> None
  in
  find_child get_rect player.children

let get_dive_sprite (player : player) : sprite option =
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | DIVE
    | DIVE_COOLDOWN ->
      Some child.sprite
    | _ -> None
  in
  find_child get_sprite player.children

let get_focus_sparkles ghost : sprite option =
  let get_sprite ((child_kind, child) : ghost_child_kind * ghost_child) =
    match child_kind with
    | FOCUS -> Some child.sprite
    | _ -> None
  in
  find_child get_sprite ghost.children

let add_child (player : player) (kind : ghost_child_kind) (child : ghost_child) =
  player.children <- Ghost_child_kind.Map.update kind (fun _ -> Some child) player.children

let remove_child (player : player) (kind : ghost_child_kind) =
  player.children <- Ghost_child_kind.Map.remove kind player.children

let make_ghost_child ?(in_front = false) (player : player) kind relative_pos texture w h =
  let name = Show.ghost_child_kind kind in
  let sprite =
    Sprite.create name texture
      { pos = Entity.get_child_pos player.ghost.entity relative_pos w h; w; h }
  in
  { relative_pos; sprite; in_front }

let spawn_child
    ?(in_front = false)
    ?(scale = 1.)
    (player : player)
    (child_kind : ghost_child_kind)
    alignment
    texture =
  let child =
    let w, h = get_scaled_texture_size scale texture in
    make_ghost_child player ~in_front child_kind alignment (Sprite.reset_animation texture) w h
  in
  add_child player child_kind child

let make_c_dash_child ?(full = false) (player : player) : unit =
  let texture =
    match (player.current.wall, full) with
    | None, true -> player.shared_textures.c_dash_crystals_full
    | None, false -> player.shared_textures.c_dash_crystals
    | Some _, true -> player.shared_textures.c_dash_wall_crystals_full
    | Some _, false -> player.shared_textures.c_dash_wall_crystals
  in
  let alignment : relative_position =
    match (player.current.wall, player.ghost.entity.sprite.facing_right) with
    | None, _ -> (CENTER, BOTTOM_INSIDE)
    | Some _, true -> (LEFT_INSIDE, CENTER)
    | Some _, false -> (RIGHT_INSIDE, CENTER)
  in
  let child_kind =
    match player.current.wall with
    | None -> C_DASH_CHARGE_CRYSTALS
    | Some _ -> C_DASH_WALL_CHARGE_CRYSTALS
  in
  Sprite.reset_animation_ texture;
  spawn_child player child_kind alignment ~scale:Config.scale.ghost texture

let animate_and_despawn_children frame_time ghost : unit =
  let advance (child_kind : ghost_child_kind) (child : ghost_child) =
    match child.sprite.texture.animation_src with
    | ONCE _ -> (
      match Sprite.advance_or_despawn frame_time child.sprite.texture child.sprite with
      | None ->
        remove_child ghost child_kind;
        make_c_dash_child ~full:true ghost
      | Some sprite -> ( (* already advanced *) ))
    | PARTICLE _ -> (
      match Sprite.advance_or_despawn frame_time child.sprite.texture child.sprite with
      | None -> remove_child ghost child_kind
      | _ -> ( (* already advanced *) ))
    | STILL _
    | LOOPED _ ->
      Sprite.advance_animation frame_time child.sprite
  in
  Ghost_child_kind.Map.iter advance ghost.children

let get_nail_damage (player : player) =
  player.weapons
  |> String.Map.to_list
  |> List.map snd
  |> List.map (fun (w : Json_t.weapon) -> w.damage)
  |> List.fold_left ( + ) 0

let get_damage (player : player) (damage_kind : damage_kind) =
  match damage_kind with
  | DREAM_NAIL -> 0
  | NAIL -> get_nail_damage player
  | VENGEFUL_SPIRIT -> (
    match (player.abilities.shade_soul, player.abilities.shaman_stone) with
    | false, false -> 15
    | true, false -> 30
    | false, true -> 20
    | true, true -> 40)
  | DESOLATE_DIVE -> (
    match (player.abilities.descending_dark, player.abilities.shaman_stone) with
    | false, false -> 15
    | true, false -> 30
    | false, true -> 23
    | true, true -> 45)
  | DESOLATE_DIVE_SHOCKWAVE -> (
    match (player.abilities.descending_dark, player.abilities.shaman_stone) with
    | false, false -> 20
    | true, false -> 40
    | false, true -> 30
    | true, true -> 50)
  | HOWLING_WRAITHS -> (
    match (player.abilities.abyss_shriek, player.abilities.shaman_stone) with
    | false, false -> 13
    | true, false -> 25
    | false, true -> 20
    | true, true -> 35)

(* TODO use collision shape for dream nail *)
let check_dream_nail_collisions (state : state) (game : game) =
  match get_dream_nail game.player with
  | None -> ()
  | Some dream_nail_dest ->
    let resolve_enemy (enemy : enemy) =
      if Enemy.is_alive enemy then
        if enemy.json.dream_nail.vulnerable then (
          match Collision.between_rects dream_nail_dest enemy.entity.sprite.dest with
          | Some c ->
            (* TODO add dream nail sound *)
            if game.player.history.dream_nail.started > Enemy.took_damage_at enemy DREAM_NAIL then (
              let text = List.sample state.global.honda_quotes in
              game.interaction.floating_text <-
                Some
                  {
                    content = text;
                    visible =
                      Interaction.make_UNTIL Config.text.long_floating_duration state.frame.time;
                    scale = 1.;
                  };
              (* TODO make a new fn Ghost.add/deduct_soul that bounds between [0, soul max] *)
              let increase =
                if game.player.abilities.dream_wielder then
                  Config.action.soul_per_cast * 2
                else
                  Config.action.soul_per_cast
              in
              game.player.soul.current <-
                Int.bound 0 (game.player.soul.current + increase) game.player.soul.max;
              let recoil_speed =
                if game.player.ghost.entity.sprite.facing_right then
                  enemy.json.dream_nail.recoil_vx
                else
                  -.enemy.json.dream_nail.recoil_vx
              in
              enemy.entity.x_recoil <-
                Some
                  {
                    speed = recoil_speed;
                    time_left = { seconds = Config.ghost.nail_recoil_time };
                    reset_v = true;
                  };
              enemy.history <-
                Enemy_action.Map.update (TOOK_DAMAGE DREAM_NAIL)
                  (fun _ -> Some { at = state.frame.time })
                  enemy.history)
          | None -> ())
    in
    let resolve_trigger (trigger : trigger) =
      match Collision.between_rects dream_nail_dest trigger.dest with
      | None -> ()
      | Some c -> maybe_begin_interaction state game trigger
    in
    List.iter resolve_enemy game.room.enemies;
    List.iter resolve_trigger game.room.triggers.d_nail

let add_phantom_floor (game : game) (target : vector) =
  (* need to set v.y to 0 here to prevent maybe_unset_current_floor from removing
     the phantom floor *)
  game.player.ghost.entity.v.y <- 0.;
  game.player.ghost.entity.current_floor <-
    Some
      ( {
          pos = { x = target.x; y = target.y +. game.player.ghost.entity.dest.h };
          w = game.player.ghost.entity.dest.w;
          h = Config.other.phantom_floor_h;
        },
        Zero.vector () )

let resolve_slash_collisions (state : state) (game : game) =
  match get_current_slash game.player with
  | None -> ()
  | Some slash ->
    let resolve_enemy (enemy : enemy) =
      if enemy.json.can_take_damage && enemy.status.check_damage_collisions then (
        match Collision.between_slash_and_entity slash enemy.entity with
        | None -> ()
        | Some collision ->
          Audio.play_sound state "punch";
          if
            Enemy.maybe_take_damage state enemy game.player.history.nail.started NAIL
              (get_damage game.player NAIL) collision
          then (
            (match collision.collided_from with
            | DOWN -> pogo game.player
            | LEFT
            | RIGHT ->
              Entity.recoil_backwards game.player.ghost.entity
                {
                  speed = Config.ghost.recoil_speed;
                  time_left = { seconds = Config.ghost.nail_recoil_time };
                  reset_v = true;
                }
            | UP ->
              if Entity.ascending game.player.ghost.entity then
                game.player.ghost.entity.v.y <- Config.ghost.upslash_vy);
            game.player.soul.current <-
              Int.bound 0
                (game.player.soul.current
                + Config.action.soul_gained_per_nail
                + game.player.abilities.soul_catcher_bonus)
                game.player.soul.max);
          if slash.direction = DOWN && game.player.ghost.entity.y_recoil = None then (
            (* TODO this isn't working (can't pogo LB projectiles) *)
            let check_projectile_pogo (projectile : projectile) =
              if projectile.pogoable then (
                match Collision.between_slash_and_sprite slash projectile.entity.sprite with
                | None -> ()
                | Some _ -> pogo game.player)
            in
            List.iter check_projectile_pogo enemy.projectiles))
    in

    let destroy_tile_group layer tile_group =
      layer.destroyed_tiles <- tile_group.tile_idxs @ layer.destroyed_tiles;
      if layer.config.permanently_removable then
        game.room.progress.removed_tile_idxs <-
          tile_group.tile_idxs @ game.room.progress.removed_tile_idxs;
      Entity.unset_removed_floor game.player.ghost.entity tile_group.dest
    in

    let maybe_pogo_platform_spikes id rect =
      match Collision.between_slash_and_rect slash rect with
      | None -> ()
      | Some coll -> (
        Audio.play_sound state "nail-hit-metal";
        match coll.collided_from with
        | DOWN -> (
          (* always pogo, but only un-rotate the platform if it is upright *)
          pogo game.player;
          let platform = List.find (fun (p : platform) -> p.id = id) game.room.platforms in
          match platform.kind with
          | Some (ROTATABLE (UPSIDE_DOWN _)) ->
            Platform.reset_rotation platform game state.global.textures
          | _platform_kind_opt -> ())
        | _direction -> ())
    in

    let maybe_pogo sound_name rect =
      match Collision.between_slash_and_rect slash rect with
      | None -> ()
      | Some coll -> (
        Audio.play_sound state sound_name;
        match coll.collided_from with
        | DOWN -> pogo game.player
        | _direction -> ())
    in

    (* destroyable and pogoable layers *)
    let resolve_colliding_layers (layer : layer) =
      if layer.config.destroyable then (
        let new_tile_groups : tile_group list ref = ref [] in
        let spawn_fragment
            (tile_group : tile_group)
            (collision : collision)
            ~(broken : bool)
            idx
            (entity : entity) =
          let new_fragment = Entity.clone entity in
          let new_pos, new_v =
            let pos = align (CENTER, TOP_INSIDE) tile_group.dest entity.dest.w entity.dest.h in
            let pos' = { pos with y = pos.y +. Random.float (tile_group.dest.h /. 2.) } in
            if broken then
              (pos', { x = Config.random_fragment_vx (); y = Config.random_fragment_vy () })
            else (
              let door_is_vertical = tile_group.dest.h > tile_group.dest.w in
              let randomize pos half = pos +. Random.float_between (-.half) half in
              let adjust_y () =
                let y_alignment, vy_direction =
                  if collision.center.y < rect_center_y game.player.ghost.entity.dest then
                    (BOTTOM_OUTSIDE, DOWN)
                  else
                    (TOP_OUTSIDE, UP)
                in
                let pos = align (CENTER, y_alignment) tile_group.dest entity.dest.w entity.dest.h in
                ( { pos with x = randomize pos.x (tile_group.dest.w /. 2.) },
                  {
                    x = Config.random_fragment_vx ();
                    y = Config.random_fragment_vy ~direction:(Some vy_direction) ();
                  } )
              in
              let adjust_x () =
                let x_alignment, vx_direction =
                  if collision.center.x < rect_center_x game.player.ghost.entity.dest then
                    (RIGHT_OUTSIDE, RIGHT)
                  else
                    (LEFT_OUTSIDE, LEFT)
                in
                let pos = align (x_alignment, CENTER) tile_group.dest entity.dest.w entity.dest.h in
                ( { pos with y = randomize pos.y (tile_group.dest.h /. 2.) },
                  {
                    x = Config.random_fragment_vx ~direction:(Some vx_direction) ();
                    y = Config.random_fragment_vy ();
                  } )
              in
              if door_is_vertical then
                adjust_x ()
              else
                adjust_y ())
          in
          new_fragment.dest.pos <- new_pos;
          new_fragment.v <- new_v;
          new_fragment.frozen <- false;
          new_fragment.sprite.facing_right <- Random.bool ();
          new_fragment
        in
        let destroy_object (tile_group : tile_group) (collision : collision) =
          (* TODO organize these sound effects (probably just combine these all into one) *)
          if not layer.config.silent then (
            Audio.play_sound state "break";
            Audio.play_sound state "alarmswitch";
            Audio.play_sound state "punch");
          layer.spawned_fragments <-
            List.mapi (spawn_fragment tile_group collision ~broken:true) tile_group.fragments
            @ layer.spawned_fragments;
          let idx = List.nth tile_group.tile_idxs 0 in
          (match List.assoc_opt idx game.room.idx_configs with
          | Some (PURPLE_PEN (name, followup_trigger')) ->
            game.progress.purple_pens_found <-
              (state.frame.idx, name) :: game.progress.purple_pens_found;
            let followup =
              match game.mode with
              | STEEL_SOLE -> None
              | CLASSIC
              | DEMO ->
                followup_trigger'
            in
            maybe_begin_interactions state game ~followup
              (make_stub_trigger PURPLE_PEN "purple-pen" name)
          | _ -> ());
          destroy_tile_group layer tile_group;
          (match game.mode with
          | DEMO
          | CLASSIC ->
            ()
          | STEEL_SOLE ->
            if layer.name = "doors" then
              add_phantom_floor game game.player.ghost.entity.dest.pos);
          if collision.collided_from = DOWN && layer.config.pogoable then
            pogo game.player;
          match tile_group.stub_sprite with
          | None -> ()
          | Some sprite ->
            layer.spawned_stub_sprites <-
              (sprite, tile_group.transformation_bits) :: layer.spawned_stub_sprites
        in
        let resolve_tile_group (tile_group : tile_group) =
          match Collision.between_slash_and_rect slash tile_group.dest with
          | None -> new_tile_groups := tile_group :: !new_tile_groups
          | Some coll -> (
            match tile_group.door_health with
            | None -> destroy_object tile_group coll
            | Some door_health ->
              if door_health.last_hit_at < game.player.history.nail.started.at then (
                door_health.last_hit_at <- state.frame.time;
                if door_health.hits > 1 then (
                  let make_random_fragment _n = List.sample tile_group.fragments in
                  let random_fragments = List.init (Random.int_between 2 4) make_random_fragment in
                  layer.spawned_fragments <-
                    List.mapi (spawn_fragment tile_group coll ~broken:false) random_fragments
                    @ layer.spawned_fragments;
                  door_health.hits <- door_health.hits - 1;
                  Audio.play_sound state "alarmswitch";
                  Audio.play_sound state "punch";
                  new_tile_groups := tile_group :: !new_tile_groups)
                else
                  destroy_object tile_group coll)
              else
                new_tile_groups := tile_group :: !new_tile_groups;
              ())
        in
        List.iter resolve_tile_group layer.tile_groups;
        layer.tile_groups <- !new_tile_groups)
    in

    let resolve_lever (lever : lever) =
      let layer =
        match List.find_opt (fun (l : layer) -> l.name = "lever-doors") game.room.layers with
        | None ->
          failwithf "room %s has levers '%s', but no lever-doors layer" (Show.room_id game.room.id)
            lever.trigger.full_name
        | Some l -> l
      in
      let new_tile_groups : tile_group list ref = ref [] in
      match Collision.between_slash_and_sprite slash lever.sprite with
      | None -> ()
      | Some collision ->
        if lever.sprite.texture = state.global.textures.door_lever then (
          Audio.play_sound state "nail-hit-metal";
          Audio.play_sound state "punch");
        lever.sprite.texture <- state.global.textures.door_lever_struck;
        let maybe_remove_door (tile_group : tile_group) =
          let check_idx (idx : int) =
            if List.mem idx tile_group.tile_idxs then (
              layer.tile_groups <-
                List.filter (fun (t : tile_group) -> t.dest <> tile_group.dest) layer.tile_groups;
              destroy_tile_group layer tile_group)
          in
          List.Non_empty.iter check_idx lever.door_tile_idxs
        in
        List.iter maybe_remove_door layer.tile_groups
    in

    List.iter resolve_lever game.room.triggers.levers;
    List.iter resolve_colliding_layers game.room.layers;
    List.iter (maybe_pogo "nail-hit-metal") game.room.spikes;
    String.Map.iter maybe_pogo_platform_spikes game.room.platform_spikes;
    List.iter resolve_enemy game.room.enemies

let reset_current_status ?(taking_hazard_damage = None) () =
  {
    wall = None;
    water = None;
    is_diving = false;
    is_c_dashing = false;
    is_charging_c_dash = false;
    is_taking_hazard_damage =
      (match taking_hazard_damage with
      | None -> false
      | Some v -> v);
    can_dash = true;
    can_flap = true;
  }

let set_ghost_textures ghost ~head_texture ~body_texture =
  ghost.head <- head_texture;
  Entity.update_sprite_texture ~scale:Config.scale.ghost ghost.entity body_texture

(* - this function handles state updates that should happen every frame of a pose
   -- but some actions don't use start/continue_action, so they set things on every frame
   - anything that only happens once should happen in start_pose
   - anything that happens every frame except the first should happen in continue_pose
*)
let set_pose
    (game : game)
    (new_pose : ghost_pose)
    (bodies : ghost_body_textures)
    (frame_time : float) =
  let player = game.player in
  let update_vx multiplier =
    let mult = if player.ghost.entity.sprite.facing_right then multiplier else -1. *. multiplier in
    player.ghost.entity.v.x <- mult *. Config.ghost.vx
  in
  (* TODO bad name *)
  let reset_standing_abilities () =
    if Entity.on_ground player.ghost.entity then (
      player.current.can_dash <- true;
      player.current.can_flap <- true)
  in

  let handle_cast (spell_kind : spell_kind) : texture * texture =
    match spell_kind with
    | VENGEFUL_SPIRIT ->
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.walk, bodies.cast)
    | DESOLATE_DIVE ->
      update_vx 0.;
      player.ghost.entity.v.y <- Config.ghost.dive_vy;
      (player.ghost.head_textures.look_down, bodies.dive)
    | HOWLING_WRAITHS ->
      player.ghost.entity.v.x <- 0.;
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.walk, bodies.cast)
  in

  let handle_action_kind action_kind : texture * texture =
    match action_kind with
    | ATTACK direction ->
      (* handle_attacking is called after handle_walking, so this allows the ghost to walk backwards while attacking *)
      Entity.set_facing_right player.ghost.entity direction;
      let head =
        match direction with
        | UP -> player.ghost.head_textures.look_up
        | DOWN -> player.ghost.head_textures.look_down
        | _ -> player.ghost.head_textures.idle
      in
      (head, bodies.nail)
    | DREAM_NAIL ->
      update_vx 0.;
      (player.ghost.head_textures.idle, bodies.nail)
    | HARDFALL ->
      update_vx 0.;
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.look_down, bodies.focus)
    | C_DASH_WALL_COOLDOWN ->
      update_vx 0.;
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.wall_slide, bodies.wall_slide)
    | C_DASH_COOLDOWN ->
      update_vx (3. -. (4. *. (frame_time -. player.history.c_dash_cooldown.started.at)));
      player.ghost.entity.v.y <- 0.;
      (* TODO new image/texture for this *)
      (player.ghost.head_textures.walk, bodies.cast)
    | C_DASH ->
      player.ghost.entity.v.y <- 0.;
      update_vx 3.;
      (player.ghost.head_textures.idle, bodies.dash)
    | C_DASH_CHARGE ->
      update_vx 0.;
      player.ghost.entity.v.y <- 0.;
      (player.ghost.head_textures.look_down, bodies.focus)
    | SHADE_DASH
    | DASH ->
      player.ghost.entity.v.y <- 0.;
      update_vx 2.;
      (player.ghost.head_textures.idle, bodies.dash)
    | CAST spell_kind -> handle_cast spell_kind
    | DIVE_COOLDOWN ->
      update_vx 0.;
      (player.ghost.head_textures.look_up, bodies.dive)
    | FOCUS ->
      update_vx 0.;
      (player.ghost.head_textures.look_down, bodies.focus)
    | TAKE_DAMAGE_AND_RESPAWN -> (player.ghost.head_textures.take_damage, bodies.take_damage)
    | DIE -> (player.ghost.head_textures.take_damage, bodies.take_damage)
    | TAKE_DAMAGE (damage, direction) ->
      (* from recoiling upwards *)
      player.ghost.entity.current_floor <- None;
      (player.ghost.head_textures.take_damage, bodies.take_damage)
    | JUMP ->
      player.ghost.entity.v.y <- Config.ghost.jump_vy;
      player.ghost.entity.current_floor <- None;
      (player.ghost.head_textures.idle, bodies.jump)
    | WALL_KICK ->
      player.ghost.entity.v.y <- Config.ghost.wall_jump_vy;
      update_vx 1.;
      (player.ghost.head_textures.idle, bodies.jump)
    | FLAP -> (player.ghost.head_textures.idle, bodies.flap)
  in

  let (head_texture, body_texture) : texture * texture =
    match new_pose with
    | PERFORMING action_kind -> handle_action_kind action_kind
    | AIRBORNE new_vy ->
      player.ghost.entity.v.y <- new_vy;
      player.ghost.entity.current_floor <- None;
      if player.ghost.entity.v.y > Config.physics.jump_fall_threshold then
        (player.ghost.head_textures.idle, bodies.fall)
      else
        (player.ghost.head_textures.idle, bodies.jump)
    | CRAWLING ->
      update_vx 0.;
      (player.ghost.head_textures.idle, bodies.crawl)
    | IDLE ->
      reset_standing_abilities ();
      update_vx 0.;
      (player.ghost.head_textures.idle, bodies.idle)
    | READING ->
      update_vx 0.;
      (player.ghost.head_textures.read, bodies.read)
    | WALKING direction ->
      reset_standing_abilities ();
      (* don't update facing_right when wallsliding because even though it gets re-set
         correctly before the frame ends, slash direction gets messed up when it is wrong
         in the middle of the frame *)
      Entity.walk_ghost
        ~update_facing:(Option.is_none player.current.wall)
        player.ghost.entity direction;
      (player.ghost.head_textures.walk, bodies.walk)
    | WALL_SLIDING wall ->
      let wall_to_the_left = wall.pos.x < player.ghost.entity.sprite.dest.pos.x in
      player.ghost.entity.sprite.facing_right <- wall_to_the_left;
      player.current.wall <- Some wall;
      player.ghost.entity.sprite.dest.pos.x <-
        (if wall_to_the_left then
           wall.pos.x +. wall.w
         else
           wall.pos.x -. (Config.ghost.width *. Config.scale.ghost));
      update_vx 0.;
      player.current.can_dash <- true;
      player.current.can_flap <- true;
      (player.ghost.head_textures.wall_slide, bodies.wall_slide)
    | SWIMMING rect ->
      player.current.can_dash <- true;
      player.current.can_flap <- true;
      player.current.water <- Some rect;
      player.ghost.entity.v.y <- 0.;
      (* TODO add swimming texture *)
      (player.ghost.head_textures.idle, bodies.fall)
  in
  set_ghost_textures player.ghost ~head_texture ~body_texture

let past_cooldown ?(debug = false) pose_frames frame_time : bool =
  pose_frames.blocked_until.at < frame_time

let spawn_vengeful_spirit ?(start = None) ?(direction : direction option = None) state game =
  let texture =
    if game.player.abilities.shade_soul then
      game.player.shared_textures.shade_soul
    else
      game.player.shared_textures.vengeful_spirit
  in
  let w, h =
    let scale =
      if game.player.abilities.shaman_stone then
        Config.scale.ghost *. 1.3333
      else
        Config.scale.ghost
    in
    get_scaled_texture_size scale texture
  in
  let pos =
    match start with
    | Some p -> p
    | None ->
      align
        (IN_FRONT game.player.ghost.entity.sprite.facing_right, CENTER)
        game.player.ghost.entity.dest w h
  in
  let dest = { pos; w; h } in
  let facing_right =
    match direction with
    | None -> game.player.ghost.entity.sprite.facing_right
    | Some d -> (
      match d with
      | LEFT -> false
      | RIGHT -> true
      | _ -> failwithf "spawn_vengeful_spirit invalid direction: %s" (Show.direction d))
  in
  let vengeful_spirit : sprite =
    {
      ident = fmt "vengeful_spirit %d" (List.length game.player.spawned_vengeful_spirits);
      texture;
      dest;
      facing_right;
      collision = Some DEST (* TODO add VS collision shape *);
    }
  in
  let vx =
    let vx =
      if game.player.abilities.shade_soul then
        Config.action.shade_soul_vx
      else
        Config.action.vengeful_spirit_vx
    in
    if facing_right then vx else -1. *. vx
  in
  let projectile : projectile =
    {
      entity =
        Entity.create_for_sprite ~v:{ x = vx; y = 0. } ~gravity_multiplier:0. vengeful_spirit dest;
      despawn = TIME_LEFT { seconds = Config.action.vengeful_spirit_duration };
      spawned = { at = state.frame.time };
      update_v = None;
      pogoable = true;
      damage = get_damage game.player VENGEFUL_SPIRIT;
      (* TODO this should be based on shade soul *)
      collide_with_floors = false;
      draw_on_top = false;
      orbiting = None;
    }
  in
  game.player.spawned_vengeful_spirits <- projectile :: game.player.spawned_vengeful_spirits

let respawn_ghost game = game.player.ghost.entity.dest.pos <- clone_vector game.room.respawn.target

let cancel_action (state : state) (game : game) (action_kind : ghost_action_kind) =
  let action =
    match action_kind with
    | FLAP ->
      game.player.current.can_flap <- true;
      game.player.history.flap
    | WALL_KICK
    | JUMP
    | TAKE_DAMAGE_AND_RESPAWN
    | TAKE_DAMAGE _
    | CAST _
    | DIVE_COOLDOWN
    | SHADE_DASH
    | DASH
    | C_DASH_CHARGE
    | C_DASH
    | C_DASH_COOLDOWN
    | C_DASH_WALL_COOLDOWN
    | HARDFALL
    | DREAM_NAIL
    | ATTACK _
    | DIE
    | FOCUS ->
      failwithf "cannot cancel action: %s" (Show.ghost_action_kind action_kind)
  in
  action.doing_until.at <- state.frame.time

let start_action ?(debug = false) (state : state) (game : game) (action_kind : ghost_action_kind) =
  let cooldown_scale = ref 1.0 in
  let is_dream_nail = ref false in
  let action : ghost_action =
    match action_kind with
    | ATTACK direction ->
      Audio.play_sound state "nail-swing";
      let relative_pos : relative_position =
        match direction with
        | UP -> (CENTER, BOTTOM_INSIDE)
        | DOWN -> (CENTER, TOP_INSIDE)
        | LEFT
        | RIGHT ->
          (IN_FRONT game.player.ghost.entity.sprite.facing_right, CENTER)
      in
      let slash =
        make_slash game.player direction relative_pos game.player.ghost.entity.sprite
          state.frame.time
      in
      game.player.children <-
        Ghost_child_kind.Map.update (NAIL slash)
          (fun _ -> Some { relative_pos; sprite = slash.sprite; in_front = true })
          game.player.children;
      cooldown_scale := game.player.current_weapon.cooldown_scale;
      game.player.history.nail
    | DREAM_NAIL ->
      is_dream_nail := true;
      game.player.history.dream_nail
    | HARDFALL ->
      state.camera.shake <- 1.;
      game.player.history.hardfall
    | C_DASH_WALL_COOLDOWN ->
      (* TODO stop playing c-dash sound *)
      game.player.ghost.entity.sprite.facing_right <-
        not game.player.ghost.entity.sprite.facing_right;
      game.player.current.is_c_dashing <- false;
      remove_child game.player C_DASH_WHOOSH;
      game.player.history.c_dash_wall_cooldown
    | C_DASH_COOLDOWN ->
      (* TODO stop playing c-dash sound *)
      game.player.current.is_c_dashing <- false;
      game.player.children <- Ghost_child_kind.Map.remove C_DASH_WHOOSH game.player.children;
      game.player.history.c_dash_cooldown
    | C_DASH ->
      (* TODO this probably won't work as a sound, needs to be a music stream that repeats *)
      (* Audio.play_sound state "spray"; *)
      (* TODO maybe only should track this stuff when game_mode is STEEL_SOLE *)
      if game.room.area.id <> COMPUTER_WING then
        (* not counting these in COMPUTER_WING since there are some rooms that require it  *)
        game.progress.steel_sole.c_dashes <- 1 + game.progress.steel_sole.c_dashes;
      game.player.current.is_c_dashing <- true;
      game.player.current.is_charging_c_dash <- false;
      game.player.ghost.entity.current_floor <- None;
      let alignment =
        if game.player.ghost.entity.sprite.facing_right then
          (RIGHT_INSIDE, CENTER)
        else
          (LEFT_INSIDE, CENTER)
      in
      spawn_child game.player C_DASH_WHOOSH ~scale:Config.ghost.c_dash_whoosh_scale alignment
        game.player.shared_textures.c_dash_whoosh;
      game.player.history.c_dash
    | C_DASH_CHARGE ->
      game.player.current.is_charging_c_dash <- true;
      make_c_dash_child game.player;
      game.player.history.charge_c_dash
    | SHADE_DASH ->
      Audio.play_sound state "dash";
      game.player.history.shade_dash
    | DASH ->
      (* TODO the dash sound should have the footsteps at the end when the ghost lands *)
      Audio.play_sound state "dash";
      game.player.ghost.entity.y_recoil <- None;
      (* set this based on current_floor to allow ghost to dash again after dashing off a ledge *)
      game.player.current.can_dash <- Option.is_some game.player.ghost.entity.current_floor;
      game.player.history.dash
    | CAST spell_kind -> (
      game.player.current.wall <- None;
      let soul_per_cast =
        if game.player.abilities.spell_twister then
          Config.action.spell_twister_soul_per_cast
        else
          Config.action.soul_per_cast
      in
      game.player.soul.current <- game.player.soul.current - soul_per_cast;
      match spell_kind with
      | VENGEFUL_SPIRIT ->
        Entity.recoil_backwards game.player.ghost.entity
          {
            speed = Config.action.vengeful_spirit_recoil;
            time_left = { seconds = game.player.history.cast_vs.config.duration.seconds };
            reset_v = true;
          };
        spawn_vengeful_spirit state game;
        game.player.history.cast_vs
      | DESOLATE_DIVE ->
        (* TODO probably should set is_diving in this fn (like how c-dash does it) *)
        let w, h =
          (* TODO these are temporarily scaled so the dive.png image can be reused *)
          (game.player.ghost.entity.dest.w *. 5., game.player.ghost.entity.dest.h *. 5.)
        in
        let child =
          make_ghost_child game.player DIVE (CENTER, BOTTOM_INSIDE)
            game.player.shared_textures.desolate_dive w h
        in
        add_child game.player DIVE child;
        game.player.history.cast_dive
      | HOWLING_WRAITHS ->
        spawn_child game.player WRAITHS (CENTER, BOTTOM_INSIDE) ~scale:Config.ghost.wraiths_scale
          game.player.shared_textures.howling_wraiths;
        game.player.history.cast_wraiths)
    | DIE ->
      Audio.play_sound state "break";
      Audio.play_sound state "spray";
      game.player.history.die
    | TAKE_DAMAGE_AND_RESPAWN ->
      (match game.mode with
      | DEMO
      | CLASSIC ->
        game.player.health.current <- game.player.health.current - 1
      | STEEL_SOLE ->
        game.player.soul.current <- game.player.soul.max;
        let jug_layers : layer list =
          List.filter
            (fun (layer : layer) ->
              String.length layer.name > 5 && Str.last_chars layer.name 5 = "-jugs")
            game.room.layers
        in
        List.iter (fun layer -> layer.destroyed_tiles <- []) jug_layers;
        Room.reset_tile_groups game.room);
      Entity.freeze game.player.ghost.entity;
      game.player.history.take_damage_and_respawn
    | TAKE_DAMAGE (damage, direction) ->
      (* TODO separate sound for this *)
      Audio.play_sound state "punch";
      state.camera.shake <- 0.5;
      let x_recoil_speed =
        match direction with
        | LEFT -> -.Config.ghost.recoil_speed
        | RIGHT -> Config.ghost.recoil_speed
        | _ ->
          if game.player.ghost.entity.sprite.facing_right then
            Config.ghost.recoil_speed
          else
            -.Config.ghost.recoil_speed
      in
      game.player.ghost.entity.x_recoil <-
        Some
          {
            speed = x_recoil_speed;
            time_left = { seconds = Config.ghost.damage_recoil_time };
            reset_v = true;
          };
      game.player.ghost.entity.y_recoil <-
        Some
          {
            speed = -.Config.ghost.recoil_speed;
            time_left = { seconds = Config.ghost.damage_recoil_time };
            reset_v = true;
          };
      (* preserve the original value of is_taking_hazard_damage to prevent soft-locks when
         recoiling away from hazard while hazard respawning *)
      game.player.current <-
        reset_current_status
          ~taking_hazard_damage:(Some game.player.current.is_taking_hazard_damage) ();
      game.player.children <- Ghost_child_kind.Map.empty;
      (match game.mode with
      | STEEL_SOLE -> ()
      | CLASSIC
      | DEMO ->
        game.player.health.current <- game.player.health.current - damage);
      game.player.history.take_damage
    | DIVE_COOLDOWN -> game.player.history.dive_cooldown
    | FOCUS ->
      game.player.soul.at_focus_start <- game.player.soul.current;
      game.player.soul.health_at_focus_start <- game.player.health.current;
      game.player.soul.last_decremented <- { at = state.frame.time };
      spawn_child game.player FOCUS ~in_front:true ~scale:Config.scale.focus (CENTER, CENTER)
        game.player.shared_textures.focus_sparkles;
      game.player.history.focus
    | JUMP -> game.player.history.jump
    | FLAP ->
      game.player.current.can_flap <- false;
      game.player.history.flap
    | WALL_KICK -> game.player.history.wall_kick
  in
  let duration =
    if !is_dream_nail then
      if game.player.abilities.dream_wielder then
        action.config.duration.seconds *. Config.action.dream_wielder_speed
      else
        action.config.duration.seconds
    else
      action.config.duration.seconds
  in
  action.started <- { at = state.frame.time };
  action.doing_until.at <- state.frame.time +. duration;
  action.blocked_until.at <- state.frame.time +. (action.config.cooldown.seconds *. !cooldown_scale);
  set_pose game (PERFORMING action_kind) state.global.textures.ghost_bodies state.frame.time

let continue_action (state : state) (game : game) (action_kind : ghost_action_kind) =
  (match action_kind with
  | FOCUS ->
    let focus_duration =
      if game.player.abilities.quick_focus then
        game.player.history.focus.config.duration.seconds *. Config.action.quick_focus_speed
      else
        game.player.history.focus.config.duration.seconds
    in
    let decr_dt = focus_duration /. (Config.action.soul_per_cast |> Int.to_float) in
    if game.player.soul.at_focus_start - game.player.soul.current >= Config.action.soul_per_cast
    then (
      let increase = if game.player.abilities.deep_focus then 2 else 1 in
      game.player.health.current <-
        Int.bound 0 (game.player.health.current + increase) game.player.health.max;
      if
        game.player.soul.current >= Config.action.soul_per_cast
        && game.player.health.current < game.player.health.max
      then
        start_action state game FOCUS
      else
        remove_child game.player FOCUS)
    else if state.frame.time -. game.player.soul.last_decremented.at > decr_dt then (
      game.player.soul.current <- game.player.soul.current - 1;
      game.player.soul.last_decremented <- { at = state.frame.time })
  | FLAP ->
    let flap_duration =
      (* this uses jump input buffer to handle flapping vs. buffering a jump off the ground *)
      game.player.history.jump.config.input_buffer.seconds -. 0.05
    in
    if state.frame.time -. game.player.history.flap.started.at > flap_duration then
      game.player.ghost.entity.v.y <- Config.ghost.flap_vy
  | DREAM_NAIL ->
    let charge_duration =
      if game.player.abilities.dream_wielder then
        game.player.history.dream_nail.config.duration.seconds *. Config.action.dream_wielder_speed
      else
        game.player.history.dream_nail.config.duration.seconds
    in
    let swing_duration =
      (* need to subtract this to prevent the ghost from turning around immediately after
         starting the dream nail *)
      0.15
    in
    if
      state.frame.time -. game.player.history.dream_nail.started.at
      > charge_duration -. swing_duration
    then (
      let alignment = (IN_FRONT game.player.ghost.entity.sprite.facing_right, CENTER) in
      spawn_child game.player DREAM_NAIL alignment game.player.shared_textures.slash
        ~scale:Config.scale.dream_nail;
      check_dream_nail_collisions state game)
  | DIE -> (
    match state.screen_fade with
    | None ->
      state.screen_fade <-
        Some { target_alpha = 255; timer = Some (make_timer 3.); show_ghost = false }
    | Some fade -> (
      match fade.timer with
      | None -> failwith "unreachable"
      | Some timer ->
        if timer.left.seconds < -1.5 then (
          Audio.stop_sound state "spray";
          respawn_ghost game;
          game.player.health.current <- game.player.health.max;
          state.game_context <- DIED game;
          state.screen_fade <- None)))
  | C_DASH
  | C_DASH_CHARGE
  | SHADE_DASH
  | TAKE_DAMAGE_AND_RESPAWN
  | WALL_KICK
  | JUMP
  | TAKE_DAMAGE _
  | CAST _
  | DIVE_COOLDOWN
  | HARDFALL
  | DASH
  | C_DASH_COOLDOWN
  | C_DASH_WALL_COOLDOWN
  | ATTACK _ ->
    ());
  set_pose game (PERFORMING action_kind) state.global.textures.ghost_bodies state.frame.time

let is_doing (player : player) (action_kind : ghost_action_kind) (frame_time : float) : bool =
  let check_action action =
    action.started.at <= frame_time && frame_time <= action.doing_until.at
  in
  match action_kind with
  | ATTACK _ -> Option.is_some (get_current_slash player)
  | FOCUS -> Option.is_some (get_focus_sparkles player)
  | CAST spell_kind -> (
    match spell_kind with
    | VENGEFUL_SPIRIT -> check_action player.history.cast_vs
    | HOWLING_WRAITHS -> check_action player.history.cast_wraiths
    | DESOLATE_DIVE -> player.current.is_diving)
  | C_DASH_CHARGE -> player.current.is_charging_c_dash
  | C_DASH -> player.current.is_c_dashing
  | C_DASH_WALL_COOLDOWN -> check_action player.history.c_dash_wall_cooldown
  | C_DASH_COOLDOWN -> check_action player.history.c_dash_cooldown
  | SHADE_DASH -> check_action player.history.shade_dash
  | DASH -> check_action player.history.dash
  | DIVE_COOLDOWN -> check_action player.history.dive_cooldown
  | HARDFALL -> check_action player.history.hardfall
  | TAKE_DAMAGE_AND_RESPAWN -> check_action player.history.take_damage_and_respawn
  | FLAP -> check_action player.history.flap
  | DREAM_NAIL -> check_action player.history.dream_nail
  | DIE -> check_action player.history.die
  | WALL_KICK -> check_action player.history.wall_kick
  | JUMP
  | TAKE_DAMAGE _ ->
    failwithf "is_doing - invalid action %s" (Show.ghost_action_kind action_kind)

let not_doing_any (player : player) (frame_time : float) (actions : ghost_action_kind list) : bool =
  not (List.exists (fun action -> is_doing player action frame_time) actions)

let is_casting (state : state) (game : game) =
  is_doing game.player (CAST VENGEFUL_SPIRIT) state.frame.time
  || game.player.current.is_diving
  || is_doing game.player (CAST HOWLING_WRAITHS) state.frame.time

let as_party_ghost (player : player) : party_ghost = { ghost = player.ghost; in_party = true }

let find_party_ghost (id : ghost_id) party : party_ghost option =
  List.find_opt (fun (party_ghost : party_ghost) -> party_ghost.ghost.id = id) party

let swap_current_ghost ?(in_cutscene = false) (state : state) (game : game) target_ghost_id =
  match find_party_ghost target_ghost_id game.party with
  | None -> failwithf "bad ghost_id '%s' in swap_current_ghost" (Show.ghost_id target_ghost_id)
  | Some target_ghost ->
    let old_ghost = as_party_ghost game.player in
    let current_pos = old_ghost.ghost.entity.dest.pos in
    if in_cutscene then (
      let new_party =
        let other_ghosts =
          List.filter (fun (pg : party_ghost) -> pg.ghost.id <> old_ghost.ghost.id) game.party
        in
        { old_ghost with in_party = false } :: other_ghosts
      in
      game.party <- new_party)
    else (
      Entity.unhide_at target_ghost.ghost.entity (clone_vector current_pos);
      (* TODO maybe replace with clone_sprite *)
      target_ghost.ghost.entity.sprite.facing_right <- old_ghost.ghost.entity.sprite.facing_right;
      target_ghost.ghost.entity.v <- clone_vector old_ghost.ghost.entity.v);
    game.player.ghost <- { target_ghost.ghost with entity = clone_entity target_ghost.ghost.entity };
    (* TODO if the target_ghost is already on screen, this should swap positions instead of
       hiding old_ghost *)
    Entity.hide target_ghost.ghost.entity

let change_ability ?(debug = false) ?(only_enable = false) player ability_name =
  let new_val v =
    if debug then
      print "toggling %s -> %b" ability_name (not v);
    if only_enable then true else not v
  in
  match ability_name with
  | "vengeful_spirit" ->
    player.abilities.vengeful_spirit <- new_val player.abilities.vengeful_spirit
  | "desolate_dive" -> player.abilities.desolate_dive <- new_val player.abilities.desolate_dive
  | "howling_wraiths" ->
    player.abilities.howling_wraiths <- new_val player.abilities.howling_wraiths
  | "shade_soul" -> player.abilities.shade_soul <- new_val player.abilities.shade_soul
  | "descending_dark" ->
    player.abilities.descending_dark <- new_val player.abilities.descending_dark
  | "abyss_shriek" -> player.abilities.abyss_shriek <- new_val player.abilities.abyss_shriek
  | "mothwing_cloak" -> player.abilities.mothwing_cloak <- new_val player.abilities.mothwing_cloak
  | "shade_cloak" -> player.abilities.shade_cloak <- new_val player.abilities.shade_cloak
  | "mantis_claw" -> player.abilities.mantis_claw <- new_val player.abilities.mantis_claw
  | "crystal_heart" -> player.abilities.crystal_heart <- new_val player.abilities.crystal_heart
  | "monarch_wings" -> player.abilities.monarch_wings <- new_val player.abilities.monarch_wings
  | "ismas_tear" -> player.abilities.ismas_tear <- new_val player.abilities.ismas_tear
  | "dream_nail" -> player.abilities.dream_nail <- new_val player.abilities.dream_nail
  | "Quick Focus" -> player.abilities.quick_focus <- new_val player.abilities.quick_focus
  | "Soul Catcher" -> player.abilities.soul_catcher_bonus <- player.abilities.soul_catcher_bonus + 4
  | "Dream Wielder" -> player.abilities.dream_wielder <- new_val player.abilities.dream_wielder
  | "Deep Focus" -> player.abilities.deep_focus <- new_val player.abilities.deep_focus
  | "Shaman Stone" -> player.abilities.shaman_stone <- new_val player.abilities.shaman_stone
  | "Spell Twister" -> player.abilities.spell_twister <- new_val player.abilities.spell_twister
  | _ -> failwithf "change_ability bad ability name: %s" ability_name

let enable_ability ghost ability_name = change_ability ~only_enable:true ghost ability_name
let toggle_ability ghost ability_name = change_ability ~debug:true ghost ability_name

let acquire_weapon (state : state) (game : game) weapon_name =
  match String.Map.find_opt weapon_name state.global.weapons with
  | Some weapon_config ->
    let current_weapon_names = game.player.weapons |> String.Map.to_list |> List.map fst in
    (* if not (List.mem weapon_name current_weapon_names) then *)
    game.player.weapons <-
      String.Map.update weapon_name (fun _ -> Some weapon_config) game.player.weapons
  | None -> failwithf "acquire_weapon bad weapon name: %s" weapon_name

let equip_weapon (player : player) weapon_name =
  match String.Map.find_opt weapon_name player.weapons with
  | None ->
    failwithf "can't equip %s, not in player.weapons: %s" weapon_name
      (player.weapons |> String.Map.to_list |> List.map fst |> String.join)
  | Some weapon_config ->
    player.current_weapon <-
      (let config = weapon_config.tint in
       {
         name = weapon_name;
         tint = Raylib.Color.create config.r config.g config.b config.a;
         scale_x = weapon_config.scale_x;
         scale_y = weapon_config.scale_y;
         cooldown_scale = 2. -. weapon_config.swing_speed;
       })

let get_invincibility_kind (state : state) (game : game) : invincibility_kind option =
  let in_dive_cooldown () =
    not (past_cooldown game.player.history.dive_cooldown state.frame.time)
  in
  if game.player.current.is_taking_hazard_damage then
    Some TAKING_HAZARD_DAMAGE
  else if not (past_cooldown game.player.history.take_damage state.frame.time) then
    Some TOOK_DAMAGE
  else if game.player.current.is_diving || in_dive_cooldown () then
    Some DIVE_IFRAMES
  else if is_doing game.player SHADE_DASH state.frame.time then
    Some SHADE_CLOAK
  else
    None

let is_vulnerable (state : state) (game : game) : bool =
  Option.is_none (get_invincibility_kind state game)

let handle_debug_keys (game : game) (state : state) =
  if not Env.development then
    state
  else (
    let show_camera_location () =
      let camera =
        match state.camera.subject with
        | FIXED v -> v
        | GHOST -> game.player.ghost.entity.dest.pos
      in
      let v = Raylib.Camera2D.target state.camera.raylib in
      print "camera subject at: %s" (Show.vector camera);
      print "camera at: %f, %f" (Raylib.Vector2.x v) (Raylib.Vector2.y v)
    in
    let show_ghost_location () =
      print "ghost %s at %s"
        (Show.ghost_id game.player.ghost.id)
        (Show.vector game.player.ghost.entity.dest.pos)
    in
    let show_full_ghost_location () =
      let room_location = List.assoc game.room.id state.world in
      print "================\nghost global pos: %s"
        (Show.vector (Room.get_global_pos game.player.ghost.entity.dest.pos room_location));
      print "room %s" (Room.get_filename game.room);
      print "full room_id: %s" (Show.room_id game.room.id);
      print "room_location global x/y: %f, %f" room_location.global_x room_location.global_y
    in
    let dv =
      if state.debug.enabled then
        Config.ghost.small_debug_v
      else
        Config.ghost.debug_v
    in
    let show_ghost_positions () =
      let positions : string =
        let show_pos (pg : party_ghost) =
          fmt "%s: %s" (Show.ghost_id pg.ghost.id) (Show.vector pg.ghost.entity.dest.pos)
        in
        List.map show_pos game.party |> String.join_lines
      in
      print "party ghost positions:\n%s" positions;
      print "ghost pos: %s" (Show.vector game.player.ghost.entity.dest.pos)
    in
    let toggle_safe_ss () =
      state.debug.safe_ss <- not state.debug.safe_ss;
      print "set debug.safe_ss: %b" state.debug.safe_ss
    in
    if key_down DEBUG_UP then
      game.player.ghost.entity.dest.pos.y <- game.player.ghost.entity.dest.pos.y -. dv
    else if key_down DEBUG_DOWN then
      game.player.ghost.entity.dest.pos.y <- game.player.ghost.entity.dest.pos.y +. dv
    else if key_down DEBUG_RIGHT then
      game.player.ghost.entity.dest.pos.x <- game.player.ghost.entity.dest.pos.x +. dv
    else if key_down DEBUG_LEFT then
      game.player.ghost.entity.dest.pos.x <- game.player.ghost.entity.dest.pos.x -. dv
    else if holding_shift () then (
      if key_pressed DEBUG_1 then (
        (* game.ghost.soul.current <- game.ghost.soul.max *)
        (* swap_current_ghost_in_cutscene state game ANNIE *)
        (* show_camera_location () *)
        (* show_ghost_location (); *)
        toggle_safe_ss ();
        show_ghost_positions ();
        (* game.player.health.current <- 1; *)
        ())
      else if key_pressed DEBUG_2 then (
        (* toggle_ability game.ghost "mantis_claw" *)
        (* game.player.health.current <- game.player.health.current - 1; *)
        (* toggle_ability game.player "Dream Wielder"; *)
        game.player.soul.current <- game.player.soul.max;
        (* game.player.soul <-
         *   {
         *     current = game.player.soul.max + 33;
         *     max = game.player.soul.max + 33;
         *     at_focus_start = 0;
         *     health_at_focus_start = 0;
         *     last_decremented = { at = 0. };
         *   }; *)
        ())
      else if key_pressed DEBUG_3 then (
        (* print "player water is_some: %b" (Option.is_some game.player.current.water) *)
        toggle_ability game.player "shade_soul";
        toggle_ability game.player "descending_dark";
        toggle_ability game.player "abyss_shriek")
      else if key_pressed DEBUG_4 then (
        toggle_ability game.player "desolate_dive";
        (* toggle_ability game.player "ismas_tear" *)
        ()))
    else if key_pressed DEBUG_1 then
      state.debug.paused <- not state.debug.paused;
    state)

(* this is used for actions that block other actions from happening during the same frame *)
type handled_action = { this_frame : bool }

let in_water (player : player) : bool = Option.is_some player.current.water

let tick (game : game) (state : state) =
  let stop_wall_sliding = ref false in

  let pressed_or_buffered game_action =
    let (input, buffer) : frame_input * float =
      match game_action with
      | NAIL -> (state.frame_inputs.nail, game.player.history.nail.config.input_buffer.seconds)
      | JUMP -> (state.frame_inputs.jump, game.player.history.jump.config.input_buffer.seconds)
      | DASH -> (state.frame_inputs.dash, game.player.history.dash.config.input_buffer.seconds)
      | CAST -> (state.frame_inputs.cast, game.player.history.cast_vs.config.input_buffer.seconds)
      | C_DASH -> (state.frame_inputs.c_dash, game.player.history.c_dash.config.input_buffer.seconds)
      | _ -> failwithf "bad key in pressed_or_buffered': %s" (show_game_action game_action)
    in
    let input_buffered () =
      match input.down_since with
      | None -> false
      | Some down_since_time -> input.down && state.frame.time -. buffer < down_since_time.at
    in
    input.pressed || input_buffered ()
  in
  let set_pose' (pose : ghost_pose) =
    set_pose game pose state.global.textures.ghost_bodies state.frame.time
  in

  (* TODO try moving this to interactions.ml *)
  (* return value is (currently interacting, currently warping) *)
  let handle_interactions () : bool * bool =
    (* the ghost can only collide with one trigger of each type per frame *)
    let check_for_new_interactions () : bool * bool =
      let interactable_triggers = game.room.triggers.lore in
      (match find_trigger_collision game.player interactable_triggers with
      | None -> game.room.interaction_label <- None
      | Some trigger -> (
        let label =
          match trigger.label with
          | None -> ""
          | Some label' -> label'
        in
        let check_interact_key () =
          if state.frame_inputs.interact.pressed then
            maybe_begin_interaction state game trigger
        in
        match trigger.blocking_interaction with
        | None ->
          game.room.interaction_label <- Some (label, trigger.dest);
          check_interact_key ()
        | Some blocking_interaction_name ->
          let interaction_blocked =
            match String.split_at_first_opt '$' blocking_interaction_name with
            | Some "key", key_name -> List.mem key_name game.progress.keys_found
            | _ -> Interactions.cutscene_finished game.progress.by_room blocking_interaction_name
          in
          if interaction_blocked then (
            game.room.interaction_label <- Some (label, trigger.dest);
            check_interact_key ())));
      let triggers =
        game.room.triggers.cutscene @ game.room.triggers.reflect @ game.room.triggers.boss_fight
      in
      (match find_trigger_collision game.player triggers with
      | None -> ()
      | Some trigger -> maybe_begin_interaction state game trigger);
      let respawn_trigger_collision =
        find_respawn_trigger_collision game.player game.room.triggers.respawns
      in
      (match respawn_trigger_collision with
      | None -> ()
      | Some respawn_trigger -> (
        match game.mode with
        | DEMO
        | CLASSIC ->
          (* only update this on the first frame when colliding with respawn trigger,
             not every frame *)
          if not game.room.respawn.in_trigger_now then (
            let new_respawn_pos =
              let head, tail = respawn_trigger.targets in
              let nearest = ref head in
              let distance = ref (get_distance !nearest game.player.ghost.entity.dest.pos) in
              let find_nearest target =
                let new_distance = get_distance target game.player.ghost.entity.dest.pos in
                if new_distance < !distance then (
                  distance := new_distance;
                  nearest := target)
              in
              List.Non_empty.iter find_nearest respawn_trigger.targets;
              !nearest
            in
            game.room.respawn.target <- new_respawn_pos)
        | STEEL_SOLE ->
          ();
          game.room.respawn.in_trigger_now <- true));
      game.room.respawn.in_trigger_now <- Option.is_some respawn_trigger_collision;
      (List.length game.interaction.steps > 0, false)
    in
    let speed_through_interaction =
      (* holding down c-dash will skip through interactions quickly, but still perform each step
         once so side-effects like gaining abilities still happen
         TODO this starts a c-dash after the interaction, not sure if there's a better option
      *)
      state.frame_inputs.c_dash.down
    in
    match game.interaction.text with
    | Some _text ->
      (* press interact key to advance dialogue by one *)
      if state.frame_inputs.interact.pressed || speed_through_interaction then
        game.interaction.text <- None;
      (true, false)
    | None -> (
      match List.nth_opt game.interaction.steps 0 with
      | None -> check_for_new_interactions ()
      | Some next_step ->
        let new_wait = ref 0. in
        let still_walking =
          (* it is a little weird that this is only used for walking since it makes it the only synchronous action
             (ie jumping happens and then the next step executes the following frame)
          *)
          ref false
        in
        let still_waiting_for_floor = ref false in
        let zero_vy = ref false in

        let set_layer_hidden (layer_name : string) hidden =
          (match List.find_opt (fun (l : layer) -> l.name = layer_name) game.room.layers with
          | None -> failwithf "expected %s layer" layer_name
          | Some layer_to_hide -> layer_to_hide.hidden <- hidden);
          Room.reset_tile_groups game.room
        in

        let set_ghost_camera () =
          Entity.unfreeze game.player.ghost.entity;
          state.camera.subject <- GHOST
        in

        (* could divide these up into some smaller groups like TEXT or LAYER to get rid of more of the duplication, but
           probably not really worth it *)
        let handle_general_step (general_step : Interaction.general_step) =
          match general_step with
          | SHAKE_SCREEN amount -> state.camera.shake <- amount
          | DEBUG s -> tmp "%s" s
          | INITIALIZE_INTERACTIONS options -> (
            game.player.ghost.entity.v <- Zero.vector ();
            if options.remove_nail then (
              match get_current_slash game.player with
              | Some slash -> game.player.children <- Ghost_child_kind.Map.empty
              | None -> ());
            match options.autosave_pos with
            | None -> ()
            | Some autosave_pos ->
              state.save_pos <- Some autosave_pos)
          | CONCLUDE_INTERACTIONS trigger ->
            state.ignore_camera_triggers <- false;
            state.camera.motion <-
              SMOOTH (Config.window.camera_motion.x, Config.window.camera_motion.y);
            set_ghost_camera ();
            zero_vy := true;
            game.room.progress.finished_interactions <-
              Interactions.trigger_name trigger :: game.room.progress.finished_interactions;
            game.interaction.use_dashes_in_archives <- None
          | SET_SCREEN_FADE fade -> state.screen_fade <- Some fade
          | CLEAR_SCREEN_FADE -> state.screen_fade <- None
          | DOOR_WARP trigger_kind
          | WARP trigger_kind -> (
            match trigger_kind with
            | WARP warp -> (
              let target_room_location = Tiled.JsonRoom.locate_by_name state.world warp.room_name in
              let warp_to pos = Room.change_current_room state game target_room_location pos in
              match game.mode with
              | DEMO
              | CLASSIC ->
                warp_to warp.target
              | STEEL_SOLE ->
                let target_y =
                  if warp.room_name = "ventways_hub" || game.room.id = VENT_HUB then
                    warp.target.y
                  else
                    warp.target.y -. Config.other.ss_warp_y_offset
                in
                warp_to { warp.target with y = target_y };
                (* can't take floor damage in vents, so ghost doesn't need a phantom floor *)
                if warp.room_name <> "ventways_hub" then
                  add_phantom_floor game game.room.respawn.target)
            | _ ->
              failwithf "need WARP trigger kind for WARP steps, got '%s'"
                (Show.trigger_kind trigger_kind))
          | FLOATING_TEXT (text, duration) ->
            game.interaction.floating_text <-
              Some
                {
                  content = text;
                  visible = Interaction.make_UNTIL duration state.frame.time;
                  scale = 1.;
                }
          | TEXT paragraphs ->
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN paragraphs)
          | FOCUS_ABILITY_TEXT (top_paragraphs, outline_src, bottom_paragraphs) ->
            game.interaction.speaker_name <- None;
            game.interaction.text <-
              Some (FOCUS_ABILITY { top_paragraphs; outline_src; bottom_paragraphs })
          | ABILITY_TEXT (outline_src, bottom_paragraphs) ->
            game.interaction.speaker_name <- None;
            game.interaction.text <-
              Some (ABILITY { top_paragraphs = []; outline_src; bottom_paragraphs })
          | SET_FIXED_CAMERA (tile_x, tile_y) ->
            let tile = (tile_x, tile_y) |> Tiled.Tile.coords_to_pos in
            let x, y = (tile.x *. Config.scale.room, tile.y *. Config.scale.room) in
            state.camera.subject <- FIXED { x; y }
          | SET_CAMERA_MOTION motion -> state.camera.motion <- motion
          | SET_GHOST_CAMERA -> set_ghost_camera ()
          | SET_IGNORE_CAMERA_TRIGGERS b -> state.ignore_camera_triggers <- b
          | WAIT time -> new_wait := time -. state.frame.dt
          | HIDE_LAYER layer_name -> set_layer_hidden layer_name true
          | UNHIDE_LAYER layer_name -> set_layer_hidden layer_name false
          | SPAWN_VENGEFUL_SPIRIT (direction, end_tile_x, end_tile_y) ->
            let tile = (end_tile_x, end_tile_y) |> Tiled.Tile.coords_to_pos in
            let end_x, end_y = (tile.x *. Config.scale.room, tile.y *. Config.scale.room) in
            let vs_length =
              (* end_tile_x is off by a few tiles to the left, but this step is probably only going to be
                 used once so it's probably not worth fixing *)
              Config.action.vengeful_spirit_duration *. Config.action.vengeful_spirit_vx
            in
            let start_x = end_x -. vs_length in
            let v = { x = start_x; y = end_y } in
            spawn_vengeful_spirit ~start:(Some v) ~direction:(Some direction) state game
          | DIALOGUE (speaker, str) ->
            game.interaction.speaker_name <- Some speaker;
            game.interaction.text <- Some (DIALOGUE (speaker, str))
          | PURPLE_PEN_TEXT line ->
            Entity.freeze game.player.ghost.entity;
            let text : string list =
              let content_with_prefix =
                [ "Found a purple pen with a note:"; fmt "{{purple}} %s" line ]
              in
              let content_without_prefix = [ line ] in
              match game.mode with
              | STEEL_SOLE -> content_without_prefix
              | CLASSIC
              | DEMO ->
                if Room.in_teachers_archives game.room then
                  content_without_prefix
                else
                  content_with_prefix
            in
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
          | PLAY_SOUND_EFFECT name -> Audio.play_sound state name
        in

        let handle_entity_step (entity : entity) (entity_step : Interaction.entity_step) =
          match entity_step with
          | SET_VX new_vx ->
            Entity.unfreeze entity;
            entity.v.x <- new_vx
          | SET_VY new_vy -> entity.v.y <- new_vy
          | UNSET_FLOOR -> entity.current_floor <- None
          | SET_FACING direction -> Entity.set_facing_right entity direction
          | WAIT_UNTIL_LANDED can_hardfall ->
            if not can_hardfall then
              game.player.ghost.hardfall_timer <- None;
            if Option.is_none entity.current_floor then
              still_waiting_for_floor := true
          | UNHIDE -> Entity.unhide entity
          | UNHIDE_AT (start_tile_x, start_tile_y, x_offset, y_offset) ->
            let tile = (start_tile_x, start_tile_y) |> Tiled.Tile.coords_to_pos in
            let x, y =
              ((tile.x +. x_offset) *. Config.scale.room, (tile.y +. y_offset) *. Config.scale.room)
            in
            state.debug.rects <- (Raylib.Color.green, entity.dest) :: state.debug.rects;
            Entity.unhide_at entity { x; y }
          | HIDE -> Entity.hide entity
          | FREEZE -> Entity.freeze entity
          | UNFREEZE -> Entity.unfreeze entity
          | MOVE_TO (target_tile_x, target_tile_y) ->
            let target_pos = (target_tile_x, target_tile_y) |> Tiled.Tile.coords_to_pos in
            entity.current_floor <- None;
            entity.dest.pos <-
              { x = target_pos.x *. Config.scale.room; y = target_pos.y *. Config.scale.room }
        in

        let add_item (item_kind : Interaction.item_kind) =
          match item_kind with
          | ABILITY ability_name -> enable_ability game.player ability_name
          | KEY key_name -> game.progress.keys_found <- key_name :: game.progress.keys_found
          | DREAMER (item_name, dreamer_item_text) ->
            let text : string list =
              [ fmt "Found a dreamer item: {{blue}} %s" item_name; ""; dreamer_item_text ]
            in
            game.progress.dreamer_items_found <- game.progress.dreamer_items_found + 1;
            game.interaction.speaker_name <- None;
            game.interaction.use_dashes_in_archives <- Some false;
            game.interaction.text <- Some (PLAIN text)
          | WEAPON weapon_name ->
            let weapon_config = String.Map.find weapon_name state.global.weapons in
            let text : string list =
              [
                fmt "Acquired the {{%s}} %s" weapon_config.text_color weapon_name;
                weapon_config.pickup_text;
              ]
            in
            acquire_weapon state game weapon_name;
            game.interaction.speaker_name <- None;
            game.interaction.text <- Some (PLAIN text)
        in

        let handle_party_ghost_step
            ghost_id
            (party_ghost : party_ghost)
            (step : Interaction.party_ghost_step) =
          let set_interaction_pose pose =
            let head_texture, body_texture =
              match pose with
              | IDLE ->
                party_ghost.ghost.entity.v.x <- 0.;
                (party_ghost.ghost.head_textures.idle, state.global.textures.ghost_bodies.idle)
              | PERFORMING action -> (
                match action with
                | JUMP ->
                  party_ghost.ghost.entity.v.y <- Config.ghost.jump_vy;
                  (party_ghost.ghost.head_textures.idle, state.global.textures.ghost_bodies.jump)
                | ATTACK _ ->
                  (party_ghost.ghost.head_textures.idle, state.global.textures.ghost_bodies.nail)
                | CAST DESOLATE_DIVE ->
                  party_ghost.ghost.entity.v.y <- Config.ghost.dive_vy;
                  party_ghost.ghost.entity.v.x <- 0.;
                  (* TODO maybe add dive children
                     - would probably need a new step UNSET_CHILDREN to remove the child
                     - would need to move player.children to ghost, and update draw_party_ghost in render.ml
                  *)
                  (party_ghost.ghost.head_textures.look_up, state.global.textures.ghost_bodies.dive)
                | _ ->
                  failwithf "bad PERFORMING action for party ghost: %s"
                    (Show.ghost_action_kind action))
              | AIRBORNE _ ->
                (party_ghost.ghost.head_textures.idle, state.global.textures.ghost_bodies.fall)
              | CRAWLING ->
                (party_ghost.ghost.head_textures.look_down, state.global.textures.ghost_bodies.focus)
              | WALKING direction ->
                Entity.walk_ghost party_ghost.ghost.entity direction;
                if speed_through_interaction then
                  (* can't go faster than 2x because it breaks the locker-boys-killed
                     cutscene because Abed doesn't have enough time to fall to the lower level *)
                  party_ghost.ghost.entity.v.x <- party_ghost.ghost.entity.v.x *. 2.;
                (party_ghost.ghost.head_textures.walk, state.global.textures.ghost_bodies.walk)
              | READING
              | WALL_SLIDING _
              | SWIMMING _ ->
                failwithf "bad interaction pose '%s' for party ghost %s" (Show.ghost_pose pose)
                  (Show.ghost_id ghost_id)
            in
            set_ghost_textures party_ghost.ghost ~head_texture ~body_texture
          in
          match step with
          | ADD_TO_PARTY -> party_ghost.in_party <- true
          | REMOVE_FROM_PARTY -> party_ghost.in_party <- false
          | JUMP (_direction, vx) ->
            set_interaction_pose (PERFORMING JUMP);
            party_ghost.ghost.entity.v.x <- vx;
            party_ghost.ghost.entity.current_floor <- None
          | SET_POSE pose -> set_interaction_pose pose
          | MAKE_CURRENT_GHOST -> swap_current_ghost ~in_cutscene:true state game ghost_id
          | ENTITY entity_step ->
            (match entity_step with
            | HIDE ->
              if ghost_id = game.player.ghost.id then
                failwithf "can't hide current ghost %s" (Show.ghost_id ghost_id)
            | UNHIDE ->
              failwithf "can't unhide %s, use UNHIDE_AT for ghosts" (Show.ghost_id ghost_id)
            | _ -> ());
            handle_entity_step party_ghost.ghost.entity entity_step
          | WALK_TO target_tile_x ->
            let tile = (target_tile_x, 1) |> Tiled.Tile.coords_to_pos in
            let dist = (tile.x *. Config.scale.room) -. party_ghost.ghost.entity.dest.pos.x in
            still_walking := abs_float dist > 10.;
            if not !still_walking then
              set_interaction_pose IDLE
            else if dist > 0. then
              set_interaction_pose (WALKING RIGHT)
            else
              set_interaction_pose (WALKING LEFT)
        in

        let handle_ghost_step (player : player) (step : Interaction.ghost_step) =
          match step with
          | SET_POSE pose -> set_pose game pose state.global.textures.ghost_bodies state.frame.time
          | FILL_LIFE_VAPOR -> player.soul.current <- player.soul.max
          | CLAIM_REWARD (amount, reward) -> (
            game.progress.last_upgrade_claimed <- amount;
            match reward with
            | INCREASE_MAX_SOUL ->
              let new_max_soul = game.player.soul.max + 33 in
              if new_max_soul > 198 then
                failwithf "invalid new_max_soul %d" new_max_soul;
              player.soul <-
                {
                  current = new_max_soul;
                  max = new_max_soul;
                  at_focus_start = 0;
                  health_at_focus_start = 0;
                  last_decremented = { at = 0. };
                }
            | ABILITY (ability_name, _desc) -> enable_ability game.player ability_name)
          | INCREASE_HEALTH_TEXT str ->
            player.health.max <- player.health.max + 1;
            player.health.current <- player.health.max;
            game.interaction.speaker_name <- None;
            game.interaction.use_dashes_in_archives <- Some false;
            game.interaction.text <- Some (PLAIN [ str ])
          | ADD_ITEM item_kind -> add_item item_kind
          | ENTITY entity_step -> handle_entity_step player.ghost.entity entity_step
          | PARTY party_step ->
            handle_party_ghost_step player.ghost.id (as_party_ghost player) party_step
        in

        let handle_enemy_step ?(idx = None) enemy_id (enemy_step : Interaction.enemy_step) =
          let enemies : enemy list =
            List.filter (fun (e : enemy) -> e.id = enemy_id) game.room.enemies
          in
          let apply_to_all fn =
            match idx with
            | None -> List.iter fn enemies
            | Some idx' -> List.nth enemies idx' |> fn
          in
          let apply_to_only step_name fn =
            match idx with
            | None ->
              if List.length enemies <> 1 then
                failwithf "can't use %s when there are multiple enemies" step_name
              else
                fn (List.hd enemies)
            | Some idx' -> List.nth enemies idx' |> fn
          in
          let walk_to target_tile_x dead =
            apply_to_only "WALK_TO" (fun (enemy : enemy) ->
                let tile = (target_tile_x, 1) |> Tiled.Tile.coords_to_pos in
                let dist = (tile.x *. Config.scale.room) -. enemy.entity.dest.pos.x in
                let (module M : Enemy.M) = Enemy.get_module enemy.id in
                still_walking := abs_float dist > 10.;
                let action_name = if dead then "walking-dead" else "walking" in
                if not !still_walking then (
                  enemy.entity.v.x <- 0.;
                  Enemy.set_pose enemy "idle")
                else if dist > 0. then (
                  enemy.entity.sprite.facing_right <- true;
                  M.Action.set enemy (M.Action.from_string action_name) ~frame_time:state.frame.time)
                else (
                  enemy.entity.sprite.facing_right <- false;
                  M.Action.set enemy (M.Action.from_string action_name) ~frame_time:state.frame.time))
          in
          match enemy_step with
          | DEAD_WALK_TO target_tile_x -> walk_to target_tile_x true
          | WALK_TO target_tile_x -> walk_to target_tile_x false
          | SET_POSE pose_name ->
            apply_to_all (fun (enemy : enemy) ->
                let (module M : Enemy.M) = Enemy.get_module enemy.id in
                M.Action.log enemy pose_name state.frame.time;
                Enemy.set_pose enemy pose_name)
          | START_ACTION action_name ->
            apply_to_all (fun (enemy : enemy) ->
                let (module M : Enemy.M) = Enemy.get_module enemy.id in
                M.Action.log enemy action_name state.frame.time;
                M.Action.set enemy (M.Action.from_string action_name) ~frame_time:state.frame.time)
          | ENTITY entity_step ->
            let active =
              match entity_step with
              | UNFREEZE -> true
              | _ -> false
            in
            apply_to_all (fun (e : enemy) ->
                e.status.active <- active;
                e.status.check_damage_collisions <- active;
                handle_entity_step e.entity entity_step)
        in

        let set_npc_pose (npc : npc) pose =
          let texture =
            match String.Map.find_opt pose npc.textures with
            | Some t -> t
            | None -> failwithf "could not find pose %s for npc %s" pose (Show.npc_id npc.id)
          in
          Entity.update_sprite_texture ~scale:Config.scale.ghost npc.entity texture
        in

        let handle_npc_step (npc : npc) (npc_step : Interaction.npc_step) =
          match npc_step with
          | ENTITY entity_step -> handle_entity_step npc.entity entity_step
          | WALK_TO target_tile_x ->
            (* TODO duplicated in walk_ghost *)
            let tile = (target_tile_x, 1) |> Tiled.Tile.coords_to_pos in
            let dist = (tile.x *. Config.scale.room) -. npc.entity.dest.pos.x in
            still_walking := abs_float dist > 10.;
            if not !still_walking then (
              npc.entity.v.x <- 0.;
              set_npc_pose npc "idle")
            else if dist > 0. then (
              npc.entity.sprite.facing_right <- true;
              (* TODO use a config (maybe npc-specific configs, but a global
                 config for shared things like walk_vx might be nice)
              *)
              npc.entity.v.x <- 400.;
              set_npc_pose npc "walking")
            else (
              npc.entity.sprite.facing_right <- false;
              npc.entity.v.x <- -400.;
              set_npc_pose npc "walking")
          | SET_POSE target_pose -> set_npc_pose npc target_pose
        in

        (match next_step with
        | STEP general_step -> handle_general_step general_step
        | CURRENT_GHOST ghost_step -> handle_ghost_step game.player ghost_step
        | PARTY_GHOST (ghost_id, party_ghost_step) ->
          let find_party_ghost () : party_ghost =
            match find_party_ghost ghost_id game.party with
            | Some ghost -> ghost
            | None -> failwithf "ghost_id %s needs to be in game.players" (Show.ghost_id ghost_id)
          in
          handle_party_ghost_step ghost_id (find_party_ghost ()) party_ghost_step
        | NPC (target_npc_id, npc_step) ->
          let find_existing_npc () =
            match List.find_opt (fun (npc : npc) -> npc.id = target_npc_id) game.room.npcs with
            | None -> failwithf "can't find npc %s" (Show.npc_id target_npc_id)
            | Some npc -> npc
          in
          handle_npc_step (find_existing_npc ()) npc_step
        | ENEMY (enemy_id, enemy_step) -> handle_enemy_step enemy_id enemy_step
        | NTH_ENEMY (idx, enemy_id, enemy_step) ->
          handle_enemy_step ~idx:(Some idx) enemy_id enemy_step);

        if !still_walking || !still_waiting_for_floor then
          ( (* leave the current step on top of the stack *) )
        else if !new_wait > 0. && not speed_through_interaction then
          game.interaction.steps <- STEP (WAIT !new_wait) :: List.tl game.interaction.steps
        else
          game.interaction.steps <- List.tl game.interaction.steps;
        (true, !zero_vy))
  in

  let handle_casting () : handled_action =
    let trying_cast =
      let soul_per_cast =
        if game.player.abilities.spell_twister then
          Config.action.spell_twister_soul_per_cast
        else
          Config.action.soul_per_cast
      in
      pressed_or_buffered CAST
      && game.player.soul.current >= soul_per_cast
      && past_cooldown game.player.history.cast_vs state.frame.time
      && (not (is_doing game.player (ATTACK RIGHT) state.frame.time))
      && not (is_casting state game)
    in

    let this_frame =
      if trying_cast then (
        let cast_spell spell_kind =
          start_action ~debug:true state game (CAST spell_kind);
          true
        in
        let can_howl () =
          game.player.abilities.howling_wraiths
          && past_cooldown game.player.history.cast_wraiths state.frame.time
          && state.frame_inputs.up.down
        in
        let can_dive () =
          game.player.abilities.desolate_dive
          && past_cooldown game.player.history.cast_dive state.frame.time
          && state.frame_inputs.down.down
        in
        let can_vs () =
          game.player.abilities.vengeful_spirit
          && past_cooldown game.player.history.cast_vs state.frame.time
        in
        if can_howl () then
          cast_spell HOWLING_WRAITHS
        else if can_dive () then (
          game.player.current.is_diving <- true;
          cast_spell DESOLATE_DIVE)
        else if can_vs () then
          cast_spell VENGEFUL_SPIRIT
        else
          false)
      else (
        let continuing_spell_kind =
          (* this should not be checking DESOLATE_DIVE because that ends when landing on a floor *)
          List.find_opt
            (fun s -> is_doing game.player (CAST s) state.frame.time)
            [ VENGEFUL_SPIRIT; HOWLING_WRAITHS ]
        in

        match continuing_spell_kind with
        | None ->
          if game.player.current.is_diving then (* TODO also need to check if ghost is in water *)
            if Option.is_some game.player.ghost.entity.current_floor then (
              state.camera.shake <- 1.;
              game.player.current.is_diving <- false;
              start_action state game DIVE_COOLDOWN;
              let child =
                let dest = game.player.ghost.entity.dest in
                let relative_pos : relative_position = (CENTER, BOTTOM_INSIDE) in
                let w, h =
                  (* TODO this scaling is temporary so the current dive.png can be used *)
                  (dest.w *. 15., dest.h *. 3.)
                in
                make_ghost_child game.player DIVE_COOLDOWN relative_pos
                  game.player.shared_textures.dive_shockwave w h
              in
              add_child game.player DIVE_COOLDOWN child;
              remove_child game.player DIVE;
              false)
            else (
              match get_dive_sprite game.player with
              | None -> false
              | Some dive_sprite ->
                continue_action state game (CAST DESOLATE_DIVE);
                true)
          else
            false
        | Some kind ->
          continue_action state game (CAST kind);
          true)
    in

    { this_frame }
  in

  let handle_c_dashing () : handled_action =
    let still_on_surface () =
      (* TODO maybe make a fn for this - after moving to Entity *)
      (* this is used for things like disappearing platforms *)
      Entity.on_ground game.player.ghost.entity || Option.is_some game.player.current.wall
    in
    let starting_charge () =
      (* attack direction is arbitrary *)
      game.player.abilities.crystal_heart
      && pressed_or_buffered C_DASH
      && still_on_surface ()
      && (not (is_doing game.player (ATTACK RIGHT) state.frame.time))
      && (not (is_casting state game))
      && (not game.player.current.is_c_dashing)
      && not game.player.current.is_charging_c_dash
    in
    let released_charge () =
      is_doing game.player C_DASH_CHARGE state.frame.time && state.frame_inputs.c_dash.released
    in
    let still_charging () =
      is_doing game.player C_DASH_CHARGE state.frame.time && state.frame_inputs.c_dash.down
    in
    let still_c_dashing () = is_doing game.player C_DASH state.frame.time in
    let still_cooling_down () = is_doing game.player C_DASH_COOLDOWN state.frame.time in

    let maybe_end_c_dash () =
      let end_c_dash () =
        (* this only handles cancelling the dash in mid-air - wall collisions are handled below *)
        state.frame_inputs.jump.pressed || state.frame_inputs.c_dash.pressed
      in
      if end_c_dash () then
        start_action state game C_DASH_COOLDOWN
      else
        continue_action state game C_DASH
    in

    let start_c_dash_or_cancel_charge () =
      let can_start_c_dash () =
        let down_since = state.frame.time -. game.player.history.charge_c_dash.started.at in
        still_on_surface ()
        && down_since > game.player.history.charge_c_dash.config.duration.seconds
      in

      if can_start_c_dash () then (
        stop_wall_sliding := true;
        start_action state game C_DASH;
        true)
      else (
        game.player.current.is_charging_c_dash <- false;
        false)
    in

    let cancel_c_dash () =
      remove_child game.player C_DASH_CHARGE_CRYSTALS;
      remove_child game.player C_DASH_WALL_CHARGE_CRYSTALS;
      start_c_dash_or_cancel_charge ()
    in

    let this_frame =
      if starting_charge () then (
        start_action state game C_DASH_CHARGE;
        true)
      else if still_charging () then
        if not (still_on_surface ()) then
          cancel_c_dash ()
        else (
          continue_action state game C_DASH_CHARGE;
          true)
      else if still_cooling_down () then (
        let action_kind =
          match game.player.current.wall with
          | None -> C_DASH_COOLDOWN
          | Some _ -> C_DASH_WALL_COOLDOWN
        in
        continue_action state game action_kind;
        true)
      else if still_c_dashing () then (
        (* this function should still return true if the ghost is ending the c-dash this frame
           because if it is false, the ghost has a frame to input directions to turn around and
           cooldown in the wrong direction
        *)
        maybe_end_c_dash ();
        true)
      else if released_charge () then
        cancel_c_dash ()
      else
        false
    in
    { this_frame }
  in

  let handle_dashing () : handled_action =
    let starting_dash () =
      let not_attacking () =
        (not (is_doing game.player (ATTACK RIGHT) state.frame.time))
        || Option.is_some game.player.ghost.entity.y_recoil
      in
      game.player.abilities.mothwing_cloak
      && pressed_or_buffered DASH
      && not_attacking ()
      && (not (is_casting state game))
      && Option.is_none game.player.ghost.entity.x_recoil
      && game.player.current.can_dash
      && past_cooldown game.player.history.dash state.frame.time
    in
    let still_dashing () = is_doing game.player DASH state.frame.time in
    let still_shade_dashing () = is_doing game.player SHADE_DASH state.frame.time in
    let this_frame =
      if starting_dash () then (
        (* TODO "dash in currently-facing direction" almost always works, but not when walljumping,
           so this should probably be `DASHING of direction` and check LEFT/RIGHT keys
        *)
        let can_shade_dash () =
          game.player.abilities.shade_cloak
          && past_cooldown game.player.history.shade_dash state.frame.time
        in

        if can_shade_dash () then (
          spawn_child game.player SHADE_DASH_SPARKLES ~in_front:true
            ~scale:Config.ghost.shade_dash_sparkles_scale (CENTER, CENTER)
            game.player.shared_textures.shade_cloak_sparkles;
          start_action state game SHADE_DASH);
        start_action state game DASH;
        stop_wall_sliding := true;
        true)
      else if still_dashing () then (
        if still_shade_dashing () then
          continue_action state game SHADE_DASH;
        continue_action state game DASH;
        true)
      else
        false
    in
    { this_frame }
  in

  let handle_walking () =
    (* this fn always calls set_pose' to update vx, but it is often overridden by
       another call to set_pose' later in the same frame (eg. if the player is also attacking/jumping/etc) *)
    let move_right () =
      (match game.player.current.wall with
      | None -> ()
      | Some wall ->
        if wall.pos.x < game.player.ghost.entity.sprite.dest.pos.x then
          stop_wall_sliding := true);
      set_pose' (WALKING RIGHT)
    in
    let move_left () =
      (match game.player.current.wall with
      | None -> ()
      | Some wall ->
        if wall.pos.x > game.player.ghost.entity.sprite.dest.pos.x then
          stop_wall_sliding := true);
      set_pose' (WALKING LEFT)
    in
    match (state.frame_inputs.left.down_since, state.frame_inputs.right.down_since) with
    | Some left_down_since, Some right_down_since ->
      if left_down_since > right_down_since then
        move_left ()
      else
        move_right ()
    | None, Some _time -> move_right ()
    | Some _time, None -> move_left ()
    | None, None -> set_pose' IDLE
  in

  let handle_wall_kicking () : handled_action =
    let starting_wall_kick () =
      Option.is_some game.player.current.wall && pressed_or_buffered JUMP
    in
    let continuing_wall_kick () = is_doing game.player WALL_KICK state.frame.time in
    let this_frame =
      if starting_wall_kick () then (
        stop_wall_sliding := true;
        start_action state game WALL_KICK;
        true)
      else if continuing_wall_kick () then (
        stop_wall_sliding := true;
        continue_action state game WALL_KICK;
        true)
      else
        false
    in
    { this_frame }
  in

  let handle_jumping new_vy =
    match game.player.ghost.entity.current_floor with
    | None -> (
      match game.player.current.water with
      | None ->
        if
          game.player.abilities.monarch_wings
          && game.player.current.can_flap
          (* using .pressed because pressed_or_buffered detects the same keypress as
             the initial jump *)
          && state.frame_inputs.jump.pressed
        then
          start_action state game FLAP
        else if is_doing game.player FLAP state.frame.time then (
          set_pose' (AIRBORNE new_vy);
          if state.frame_inputs.jump.down then
            continue_action state game FLAP
          else
            cancel_action state game FLAP)
        else
          set_pose' (AIRBORNE new_vy)
      | Some water ->
        if is_doing game.player FLAP state.frame.time then
          cancel_action state game FLAP)
    | Some (floor, floor_v) ->
      if is_doing game.player FLAP state.frame.time then
        (* TODO need a check like this for current_wall too (to prevent extra height from buffering jump off wallslide) *)
        cancel_action state game FLAP
      else if pressed_or_buffered JUMP then
        set_pose' (PERFORMING JUMP)
  in

  let handle_attacking () =
    let starting_attack () =
      past_cooldown game.player.history.nail state.frame.time && pressed_or_buffered NAIL
    in
    if starting_attack () then (
      let direction : direction =
        match
          (game.player.current.wall, state.frame_inputs.up.down, state.frame_inputs.down.down)
        with
        | None, true, false -> UP
        | None, false, true ->
          if Entity.on_ground game.player.ghost.entity then
            if game.player.ghost.entity.sprite.facing_right then RIGHT else LEFT
          else
            DOWN
        | _, _, _ ->
          (* don't really care about socd for up/down *)
          if game.player.ghost.entity.sprite.facing_right then RIGHT else LEFT
      in
      start_action state game (ATTACK direction))
    else (
      match get_current_slash game.player with
      | None -> ()
      | Some slash -> (
        continue_action state game (ATTACK slash.direction);
        match Sprite.advance_or_despawn state.frame.time slash.sprite.texture slash.sprite with
        | None -> remove_child game.player (NAIL slash)
        | Some _ -> ()))
  in

  let handle_dream_nail () : handled_action =
    let starting_dream_nail () =
      game.player.abilities.dream_nail
      && state.frame_inputs.dream_nail.pressed
      && Entity.on_ground game.player.ghost.entity
      && not_doing_any game.player state.frame.time [ ATTACK RIGHT; DASH ]
    in
    (* arbitrary directions *)
    let still_dream_nailing () =
      is_doing game.player DREAM_NAIL state.frame.time && state.frame_inputs.dream_nail.down
    in
    let this_frame =
      if starting_dream_nail () then (
        start_action state game DREAM_NAIL;
        true)
      else if still_dream_nailing () then (
        continue_action state game DREAM_NAIL;
        true)
      else
        false
    in
    { this_frame }
  in

  let handle_focusing () : handled_action =
    let starting_focus () =
      state.frame_inputs.focus.pressed
      && Entity.on_ground game.player.ghost.entity
      && game.player.soul.current >= Config.action.soul_per_cast
      && game.player.health.current < game.player.health.max
      && not_doing_any game.player state.frame.time [ ATTACK RIGHT; DASH ]
      && not (is_casting state game)
    in
    let still_focusing () =
      is_doing game.player FOCUS state.frame.time && state.frame_inputs.focus.down
    in
    let this_frame =
      if starting_focus () then (
        start_action state game FOCUS;
        true)
      else if still_focusing () then (
        continue_action state game FOCUS;
        true)
      else (
        remove_child game.player FOCUS;
        false)
    in
    { this_frame }
  in

  let start_swimming (rect : rect) =
    game.player.current.is_diving <- false;
    game.player.children <- Ghost_child_kind.Map.empty;
    if not (in_water game.player) then
      game.player.ghost.entity.dest.pos.y <-
        game.player.ghost.entity.dest.pos.y +. (game.player.ghost.entity.dest.h /. 2.);
    (* seems weird to check jump inputs here instead of checking current.water in handle_jumping,
       but set_pose SWIMMING resets ghost.v.y <- 0
       - could use another ref (like stop_wall_sliding), but that doesn't really seem better
    *)
    if state.frame_inputs.jump.pressed then (
      game.player.current.water <- None;
      game.player.ghost.entity.dest.pos.y <-
        game.player.ghost.entity.dest.pos.y -. (game.player.ghost.entity.dest.h /. 2.);
      start_action state game JUMP)
    else
      set_pose' (SWIMMING rect)
  in

  let handle_collisions () =
    (* returns the first enemy collision *)
    let get_enemy_collision (player : player) (room : room) : (int * direction) option =
      let find_colliding_enemy (enemy : enemy) =
        if Enemy.is_dead enemy then
          None
        else if Collision.between_entities player.ghost.entity enemy.entity then (
          let direction : direction =
            if rect_center_x enemy.entity.dest > rect_center_x player.ghost.entity.dest then
              LEFT
            else
              RIGHT
          in
          Some (enemy.damage, direction))
        else
          None
      in
      if past_cooldown game.player.history.take_damage state.frame.time then
        List.find_map find_colliding_enemy room.enemies
      else
        None
    in
    let check_enemy_collisions () =
      if is_vulnerable state game then (
        match get_enemy_collision game.player game.room with
        | Some (damage, direction) -> start_action state game (TAKE_DAMAGE (damage, direction))
        | None -> ())
    in
    let check_damage_collisions hazard_collisions platform_spike_collisions =
      state.debug.rects <-
        List.map (fun c -> (Raylib.Color.red, c.other_rect)) hazard_collisions @ state.debug.rects;
      let collisions' =
        if game.player.abilities.ismas_tear then
          hazard_collisions
        else (
          let acid_collisions = Entity.get_acid_collisions game.room game.player.ghost.entity in
          hazard_collisions @ acid_collisions)
      in
      if List.length collisions' = 0 && not game.player.current.is_taking_hazard_damage then (
        match List.nth_opt platform_spike_collisions 0 with
        | Some spike_collision -> start_action state game (TAKE_DAMAGE (1, UP))
        | None -> (
          let projectile_collisions =
            if is_vulnerable state game then
              Entity.get_loose_projectile_collisions game.room game.player.ghost.entity
            else
              []
          in
          match List.nth_opt projectile_collisions 0 with
          | Some (collision, projectile) ->
            state.camera.shake <- 1.;
            (* arbitrary direction *)
            start_action state game (TAKE_DAMAGE (projectile.damage, UP))
          | None ->
            (* TODO doesn't seem like this is working, can still "pogo" upwards while taking damage *)
            if
              game.player.current.is_taking_hazard_damage
              && Option.is_some game.player.ghost.entity.y_recoil
            then
              game.player.ghost.entity.y_recoil <- None))
      else if is_doing game.player TAKE_DAMAGE_AND_RESPAWN state.frame.time then
        continue_action state game TAKE_DAMAGE_AND_RESPAWN
      else if game.player.current.is_taking_hazard_damage then (
        game.player.current.is_taking_hazard_damage <- false;
        let hazard_respawn (state : state) (game : game) =
          (match game.mode with
          | CLASSIC
          | DEMO ->
            ()
          | STEEL_SOLE -> add_phantom_floor game game.room.respawn.target);
          Entity.unfreeze game.player.ghost.entity;
          game.player.ghost.hardfall_timer <- None;
          state.camera.update_instantly <- true;
          respawn_ghost game
        in
        if game.player.health.current > 0 then
          hazard_respawn state game)
      else if game.player.health.current > 0 then (
        game.player.current.is_diving <- false;
        game.player.children <- Ghost_child_kind.Map.empty;
        state.camera.shake <- 1.;
        game.player.current.is_taking_hazard_damage <- true;
        start_action state game TAKE_DAMAGE_AND_RESPAWN)
    in
    let handle_c_dash_wall_collisions (collisions : collision list) =
      let wall_collision =
        List.find_opt (fun (c : collision) -> List.mem c.collided_from [ LEFT; RIGHT ]) collisions
      in
      match wall_collision with
      | None -> ()
      | Some coll ->
        if game.player.current.is_c_dashing then (
          if game.player.abilities.mantis_claw then
            game.player.current.wall <- Some coll.other_rect;
          state.camera.shake <- 1.;
          start_action state game C_DASH_WALL_COOLDOWN)
    in
    let handle_wall_sliding (collisions : collision list) =
      let high_enough_to_slide_on wall =
        let wall_bottom_y = wall.pos.y +. wall.h in
        game.player.ghost.entity.dest.pos.y +. (game.player.ghost.entity.dest.h /. 2.)
        < wall_bottom_y
      in
      let still_wall_sliding = Option.is_some game.player.current.wall in
      if game.player.abilities.mantis_claw then
        if !stop_wall_sliding || Option.is_some game.player.ghost.entity.current_floor then
          game.player.current.wall <- None
        else if still_wall_sliding then (
          let wall = Option.get game.player.current.wall in
          (* TODO also check that wall is still within x range too
             - would fix wall-cling storage for hazard respawns
          *)
          if high_enough_to_slide_on wall then
            set_pose' (WALL_SLIDING wall)
          else (
            let dx =
              (* can't check sprite.facing_right because it gets updated for the frame based on input, so it depends on whether a direction is being held *)
              if Entity.get_center game.player.ghost.entity > get_rect_center wall then -1. else 1.
            in
            game.player.ghost.entity.dest.pos.x <- game.player.ghost.entity.dest.pos.x +. dx;
            match
              List.find_opt
                (fun coll -> coll.other_rect <> wall)
                (Entity.get_floor_collisions game.room game.player.ghost.entity)
            with
            | None ->
              game.player.ghost.entity.dest.pos.x <- game.player.ghost.entity.dest.pos.x -. dx;
              game.player.current.wall <- None
            | Some collision -> set_pose' (WALL_SLIDING collision.other_rect)))
        else (
          game.player.current.wall <- None;
          if not (is_doing game.player (ATTACK RIGHT) state.frame.time) then (
            let check_collision (collision : collision) =
              if
                (not (Entity.on_ground game.player.ghost.entity))
                && Entity.descending game.player.ghost.entity
              then (
                let wall = collision.other_rect in
                let wall_is_tall_enough = wall.h > game.player.ghost.entity.dest.h /. 2. in
                if high_enough_to_slide_on wall && wall_is_tall_enough then (
                  match collision.collided_from with
                  | LEFT -> set_pose' (WALL_SLIDING wall)
                  | RIGHT -> set_pose' (WALL_SLIDING wall)
                  | _ -> ()))
            in
            List.iter check_collision collisions))
    in

    let floor_collisions' = Entity.get_floor_collisions game.room game.player.ghost.entity in
    let floor_v, floor_collisions =
      match Entity.get_conveyor_belt_collision game.room game.player.ghost.entity with
      | None -> (Zero.vector (), floor_collisions')
      | Some (collision, floor_v) ->
        (* TODO this is causing the falling-through-floor bug in forgotten_g *)
        (floor_v, [ collision ])
    in

    let up_floor_collision : collision option =
      List.find_opt (fun (c : collision) -> c.collided_from = UP) floor_collisions
    in

    let hazard_collisions, platform_spike_collisions' =
      let damage = Entity.get_damage_collisions game.room game.player.ghost.entity in
      match game.mode with
      | CLASSIC
      | DEMO ->
        (damage.hazards, damage.platform_spikes)
      | STEEL_SOLE -> (
        let up_floor_collision' : collision option =
          (* safe to walk on the ground in the vents *)
          if
            state.debug.safe_ss
            || game.room.area.id = VENTWAYS
            || Entity.ascending game.player.ghost.entity
          then
            None
          else (
            match List.length floor_collisions with
            | 0 -> None
            | 1 -> up_floor_collision
            | n ->
              if List.for_all (fun c -> c.collided_from = UP) floor_collisions then
                up_floor_collision
              else
                None)
        in
        match up_floor_collision' with
        | None -> (damage.hazards, damage.platform_spikes)
        | Some coll ->
          game.progress.steel_sole.dunks <- 1 + game.progress.steel_sole.dunks;
          ([ coll ], damage.platform_spikes))
    in
    let platform_spike_collisions =
      if is_vulnerable state game then
        platform_spike_collisions'
      else
        []
    in
    let bench_collisions = Entity.get_bench_collisions game.room game.player.ghost.entity in
    (* since these are collisions from above, they are only detected for the frame that the
       ghost collides (and then again the frame after, which is a bug)
    *)
    (match List.nth_opt bench_collisions 0 with
    | None -> ()
    | Some coll -> (
      match coll.collided_from with
      | UP ->
        game.interaction.corner_text <-
          Some
            {
              content = "Game saved.";
              visible = Interaction.make_UNTIL 1.5 state.frame.time;
              scale = 1.;
            };
        state.save_pos <- Some game.player.ghost.entity.dest.pos;
        game.player.health.current <- game.player.health.max
      | _ -> ()));
    let liquid_collisions =
      let water_collisions = Entity.get_water_collisions game.room game.player.ghost.entity in
      if game.player.abilities.ismas_tear then (
        let acid_collisions = Entity.get_acid_collisions game.room game.player.ghost.entity in
        water_collisions @ acid_collisions)
      else
        water_collisions
    in
    (match List.nth_opt liquid_collisions 0 with
    | None -> game.player.current.water <- None
    | Some coll -> start_swimming coll.other_rect);
    check_enemy_collisions ();
    state.debug.rects <-
      List.map (fun c -> (Raylib.Color.green, c.other_rect)) floor_collisions @ state.debug.rects;
    Entity.apply_collisions ~floor_v game.player.ghost.entity floor_collisions;
    (match up_floor_collision with
    | None -> ()
    | Some coll ->
      let started_hardfalling : time =
        match game.player.ghost.hardfall_timer with
        | None -> { at = state.frame.time }
        | Some time -> time
      in
      if started_hardfalling.at +. Config.ghost.hardfall_duration < state.frame.time then
        start_action state game HARDFALL);
    let reset_hardfall_timer =
      Option.is_some game.player.ghost.entity.current_floor
      || Option.is_some game.player.current.wall
      || Option.is_some game.player.ghost.entity.y_recoil
      || game.player.current.is_diving
      || game.player.ghost.entity.v.y <= 0.
    in
    if reset_hardfall_timer then
      game.player.ghost.hardfall_timer <- None
    else (
      match game.player.ghost.hardfall_timer with
      | None -> game.player.ghost.hardfall_timer <- Some { at = state.frame.time }
      | Some _ -> ());
    handle_wall_sliding floor_collisions;
    check_damage_collisions hazard_collisions platform_spike_collisions;
    handle_c_dash_wall_collisions (floor_collisions @ hazard_collisions)
  in

  let check_cooldowns cooldowns =
    let cooling_down = ref false in
    let check cooldown =
      if not !cooling_down then
        if is_doing game.player cooldown state.frame.time then (
          continue_action state game cooldown;
          cooling_down := true)
    in
    List.iter check cooldowns;
    !cooling_down
  in

  let reveal_shadows () =
    let colliding (trigger : trigger) =
      Option.is_some (Collision.with_entity game.player.ghost.entity trigger.dest)
    in
    match List.find_opt colliding game.room.triggers.shadows with
    | None -> ()
    | Some trigger -> (
      match List.find_opt (fun (l : layer) -> l.name = trigger.name_suffix) game.room.layers with
      | None -> failwithf "Player.update: could not find layer '%s' to reveal" trigger.name_suffix
      | Some layer ->
        if not (List.mem trigger.name_suffix game.room.progress.revealed_shadow_layers) then
          game.room.progress.revealed_shadow_layers <-
            trigger.name_suffix :: game.room.progress.revealed_shadow_layers;
        layer.hidden <- true)
  in

  if game.player.health.current <= 0 then
    if is_doing game.player DIE state.frame.time then
      continue_action state game DIE
    else
      start_action state game DIE
  else (
    game.player.ghost.entity.current_platforms <- [];
    Entity.apply_v state.frame.dt game.player.ghost.entity;
    (match game.player.ghost.entity.current_floor with
    | None -> ()
    | Some (floor, v) ->
      let e = game.player.ghost.entity in
      let dt = state.frame.dt in
      (* only need to update x here because conveyor belts are only horizontal *)
      e.dest.pos.x <- e.dest.pos.x +. (v.x *. dt));

    let new_vy =
      let vy' =
        let vy = game.player.ghost.entity.v.y in
        let ascending = vy < 0. in
        let dvy = Config.physics.gravity *. state.frame.dt in
        if key_up JUMP && ascending then
          (vy *. Config.physics.jump_damping) +. dvy
        else (
          match game.player.current.wall with
          (* could check current_floor here and set to 0, but this already happens by set_pose *)
          | Some _ -> Config.ghost.wall_slide_vy
          | None -> vy +. dvy)
      in
      (* no max_vy applied when ascending, only descending *)
      Float.bound (-1. *. Float.max_float) vy' Config.ghost.max_vy
    in
    let exiting = Room.handle_transitions state game in
    if not exiting then (
      let interacting, zero_vy = handle_interactions () in
      if interacting then (
        if not game.player.ghost.entity.frozen then (
          (* after warping, set vy to 0. so the ghost doesn't continue to jump back up into the
             vent they came from
          *)
          game.player.ghost.entity.v.y <- (if zero_vy then 0. else new_vy);
          handle_collisions ()))
      else (
        reveal_shadows ();
        if not game.player.ghost.entity.frozen then (
          let cooling_down =
            check_cooldowns [ DIVE_COOLDOWN; C_DASH_COOLDOWN; C_DASH_WALL_COOLDOWN; HARDFALL ]
          in
          if not cooling_down then
            if in_water game.player then (
              handle_walking ();
              handle_jumping new_vy)
            else (
              let dream_nailing = handle_dream_nail () in
              if not dream_nailing.this_frame then (
                let focusing = handle_focusing () in
                if not focusing.this_frame then (
                  let c_dashing = handle_c_dashing () in
                  if not c_dashing.this_frame then (
                    let dashing = handle_dashing () in
                    if not dashing.this_frame then (
                      let casting = handle_casting () in
                      if not casting.this_frame then (
                        let wall_kicking = handle_wall_kicking () in
                        if not wall_kicking.this_frame then (
                          handle_walking ();
                          handle_jumping new_vy);
                        handle_attacking ();
                        resolve_slash_collisions state game)))))))));
    animate_and_despawn_children state.frame.time game.player;
    handle_collisions ();
    Entity.maybe_unset_current_floor game.player.ghost.entity game.room;
    Sprite.advance_animation state.frame.time game.player.ghost.entity.sprite);
  state

let load_shared_textures (shared_texture_configs : texture_config String.Map.t) =
  let build_shared_texture ?(particle = false) ?(once = false) ?(config_name = None) pose_name =
    let config =
      let config_name' =
        (* the "nail" shared config applies to "slash"/"upslash"/"downslash", but otherwise the pose_name *)
        match config_name with
        | None -> pose_name
        | Some name -> name
      in
      match String.Map.find_opt config_name' shared_texture_configs with
      | Some config ->
        (* the `with pose_name` is also because of the "nail" config naming *)
        { config with path = { config.path with pose_name } }
      | None ->
        {
          path = { asset_dir = GHOSTS; character_name = "shared"; pose_name };
          x_offset = 0.;
          y_offset = 0.;
          duration = { seconds = 0. };
          count = 1;
        }
    in

    Sprite.build_texture_from_config ~particle ~once config
  in

  let build_slash_texture name =
    build_shared_texture ~particle:true ~config_name:(Some "nail") name
  in
  {
    slash = build_slash_texture "slash";
    upslash = build_slash_texture "upslash";
    downslash = build_slash_texture "downslash";
    shine = build_shared_texture "shine";
    health = build_shared_texture "health";
    energon_pod_base = build_shared_texture "energon-pod-base";
    energon_pod_1 = build_shared_texture "energon-pod-1";
    energon_pod_2 = build_shared_texture "energon-pod-2";
    energon_pod_3 = build_shared_texture "energon-pod-3";
    energon_pod_4 = build_shared_texture "energon-pod-4";
    vengeful_spirit = build_shared_texture "vengeful-spirit";
    shade_soul = build_shared_texture "shade-soul";
    desolate_dive = build_shared_texture "desolate-dive";
    dive_shockwave = build_shared_texture ~particle:true "desolate-dive";
    howling_wraiths = build_shared_texture ~particle:true "howling-wraiths";
    focus_sparkles = build_shared_texture "focus-sparkles";
    c_dash_crystals = build_shared_texture ~once:true "c-dash-crystals";
    c_dash_crystals_full = build_shared_texture "c-dash-crystals-full";
    c_dash_wall_crystals = build_shared_texture ~once:true "c-dash-wall-crystals";
    c_dash_wall_crystals_full = build_shared_texture "c-dash-wall-crystals-full";
    c_dash_whoosh = build_shared_texture "c-dash-whoosh";
    shade_cloak_sparkles = build_shared_texture ~particle:true "shade-cloak-sparkles";
  }

(* the idle_texture is for setting the initial entity.sprite.texture *)
let init
    (ghost_id : ghost_id)
    (idle_texture : texture)
    (head_textures : ghost_head_textures)
    (action_config : ghost_action_config String.Map.t)
    (start_pos : vector)
    (save_file : Json_t.save_file)
    (weapons_configs : Json_t.weapon String.Map.t)
    shared_textures : player =
  let use_json_action_config action_name : ghost_action_config =
    match String.Map.find_opt action_name action_config with
    | None -> failwithf "could not find action config for '%s' in config/ghosts.json" action_name
    | Some config -> config
  in
  let dest = Sprite.make_dest ~scale:Config.scale.ghost start_pos.x start_pos.y idle_texture in
  let make_action (config_name : string) : ghost_action =
    {
      doing_until = { at = -1000. };
      blocked_until = { at = -1000. };
      config = use_json_action_config config_name;
      started = { at = -1000. };
    }
  in

  let load_weapon name = (name, String.Map.find name weapons_configs) in
  let weapons = List.map load_weapon save_file.weapons in

  {
    ghost =
      {
        id = ghost_id;
        head = head_textures.idle;
        head_textures;
        entity =
          Entity.create_for_sprite
            (Sprite.create (fmt "ghost-%s" (Show.ghost_id ghost_id)) idle_texture dest)
            {
              pos = clone_vector dest.pos;
              w = Config.ghost.width *. Config.scale.ghost;
              h = Config.ghost.height *. Config.scale.ghost;
            };
        hardfall_timer = None;
      };
    current = reset_current_status ();
    children = Ghost_child_kind.Map.empty;
    history =
      {
        cast_dive = make_action "cast-dive";
        cast_vs = make_action "cast-vs";
        cast_wraiths = make_action "cast-wraiths";
        charge_c_dash = make_action "charge-c-dash";
        c_dash = make_action "c-dash";
        c_dash_cooldown = make_action "c-dash-cooldown";
        c_dash_wall_cooldown = make_action "c-dash-cooldown";
        die = make_action "die";
        dream_nail = make_action "dream-nail";
        dash = make_action "dash";
        dive_cooldown = make_action "dive-cooldown";
        flap = make_action "flap";
        focus = make_action "focus";
        hardfall = make_action "hardfall";
        jump = make_action "jump";
        nail = make_action "nail";
        shade_dash = make_action "shade-dash";
        take_damage = make_action "take-damage";
        take_damage_and_respawn = make_action "take-damage-and-respawn";
        wall_kick = make_action "wall-kick";
      };
    shared_textures;
    health = { current = save_file.max_health; max = save_file.max_health };
    soul =
      {
        current = save_file.max_soul;
        max = save_file.max_soul;
        at_focus_start = 0;
        health_at_focus_start = 0;
        last_decremented = { at = 0. };
      };
    spawned_vengeful_spirits = [];
    abilities = clone_abilities save_file.abilities;
    weapons = weapons |> List.to_string_map;
    current_weapon =
      (* TODO should read this from weapons.json *)
      {
        name = "old-nail";
        tint = Raylib.Color.raywhite;
        scale_x = 1.;
        scale_y = 1.;
        cooldown_scale = 1.;
      };
  }

let init_party
    (id : ghost_id)
    (head_textures : ghost_head_textures)
    (idle_texture : texture)
    ~(start_pos : vector)
    (in_party : bool) : party_ghost =
  let w, h = get_scaled_texture_size Config.scale.ghost idle_texture in
  let dest = { pos = start_pos; w; h } in
  let entity =
    let entity' =
      Entity.create_for_sprite
        (Sprite.create (fmt "ghost-%s" (Show.ghost_id id)) ~collision:(Some DEST) idle_texture dest)
        {
          pos = clone_vector start_pos;
          w = Config.ghost.width *. Config.scale.ghost;
          h = Config.ghost.height *. Config.scale.ghost;
        }
    in
    Entity.hide entity';
    entity'
  in
  {
    ghost = { id; head = head_textures.idle; head_textures; entity; hardfall_timer = None };
    in_party;
  }
