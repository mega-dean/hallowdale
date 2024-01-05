open Utils
open Types

let get_normal (line : line) : line =
  if line.b = 0. then
    { a = 0.; b = 1.; c = 0. }
  else if line.a = 0. then
    { a = 1.; b = 0.; c = 0. }
  else (
    let slope = -1. *. (line.a /. line.b) in
    { a = 1. /. slope; b = 1.; c = 0. })

(* from https://math.stackexchange.com/a/4604660 *)
let project_point_onto_line (line : line) (point : vector) : vector =
  let scale (point : vector) (a : float) : vector = { x = a *. point.x; y = a *. point.y } in
  let add_points (p1 : vector) (p2 : vector) = { x = p1.x +. p2.x; y = p1.y +. p2.y } in
  let subtract_points (p1 : vector) (p2 : vector) = { x = p1.x -. p2.x; y = p1.y -. p2.y } in
  let dot (p1 : vector) (p2 : vector) : float = (p1.x *. p2.x) +. (p1.y *. p2.y) in
  let p0 =
    if line.b = 0. then (
      if line.a = 0. then
        failwith "cannot project onto zero vector";
      { x = -1. *. (line.c /. line.a); y = 0. })
    else
      { x = 0.; y = -1. *. (line.c /. line.b) }
  in
  let (v : vector) =
    if line.a = 0. then
      { x = 1.; y = 0. }
    else if line.b = 0. then
      { x = 0.; y = 1. }
    else
      { x = line.b; y = -1. *. line.a }
  in
  let s : float = dot (subtract_points point p0) v /. dot v v in
  let scaled_v = scale v s in
  let projected = add_points p0 scaled_v in
  if (line.a *. projected.x) +. (line.b *. projected.y) +. line.c > 0.001 then
    failwithf "bad projection: %f" ((line.a *. projected.x) +. (line.b *. projected.y) +. line.c);
  projected

(*
   - for both shapes:
   -   for each edge:
   -     find the normal axis
   -     for each point:
   -       project onto normal
   -     find range of projected points
   -     if no overlap, shapes are not colliding
   -     otherwise continue
*)
let between_shapes (s1 : shape) (s2 : shape) : bool =
  (* return value is true when _not_ overlapping *)
  let edge_has_separating_axis ((point, line) : vector * line) : line option =
    let normal = get_normal line in
    let first xs = List.nth xs 0 in
    let get_projected sort_fn =
      ( List.map (project_point_onto_line normal) (get_points s1) |> sort_fn,
        List.map (project_point_onto_line normal) (get_points s2) |> sort_fn )
    in

    let compare_points' get_coord =
      let sort (points : vector list) =
        List.sort (fun p1 p2 -> compare (get_coord p1) (get_coord p2)) points
      in
      let projected_from_s1, projected_from_s2 =
        ( List.map (project_point_onto_line normal) (get_points s1) |> sort,
          List.map (project_point_onto_line normal) (get_points s2) |> sort )
      in
      let has_separating_axis () =
        if get_coord (first projected_from_s1) > get_coord (first projected_from_s2) then
          get_coord (first projected_from_s1) > get_coord (List.last projected_from_s2)
        else if get_coord (first projected_from_s1) < get_coord (first projected_from_s2) then
          get_coord (List.last projected_from_s1) < get_coord (first projected_from_s2)
        else
          false
      in
      if has_separating_axis () then
        Some normal
      else
        None
    in

    if vertical normal then
      compare_points' (fun v -> v.y)
    else
      compare_points' (fun v -> v.x)
  in
  (* this returns the normal that is the separating axis *)
  let edge_with_separating_axis (shape : shape) : line option =
    List.find_map edge_has_separating_axis shape.edges
  in
  Option.is_none (List.find_map edge_with_separating_axis [ s1; s2 ])

let between_rects (r1 : rect) (r2 : rect) : collision option =
  let overlap =
    Raylib.get_collision_rec (rect_to_Rect r1) (rect_to_Rect r2) |> raylib_Rect_to_rect
  in
  let no_collision = overlap.w < 0.1 || overlap.h < 0.1 in
  if no_collision then
    None
  else
    Some
      {
        center = get_rect_center overlap;
        other_rect = r2;
        collided_from =
          (let feq a b =
             (* Float.equal is too precise
                - was previously comparing to 0.001, but that broke for small window_scale
             *)
             abs_float (a -. b) < 0.1
           in
           let up = feq r2.pos.y overlap.pos.y in
           let down = feq (r2.pos.y +. r2.h) (overlap.pos.y +. overlap.h) in
           let left = feq r2.pos.x overlap.pos.x in
           let right = feq (r2.pos.x +. r2.w) (overlap.pos.x +. overlap.w) in

           match (up, down, left, right) with
           (* one side *)
           | true, false, false, false -> UP
           | false, true, false, false -> DOWN
           | false, false, true, false -> LEFT
           | false, false, false, true -> RIGHT
           (* top-bottom / left-right
              - these are arbitrary directions, because this currently only happens for VS
              - when VS knockback is implemented, these will probably need to be fixed
           *)
           | true, true, false, false -> UP
           | false, false, true, true -> UP
           (* corners *)
           (* TODO this is too "slippery": falling too fast causes top collisions to be considered side collisions
              - can probably fix this by considering position of entity at start of frame (before applying v)
              - this is much more noticeable at 60fps
           *)
           | false, true, true, false -> if overlap.h < overlap.w then DOWN else LEFT
           | true, false, true, false ->
             if overlap.h < overlap.w then
               UP
             else
               LEFT
           | false, true, false, true -> if overlap.h < overlap.w then DOWN else RIGHT
           | true, false, false, true ->
             if overlap.h < overlap.w then
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
           (* ghost is entirely inside collision
              - this happens for interaction triggers, camera bounds, etc., so the direction doesn't matter
           *)
           | false, false, false, false ->
             DOWN);
      }

let with_entity (entity : entity) (r2 : rect) : collision option = between_rects entity.dest r2

(* TODO this (and between_shapes) should probably return a collision option like the other fns in this file *)
let between_entities (entity1 : entity) (entity2 : entity) : bool =
  match with_entity entity1 entity2.dest with
  | None -> false
  | Some c ->
    let get_aligned_shape (entity : entity) =
      match entity.sprite.collision with
      | Some (SHAPE shape) -> align_shape_with_parent_sprite entity.sprite shape
      | Some DEST -> shape_of_rect entity.dest
      | None -> failwith "can't get collision for entity.sprite without collision_shape"
    in
    let shape1 = get_aligned_shape entity1 in
    let shape2 = get_aligned_shape entity2 in
    between_shapes shape1 shape2

let between_slash_and_shape (slash : slash) (shape : shape) (other_rect : rect) : collision option =
  let slash_shape = get_slash_shape slash in
  if between_shapes slash_shape shape then
    Some
      {
        center = get_midpoint slash.sprite.dest other_rect;
        other_rect;
        collided_from = slash.direction;
      }
  else
    None

let between_slash_and_rect (slash : slash) (rect : rect) : collision option =
  between_slash_and_shape slash (shape_of_rect rect) rect

let between_slash_and_sprite (slash : slash) (sprite : sprite) : collision option =
  between_slash_and_shape slash (get_collision_shape sprite) sprite.dest
