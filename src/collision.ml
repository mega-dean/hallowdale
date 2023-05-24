open Types

[@@@ocaml.warning "-26-27-32"]

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
    let sort (points : vector list) = List.sort (fun p1 p2 -> compare p1.x p2.x) points in
    let projected_from_s1 : vector list = List.map (project_point_onto_line normal) (get_points s1) |> sort in
    let projected_from_s2 : vector list = List.map (project_point_onto_line normal) (get_points s2) |> sort in
    let first xs = List.nth xs 0 in
    let last xs = List.nth xs (List.length xs - 1) in
    let has_separating_axis =
      if (first projected_from_s1).x > (first projected_from_s2).x then
        (first projected_from_s1).x > (last projected_from_s2).x
      else if (first projected_from_s1).x < (first projected_from_s2).x then
        (last projected_from_s1).x < (first projected_from_s2).x
      else
        false
    in
    if has_separating_axis then Some normal else None
  in
  (* this returns the normal that is the separating axis *)
  let edge_with_separating_axis (shape : shape) : line option = List.find_map edge_has_separating_axis shape.edges in
  Option.is_none (List.find_map edge_with_separating_axis [ s1; s2 ])

(* direction here means "the direction that the entity is (probably) colliding from" *)
(* FIXME-3 update uses of this to also check collision shape when needed *)
let between_rects (entity : entity) (r2 : rect) : collision option =
  let cr : rect = Raylib.get_collision_rec (to_Rect entity.dest) (to_Rect r2) |> of_Rect in
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
             if entity.v.y >= 0. then
               UP
             else
               DOWN
           | false, false, true, true -> if entity.v.x >= 0. then LEFT else RIGHT
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
             match (entity.v.x > 0., entity.v.y > 0., entity.v.x > entity.v.y) with
             | true, true, true -> LEFT
             | true, true, false -> UP
             | true, false, true -> LEFT
             | true, false, false -> DOWN
             | false, true, true -> RIGHT
             | false, true, false -> UP
             | false, false, true -> RIGHT
             | false, false, false -> DOWN));
      }

