open Utils
open Types

let tick (game : game) (state : state) =
  let trigger_config : trigger option =
    Player.find_trigger_collision game.player game.room.triggers.camera
  in
  let subject =
    match state.camera.subject with
    | GHOST ->
      let e = game.player.ghost.entity in
      let offset = (* TODO move to config *) 50. in
      if e.sprite.facing_right then
        { e.dest.pos with x = e.dest.pos.x +. offset }
      else
        { e.dest.pos with x = e.dest.pos.x -. offset }
    | FIXED pos -> pos
  in
  let new_camera =
    let current = Raylib.Camera2D.target state.camera.raylib in
    let camera_state =
      {
        current_pos = { x = Raylib.Vector2.x current; y = Raylib.Vector2.y current };
        subject;
        room_bounds = game.room.camera_bounds;
      }
    in
    let bounded_target () =
      let subject = camera_state.subject in
      let target_x, target_y =
        match trigger_config with
        | None -> (subject.x, subject.y)
        | Some trigger ->
          let x_config, y_config =
            match trigger.kind with
            | CAMERA (x, y) -> (x, y)
            | _ -> failwith "camera trigger needs CAMERA kind"
          in
          let x_bound =
            let left = trigger.dest.pos.x +. Config.window.center.x in
            let right = trigger.dest.pos.x +. trigger.dest.w -. Config.window.center.x in
            match x_config with
            | "gx" -> subject.x
            | ">x" -> Float.bound left subject.x camera_state.room_bounds.max.x
            | "<x" -> Float.bound camera_state.room_bounds.min.x subject.x right
            | "x" -> Float.bound left subject.x right
            | _ -> failwithf "invalid x_bound %s" x_config
          in
          let y_bound =
            let top = trigger.dest.pos.y +. Config.window.center.y in
            let bottom = trigger.dest.pos.y +. trigger.dest.h -. Config.window.center.y in
            match y_config with
            | "gy" -> subject.y
            | ">y" -> Float.bound top subject.y camera_state.room_bounds.max.y
            | "<y" -> Float.bound camera_state.room_bounds.min.y subject.y bottom
            | "y" -> Float.bound top subject.y bottom
            | _ -> failwithf "invalid y_bound %s" y_config
          in
          (x_bound, y_bound)
      in
      let current_x, current_y = (camera_state.current_pos.x, camera_state.current_pos.y) in
      let between amount a b =
        (* amount = 1.0 returns the midpoint, larger amount is closer to `a` *)
        ((amount *. a) +. b) /. (amount +. 1.)
      in
      let diff_x, diff_y = (abs_float (current_x -. target_x), abs_float (current_y -. target_y)) in
      let create_bounded_camera x y =
        Raylib.Vector2.create
          (Float.bound camera_state.room_bounds.min.x x camera_state.room_bounds.max.x)
          (Float.bound camera_state.room_bounds.min.y y camera_state.room_bounds.max.y)
      in
      if state.camera.update_instantly then (
        state.camera.update_instantly <- false;
        create_bounded_camera target_x target_y)
      else (
        (* TODO adjust camera motion for fps *)
        let smooth_x, smooth_y =
          match state.camera.motion with
          | LINEAR speed ->
            let between' speed current target =
              let eps = current -. target in
              if abs_float eps < speed then
                target
              else if eps < speed then
                current +. speed
              else if eps > speed then
                current -. speed
              else
                target
            in
            (between' speed current_x target_x, between' speed current_y target_y)
          | SMOOTH (x_speed, y_speed) ->
            let smooth_x =
              if diff_x > 1. then
                between x_speed current_x target_x
              else
                target_x
            in
            let smooth_y =
              if diff_y > 1. then
                between y_speed current_y target_y
              else
                target_y
            in
            (smooth_x, smooth_y)
        in

        create_bounded_camera smooth_x smooth_y)
    in
    Tiled.create_camera_at (bounded_target ()) state.camera.shake Config.window.center.x
      Config.window.center.y
  in
  state.camera.raylib <- new_camera;
  if state.camera.shake > 0. then
    state.camera.shake <- state.camera.shake -. state.frame.dt;
  state
