open Hallowdale
open Utils
open Types

let rec loop (state : Types.state) =
  state.frame.time <- Raylib.get_time ();
  state.frame.idx <- 1 + state.frame.idx;
  state.frame.dt <- Raylib.get_frame_time ();

  if Raylib.window_should_close () || state.frame.idx > state.frame.timeout then (
    print "closing at: %d" state.frame.idx;
    Raylib.close_audio_device ();
    Raylib.close_window ())
  else
    state |> State.tick |> Render.tick |> loop

let () =
  Raylib.set_window_size (Config.window.w |> Float.to_int) (Config.window.h |> Float.to_int);
  (* Raylib.set_window_min_size
   *   (Config.window.min_width |> Float.to_int)
   *   (Config.window.min_height |> Float.to_int); *)
  Raylib.init_audio_device ();
  Raylib.set_exit_key Raylib.Key.Null;

  if Env.development then
    Raylib.set_window_position 340 400
  else (
    Sys.chdir (Filename.dirname Sys.executable_name);
    Raylib.set_window_position 0 0);

  Raylib.set_target_fps Config.window.fps;
  Random.self_init ();
  File.maybe_create_saves_dir ();
  State.init () |> loop
