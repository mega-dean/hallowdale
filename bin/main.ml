open Hallowdale.Utils
open Hallowdale.Types

let rec loop (state : Hallowdale.Types.state) =
  state.frame.time <- Raylib.get_time ();
  state.frame.idx <- 1 + state.frame.idx;
  state.frame.dt <- Raylib.get_frame_time ();

  if Raylib.window_should_close () then (
    print "closing at: %d" state.frame.idx;
    Raylib.close_audio_device ();
    Raylib.close_window ())
  else
    state |> Hallowdale.State.tick |> Hallowdale.Render.tick |> loop

let () =
  Raylib.set_window_size
    (Hallowdale.Config.window.w |> Float.to_int)
    (Hallowdale.Config.window.h |> Float.to_int);
  (* Raylib.set_window_min_size
   *   (Hallowdale.Config.window.min_width |> Float.to_int)
   *   (Hallowdale.Config.window.min_height |> Float.to_int); *)
  Raylib.init_audio_device ();
  Raylib.set_exit_key Raylib.Key.Backspace;

  if Hallowdale.Env.development then
    Raylib.set_window_position 340 400
  else (
    Sys.chdir (Filename.dirname Sys.executable_name);
    Raylib.set_window_position 0 0);

  Raylib.set_target_fps Hallowdale.Config.window.fps;
  Random.self_init ();
  Hallowdale.State.init Hallowdale.Config.window.w Hallowdale.Config.window.h
    Hallowdale.Config.window.scale
  |> loop
