open Hallowdale.Types

let rec loop (state : state) =
  state.frame.time <- Raylib.get_time ();
  state.frame.idx <- 1 + state.frame.idx;
  state.frame.dt <- Raylib.get_frame_time ();
  if Raylib.window_should_close () then (
    print "closing at: %d" state.frame.idx;
    Raylib.close_window ())
  else
    state |> Hallowdale.State.tick |> Hallowdale.Render.tick |> loop

let () =
  Raylib.set_config_flags [ Raylib.ConfigFlags.Window_maximized ];
  Raylib.init_window Hallowdale.Config.window.width Hallowdale.Config.window.height "hallowdale";
  Raylib.set_window_position 200 200;
  Raylib.set_target_fps Hallowdale.Config.window.fps;
  Random.self_init ();
  Hallowdale.State.init () |> loop
