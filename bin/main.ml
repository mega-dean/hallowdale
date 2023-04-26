open Hallowdale.Types

let rec loop (state : state) =
  state.frame.time <- Raylib.get_time ();
  state.frame.idx <- 1 + state.frame.idx;
  state.frame.dt <- Raylib.get_frame_time ();
  match Raylib.window_should_close () with
  | true ->
    print "closing at: %d" state.frame.idx;
    Raylib.close_window ()
  | false ->
    if Hallowdale.Controls.key_pressed DEBUG_5 then
      state.debug.enabled <- not state.debug.enabled;
    state |> Hallowdale.State.tick |> Hallowdale.Render.tick |> loop

let () =
  Raylib.set_config_flags [ Raylib.ConfigFlags.Window_maximized ];
  Raylib.init_window Hallowdale.Config.window.width Hallowdale.Config.window.height "hallowdale";
  Raylib.set_target_fps Hallowdale.Config.window.fps;
  Random.self_init ();
  Hallowdale.State.init () |> loop
