open Hallowdale.Types

[@@@ocaml.warning "-26-27-32"]

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
  Raylib.init_window Hallowdale.Config.window.width Hallowdale.Config.window.height "hallowdale";
  Raylib.init_audio_device ();
  Raylib.set_exit_key Raylib.Key.Backspace;
  Raylib.set_window_position 340 400;
  Raylib.set_target_fps Hallowdale.Config.window.fps;
  Random.self_init ();
  Hallowdale.State.init () |> loop
