open Utils
open Types

let stop_sound' sound =
  if Raylib.is_sound_playing sound then
    Raylib.stop_sound sound

let stop_sound state sound_name =
  let sound = String.Map.find sound_name state.global.sounds in
  stop_sound' sound

let play_sound state sound_name =
  let sound = String.Map.find sound_name state.global.sounds in
  stop_sound' sound;
  if not (Raylib.is_sound_playing sound) then (
    Raylib.set_sound_volume sound state.settings.sound_effects_volume;
    Raylib.play_sound sound)

let play_music music =
  if Raylib.get_music_time_played music.t > music.loop_end.at then
    Raylib.seek_music_stream music.t (Float.bound 0.1 music.loop_start.at Float.max_float);
  Raylib.update_music_stream music.t

let play_continuous_sound state sound_name =
  play_music (String.Map.find sound_name state.global.continuous_sounds)

let get_area_music area_id area_musics = List.find (fun am -> List.mem area_id am.areas) area_musics
let get_new_volume initial change = Float.bound 0. (initial +. change) 1.

let change_music_volume change state game =
  let new_volume = get_new_volume state.settings.music_volume change in
  state.settings.music_volume <- new_volume;
  Raylib.set_music_volume game.music.music.t new_volume

let change_sound_effects_volume change state =
  let new_volume = get_new_volume state.settings.sound_effects_volume change in
  state.settings.sound_effects_volume <- new_volume

let increase_music_volume state game = change_music_volume 0.1 state game
let decrease_music_volume state game = change_music_volume (-0.1) state game
let increase_sound_effects_volume state = change_sound_effects_volume 0.1 state
let decrease_sound_effects_volume state = change_sound_effects_volume (-0.1) state
let play_menu_music state = play_music state.menu_music
let play_game_music game = play_music game.music.music
let stop_music music = Raylib.stop_music_stream music
let reset_music music = Raylib.seek_music_stream music.t 0.

let load_music dir name extension ?(intro = 0.) ?(loop = Float.max_float) music_volume =
  let music =
    Raylib.load_music_stream (File.make_assets_path [ "audio"; dir; fmt "%s.%s" name extension ])
  in
  Raylib.set_music_volume music music_volume;
  Raylib.play_music_stream music;
  { name; t = music; loop_start = { at = intro }; loop_end = { at = loop } }

let load_area_music name ?(intro = 0.) ?(loop = Float.max_float) areas music_volume : area_music =
  let music = load_music "music" name "ogg" ~intro ~loop music_volume in
  { areas; music }

let load_continuous_sound ?(loop = Float.max_float) name music_volume : music =
  load_music "continuous" name "wav" ~intro:0. ~loop music_volume

let load_sound name =
  let path = File.make_assets_path [ "audio"; "sound-effects"; fmt "%s.wav" name ] in
  (name, Raylib.load_sound path)
