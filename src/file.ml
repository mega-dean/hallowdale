open Utils

let make_path (segments : string list) : string = String.concat Filename.dir_sep segments
let make_root_path (segments : string list) : string = make_path (Env.project_root :: segments)
let make_assets_path (segments : string list) : string = make_root_path ("assets" :: segments)
let save_file_path idx = make_root_path [ "saves"; fmt "%d.json" idx ]
let mkdir (dir : string) : unit = Sys.mkdir dir 0o775
let ls (dir : string) : string list = Sys.readdir dir |> Array.to_list

let read (filename : string) : string =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* this can be used for files that might not exist *)
let maybe_read (filename : string) : string option =
  if Sys.file_exists filename then
    Some (read filename)
  else
    None

let write (filename : string) (contents : string) =
  let ch = open_out filename in
  let s = Printf.fprintf ch "%s\n" contents in
  close_out ch

let read_config file_name (convert : string -> 'a) : 'a =
  let full_path = make_root_path [ "config"; Printf.sprintf "%s.json" file_name ] in
  read full_path |> convert

let delete_save idx =
  let path = save_file_path idx in
  let new_path =
    let saves_dir = Filename.dirname path in
    let save_file = Filename.basename path in
    fmt "%s%sdeleted-%d_%s" saves_dir Filename.dir_sep (Unix.time () |> Float.to_int) save_file
  in
  Sys.rename path new_path

let copy_file src dest =
  let contents = read src in
  write dest contents

let delete_bindings filename =
  let path = make_root_path [ "config"; filename ] in
  let blank_file = make_root_path [ "config"; "blank-config.json" ] in
  if Sys.file_exists path then (
    let new_path =
      make_root_path [ "config"; fmt "deleted-%d_%s" (Unix.time () |> Float.to_int) filename ]
    in
    Sys.rename path new_path;
    copy_file blank_file path
  )
  else
    failwithf "could not find bindings file %s to remove" path

let delete_keyboard_bindings () = delete_bindings "key_overrides.json"
let delete_gamepad_bindings () = delete_bindings "button_overrides.json"
