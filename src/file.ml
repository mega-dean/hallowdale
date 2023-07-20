open Types

[@@@ocaml.warning "-26-27-32"]

let convert_path path_with_slashes =
  let filename_parts = String.split_on_char '/' path_with_slashes in
  List.fold_left Filename.concat "" filename_parts

let ls (dir : string) : string list =
  Sys.readdir (convert_path dir) |> Array.to_list

let read (filename : string) : string =
  let filename' = convert_path filename in
  let ch = open_in_bin filename' in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* this can be used for files that might not exist *)
let maybe_read (filename : string) : string option =
  if Sys.file_exists filename then
    Some (read filename)
  else
    None

let write (filename : string) (contents : string) : bool =
  let filename' = convert_path filename in
  let ch = open_out filename' in
  let s = Printf.fprintf ch "%s\n" contents in
  close_out ch;
  true

let read_config file_name (convert : string -> 'a) : 'a =
  let full_path = fmt "../config/%s.json" file_name |> convert_path in
  read full_path |> convert
