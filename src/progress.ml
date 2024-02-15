open Utils
open Types

(* TODO this won't be consistent if a game is started with one fps, and loaded with a different one
   - need to convert and save hours/minutes/seconds/ms to save file, then use current fps to count up
*)
let get_total_game_time (frames : int) : string =
  let ms' =
    fmt "%.3f"
      ((frames mod Config.window.fps |> Int.to_float) /. (Config.window.fps |> Int.to_float))
  in
  let ms = String.sub ms' 1 (String.length ms' - 1) in
  let seconds' = frames / Config.window.fps in
  let seconds = seconds' mod 60 in
  let minutes' = seconds' / 60 in
  let minutes = minutes' mod 60 in
  let hours' = minutes' / 60 in
  let hours = hours' mod 60 in
  fmt "%02d:%02d:%02d%s" hours minutes seconds ms

type row = {
  name : string;
  suffix : string;
  found : int;
  total : int;
  (* this is total percentage, not per item *)
  percentage : float;
}

type t = {
  weapons : row;
  dreamer_items : row;
  keys : row;
  abilities : row;
  purple_pens : row;
  total : float;
}

let count_lore state prefix =
  let lore = state.global.lore |> String.Map.to_list in
  List.filter (fun (name, _) -> String.starts_with ~prefix:(fmt "%s:" prefix) name) lore
  |> List.length

let get_row (state : state) (game : game) ?(suffix = "") name : row =
  let weapon_percent = 18. in
  let dreamer_item_percent = 24. in
  let key_percent = 15. in
  let ability_percent = 20. in
  let purple_pen_percent =
    100. -. (weapon_percent +. dreamer_item_percent +. key_percent +. ability_percent)
  in
  let found, total, percentage =
    match name with
    | "Weapons" ->
      ( game.player.weapons |> String.Map.to_list |> List.length,
        state.global.weapons |> String.Map.to_list |> List.length,
        weapon_percent )
    | "Dreamer Items" ->
      (game.progress.dreamer_items_found, count_lore state "dreamer", dreamer_item_percent)
    | "Keys" -> (List.length game.progress.keys_found, count_lore state "key", key_percent)
    | "Abilities" ->
      ( [
          game.player.abilities.vengeful_spirit;
          game.player.abilities.desolate_dive;
          game.player.abilities.howling_wraiths;
          game.player.abilities.shade_soul;
          game.player.abilities.descending_dark;
          game.player.abilities.abyss_shriek;
          game.player.abilities.mothwing_cloak;
          game.player.abilities.shade_cloak;
          game.player.abilities.mantis_claw;
          game.player.abilities.crystal_heart;
          game.player.abilities.monarch_wings;
          game.player.abilities.ismas_tear;
          game.player.abilities.dream_nail;
        ]
        |> List.filter (fun a -> a)
        |> List.length,
        count_lore state "ability",
        ability_percent )
    | "Purple Pens" ->
      ( List.length game.progress.purple_pens_found,
        count_lore state "purple-pen",
        purple_pen_percent )
    | _ -> failwith "no"
  in
  { name; suffix; found; total; percentage }

let get_all_progress state game : t =
  let damage = get_nail_damage game.player in
  let weapons = get_row state game ~suffix:(fmt "  (damage %d)" damage) "Weapons" in
  let dreamer_items = get_row state game "Dreamer Items" in
  let keys = get_row state game "Keys" in
  let abilities = get_row state game "Abilities" in
  let purple_pens = get_row state game "Purple Pens" in
  let total =
    let percentage row =
      (row.found |> Int.to_float) /. (row.total |> Int.to_float) *. row.percentage
    in
    percentage weapons
    +. percentage keys
    +. percentage abilities
    +. percentage dreamer_items
    +. percentage purple_pens
  in
  { weapons; dreamer_items; keys; abilities; purple_pens; total }
