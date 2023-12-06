open Utils
open Types

let parse_name context name : npc_id =
  match name with
  | "ANNIES_BOOBS" -> ANNIES_BOOBS
  | "SHIRLEY" -> SHIRLEY
  | "NEIL" -> NEIL
  | "CHANG" -> CHANG
  | "HICKEY" -> HICKEY
  | "VICKI" -> VICKI
  | "LEONARD" -> LEONARD
  | "GARRETT" -> GARRETT
  | "JERRY" -> JERRY
  | "BLACKSMITH_WIFE" -> BLACKSMITH_WIFE
  | "HILDA" -> HILDA
  | "FRANKIE" -> FRANKIE
  | "HUMAN_BEING" -> HUMAN_BEING
  | "POTTERY_TEACHER" -> POTTERY_TEACHER
  | _ -> failwithf "Npc.parse_name: found unrecognized npc name '%s' in %s" name context

let create_from_rects
    (npc_rects : (npc_id * rect * bool) list)
    (npc_configs : (npc_id * Json_t.npc_config) list) =
  let build_npc_from_rect ((npc_id, dest, facing_right) : npc_id * rect * bool) : npc =
    let npc_name = Show.npc_id npc_id in
    let npc_config : Json_t.npc_config =
      match List.assoc_opt npc_id npc_configs with
      | None -> failwithf "missing npc config for %s" npc_name
      | Some config -> config
    in
    let w, h =
      ( (npc_config.w |> Int.to_float) *. Config.scale.ghost,
        (npc_config.h |> Int.to_float) *. Config.scale.ghost )
    in
    let texture_configs : texture_config list =
      List.map (Entity.to_texture_config NPCS npc_name) npc_config.texture_configs
    in
    let entity, textures = Entity.create_from_texture_configs texture_configs { dest with w; h } in

    (* TODO create_from_textures could also take a facing_right arg *)
    entity.sprite.facing_right <- facing_right;

    { id = npc_id; entity; textures = textures |> List.to_string_map }
  in

  List.map build_npc_from_rect npc_rects
