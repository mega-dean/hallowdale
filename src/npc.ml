open Utils
open Types

let parse_name context name : npc_id =
  match name with
  | "ANNIES_BOOBS" -> ANNIES_BOOBS
  | "BLACKSMITH_WIFE" -> BLACKSMITH_WIFE
  | "CHANG" -> CHANG
  | "FRANKIE" -> FRANKIE
  | "GARRETT" -> GARRETT
  | "GILBERT" -> GILBERT
  | "HILDA" -> HILDA
  | "HUMAN_BEING" -> HUMAN_BEING
  | "JERRY" -> JERRY
  | "LEONARD" -> LEONARD
  | "LESLIE" -> LESLIE
  | "MIKE" -> MIKE
  | "NEIL" -> NEIL
  | "OLD_MAN" -> OLD_MAN
  | "POTTERY_TEACHER" -> POTTERY_TEACHER
  | "RICH" -> RICH
  | "SHIRLEY" -> SHIRLEY
  | "SOPHIE_B" -> SOPHIE_B
  | "TOWNSPERSON" -> TOWNSPERSON
  | "TOWN_LADY" -> TOWN_LADY
  | "TROY_AND_ABED_IN_A_BUBBLE" -> TROY_AND_ABED_IN_A_BUBBLE
  | "VAUGHN" -> VAUGHN
  | "VICKI" -> VICKI
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
      ( (npc_config.w |> Int.to_float) *. Config.scale.npc,
        (npc_config.h |> Int.to_float) *. Config.scale.npc )
    in
    let pos = align_to_bottom dest w h in
    let texture_configs : texture_config ne_list =
      List.map (Entity.to_texture_config NPCS npc_name) npc_config.texture_configs
      |> List.to_ne_list
    in
    let entity, textures =
      Entity.create_from_texture_configs
        (Entity.scale_texture_configs Config.scale.npc texture_configs)
        { pos; w; h }
    in

    (* TODO create_from_textures could also take a facing_right arg *)
    entity.sprite.facing_right <- facing_right;

    { id = npc_id; entity; textures = textures |> List.Non_empty.to_list |> List.to_string_map }
  in

  List.map build_npc_from_rect npc_rects
