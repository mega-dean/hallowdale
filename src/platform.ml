open Utils
open Types

let get_spikes (platform : platform) game =
  match String.Map.find_opt platform.id game.room.platform_spikes with
  | None -> failwith "rotatable platform needs spikes"
  | Some s -> s

let set_texture (platform : platform) texture_cache name =
  let texture =
    match String.Map.find_opt name texture_cache.platforms with
    | None -> failwithf "could not find texture with name %s" name
    | Some t -> t
  in
  platform.sprite.texture <- texture

let start_rotating (platform : platform) game texture_cache =
  platform.sprite.texture <- texture_cache.rotating_platform;
  platform.kind <- Some (ROTATABLE ROTATING_NOW)

let finish_rotating platform game texture_cache =
  let spikes = get_spikes platform game in
  spikes.pos.y <- spikes.pos.y -. Config.platform.rotatable_spikes_dy;
  set_texture platform texture_cache "rotatable-upside-down";
  platform.kind <- Some (ROTATABLE (UPSIDE_DOWN Config.platform.rotatable_upside_down_time))

let reset_rotation platform game texture_cache =
  let spikes = get_spikes platform game in
  spikes.pos.y <- spikes.pos.y +. Config.platform.rotatable_spikes_dy;
  platform.sprite.dest.pos.x <- platform.original_x;
  set_texture platform texture_cache "rotatable";
  platform.kind <- Some (ROTATABLE UPRIGHT)
