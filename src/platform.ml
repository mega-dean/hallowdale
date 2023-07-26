open Types

[@@@ocaml.warning "-26-27-32"]

let get_spikes (platform : platform) game =
  match List.assoc_opt platform.id game.room.platform_spikes with
  | None -> failwith "rotatable platform needs spikes"
  | Some s -> s

let set_texture (platform : platform) texture_cache name =
  let texture =
    match List.assoc_opt name texture_cache.platforms with
    | None -> failwithf "could not find texture with name %s" name
    | Some t -> t
  in
  platform.sprite.texture <- texture

let spikes_rotation_dy =
  (* this is specific to the rotating c-heart spikes (would have to look this up from texture.h to do it generically) *)
  70.

let start_rotating platform game texture_cache =
  let spikes = get_spikes platform game in
  spikes.pos.y <- spikes.pos.y -. spikes_rotation_dy;
  platform.sprite.texture <- texture_cache.rotating_platform;
  platform.kind <- Some (ROTATABLE ROTATING_NOW)

let finish_rotating platform game texture_cache =
  let spikes = get_spikes platform game in
  set_texture platform texture_cache "rotatable-upside-down";
  (* CLEANUP this duration should come from a config *)
  platform.kind <- Some (ROTATABLE (UPSIDE_DOWN 2.))

let reset_rotation platform game texture_cache =
  let spikes = get_spikes platform game in
  spikes.pos.y <- spikes.pos.y +. spikes_rotation_dy;
  set_texture platform texture_cache "rotatable";
  platform.kind <- Some (ROTATABLE UPRIGHT)
