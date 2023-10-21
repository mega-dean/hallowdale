open Types

(* TODO prefix these with K or KEY_ so the names don't collide with type direction *)
type key_action =
  (* actions *)
  | CAST
  | C_DASH
  | DASH
  | D_NAIL
  | FOCUS
  | INTERACT
  | JUMP
  | NAIL
  | PAUSE
  (* directions *)
  | ARROW of direction
  (* debug *)
  | DEBUG_UP
  | DEBUG_DOWN
  | DEBUG_LEFT
  | DEBUG_RIGHT
  | DEBUG_1
  | DEBUG_2
  | DEBUG_3
  | DEBUG_4
  | DEBUG_5

let show_key_action k =
  match k with
  (* actions *)
  | CAST -> "CAST"
  | C_DASH -> "C_DASH"
  | DASH -> "DASH"
  | D_NAIL -> "D_NAIL"
  | FOCUS -> "FOCUS"
  | INTERACT -> "INTERACT"
  | JUMP -> "JUMP"
  | NAIL -> "NAIL"
  | PAUSE -> "PAUSE"
  (* directions *)
  | ARROW direction -> fmt "ARROW (%s)" (Show.direction direction)
  (* debug *)
  | DEBUG_1 -> "DEBUG_1"
  | DEBUG_2 -> "DEBUG_2"
  | DEBUG_3 -> "DEBUG_3"
  | DEBUG_4 -> "DEBUG_4"
  | DEBUG_5 -> "DEBUG_5"
  | DEBUG_UP -> "DEBUG_UP"
  | DEBUG_DOWN -> "DEBUG_DOWN"
  | DEBUG_LEFT -> "DEBUG_LEFT"
  | DEBUG_RIGHT -> "DEBUG_RIGHT"

(* FIXME need some way to re-bind these after program start *)
let override_keybinds : (key_action * Raylib.Key.t) list =
  let keybinds_json : (string * string) list =
    try File.read_config "key_overrides" Json_j.keybinds_file_of_string with
    | Sys_error _ -> []
  in
  let override (action_name, key_name) =
    let action =
      match action_name with
      (* actions *)
      | "CAST" -> CAST
      | "C_DASH" -> C_DASH
      | "DASH" -> DASH
      | "D_NAIL" -> D_NAIL
      | "FOCUS" -> FOCUS
      | "INTERACT" -> INTERACT
      | "PAUSE" -> PAUSE
      | "JUMP" -> JUMP
      | "NAIL" -> NAIL
      (* directions *)
      | "UP" -> ARROW UP
      | "DOWN" -> ARROW DOWN
      | "LEFT" -> ARROW LEFT
      | "RIGHT" -> ARROW RIGHT
      (* debug *)
      | "DEBUG_1" -> DEBUG_1
      | "DEBUG_2" -> DEBUG_2
      | "DEBUG_3" -> DEBUG_3
      | "DEBUG_4" -> DEBUG_4
      | "DEBUG_5" -> DEBUG_5
      | "DEBUG_UP" -> DEBUG_UP
      | "DEBUG_DOWN" -> DEBUG_DOWN
      | "DEBUG_LEFT" -> DEBUG_LEFT
      | "DEBUG_RIGHT" -> DEBUG_RIGHT
      | _ -> failwithf "bad override action_name: %s" action_name
    in
    let key =
      match key_name with
      | "[up]" -> Raylib.Key.Up
      | "[down]" -> Raylib.Key.Down
      | "[left]" -> Raylib.Key.Left
      | "[right]" -> Raylib.Key.Right
      | "[tab]" -> Raylib.Key.Tab
      | "A" -> Raylib.Key.A
      | "B" -> Raylib.Key.B
      | "C" -> Raylib.Key.C
      | "D" -> Raylib.Key.D
      | "E" -> Raylib.Key.E
      | "F" -> Raylib.Key.F
      | "G" -> Raylib.Key.G
      | "H" -> Raylib.Key.H
      | "I" -> Raylib.Key.I
      | "J" -> Raylib.Key.J
      | "K" -> Raylib.Key.K
      | "L" -> Raylib.Key.L
      | "M" -> Raylib.Key.M
      | "N" -> Raylib.Key.N
      | "O" -> Raylib.Key.O
      | "P" -> Raylib.Key.P
      | "Q" -> Raylib.Key.Q
      | "R" -> Raylib.Key.R
      | "S" -> Raylib.Key.S
      | "T" -> Raylib.Key.T
      | "U" -> Raylib.Key.U
      | "V" -> Raylib.Key.V
      | "W" -> Raylib.Key.W
      | "X" -> Raylib.Key.X
      | "Y" -> Raylib.Key.Y
      | "Z" -> Raylib.Key.Z
      | "0" -> Raylib.Key.Zero
      | "1" -> Raylib.Key.One
      | "2" -> Raylib.Key.Two
      | "3" -> Raylib.Key.Three
      | "4" -> Raylib.Key.Four
      | "5" -> Raylib.Key.Five
      | "6" -> Raylib.Key.Six
      | "7" -> Raylib.Key.Seven
      | "8" -> Raylib.Key.Eight
      | "9" -> Raylib.Key.Nine
      | "/" -> Raylib.Key.Slash
      | ";" -> Raylib.Key.Semicolon
      | "{" -> Raylib.Key.Left_bracket
      | "}" -> Raylib.Key.Right_bracket
      | "`" -> Raylib.Key.Grave
      | "'" -> Raylib.Key.Apostrophe
      | _ -> failwithf "bad override key_name: %s" key_name
    in
    (action, key)
  in
  List.map override keybinds_json

let default_keybinds : (key_action * Raylib.Key.t) list =
  [
    (* actions *)
    (JUMP, Raylib.Key.Z);
    (NAIL, Raylib.Key.X);
    (DASH, Raylib.Key.C);
    (FOCUS, Raylib.Key.A);
    (C_DASH, Raylib.Key.S);
    (D_NAIL, Raylib.Key.D);
    (CAST, Raylib.Key.F);
    (INTERACT, Raylib.Key.Left_shift);
    (PAUSE, Raylib.Key.Space);
    (* directions *)
    (ARROW UP, Raylib.Key.Up);
    (ARROW DOWN, Raylib.Key.Down);
    (ARROW LEFT, Raylib.Key.Left);
    (ARROW RIGHT, Raylib.Key.Right);
    (* debug *)
    (DEBUG_1, Raylib.Key.One);
    (DEBUG_2, Raylib.Key.Two);
    (DEBUG_3, Raylib.Key.Three);
    (DEBUG_4, Raylib.Key.Four);
    (DEBUG_5, Raylib.Key.Five);
    (DEBUG_UP, Raylib.Key.I);
    (DEBUG_LEFT, Raylib.Key.J);
    (DEBUG_DOWN, Raylib.Key.K);
    (DEBUG_RIGHT, Raylib.Key.L);
  ]

let override_gamepad_buttons : (key_action * Raylib.GamepadButton.t) list =
  let keybinds_json : (string * string) list =
    try File.read_config "button_overrides" Json_j.keybinds_file_of_string with
    | Sys_error _ -> []
  in
  let override (action_name, key_name) =
    let action =
      match action_name with
      (* actions *)
      | "CAST" -> CAST
      | "C_DASH" -> C_DASH
      | "DASH" -> DASH
      | "D_NAIL" -> D_NAIL
      | "FOCUS" -> FOCUS
      | "INTERACT" -> INTERACT
      | "PAUSE" -> PAUSE
      | "JUMP" -> JUMP
      | "NAIL" -> NAIL
      (* directions *)
      | "UP" -> ARROW UP
      | "DOWN" -> ARROW DOWN
      | "LEFT" -> ARROW LEFT
      | "RIGHT" -> ARROW RIGHT
      (* debug *)
      | "DEBUG_1" -> DEBUG_1
      | "DEBUG_2" -> DEBUG_2
      | "DEBUG_3" -> DEBUG_3
      | "DEBUG_4" -> DEBUG_4
      | "DEBUG_5" -> DEBUG_5
      | "DEBUG_UP" -> DEBUG_UP
      | "DEBUG_DOWN" -> DEBUG_DOWN
      | "DEBUG_LEFT" -> DEBUG_LEFT
      | "DEBUG_RIGHT" -> DEBUG_RIGHT
      | _ -> failwithf "bad override action_name: %s" action_name
    in
    let key =
      match key_name with
      | "[up]" -> Raylib.GamepadButton.Left_face_up
      | "[down]" -> Raylib.GamepadButton.Left_face_down
      | "[left]" -> Raylib.GamepadButton.Left_face_left
      | "[right]" -> Raylib.GamepadButton.Left_face_right
      | "x" -> Raylib.GamepadButton.Right_face_down
      | "square" -> Raylib.GamepadButton.Right_face_left
      | "circle" -> Raylib.GamepadButton.Right_face_right
      | "triangle" -> Raylib.GamepadButton.Right_face_up
      | "r1" -> Raylib.GamepadButton.Right_trigger_1
      | "r2" -> Raylib.GamepadButton.Right_trigger_2
      | "l1" -> Raylib.GamepadButton.Left_trigger_1
      | "l2" -> Raylib.GamepadButton.Left_trigger_1
      | _ -> failwithf "bad override gamepad button name: %s" key_name
    in
    (action, key)
  in
  List.map override keybinds_json

let default_gamepad_buttons : (key_action * Raylib.GamepadButton.t) list =
  [
    (* actions *)
    (JUMP, Raylib.GamepadButton.Right_face_down);
    (NAIL, Raylib.GamepadButton.Right_face_left);
    (DASH, Raylib.GamepadButton.Right_trigger_1);
    (FOCUS, Raylib.GamepadButton.Right_thumb);
    (C_DASH, Raylib.GamepadButton.Left_trigger_1);
    (D_NAIL, Raylib.GamepadButton.Right_face_up);
    (CAST, Raylib.GamepadButton.Right_face_right);
    (INTERACT, Raylib.GamepadButton.Right_trigger_2);
    (PAUSE, Raylib.GamepadButton.Middle_right);
    (* directions *)
    (ARROW UP, Raylib.GamepadButton.Left_face_up);
    (ARROW DOWN, Raylib.GamepadButton.Left_face_down);
    (ARROW LEFT, Raylib.GamepadButton.Left_face_left);
    (ARROW RIGHT, Raylib.GamepadButton.Left_face_right);
    (* debug *)
    (DEBUG_1, Raylib.GamepadButton.Left_thumb);
    (DEBUG_2, Raylib.GamepadButton.Left_thumb);
    (DEBUG_3, Raylib.GamepadButton.Left_thumb);
    (DEBUG_4, Raylib.GamepadButton.Left_thumb);
    (DEBUG_5, Raylib.GamepadButton.Left_thumb);
    (DEBUG_UP, Raylib.GamepadButton.Left_thumb);
    (DEBUG_LEFT, Raylib.GamepadButton.Left_thumb);
    (DEBUG_DOWN, Raylib.GamepadButton.Left_thumb);
    (DEBUG_RIGHT, Raylib.GamepadButton.Left_thumb);
  ]

let get_key k =
  match List.assoc_opt k override_keybinds with
  | Some key -> key
  | None -> List.assoc k default_keybinds

let get_button k =
  match List.assoc_opt k override_gamepad_buttons with
  | Some key -> key
  | None -> List.assoc k default_gamepad_buttons

let holding_shift () =
  Raylib.is_key_down Raylib.Key.Left_shift || Raylib.is_key_down Raylib.Key.Right_shift

let gamepad =
  (* CLEANUP not sure if there is a better choice here
     - raylib example also hardcodes 0
  *)
  0

let check_gamepad_input check_button direction k =
  match (direction, k) with
  | false, _ -> check_button gamepad (get_button k)
  | true, ARROW DOWN ->
    check_button gamepad (get_button k)
    || Raylib.get_gamepad_axis_movement gamepad Raylib.GamepadAxis.Left_y > 0.5
  | true, ARROW UP ->
    check_button gamepad (get_button k)
    || Raylib.get_gamepad_axis_movement gamepad Raylib.GamepadAxis.Left_y < -0.5
  | true, ARROW RIGHT ->
    check_button gamepad (get_button k)
    || Raylib.get_gamepad_axis_movement gamepad Raylib.GamepadAxis.Left_x > 0.5
  | true, ARROW LEFT ->
    check_button gamepad (get_button k)
    || Raylib.get_gamepad_axis_movement gamepad Raylib.GamepadAxis.Left_x < -0.5
  | true, _ ->
    failwithf "tried checking direction input for non-ARROW action %s" (show_key_action k)

let key_up k = Raylib.is_key_up (get_key k) && Raylib.is_gamepad_button_up gamepad (get_button k)

let key_down ?(direction = false) k =
  Raylib.is_key_down (get_key k) || check_gamepad_input Raylib.is_gamepad_button_down direction k

let key_pressed ?(direction = false) k =
  Raylib.is_key_pressed (get_key k)
  || check_gamepad_input Raylib.is_gamepad_button_pressed direction k

let key_released ?(direction = false) k =
  Raylib.is_key_released (get_key k)
  || check_gamepad_input Raylib.is_gamepad_button_released direction k
