open Types

(* TODO prefix these with K or KEY_ so the names don't collide with type direction *)
type game_action =
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
  | OPEN_MAP
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

let all_game_actions =
  [
    CAST;
    C_DASH;
    DASH;
    D_NAIL;
    FOCUS;
    INTERACT;
    JUMP;
    NAIL;
    PAUSE;
    OPEN_MAP;
    ARROW UP;
    ARROW DOWN;
    ARROW LEFT;
    ARROW RIGHT;
  ]

let debug_game_actions =
  [ DEBUG_UP; DEBUG_DOWN; DEBUG_LEFT; DEBUG_RIGHT; DEBUG_1; DEBUG_2; DEBUG_3; DEBUG_4; DEBUG_5 ]

let show_game_action k =
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
  | OPEN_MAP -> "OPEN_MAP"
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

let get_action action_name =
  match action_name with
  (* actions *)
  | "CAST" -> CAST
  | "C_DASH" -> C_DASH
  | "DASH" -> DASH
  | "D_NAIL" -> D_NAIL
  | "FOCUS" -> FOCUS
  | "INTERACT" -> INTERACT
  | "PAUSE" -> PAUSE
  | "OPEN_MAP" -> OPEN_MAP
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

(* TODO need some way to re-bind these after program start *)
let overridden_keybinds : (game_action * Raylib.Key.t) list =
  let keybinds_json : (string * string) list =
    try File.read_config "key_overrides" Json_j.keybinds_file_of_string with
    | Sys_error _ -> []
  in
  let override (action_name, key_name) =
    let key =
      match key_name with
      | "[up]" -> Raylib.Key.Up
      | "[down]" -> Raylib.Key.Down
      | "[left]" -> Raylib.Key.Left
      | "[right]" -> Raylib.Key.Right
      | "[tab]" -> Raylib.Key.Tab
      | "[escape]" -> Raylib.Key.Escape
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
    (get_action action_name, key)
  in
  List.map override keybinds_json

let default_keybinds : (game_action * Raylib.Key.t) list =
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
    (* menus *)
    (OPEN_MAP, Raylib.Key.Tab);
    (PAUSE, Raylib.Key.Escape);
    (* directions *)
    (ARROW UP, Raylib.Key.Up);
    (ARROW DOWN, Raylib.Key.Down);
    (ARROW LEFT, Raylib.Key.Left);
    (ARROW RIGHT, Raylib.Key.Right);
  ]
  (* debug *)
  @
  if Env.development then
    [
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
  else
    [
      (DEBUG_1, Raylib.Key.Null);
      (DEBUG_2, Raylib.Key.Null);
      (DEBUG_3, Raylib.Key.Null);
      (DEBUG_4, Raylib.Key.Null);
      (DEBUG_5, Raylib.Key.Null);
      (DEBUG_UP, Raylib.Key.Null);
      (DEBUG_LEFT, Raylib.Key.Null);
      (DEBUG_DOWN, Raylib.Key.Null);
      (DEBUG_RIGHT, Raylib.Key.Null);
    ]

let overridden_gamepad_buttons : (game_action * Raylib.GamepadButton.t) list =
  let keybinds_json : (string * string) list =
    try File.read_config "button_overrides" Json_j.keybinds_file_of_string with
    | Sys_error _ -> []
  in
  let override (action_name, key_name) =
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
      | "[start]" -> Raylib.GamepadButton.Middle_left
      | "[select]" -> Raylib.GamepadButton.Middle_right
      | _ -> failwithf "bad override gamepad button name: %s" key_name
    in
    (get_action action_name, key)
  in
  List.map override keybinds_json

let default_gamepad_buttons : (game_action * Raylib.GamepadButton.t) list =
  [
    (* actions *)
    (JUMP, Raylib.GamepadButton.Right_face_down);
    (NAIL, Raylib.GamepadButton.Right_face_left);
    (DASH, Raylib.GamepadButton.Right_trigger_2);
    (FOCUS, Raylib.GamepadButton.Right_trigger_1);
    (C_DASH, Raylib.GamepadButton.Left_trigger_2);
    (D_NAIL, Raylib.GamepadButton.Right_face_up);
    (CAST, Raylib.GamepadButton.Right_face_right);
    (INTERACT, Raylib.GamepadButton.Left_trigger_1);
    (* menus *)
    (OPEN_MAP, Raylib.GamepadButton.Middle_left);
    (PAUSE, Raylib.GamepadButton.Middle_right);
    (* directions *)
    (ARROW UP, Raylib.GamepadButton.Left_face_up);
    (ARROW DOWN, Raylib.GamepadButton.Left_face_down);
    (ARROW LEFT, Raylib.GamepadButton.Left_face_left);
    (ARROW RIGHT, Raylib.GamepadButton.Left_face_right);
    (* debug - these aren't supported for gamepads *)
    (DEBUG_1, Raylib.GamepadButton.Unknown);
    (DEBUG_2, Raylib.GamepadButton.Unknown);
    (DEBUG_3, Raylib.GamepadButton.Unknown);
    (DEBUG_4, Raylib.GamepadButton.Unknown);
    (DEBUG_5, Raylib.GamepadButton.Unknown);
    (DEBUG_UP, Raylib.GamepadButton.Unknown);
    (DEBUG_LEFT, Raylib.GamepadButton.Unknown);
    (DEBUG_DOWN, Raylib.GamepadButton.Unknown);
    (DEBUG_RIGHT, Raylib.GamepadButton.Unknown);
  ]

let make_bindings
    (label : string)
    (defaults : (game_action * 'a) list)
    (overrides : (game_action * 'a) list) =
  let build_binding game_action =
    let override_or_default =
      match List.assoc_opt game_action overrides with
      | Some key -> key
      | None -> (
        match List.assoc_opt game_action defaults with
        | Some key -> key
        | None -> failwithf "missing %s binding for %s" label (show_game_action game_action))
    in
    (game_action, override_or_default)
  in
  List.map build_binding (all_game_actions @ debug_game_actions)

let key_bindings : (game_action * Raylib.Key.t) list =
  make_bindings "key" default_keybinds overridden_keybinds

let gamepad_bindings : (game_action * Raylib.GamepadButton.t) list =
  make_bindings "gamepad button" default_gamepad_buttons overridden_gamepad_buttons

let show_key key =
  match key with
  | Raylib.Key.Up -> "[up]"
  | Raylib.Key.Down -> "[down]"
  | Raylib.Key.Left -> "[left]"
  | Raylib.Key.Right -> "[right]"
  | Raylib.Key.Tab -> "[tab]"
  | Raylib.Key.A -> "A"
  | Raylib.Key.B -> "B"
  | Raylib.Key.C -> "C"
  | Raylib.Key.D -> "D"
  | Raylib.Key.E -> "E"
  | Raylib.Key.F -> "F"
  | Raylib.Key.G -> "G"
  | Raylib.Key.H -> "H"
  | Raylib.Key.I -> "I"
  | Raylib.Key.J -> "J"
  | Raylib.Key.K -> "K"
  | Raylib.Key.L -> "L"
  | Raylib.Key.M -> "M"
  | Raylib.Key.N -> "N"
  | Raylib.Key.O -> "O"
  | Raylib.Key.P -> "P"
  | Raylib.Key.Q -> "Q"
  | Raylib.Key.R -> "R"
  | Raylib.Key.S -> "S"
  | Raylib.Key.T -> "T"
  | Raylib.Key.U -> "U"
  | Raylib.Key.V -> "V"
  | Raylib.Key.W -> "W"
  | Raylib.Key.X -> "X"
  | Raylib.Key.Y -> "Y"
  | Raylib.Key.Z -> "Z"
  | Raylib.Key.Zero -> "0"
  | Raylib.Key.One -> "1"
  | Raylib.Key.Two -> "2"
  | Raylib.Key.Three -> "3"
  | Raylib.Key.Four -> "4"
  | Raylib.Key.Five -> "5"
  | Raylib.Key.Six -> "6"
  | Raylib.Key.Seven -> "7"
  | Raylib.Key.Eight -> "8"
  | Raylib.Key.Nine -> "9"
  | Raylib.Key.Slash -> "/"
  | Raylib.Key.Semicolon -> ";"
  | Raylib.Key.Left_bracket -> "{"
  | Raylib.Key.Right_bracket -> "}"
  | Raylib.Key.Grave -> "`"
  | Raylib.Key.Apostrophe -> "'"
  | Raylib.Key.Left_shift -> "Left shift"
  | Raylib.Key.Right_shift -> "Right shift"
  | Raylib.Key.Escape -> "Escape"
  | Raylib.Key.Space -> "Space"
  | _ -> "(unknown)"

let get_key k = List.assoc k key_bindings
let get_button k = List.assoc k gamepad_bindings

let holding_shift () =
  Raylib.is_key_down Raylib.Key.Left_shift || Raylib.is_key_down Raylib.Key.Right_shift

let gamepad =
  (* not sure if there is a better choice here (raylib example also hardcodes 0) *)
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
    failwithf "tried checking direction input for non-ARROW action %s" (show_game_action k)

let key_up k =
  Raylib.is_key_up (get_key k)
  && ((not (Raylib.is_gamepad_available gamepad))
     || Raylib.is_gamepad_button_up gamepad (get_button k))

let key_down ?(direction = false) k =
  Raylib.is_key_down (get_key k) || check_gamepad_input Raylib.is_gamepad_button_down direction k

let key_pressed ?(direction = false) k =
  Raylib.is_key_pressed (get_key k)
  || (* TODO this isn't really correct: when the left stick is held in a direction, it is
        registering as being pressed every frame
        - need to check whether or not it was down the previous frame in State.update_frame_inputs
     *)
  check_gamepad_input Raylib.is_gamepad_button_pressed direction k

let key_released ?(direction = false) k =
  Raylib.is_key_released (get_key k)
  || check_gamepad_input Raylib.is_gamepad_button_released direction k
