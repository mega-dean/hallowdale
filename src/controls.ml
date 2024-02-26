open Utils
open Types

let show_direction (d : direction) : string =
  match d with
  | UP -> "up"
  | DOWN -> "down"
  | LEFT -> "left"
  | RIGHT -> "right"

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
  | ARROW UP -> "UP"
  | ARROW DOWN -> "DOWN"
  | ARROW LEFT -> "LEFT"
  | ARROW RIGHT -> "RIGHT"
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

let load_key_overrides () : (game_action * Raylib.Key.t) list =
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
      | "[caps lock]" -> Raylib.Key.Caps_lock
      | "[enter]" -> Raylib.Key.Enter
      | "[space]" -> Raylib.Key.Space
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
      | "\\" -> Raylib.Key.Backslash
      | "=" -> Raylib.Key.Equal
      | "," -> Raylib.Key.Comma
      | "-" -> Raylib.Key.Minus
      | "." -> Raylib.Key.Period
      | "Kp_0" -> Raylib.Key.Kp_0
      | "Kp_1" -> Raylib.Key.Kp_1
      | "Kp_2" -> Raylib.Key.Kp_2
      | "Kp_3" -> Raylib.Key.Kp_3
      | "Kp_4" -> Raylib.Key.Kp_4
      | "Kp_5" -> Raylib.Key.Kp_5
      | "Kp_6" -> Raylib.Key.Kp_6
      | "Kp_7" -> Raylib.Key.Kp_7
      | "Kp_8" -> Raylib.Key.Kp_8
      | "Kp_9" -> Raylib.Key.Kp_9
      | "Kp_decimal" -> Raylib.Key.Kp_decimal
      | "Kp_divide" -> Raylib.Key.Kp_divide
      | "Kp_multiply" -> Raylib.Key.Kp_multiply
      | "Kp_subtract" -> Raylib.Key.Kp_subtract
      | "Kp_add" -> Raylib.Key.Kp_add
      | "Kp_enter" -> Raylib.Key.Kp_enter
      | "Kp_equal" -> Raylib.Key.Kp_equal
      (* don't allow modifier keys because they can mess things up:
         - pressing Shift too many times can pop up sticky keys window
         - Alt+tab / Super+tab can switch application
      *)
      (* | "Left alt" -> Raylib.Key.Left_alt
       * | "Left control" -> Raylib.Key.Left_control
       * | "Left shift" -> Raylib.Key.Left_shift
       * | "Left super" -> Raylib.Key.Left_super
       * | "Right alt" -> Raylib.Key.Right_alt
       * | "Right control" -> Raylib.Key.Right_control
       * | "Right shift" -> Raylib.Key.Right_shift
       * | "Right super" -> Raylib.Key.Right_super *)
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

let load_gamepad_overrides () : (game_action * Raylib.GamepadButton.t) list =
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
      | "X" -> Raylib.GamepadButton.Right_face_down
      | "Square" -> Raylib.GamepadButton.Right_face_left
      | "Circle" -> Raylib.GamepadButton.Right_face_right
      | "Triangle" -> Raylib.GamepadButton.Right_face_up
      | "R1" -> Raylib.GamepadButton.Right_trigger_1
      | "R2" -> Raylib.GamepadButton.Right_trigger_2
      | "L1" -> Raylib.GamepadButton.Left_trigger_1
      | "L2" -> Raylib.GamepadButton.Left_trigger_1
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

let load_key_bindings () : (game_action * Raylib.Key.t) list =
  make_bindings "key" default_keybinds (load_key_overrides ())

let load_gamepad_bindings () : (game_action * Raylib.GamepadButton.t) list =
  make_bindings "gamepad button" default_gamepad_buttons (load_gamepad_overrides ())

let load () =
  {
    keyboard = load_key_bindings () |> Game_action.Map.of_list;
    gamepad = load_gamepad_bindings () |> Game_action.Map.of_list;
  }

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
  | Raylib.Key.Escape -> "[escape]"
  | Raylib.Key.Space -> "[space]"
  | Raylib.Key.Null -> "Null"
  | Raylib.Key.Comma -> ","
  | Raylib.Key.Minus -> "-"
  | Raylib.Key.Period -> "."
  | Raylib.Key.Equal -> "="
  | Raylib.Key.Backslash -> "\\"
  | Raylib.Key.Enter -> "[enter]"
  | Raylib.Key.Backspace -> "Backspace"
  | Raylib.Key.Insert -> "Insert"
  | Raylib.Key.Delete -> "Delete"
  | Raylib.Key.Page_up -> "Page_up"
  | Raylib.Key.Page_down -> "Page_down"
  | Raylib.Key.Home -> "Home"
  | Raylib.Key.End -> "End"
  | Raylib.Key.Caps_lock -> "Caps_lock"
  | Raylib.Key.Scroll_lock -> "Scroll_lock"
  | Raylib.Key.Num_lock -> "Num_lock"
  | Raylib.Key.Print_screen -> "Print_screen"
  | Raylib.Key.Pause -> "Pause"
  | Raylib.Key.F1 -> "F1"
  | Raylib.Key.F2 -> "F2"
  | Raylib.Key.F3 -> "F3"
  | Raylib.Key.F4 -> "F4"
  | Raylib.Key.F5 -> "F5"
  | Raylib.Key.F6 -> "F6"
  | Raylib.Key.F7 -> "F7"
  | Raylib.Key.F8 -> "F8"
  | Raylib.Key.F9 -> "F9"
  | Raylib.Key.F10 -> "F10"
  | Raylib.Key.F11 -> "F11"
  | Raylib.Key.F12 -> "F12"
  | Raylib.Key.Left_control -> "Left_control"
  | Raylib.Key.Left_alt -> "Left_alt"
  | Raylib.Key.Left_super -> "Left_super"
  | Raylib.Key.Right_control -> "Right_control"
  | Raylib.Key.Right_alt -> "Right_alt"
  | Raylib.Key.Right_super -> "Right_super"
  | Raylib.Key.Kb_menu -> "Kb_menu"
  | Raylib.Key.Kp_0 -> "Kp_0"
  | Raylib.Key.Kp_1 -> "Kp_1"
  | Raylib.Key.Kp_2 -> "Kp_2"
  | Raylib.Key.Kp_3 -> "Kp_3"
  | Raylib.Key.Kp_4 -> "Kp_4"
  | Raylib.Key.Kp_5 -> "Kp_5"
  | Raylib.Key.Kp_6 -> "Kp_6"
  | Raylib.Key.Kp_7 -> "Kp_7"
  | Raylib.Key.Kp_8 -> "Kp_8"
  | Raylib.Key.Kp_9 -> "Kp_9"
  | Raylib.Key.Kp_decimal -> "Kp_decimal"
  | Raylib.Key.Kp_divide -> "Kp_divide"
  | Raylib.Key.Kp_multiply -> "Kp_multiply"
  | Raylib.Key.Kp_subtract -> "Kp_subtract"
  | Raylib.Key.Kp_add -> "Kp_add"
  | Raylib.Key.Kp_enter -> "Kp_enter"
  | Raylib.Key.Kp_equal -> "Kp_equal"
  | Raylib.Key.Back -> "Back"
  | Raylib.Key.Menu -> "Menu"
  | Raylib.Key.Volume_up -> "Volume_up"
  | Raylib.Key.Volume_down -> "Volume_down"

let show_button (button : Raylib.GamepadButton.t) =
  match button with
  | Raylib.GamepadButton.Left_face_up -> "Up"
  | Raylib.GamepadButton.Left_face_right -> "Right"
  | Raylib.GamepadButton.Left_face_down -> "Down"
  | Raylib.GamepadButton.Left_face_left -> "Left"
  | Raylib.GamepadButton.Right_face_up -> "Triangle"
  | Raylib.GamepadButton.Right_face_right -> "Circle"
  | Raylib.GamepadButton.Right_face_down -> "X"
  | Raylib.GamepadButton.Right_face_left -> "Square"
  | Raylib.GamepadButton.Left_trigger_1 -> "L1"
  | Raylib.GamepadButton.Left_trigger_2 -> "L2"
  | Raylib.GamepadButton.Right_trigger_1 -> "R1"
  | Raylib.GamepadButton.Right_trigger_2 -> "R2"
  | Raylib.GamepadButton.Middle_left -> "Select"
  | Raylib.GamepadButton.Middle_right -> "Start"
  | Raylib.GamepadButton.Left_thumb -> "Left stick"
  | Raylib.GamepadButton.Right_thumb -> "Right stick"
  | Raylib.GamepadButton.Middle -> "PS"
  | Raylib.GamepadButton.Unknown -> "(unknown)"

let gamepad =
  (* not sure if there is a better choice here (raylib example also hardcodes 0) *)
  0

(* can't use Raylib.get_gamepad_button_pressed because it checks button_down
   instead of button_pressed *)
let get_pressed_button () : Raylib.GamepadButton.t option =
  List.find_opt
    (Raylib.is_gamepad_button_pressed gamepad)
    [
      Raylib.GamepadButton.Left_face_up;
      Raylib.GamepadButton.Left_face_right;
      Raylib.GamepadButton.Left_face_down;
      Raylib.GamepadButton.Left_face_left;
      Raylib.GamepadButton.Right_face_up;
      Raylib.GamepadButton.Right_face_right;
      Raylib.GamepadButton.Right_face_down;
      Raylib.GamepadButton.Right_face_left;
      Raylib.GamepadButton.Left_trigger_1;
      Raylib.GamepadButton.Left_trigger_2;
      Raylib.GamepadButton.Right_trigger_1;
      Raylib.GamepadButton.Right_trigger_2;
      Raylib.GamepadButton.Middle_left;
      Raylib.GamepadButton.Middle_right;
      Raylib.GamepadButton.Left_thumb;
      Raylib.GamepadButton.Right_thumb;
      Raylib.GamepadButton.Middle;
      Raylib.GamepadButton.Unknown;
    ]

let get_key (controls : controls) k =
  match Game_action.Map.find_opt k controls.keyboard with
  | Some key -> key
  | None -> failwithf "get_key: could not find %s" (show_game_action k)

let get_button (controls : controls) k =
  match Game_action.Map.find_opt k controls.gamepad with
  | Some button -> button
  | None -> failwithf "get_button: could not find %s" (show_game_action k)

let holding_shift () =
  Raylib.is_key_down Raylib.Key.Left_shift || Raylib.is_key_down Raylib.Key.Right_shift

let check_gamepad_input controls check_button direction k =
  if not direction then
    check_button gamepad (get_button controls k)
  else (
    match k with
    | ARROW DOWN ->
      check_button gamepad (get_button controls k)
      || Raylib.get_gamepad_axis_movement gamepad Raylib.GamepadAxis.Left_y > 0.5
    | ARROW UP ->
      check_button gamepad (get_button controls k)
      || Raylib.get_gamepad_axis_movement gamepad Raylib.GamepadAxis.Left_y < -0.5
    | ARROW RIGHT ->
      check_button gamepad (get_button controls k)
      || Raylib.get_gamepad_axis_movement gamepad Raylib.GamepadAxis.Left_x > 0.5
    | ARROW LEFT ->
      check_button gamepad (get_button controls k)
      || Raylib.get_gamepad_axis_movement gamepad Raylib.GamepadAxis.Left_x < -0.5
    | _ -> failwithf "tried checking direction input for non-ARROW action %s" (show_game_action k))

let key_up (controls : controls) k =
  Raylib.is_key_up (get_key controls k)
  && ((not (Raylib.is_gamepad_available gamepad))
     || Raylib.is_gamepad_button_up gamepad (get_button controls k))

let key_down ?(direction = false) (controls : controls) k =
  Raylib.is_key_down (get_key controls k)
  || check_gamepad_input controls Raylib.is_gamepad_button_down direction k

let key_pressed ?(direction = false) (controls : controls) k =
  Raylib.is_key_pressed (get_key controls k)
  || (* TODO this isn't really correct: when the left stick is held in a direction, it is
        registering as being pressed every frame
        - need to check whether or not it was down the previous frame in State.update_frame_inputs
        - I think this only affects menus, but might have other problems
     *)
  check_gamepad_input controls Raylib.is_gamepad_button_pressed direction k

let key_released ?(direction = false) (controls : controls) k =
  Raylib.is_key_released (get_key controls k)
  || check_gamepad_input controls Raylib.is_gamepad_button_released direction k
