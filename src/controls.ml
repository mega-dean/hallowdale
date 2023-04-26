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
  (* directions *)
  | UP
  | DOWN
  | LEFT
  | RIGHT
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
  (* directions *)
  | UP -> "UP"
  | DOWN -> "DOWN"
  | LEFT -> "LEFT"
  | RIGHT -> "RIGHT"
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

let override_keybinds : (key_action * Raylib.Key.t) list =
  let keybinds_json : (string * string) list = Tiled.read_keybinds () in
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
      | "JUMP" -> JUMP
      | "NAIL" -> NAIL
      (* directions *)
      | "UP" -> UP
      | "DOWN" -> DOWN
      | "LEFT" -> LEFT
      | "RIGHT" -> RIGHT
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
      | _ -> Types.failwithf "bad override action_name: %s" action_name
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
      (* TODO map other keys *)
      | _ ->
        (* TODO print out all valid key names *)
        Types.failwithf "bad override key_name: %s" key_name
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
    (* directions *)
    (UP, Raylib.Key.Up);
    (DOWN, Raylib.Key.Down);
    (LEFT, Raylib.Key.Left);
    (RIGHT, Raylib.Key.Right);
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

let get_key k =
  match List.assoc_opt k override_keybinds with
  | Some key -> key
  | None -> List.assoc k default_keybinds

let key_up k = Raylib.is_key_up (get_key k)
let key_down k = Raylib.is_key_down (get_key k)
let key_pressed k = Raylib.is_key_pressed (get_key k)
