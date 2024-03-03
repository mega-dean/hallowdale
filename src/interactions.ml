open Utils
open Types
open Types.Interaction

let key_or_button' control_type controls ?(color = true) action =
  let s =
    match control_type with
    | KEYBOARD -> Controls.get_key controls action |> Controls.show_key
    | GAMEPAD -> Controls.get_button controls action |> Controls.show_button
  in
  if color then
    fmt "{{blue}} [%s] {{white}}" s
  else
    fmt "[%s]" s

let key_or_button state ?(color = true) action =
  key_or_button' state.frame_inputs.control_type state.controls ~color action

let cutscene_finished
    (progress_by_room : (string * Json_t.room_progress) list)
    (cutscene_name : string) : bool =
  let all_finished_interactions =
    let get_interaction_names (r : Json_t.room_progress) =
      List.map (String.maybe_trim_before ':') r.finished_interactions
    in
    List.map snd progress_by_room |> List.map get_interaction_names |> List.flatten
  in
  List.mem cutscene_name all_finished_interactions

let trigger_name trigger : string = String.maybe_trim_before '|' trigger.full_name

let get_steps
    state
    game
    ?(autosave_pos = None)
    ?(followup : trigger option = None)
    (trigger : trigger) : step list =
  let config = Config.interactions in
  let ability_text_outline x y =
    let w, h = (config.ability_outline_w, config.ability_outline_h) in
    { pos = { x = x *. w; y = y *. h }; w; h }
  in
  let remove_nail = ref true in
  let autosave_pos' = ref autosave_pos in

  let get_interaction_steps (trigger : trigger) : step list =
    let fade_screen =
      [
        STEP (SET_SCREEN_FADE (make_screen_fade (0, config.screen_fade_half) 1.5)); STEP (WAIT 1.5);
      ]
    in
    let clear_screen_fade starting_fade time =
      [ STEP (CLEAR_SCREEN_FADE (starting_fade, time)); STEP (WAIT time) ]
    in
    let fade_screen_with_dramatic_pause steps =
      [ STEP (WAIT 0.5); CURRENT_GHOST (SET_POSE (PERFORMING FOCUS)); STEP (WAIT 1.) ]
      @ fade_screen
      @ steps
      @ clear_screen_fade config.screen_fade_half 1.
      @ [ CURRENT_GHOST (SET_POSE IDLE) ]
    in

    (* quote is optional because abilities gained during cutscenes don't have quotes *)
    let get_ability_steps ability_name outline_x outline_y ?(quote = None) top_lines bottom_lines =
      let bottom_lines' =
        match quote with
        | None -> bottom_lines
        | Some quote' -> bottom_lines @ [ "============================="; quote' ]
      in
      fade_screen_with_dramatic_pause
        [
          CURRENT_GHOST (ADD_ITEM (ABILITY ability_name));
          STEP
            (ABILITY_TEXT
               ( ability_text_outline outline_x outline_y,
                 top_lines @ [ "=============================" ] @ bottom_lines' ));
        ]
    in

    let get_lore () : string =
      match String.Map.find_opt trigger.full_name state.global.lore with
      | None -> failwithf "lore name '%s' not found in lore.json" trigger.full_name
      | Some lore -> lore
    in

    let jump_party_ghost ?(end_pose = IDLE) ghost_id direction vx wait_time =
      [
        PARTY_GHOST (ghost_id, JUMP (direction, vx));
        STEP (WAIT wait_time);
        PARTY_GHOST (ghost_id, SET_POSE end_pose);
      ]
    in
    let fail () =
      failwithf "unknown '%s' interaction: %s" trigger.name_prefix trigger.name_suffix
    in

    let current_ghost_name =
      match game.player.ghost.id with
      | TROY -> "Troy"
      | ABED -> "Abed"
      | JEFF -> "Jeff"
      | ANNIE -> "Annie"
      | BRITTA
      | LAVA_BRITTA ->
        "Britta"
    in

    let unhide_and_unfreeze ?(direction : direction = LEFT) ghost_id (x, y) =
      [
        PARTY_GHOST (ghost_id, ENTITY (UNHIDE_AT (x, y, 0., 0.)));
        PARTY_GHOST (ghost_id, ENTITY (SET_FACING direction));
        PARTY_GHOST (ghost_id, SET_POSE IDLE);
      ]
    in
    let read_sign = [ CURRENT_GHOST (SET_POSE READING); STEP (WAIT 0.4) ] in

    let opening_poem : step list =
      [
        CURRENT_GHOST (SET_POSE (AIRBORNE (-1.)));
        CURRENT_GHOST (ENTITY FREEZE);
        STEP (WAIT 1.);
        STEP
          (CORNER_TEXT (fmt "Press %s to advance text" (key_or_button state ~color:false INTERACT)));
        STEP (CENTERED_TEXT [ "Give me some rope, tie me to dream." ]);
        STEP UNSET_CORNER_TEXT;
        STEP
          (CENTERED_TEXT
             [ "Give me some rope, tie me to dream."; "Give me the hope to run out of steam." ]);
        STEP
          (CENTERED_TEXT
             [
               "Give me some rope, tie me to dream.";
               "Give me the hope to run out of steam.";
               "Somebody said it could be here.";
             ]);
        STEP
          (CENTERED_TEXT
             [
               "Give me some rope, tie me to dream.";
               "Give me the hope to run out of steam.";
               "Somebody said it could be here.";
               "We could be roped up, tied up, dead in a year.";
             ]);
        STEP
          (CENTERED_TEXT
             [
               "Give me some rope, tie me to dream.";
               "Give me the hope to run out of steam.";
               "Somebody said it could be here.";
               "We could be roped up, tied up, dead in a year.";
               " - excerpt from \"At Least It Was Here\" by The 88";
             ]);
        STEP (WAIT 1.);
        CURRENT_GHOST (ENTITY UNFREEZE);
      ]
      @ clear_screen_fade 255 2.
    in

    match trigger.name_prefix with
    | "warp" -> [ STEP (WARP trigger.kind) ]
    | "door-warp" -> [ STEP (DOOR_WARP trigger.kind) ]
    | "weapon" ->
      let steps =
        fade_screen_with_dramatic_pause [ CURRENT_GHOST (ADD_ITEM (WEAPON trigger.name_suffix)) ]
      in
      if game.player.weapons |> String.Map.bindings |> List.length = 1 then
        steps
        @ [
            STEP
              (TEXT
                 [
                   "Equip weapons from the pause menu.";
                   "Different weapons have different size and swing speed.";
                   "Weapon damage is based the number of weapons found, so each weapon does the \
                    same amount of damage. You can see the current damage on the Progress page in \
                    the pause menu.";
                 ]);
          ]
      else
        steps
    | "purple-pen" ->
      remove_nail := false;
      [ STEP (PURPLE_PEN_TEXT (get_lore ())) ]
    | "key" ->
      [
        STEP (WAIT 0.5);
        CURRENT_GHOST (SET_POSE (PERFORMING DIVE_COOLDOWN));
        STEP (WAIT 1.);
        STEP (CENTERED_TEXT [ fmt "Found a key: {{blue}} %s" trigger.name_suffix; get_lore () ]);
        CURRENT_GHOST (ADD_ITEM (KEY trigger.name_suffix));
      ]
    | "increase-health" ->
      [
        STEP (WAIT 0.5);
        CURRENT_GHOST (SET_POSE (PERFORMING DIVE_COOLDOWN));
        STEP (WAIT 1.);
        CURRENT_GHOST (INCREASE_HEALTH_TEXT "Max health increased.");
      ]
    | "dreamer" ->
      fade_screen_with_dramatic_pause
        [ CURRENT_GHOST (ADD_ITEM (DREAMER (trigger.name_suffix, get_lore ()))) ]
    | "talk" ->
      let steps =
        match trigger.name_suffix with
        | "annies-boobs" -> (
          let rewards : (int * reward) list =
            [
              (20, INCREASE_MAX_SOUL);
              ( 30,
                ABILITY
                  ( "Soul Catcher",
                    "Increases the amount of LIFE VAPOR gained when striking an enemy." ) );
              (40, ABILITY ("Quick Focus", "Increases the speed of focusing LIFE VAPOR."));
              (50, INCREASE_MAX_SOUL);
              ( 60,
                ABILITY
                  ( "Soul Catcher",
                    "Increases the amount of LIFE VAPOR gained when striking an enemy." ) );
              ( 70,
                ABILITY
                  ( "Dream Wielder",
                    "Charge the Honda Nail faster and collect more LIFE VAPOR when striking foes."
                  ) );
              (80, INCREASE_MAX_SOUL);
              ( 90,
                ABILITY
                  ( "Soul Catcher",
                    "Increases the amount of LIFE VAPOR gained when striking an enemy." ) );
              (100, ABILITY ("Deep Focus", "Heals two masks when focusing."));
              (110, ABILITY ("Shaman Stone", "Increases the power of spells."));
              (120, ABILITY ("Spell Twister", "Reduces the LIFE VAPOR cost of casting spells."));
            ]
          in
          let purple_pens_found = List.length game.progress.purple_pens_found in
          let last_upgrade = game.progress.last_upgrade_claimed in
          match List.find_opt (fun (amount, _reward) -> amount > last_upgrade) rewards with
          | None -> [ STEP (DIALOGUE ("Annie's Boobs", "I have no more rewards for you.")) ]
          | Some (next_upgrade_amount, reward) ->
            if purple_pens_found >= next_upgrade_amount then
              [
                CURRENT_GHOST (SET_POSE (PERFORMING DIVE_COOLDOWN));
                STEP (WAIT 1.);
                CURRENT_GHOST (CLAIM_REWARD (next_upgrade_amount, reward));
                STEP (CENTERED_TEXT [ "~~~ Claimed a reward ~~~"; Show.reward reward ]);
              ]
            else
              [
                STEP
                  (DIALOGUE
                     ( "Annie's Boobs",
                       fmt
                         "You have {{purple}} %d purple pens. {{white}} Come back when you have \
                          {{purple}} %d."
                         purple_pens_found next_upgrade_amount ));
              ])
        | "Vaughn" ->
          [
            STEP (DIALOGUE (current_ghost_name, "What makes Frisbee {{green}} \"ultimate\"?"));
            STEP
              (DIALOGUE
                 ("Vaughn", "Man, if I had a nickel for every time I wish someone asked me that..."));
          ]
        | name -> [ STEP (DIALOGUE (name, get_lore ())) ]
      in
      let facing =
        if rect_center_x trigger.dest < rect_center_x game.player.ghost.entity.dest then
          LEFT
        else
          RIGHT
      in
      [ CURRENT_GHOST (SET_POSE IDLE); CURRENT_GHOST (ENTITY (SET_FACING facing)) ] @ steps
    | "info" -> (
      match trigger.name_suffix with
      | "focus" ->
        read_sign
        @ fade_screen
        @ [
            STEP
              (FOCUS_ABILITY_TEXT
                 ( [
                     "Human Beings, these words are for you alone.";
                     "";
                     "";
                     "Your great strength marks you already accepted. Focus your life vapor and \
                      you shall achieve feats of which others can only dream.";
                   ],
                   ability_text_outline 0. 0.,
                   [
                     "Collect LIFE VAPOR by striking enemies.";
                     "Once enough LIFE VAPOR is collected";
                     fmt "Hold %s" (key_or_button state FOCUS);
                     "to focus LIFE VAPOR and HEAL.";
                   ] ));
          ]
        @ clear_screen_fade config.screen_fade_half 1.
      | "true-form" ->
        read_sign
        @ fade_screen
        @ [
            STEP
              (CENTERED_TEXT
                 [
                   "Human Beings, these words are for you alone.";
                   "";
                   "Within our lands do not hide your true form. For only this campus could \
                    produce ones such as you.";
                 ]);
          ]
        @ clear_screen_fade config.screen_fade_half 1.
      | "dean-and-creator" ->
        read_sign
        @ fade_screen
        @ [
            STEP
              (CENTERED_TEXT
                 [
                   "Human Beings, these words are for you alone.";
                   "";
                   "Beyond this point you enter the land of Dean and Creator. Step across this \
                    threshold and obey our laws.";
                 ]);
            STEP (WAIT 1.);
            STEP
              (CENTERED_TEXT
                 [
                   "Bear witness to the last and only civilisation, the eternal Campus.";
                   "";
                   "Hallowdale";
                 ]);
          ]
        @ clear_screen_fade config.screen_fade_half 1.
      | _lore -> read_sign @ [ STEP (TEXT [ get_lore () ]) ])
    | "ability" -> (
      let quote = Some (get_lore ()) in
      match trigger.name_suffix with
      | "scootenanny" ->
        get_ability_steps "mothwing_cloak" 0. 2. ~quote
          [ "Taken the"; "Scootenanny Chair." ]
          [
            fmt "Press %s to scootenanny forwards." (key_or_button state DASH);
            "Use the chair to scootenanny quickly along the ground or through the air.";
          ]
      | "double-bouncies" ->
        get_ability_steps "monarch_wings" 0. 4. ~quote
          [ "Consumed the"; "Double Bouncies." ]
          [
            fmt "Press %s while in the air to double bounce." (key_or_button state JUMP);
            "Use the ethereal bounce to sail above enemies and discover new paths.";
          ]
      | "reverse-danny-thomas" ->
        get_ability_steps "mantis_claw" 0. 8. ~quote
          [ "Learned the"; "Reverse Danny Thomas." ]
          [
            fmt "Press %s while sliding against a wall to jump again." (key_or_button state JUMP);
            "Jump from wall to wall to reach new areas.";
          ]
      | "computer-heart" ->
        get_ability_steps "crystal_heart" 0. 5. ~quote
          [ "Consumed the"; "Computer Heart." ]
          [
            fmt "Hold %s while on the ground or clinging to a wall to concentrate the force."
              (key_or_button state C_DASH);
            "Release the button to blast forwards and fly through the air.";
          ]
        @ [ STEP (HIDE_LAYER "temporary-floors") ]
      | "vaughns-tear" ->
        get_ability_steps "ismas_tear" 0. 10. ~quote
          [ "Consumed the"; "Vaughn's Tear." ]
          [ "Acid shall be repelled."; "Swim in acidic waters without coming to any harm." ]
      | "pierce-hologram" ->
        get_ability_steps "shade_cloak" 0. 7. ~quote
          [ "Consumed the"; "Pierce Hologram." ]
          [
            fmt "Press %s to scootenanny forwards, cloaked in hologram." (key_or_button state DASH);
            "Use the hologram to scootenanny through enemies and their attacks without taking \
             damage.";
          ]
      | "monkey-gas" ->
        get_ability_steps "howling_wraiths" 0. 6. ~quote
          [ "Consumed the"; "Monkey Knockout Gas." ]
          [
            fmt "Tap %s while holding UP" (key_or_button state CAST); "to unleash the Knockout Gas.";
          ]
      | "honda-nail" ->
        get_ability_steps "dream_nail" 0. 9. [ "Taken the"; "Honda Nail." ] ~quote
          [
            fmt "Hold %s to charge and slash with the nail." (key_or_button state NAIL);
            "Cut through the veil between dreams and waking.";
          ]
      | "sealy-select" ->
        get_ability_steps "shade_soul" 0. 1. ~quote
          [ "Consumed the"; "Hypoallergenic Sealy Select." ]
          [
            fmt "Tap %s" (key_or_button state CAST);
            "to unleash a more powerful Cushion.";
            "This spell consumes the same amount of LIFE VAPOR,";
            "with increased power.";
          ]
      | "torvins-flesh-of-fire" ->
        get_ability_steps "descending_dark" 0. 3. ~quote
          [ "Learned"; "Torvin's Flesh of Fire." ]
          [
            fmt "Tap %s while holding DOWN" (key_or_button state CAST);
            "to strike the earth with a burst of white-hot flame.";
          ]
      | "vapors-of-magmarath" ->
        get_ability_steps "abyss_shriek" 0. 6. ~quote
          [ "Consumed the"; "Vapors of Magmarath." ]
          [ fmt "Tap %s while holding UP" (key_or_button state CAST); "to unleash the Vapors." ]
      | _ -> fail ())
    | "boss-fight" -> (
      (* this doesn't include bosses from the final cutscene sequence, since those aren't
         initiated by triggers
      *)
      game.in_boss_fight <- true;
      match Enemy.parse_name "boss-fight interaction" trigger.name_suffix with
      | DUNCAN ->
        [
          ENEMY (DUNCAN, ENTITY UNHIDE);
          ENEMY (DUNCAN, ENTITY FREEZE);
          ENEMY (DUNCAN, SET_POSE "scavenging");
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (DIALOGUE ("Britta", "Professor Duncan?"));
          STEP (SET_FIXED_CAMERA (40, 24));
          STEP (WAIT 0.7);
          ENEMY (DUNCAN, SET_POSE "idle");
          ENEMY (DUNCAN, ENTITY (SET_FACING LEFT));
          STEP (WAIT 0.4);
          STEP
            (DIALOGUE
               ( "Duncan",
                 "You stay back, Britta! I'm not afraid to push a girl into make-believe {{red}} \
                  lava! {{white}} In fact, it's been my primary strategy." ));
          STEP
            (DIALOGUE
               ("Britta", "I'm staying in the game so I can talk to Abed. I'm worried about him."));
          STEP
            (DIALOGUE
               ( "Duncan",
                 "Well I'm worried about his money. My self-published novels aren't going to \
                  publish themselves." ));
          STEP
            (DIALOGUE
               ( "Britta",
                 "Don't regress to primal behavior just because it's allowed - we're Human Beings, \
                  not the editors of Teen Vogue." ));
          STEP (WAIT 1.3);
          STEP
            (DIALOGUE
               ( "Britta",
                 "They're setting a {{red}} terrible example {{white}} for today's young women." ));
          STEP
            (DIALOGUE ("Duncan", "Well I'm sorry Britta, but it's either you or me. And I'm me."));
          STEP (UNHIDE_LAYER "boss-doors");
          ENEMY (DUNCAN, START_ACTION "jumping");
          ENEMY (DUNCAN, ENTITY (SET_VX config.duncan_initial_jump_vx));
          ENEMY (DUNCAN, ENTITY UNFREEZE);
          STEP (WAIT 0.7);
        ]
      | LOCKER_BOY ->
        [
          ENEMY (LOCKER_BOY, ENTITY HIDE);
          CURRENT_GHOST (PARTY (WALK_TO 131));
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE READING);
          STEP (WAIT 0.7);
          STEP (UNHIDE_LAYER "boss-doors");
          STEP (WAIT 1.);
          STEP (HIDE_LAYER "bg-iso2");
          STEP (UNHIDE_LAYER "bg-iso3");
          ENEMY (LOCKER_BOY, ENTITY UNHIDE);
          ENEMY (LOCKER_BOY, ENTITY FREEZE);
          STEP (WAIT 0.5);
          ENEMY (LOCKER_BOY, SET_POSE "vanish");
          STEP (WAIT 0.5);
          ENEMY (LOCKER_BOY, ENTITY UNFREEZE);
        ]
      | JOSHUA ->
        [
          CURRENT_GHOST (PARTY (WALK_TO 49));
          CURRENT_GHOST (SET_POSE IDLE);
          ENEMY (JOSHUA, ENTITY UNHIDE);
          ENEMY (JOSHUA, ENTITY FREEZE);
          ENEMY (JOSHUA, SET_POSE "clipping");
          STEP (SET_FIXED_CAMERA (20, 44));
          STEP (WAIT 2.7);
          ENEMY (JOSHUA, SET_POSE "idle");
          STEP (WAIT 1.);
          ENEMY (JOSHUA, SET_POSE "shoot");
          STEP (UNHIDE_LAYER "boss-doors");
          STEP (WAIT 1.);
          ENEMY (JOSHUA, ENTITY UNFREEZE);
        ]
      | VICE_DEAN_LAYBOURNE ->
        [
          ENEMY (VICE_DEAN_LAYBOURNE, ENTITY UNHIDE);
          ENEMY (VICE_DEAN_LAYBOURNE, ENTITY UNFREEZE);
          ENEMY (VICE_DEAN_LAYBOURNE, SET_POSE "idle");
          ENEMY (VICE_DEAN_LAYBOURNE, ENTITY (SET_FACING LEFT));
          CURRENT_GHOST (PARTY (WALK_TO 87));
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 0.7);
          ENEMY (VICE_DEAN_LAYBOURNE, SET_POSE "lunge");
          STEP (UNHIDE_LAYER "boss-doors");
          STEP (WAIT 1.);
          ENEMY (VICE_DEAN_LAYBOURNE, ENTITY UNFREEZE);
        ]
      | LUIS_GUZMAN ->
        [
          CURRENT_GHOST (PARTY (WALK_TO 78));
          CURRENT_GHOST (SET_POSE IDLE);
          ENEMY (LUIS_GUZMAN, ENTITY UNHIDE);
          ENEMY (LUIS_GUZMAN, ENTITY FREEZE);
          ENEMY (LUIS_GUZMAN, SET_POSE "idle");
          STEP (SET_FIXED_CAMERA (77, 120));
          STEP (WAIT 2.);
          ENEMY (LUIS_GUZMAN, SET_POSE "charge-shoot");
          STEP (UNHIDE_LAYER "boss-doors");
          STEP (WAIT 1.);
          ENEMY (LUIS_GUZMAN, ENTITY UNFREEZE);
        ]
      | BUDDY ->
        [
          ENEMY (BUDDY, ENTITY UNHIDE);
          ENEMY (BUDDY, ENTITY UNFREEZE);
          ENEMY (BUDDY, SET_POSE "idle");
          ENEMY (BUDDY, ENTITY (SET_FACING RIGHT));
          CURRENT_GHOST (PARTY (WALK_TO 54));
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 0.7);
          STEP
            (DIALOGUE
               ("Buddy", "I put myself out there for you! I laid my {{blue}} soul {{white}} bare!"));
          STEP (WAIT 1.);
          STEP (UNHIDE_LAYER "boss-doors");
          ENEMY (BUDDY, ENTITY UNFREEZE);
        ]
      | BORCHERT ->
        [
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 0.5);
          STEP (WAIT 0.5);
          CURRENT_GHOST (PARTY (WALK_TO 55));
          ENEMY (BORCHERT, ENTITY UNHIDE);
          ENEMY (BORCHERT, ENTITY (SET_FACING LEFT));
          ENEMY (BORCHERT, ENTITY UNFREEZE);
          ENEMY (BORCHERT, SET_POSE "dive");
          ENEMY (BORCHERT, ENTITY (SET_VY config.borchert_descend_vy));
          STEP (WAIT 1.7);
          ENEMY (BORCHERT, ENTITY (SET_VY 0.));
          STEP (WAIT 2.);
          STEP (UNHIDE_LAYER "boss-doors");
          ENEMY (BORCHERT, SET_POSE "charge-shoot");
          STEP (WAIT 1.);
          ENEMY (BORCHERT, ENTITY UNFREEZE);
        ]
      | DEAN ->
        [
          CURRENT_GHOST (PARTY (WALK_TO 43));
          CURRENT_GHOST (SET_POSE IDLE);
          ENEMY (DEAN, ENTITY UNHIDE);
          ENEMY (DEAN, ENTITY UNFREEZE);
          ENEMY (DEAN, SET_POSE "idle");
          ENEMY (DEAN, ENTITY (SET_FACING LEFT));
          ENEMY (DEAN, WALK_TO 70);
          STEP (WAIT 0.7);
          ENEMY (DEAN, SET_POSE "spikes");
          STEP (WAIT 1.);
          STEP (UNHIDE_LAYER "boss-doors");
          ENEMY (DEAN, ENTITY UNFREEZE);
        ]
      | _ -> fail ())
    | "cutscene" -> (
      match trigger.name_suffix with
      | "ss-opening-poem" -> opening_poem
      | "opening-poem" ->
        opening_poem
        @ [
            CURRENT_GHOST (ENTITY (WAIT_UNTIL_LANDED true));
            CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
            STEP (SHAKE_SCREEN 1.5);
            STEP (WAIT 2.5);
          ]
      | "mama-mahogany" ->
        [
          PARTY_GHOST (ANNIE, ENTITY (UNHIDE_AT (29, 36, 0., 0.)));
          PARTY_GHOST (JEFF, ENTITY (UNHIDE_AT (32, 36, 0., 0.)));
          PARTY_GHOST (ANNIE, SET_POSE IDLE);
          PARTY_GHOST (JEFF, SET_POSE IDLE);
          PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
          PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
          ENEMY (LOCKER_BOY, ENTITY HIDE);
          CURRENT_GHOST (SET_POSE IDLE);
          CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
          STEP (WAIT 0.7);
          STEP (SET_CAMERA_MOTION (LINEAR 10.));
          STEP (SET_FIXED_CAMERA (127, 28));
          STEP (WAIT 1.6);
          STEP (DIALOGUE ("Annie", "Mama mahogany, feast your feet on that stack of sticks."));
          STEP (WAIT 1.6);
          STEP SET_GHOST_CAMERA;
          STEP (WAIT 0.7);
          STEP (DIALOGUE ("Jeff", "Eh, too easy - could be a {{red}} trap..."));
        ]
      | "lockers" -> [ STEP (FLOATING_TEXT ("... lockers ...", 1.)) ]
      | "lockers-lockers-lockers" ->
        [ STEP (FLOATING_TEXT ("... lockers, lockers, lockers ...", 1.)) ]
      | "arrive-at-shirley-island" ->
        [
          CURRENT_GHOST (SET_POSE IDLE);
          CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
          NPC (NEIL, ENTITY (SET_FACING LEFT));
          STEP (WAIT 1.);
          STEP (DIALOGUE ("Neil", "Welcome to {{purple}} Shirley Island."));
          NPC (NEIL, ENTITY (SET_FACING RIGHT));
          STEP (SET_CAMERA_MOTION (LINEAR config.shirley_island_camera_motion));
          STEP (SET_FIXED_CAMERA (125, 48));
          STEP
            (DIALOGUE
               ( "Neil",
                 "No furniture beyond this point. Leave your weapons at the door, and any spare \
                  doors at the entrance." ));
          STEP (WAIT 3.);
          STEP
            (DIALOGUE
               ( "Garrett",
                 "Then came the now-now time, when the floors were covered with the {{red}} burny \
                  touch." ));
          CURRENT_GHOST (ENTITY (MOVE_TO (177, 67)));
          CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
        ]
        @ unhide_and_unfreeze TROY ~direction:RIGHT (174, 67)
        @ unhide_and_unfreeze JEFF (196, 67)
        @ unhide_and_unfreeze ANNIE (187, 67)
        @ [
            STEP (SET_FIXED_CAMERA (192, 62));
            STEP (WAIT 4.);
            PARTY_GHOST (JEFF, WALK_TO 180);
            STEP (WAIT 0.5);
            STEP (DIALOGUE ("Jeff", "You made it!"));
            STEP
              (DIALOGUE
                 ( "Shirley",
                   "Friends! Welcome to Shirley Island, where all your dreams come true if you \
                    dream of standing on a table and {{gold}} pissing {{white}} in a jar." ));
            STEP (DIALOGUE ("Shirley", "Where's Britta?"));
            STEP (DIALOGUE ("Abed", "She didn't make it."));
            STEP (DIALOGUE ("Shirley", "Oh, that's too bad..."));
            (* TODO add the Secular/Christians corkboard *)
            STEP (DIALOGUE ("Annie", "Poor Britta."));
            STEP
              (DIALOGUE ("Abed", "It's okay - her sacrifice got us this far, and now we can't lose."));
            STEP (DIALOGUE ("Troy", "Why not?"));
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE ("Abed", "Because now we're on Shirley Island, and according to legend..."));
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Abed", "... so is {{blue}} the orb."));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
            STEP
              (DIALOGUE
                 ( "Shirley",
                   "I'm sure I have no idea what you're talking about. This is a place of {{pink}} \
                    peace." ));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING LEFT));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Abed", "And {{green}} profit."));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Shirley", "Come again?"));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING LEFT));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ( "Abed",
                   "You're not really playing, Shirley. You're a {{green}} merchant, {{white}} and \
                    more power to you. But don't withhold power from others just to make money." ));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
            CURRENT_GHOST (PARTY (WALK_TO 195));
            STEP (DIALOGUE ("Abed", "We want {{blue}} the orb."));
            STEP (DIALOGUE ("Troy", "Abed..."));
            PARTY_GHOST (TROY, WALK_TO 193);
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ( "Troy",
                   "Listen, I'm still a little raw from what happened with Britta back there. I \
                    mean, fun is fun, but I don't want my {{red}} last day {{white}} here to be a \
                    day where everyone hates me." ));
            PARTY_GHOST (ANNIE, ADD_TO_PARTY);
            PARTY_GHOST (JEFF, ADD_TO_PARTY);
            PARTY_GHOST (TROY, ADD_TO_PARTY);
          ]
      | "fight-hickey" ->
        [
          STEP (SET_FIXED_CAMERA (70, 24));
          CURRENT_GHOST (SET_POSE IDLE);
          PARTY_GHOST (ABED, ENTITY (UNHIDE_AT (68, 23, 0., 0.)));
          PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
          STEP (WAIT 2.);
        ]
        @ clear_screen_fade 255 1.
        @ [
            STEP (WAIT 1.);
            PARTY_GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Abed", "Ok, we'll go into the vents. They'll never find us there."));
            PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP
              (DIALOGUE
                 ("Troy", "I say we take a stand here. I mean, someone's got to win sometime."));
            PARTY_GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ("Abed", "Not if we never kill each other. Then we can play {{green}} forever."));
            PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP
              (DIALOGUE
                 ( "Troy",
                   "Right. Wait. Abed, the floor can't be {{red}} lava {{white}} forever. The \
                    game's got to end." ));
            STEP (WAIT 1.5);
            PARTY_GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ( "Abed",
                   "It's not a game for me, Troy. I'm seeing real {{red}} lava {{white}} because \
                    you're leaving. It's embarrassing, and I don't want to be crazy, but I am \
                    crazy. So I made a game that made you and everyone else see what I see." ));
            STEP (WAIT 1.);
            STEP (SET_CAMERA_MOTION (LINEAR 2.));
            STEP (SET_FIXED_CAMERA (70, 34));
            STEP (WAIT 1.);
            STEP
              (DIALOGUE
                 ( "Abed",
                   "I don't want it to be there either, I swear. I want you to be able to leave, \
                    but I don't think the {{red}} lava {{white}} goes away until you stop leaving."
                 ));
            STEP
              (DIALOGUE
                 ( "Troy",
                   "So the only way I can help you is by giving up my chance to be one person?" ));
            STEP (SHAKE_SCREEN 2.);
            STEP (WAIT 2.);
            ENEMY (HICKEY, ENTITY UNHIDE);
            ENEMY (HICKEY, ENTITY UNFREEZE);
            ENEMY (LAVA_BRITTA_2, ENTITY UNHIDE);
            ENEMY (LAVA_BRITTA_2, ENTITY UNFREEZE);
            ENEMY (LAVA_BRITTA_2, SET_POSE "with-hickey");
            STEP (SET_CAMERA_MOTION (LINEAR 8.));
            STEP (SET_FIXED_CAMERA (41, 24));
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Britta", "You guys ready for closure?"));
            STEP (DIALOGUE ("Hickey", "Of your caskets?"));
            STEP
              (DIALOGUE
                 ( "Troy",
                   "Guys, stop, ok? The {{red}} lava's {{white}} real to Abed. It's not a game to \
                    him." ));
            STEP (DIALOGUE ("Britta", "Oh no..."));
            STEP
              (DIALOGUE
                 ( "Hickey",
                   "You know what I think? I think he's used to getting his own way. I think he's \
                    never met me." ));
            STEP SET_GHOST_CAMERA;
            ENEMY (HICKEY, SET_POSE "lunge");
            ENEMY (HICKEY, ENTITY (SET_VX config.hickey_dash_vx));
            ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX config.hickey_dash_vx));
            STEP (SET_FIXED_CAMERA (72, 24));
            PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
          ]
        @ jump_party_ghost ~end_pose:(AIRBORNE (-1.)) ABED RIGHT config.abed_shelves_jump_vx 0.8
        @ [
            STEP (HIDE_LAYER "bg");
            STEP (UNHIDE_LAYER "bg4");
            PARTY_GHOST (ABED, ENTITY FREEZE);
            STEP (WAIT 1.);
            ENEMY (HICKEY, SET_POSE "idle");
            ENEMY (HICKEY, ENTITY (SET_VX 0.));
            ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX 0.));
            CURRENT_GHOST (SET_POSE IDLE);
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP (WAIT 0.5);
            STEP (DIALOGUE ("Troy", "Abed!"));
            ENEMY (LAVA_BRITTA_2, ENTITY HIDE);
            PARTY_GHOST (LAVA_BRITTA, ADD_TO_PARTY);
            PARTY_GHOST (LAVA_BRITTA, MAKE_CURRENT_GHOST);
            PARTY_GHOST (TROY, REMOVE_FROM_PARTY);
            CURRENT_GHOST (ENTITY (MOVE_TO (110, 30)));
            CURRENT_GHOST (ENTITY UNFREEZE);
            ENEMY (HICKEY, ENTITY (MOVE_TO (124, 30)));
            ENEMY (HICKEY, ENTITY (SET_FACING LEFT));
            STEP (SET_FIXED_CAMERA (117, 30));
            STEP (WAIT 3.);
            STEP (UNHIDE_LAYER "boss-doors");
            ENEMY (HICKEY, SET_POSE "scream");
            CURRENT_GHOST (SET_POSE (PERFORMING (ATTACK RIGHT)));
            STEP (WAIT 1.);
            ENEMY (HICKEY, ENTITY UNFREEZE);
          ]
      | _ -> fail ())
    | "boss-killed" -> (
      game.in_boss_fight <- false;
      remove_nail := false;
      match Enemy.parse_name "boss-killed interaction" trigger.name_suffix with
      | DUNCAN ->
        [
          STEP (WAIT 1.);
          ENEMY (DUNCAN, ENTITY (SET_VX 0.));
          CURRENT_GHOST (PARTY (WALK_TO 52));
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          ENEMY (DUNCAN, WALK_TO 30);
          ENEMY (DUNCAN, SET_POSE "idle");
          ENEMY (DUNCAN, ENTITY (SET_FACING RIGHT));
          STEP (WAIT 0.6);
          STEP (SPAWN_VENGEFUL_SPIRIT (RIGHT, 32, 25));
          STEP (WAIT 0.3);
          ENEMY (DUNCAN, ENTITY (SET_VX config.duncan_chair_jump_vx));
          ENEMY (DUNCAN, START_ACTION "jumping");
          STEP (WAIT 1.2);
          ENEMY (DUNCAN, SET_POSE "scavenging-dead");
          ENEMY (DUNCAN, ENTITY FREEZE);
          STEP (HIDE_LAYER "bg-iso4");
          STEP (UNHIDE_LAYER "bg-iso5");
          PARTY_GHOST (JEFF, ENTITY (UNHIDE_AT (23, 24, 0., 0.)));
          PARTY_GHOST (ANNIE, ENTITY (UNHIDE_AT (21, 26, 0., 0.)));
          PARTY_GHOST (ANNIE, ENTITY FREEZE);
          PARTY_GHOST (JEFF, ENTITY FREEZE);
          STEP (WAIT 1.5);
          STEP (SET_FIXED_CAMERA (33, 33));
          STEP (WAIT 0.5);
          ENEMY (DUNCAN, SET_POSE "idle-dead");
          ENEMY (DUNCAN, ENTITY (SET_FACING LEFT));
        ]
        @ get_ability_steps "vengeful_spirit" 0. 1.
            [ "Consumed the"; "Vengeful Cushion" ]
            [
              fmt "Tap %s" (key_or_button state CAST);
              "to unleash the Cushion.";
              "Spells will deplete LIFE VAPOR.";
              "Replenish LIFE VAPOR by striking enemies.";
            ]
        @ [
            STEP (WAIT 0.2);
            CURRENT_GHOST FILL_LIFE_VAPOR;
            STEP (WAIT 0.2);
            STEP (DIALOGUE ("Duncan", "Real nice, Winger."));
            STEP
              (DIALOGUE
                 ( "Duncan",
                   "This is why the English never win any sports - because everyone else cheats!" ));
            ENEMY (DUNCAN, ENTITY UNFREEZE);
            ENEMY (DUNCAN, DEAD_WALK_TO 7);
            STEP (WAIT 0.2);
            STEP (DIALOGUE ("Annie", "Britta, there you are."));
            PARTY_GHOST (JEFF, SET_POSE CRAWLING);
            (* PARTY_GHOST (JEFF, SET_POSE (PERFORMING FOCUS)); *)
            STEP
              (DIALOGUE
                 ( "Jeff",
                   "Sweet, sweet portable chairs. {{gold}} Plastic gold, {{white}} four-legged \
                    diamonds!" ));
            PARTY_GHOST (JEFF, SET_POSE IDLE);
            STEP (DIALOGUE ("Jeff", "You claimin' this?"));
            PARTY_GHOST (JEFF, SET_POSE (PERFORMING (ATTACK RIGHT)));
            PARTY_GHOST (ANNIE, SET_POSE (PERFORMING (ATTACK RIGHT)));
            STEP (DIALOGUE ("Jeff", "Lava joust?"));
            STEP (DIALOGUE ("Britta", "Did you all hit your heads on each other's heads?"));
            PARTY_GHOST (JEFF, SET_POSE IDLE);
            PARTY_GHOST (ANNIE, SET_POSE IDLE);
            STEP
              (DIALOGUE
                 ( "Annie",
                   "Let's get real, Britta. Once the last of the chairs are gone, a sofa-hopper \
                    like you won't last 20 minutes." ));
            STEP
              (DIALOGUE
                 ("Annie", "You want to join this alliance? Or you want to join the {{red}} lava?"));
            STEP (DIALOGUE ("Britta", "Fine, but I'm not learning the new names for anything."));
            STEP (WAIT 0.4);
            STEP (WAIT 0.4);
            STEP SET_GHOST_CAMERA;
            STEP (WAIT 1.4);
            ENEMY (DUNCAN, ENTITY HIDE);
            STEP (HIDE_LAYER "boss-doors");
            PARTY_GHOST (JEFF, ENTITY HIDE);
            PARTY_GHOST (ANNIE, ENTITY HIDE);
          ]
      | LOCKER_BOY ->
        [
          PARTY_GHOST (ANNIE, ENTITY (UNHIDE_AT (157, 31, 0., 0.)));
          PARTY_GHOST (JEFF, ENTITY (UNHIDE_AT (156, 33, 0., 0.)));
          NPC (CHANG, ENTITY UNHIDE);
          NPC (CHANG, ENTITY (SET_FACING LEFT));
          STEP (HIDE_LAYER "boss-doors");
          STEP (WAIT 0.5);
          CURRENT_GHOST (SET_POSE IDLE);
          CURRENT_GHOST (PARTY (WALK_TO 160));
          PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
          PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
          STEP (DIALOGUE ("Chang", "Winger, Perry, and Edison..."));
          STEP (SET_FIXED_CAMERA (169, 42));
          STEP (WAIT 1.5);
          NPC (CHANG, SET_POSE "combination");
          STEP (DIALOGUE ("Chang", "What a delicious {{blue}} combination!"));
          STEP (DIALOGUE ("Britta", "What are we getting from this extra level of commitment?"));
          NPC (CHANG, SET_POSE "idle");
          STEP
            (DIALOGUE
               ( "Chang",
                 "We're getting your chairs, your food, and the names of your same-sex celebrity \
                  crushes. Everyone has one. Don't lie." ));
          STEP (DIALOGUE ("Chang", "Then you're free to go..."));
          NPC (CHANG, SET_POSE "attack");
          STEP (DIALOGUE ("Chang", "... into {{red}} lava!"));
          (* TODO sound effect for Troy/Abed entrance *)
          PARTY_GHOST (TROY, ENTITY (UNHIDE_AT (163, 20, 0., 0.)));
          PARTY_GHOST (ABED, ENTITY (UNHIDE_AT (161, 20, 0., 0.)));
          PARTY_GHOST (TROY, SET_POSE (AIRBORNE (-1.)));
          PARTY_GHOST (ABED, SET_POSE (AIRBORNE (-1.)));
          PARTY_GHOST (TROY, ENTITY FREEZE);
          PARTY_GHOST (ABED, ENTITY FREEZE);
          STEP (DIALOGUE ("Abed", "Stand down or meet your doom, Chang."));
          PARTY_GHOST (TROY, ENTITY UNFREEZE);
          PARTY_GHOST (ABED, ENTITY UNFREEZE);
          PARTY_GHOST (TROY, ENTITY (WAIT_UNTIL_LANDED false));
          PARTY_GHOST (TROY, SET_POSE IDLE);
          PARTY_GHOST (ABED, SET_POSE IDLE);
          NPC (CHANG, SET_POSE "idle");
          STEP (WAIT 0.3);
          STEP
            (DIALOGUE
               ( "Chang",
                 "You're in no position to make threats, floor-strider. Our truce ended when you \
                  {{red}} banished {{white}} us from the Payphone Bench." ));
          STEP
            (DIALOGUE
               ( "Troy",
                 "You used that bench to upset the balance. By the {{purple}} Vapors of Magmarath, \
                  {{white}} we will restore it." ));
          STEP (DIALOGUE ("Britta", "You have gods?"));
          NPC (CHANG, SET_POSE "idle-with-locker-boys");
          STEP (DIALOGUE ("Chang", "Locker Boys!"));
          STEP (SET_FIXED_CAMERA (175, 42));
          NPC (CHANG, SET_POSE "attack-with-locker-boys");
          STEP (WAIT 0.7);
          STEP (DIALOGUE ("Chang", "Earn your M&M's!"));
          PARTY_GHOST (TROY, WALK_TO 163);
          STEP (DIALOGUE ("Troy", "Troy and Abed Intimidation Stance!"));
          NPC (CHANG, SET_POSE "take-damage");
        ]
        @ jump_party_ghost ~end_pose:(PERFORMING (CAST DESOLATE_DIVE)) TROY RIGHT
            config.troy_dive_jump_vx 1.1
        @ [ STEP (WAIT 0.3); NPC (CHANG, ENTITY HIDE); STEP (WAIT 0.3) ]
        @ get_ability_steps "desolate_dive" 0. 3.
            [ "Consumed the"; "Troy and Abed Intimidation Stance." ]
            [
              fmt "Tap %s while" (key_or_button state CAST);
              "holding DOWN to strike the earth with a burst of intimidation.";
              "Spells will deplete LIFE VAPOR.";
              "Replenish LIFE VAPOR by striking enemies.";
            ]
        @ [
            STEP (WAIT 0.3);
            PARTY_GHOST (TROY, SET_POSE IDLE);
            STEP (WAIT 0.9);
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            STEP (DIALOGUE ("Hickey", "Hiya kids."));
            ENEMY (HICKEY, ENTITY UNHIDE);
            ENEMY (HICKEY, ENTITY UNFREEZE);
            ENEMY (HICKEY, ENTITY (SET_FACING LEFT));
            STEP (SET_FIXED_CAMERA (215, 17));
            STEP (WAIT 1.1);
            STEP (DIALOGUE ("Hickey", "I'm criminology professor Buzz Hickey."));
            STEP
              (DIALOGUE ("Hickey", "And this... this is just a little something I threw together."));
            ENEMY (HICKEY, SET_POSE "scream");
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            STEP (SET_FIXED_CAMERA (175, 42));
            STEP (WAIT 1.1);
            PARTY_GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ( "Abed",
                   "Jeff, Annie, get to {{purple}} Shirley Island! {{white}} We'll meet you there!"
                 ));
            PARTY_GHOST (ANNIE, WALK_TO 166);
            PARTY_GHOST (ANNIE, SET_POSE (AIRBORNE (-1.)));
            PARTY_GHOST (ANNIE, ENTITY (WAIT_UNTIL_LANDED false));
            PARTY_GHOST (ANNIE, ENTITY HIDE);
            PARTY_GHOST (JEFF, WALK_TO 166);
            PARTY_GHOST (JEFF, SET_POSE (AIRBORNE (-1.)));
            PARTY_GHOST (JEFF, ENTITY (WAIT_UNTIL_LANDED false));
            PARTY_GHOST (JEFF, ENTITY HIDE);
            PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Troy", "Abed, save yourself!"));
            PARTY_GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ( "Britta",
                   "Abed, before troy {{red}} dies in lava, {{white}} you can save yourself \
                    {{blue}} emotionally {{white}} by honestly experiencing the pain of him \
                    leaving Hallowdale." ));
            STEP (WAIT 0.4);
            PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP (WAIT 0.9);
            PARTY_GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP (WAIT 0.4);
            PARTY_GHOST (ABED, ADD_TO_PARTY);
            PARTY_GHOST (ABED, MAKE_CURRENT_GHOST);
            PARTY_GHOST (BRITTA, REMOVE_FROM_PARTY);
            STEP
              (DIALOGUE ("Abed", "We can do this in three steps. Britta, jump to that trash can."));
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (BRITTA, WALK_TO 163);
          ]
        @ jump_party_ghost BRITTA RIGHT config.britta_trash_can_jump_vx 1.1
        @ [
            STEP (WAIT 0.3);
            PARTY_GHOST (BRITTA, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Abed", "Now Troy, start inchworming."));
            (* if the cutscene is skipped, Troy ends up on the ledge above,
               so the extra WALK_TO and WAIT here is just to ensure he ends up on the bottom level
            *)
            PARTY_GHOST (TROY, WALK_TO 167);
            PARTY_GHOST (TROY, WALK_TO 157);
            STEP (WAIT 0.7);
            CURRENT_GHOST (PARTY (WALK_TO 167));
            CURRENT_GHOST (SET_POSE (AIRBORNE (-1.)));
            CURRENT_GHOST (ENTITY (WAIT_UNTIL_LANDED false));
            CURRENT_GHOST (PARTY (WALK_TO 155));
            STEP (DIALOGUE ("Britta", "What's the third step?"));
            STEP SET_GHOST_CAMERA;
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP
              (DIALOGUE ("Abed", "The third step is {{blue}} survival. {{white}} Good luck, Britta."));
            STEP (DIALOGUE ("Britta", "Seriously!?"));
            PARTY_GHOST (BRITTA, ENTITY (SET_FACING RIGHT));
            STEP (WAIT 0.5);
            PARTY_GHOST (BRITTA, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Britta", "Troy!"));
            STEP SET_GHOST_CAMERA;
            CURRENT_GHOST (PARTY (WALK_TO 153));
            PARTY_GHOST (TROY, WALK_TO 156);
            PARTY_GHOST (TROY, ENTITY (SET_FACING RIGHT));
            STEP
              (DIALOGUE
                 ( "Troy",
                   "Sorry Britta, Abed knows best. But I'll always remember you as {{red}} kinda \
                    slowing us down {{white}} and {{red}} complaining a lot." ));
            PARTY_GHOST (BRITTA, SET_POSE CRAWLING);
            PARTY_GHOST (TROY, WALK_TO 124);
            PARTY_GHOST (TROY, ENTITY HIDE);
            CURRENT_GHOST (PARTY (WALK_TO 84));
            STEP (WAIT 0.1);
          ]
      | BORCHERT ->
        [
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 1.5);
          STEP
            (DIALOGUE
               ( "Borchert",
                 "When I started Hallowdale, it was for ordinary people to access technology." ));
          STEP (SET_FIXED_CAMERA (20, 23));
          STEP (WAIT 1.);
          STEP (HIDE_LAYER "boss-doors");
          STEP (WAIT 1.);
        ]
      | LUIS_GUZMAN ->
        game.interaction.use_dashes_in_archives <- Some false;
        [
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 1.5);
          STEP
            (DIALOGUE
               ( "Luis Guzman",
                 "Don't worship the people leaving Hallowdale. Worship the people who are here. \
                  Worship this place. It changes people's lives. Look, I loved my time here. I got \
                  laid like crazy. That's way before {{orange}} Boogie Nights {{white}} too. Look, \
                  this is a special school." ));
          STEP (WAIT 1.);
          STEP (HIDE_LAYER "boss-doors");
        ]
      | BUDDY ->
        [
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 1.5);
          STEP
            (DIALOGUE
               ( "Buddy",
                 "Hola, mi amigo. Donde estas la biblioteca? Yo tengo hambre. Hola, mi amigo. \
                  Donde estas el restaurante? Lunes, martes, miercoles, vierno, no, no, no, no, \
                  no..." ));
          STEP (WAIT 1.);
          STEP (HIDE_LAYER "boss-doors");
        ]
      | VICE_DEAN_LAYBOURNE ->
        let facing =
          let boss = List.hd game.room.enemies in
          if rect_center_x boss.entity.dest < rect_center_x game.player.ghost.entity.dest then
            RIGHT
          else
            LEFT
        in
        [
          ENEMY (VICE_DEAN_LAYBOURNE, ENTITY (SET_FACING facing));
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 1.5);
          ENEMY (VICE_DEAN_LAYBOURNE, SET_POSE "dead-head-up");
          STEP (WAIT 1.5);
          ENEMY (VICE_DEAN_LAYBOURNE, SET_POSE "dead");
          STEP (WAIT 1.);
          STEP (HIDE_LAYER "boss-doors");
        ]
      | DEAN ->
        [
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 0.7);
          STEP
            (DIALOGUE
               ( "Dean",
                 "So after all my work, how will I be remembered? The bald dean with glasses, I \
                  guess." ));
          STEP (HIDE_LAYER "boss-doors");
        ]
      | JOSHUA ->
        [
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 0.7);
          STEP
            (DIALOGUE
               ( current_ghost_name,
                 "Oh my god! Joshua was {{red}} racist! {{white}} That came out of nowhere!" ));
          STEP (SET_FIXED_CAMERA (32, 50));
          STEP (SET_IGNORE_CAMERA_TRIGGERS true);
          STEP (WAIT 1.);
          STEP (HIDE_LAYER "boss-doors");
          CURRENT_GHOST (ENTITY UNSET_FLOOR);
          ENEMY (JOSHUA, ENTITY UNSET_FLOOR);
          STEP (WAIT 1.);
        ]
      | MANICORN_BOSS ->
        game.in_boss_fight <- true;
        [
          STEP (SET_IGNORE_CAMERA_TRIGGERS true);
          STEP (WAIT 1.);
          ENEMY (MANICORN_BOSS, ENTITY HIDE);
          CURRENT_GHOST (PARTY (WALK_TO 68));
          CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
          STEP (DIALOGUE ("Annie", "Yes! Ha ha ha!"));
          STEP (SPAWN_VENGEFUL_SPIRIT (RIGHT, 64, 94));
          STEP (WAIT 1.6);
          CURRENT_GHOST (SET_POSE (PERFORMING (TAKE_DAMAGE (0, RIGHT))));
          STEP (WAIT 1.);
          CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
          ENEMY (LAVA_BRITTA, ENTITY (MOVE_TO (44, 94)));
          ENEMY (LAVA_BRITTA, ENTITY UNFREEZE);
          ENEMY (LAVA_BRITTA, SET_POSE "idle");
          STEP (WAIT 1.);
          STEP (SET_FIXED_CAMERA (50, 87));
          STEP (DIALOGUE ("Britta", "Ha ha ha!"));
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          STEP (DIALOGUE ("Annie", "Britta, I'm your {{blue}} friend."));
          STEP
            (DIALOGUE
               ( "Britta",
                 "I can't hear {{red}} dead people, {{white}} Annie. I'm in a world of imagination."
               ));
          CURRENT_GHOST (PARTY (WALK_TO 75));
          PARTY_GHOST (JEFF, ADD_TO_PARTY);
          PARTY_GHOST (JEFF, ENTITY (UNHIDE_AT (75, 95, 0., 0.)));
          PARTY_GHOST (JEFF, MAKE_CURRENT_GHOST);
          PARTY_GHOST (ANNIE, REMOVE_FROM_PARTY);
          PARTY_GHOST (ANNIE, ENTITY HIDE);
          STEP (DIALOGUE ("Jeff", "Then imagine what the {{red}} floor {{white}} looks like!"));
          STEP (SET_FIXED_CAMERA (61, 87));
          STEP
            (DIALOGUE
               ( "Britta",
                 "I don't have to. I'll just imagine where {{blue}} you're {{white}} about to be."
               ));
          STEP (DIALOGUE ("Jeff", "That's the {{purple}} same {{white}} as imagining the floor."));
          STEP
            (DIALOGUE
               ("Britta", "Then you just {{green}} admitted {{white}} that's where you'll be."));
          STEP (DIALOGUE ("Jeff", "Knock knock, Britta."));
          STEP
            (DIALOGUE
               ( "Britta",
                 "I'm not gonna say \"who's there?\" because someone on the {{red}} floor \
                  {{white}} is knocking." ));
          STEP
            (DIALOGUE
               ("Jeff", "Well that's {{orange}} lame. {{white}} You have to say \"who's there?\""));
          STEP (DIALOGUE ("Britta", "Floor!"));
          STEP (DIALOGUE ("Jeff", "What?"));
          STEP (DIALOGUE ("Britta", "That's who's there!"));
          STEP (DIALOGUE ("Jeff", "Yeah, but it's {{red}} for you!"));
        ]
      | LAVA_BRITTA ->
        (* this needs to be set manually for boss fights in the final sequence because they
           aren't started with "boss-fight" triggers
        *)
        game.in_boss_fight <- true;
        [
          ENEMY (LAVA_BRITTA, ENTITY (SET_VX 0.));
          ENEMY (LAVA_BRITTA, SET_POSE "charge-lunge");
          STEP (WAIT 1.);
          NPC (SHIRLEY, ENTITY (MOVE_TO (195, 68)));
          NPC (SHIRLEY, ENTITY (SET_FACING LEFT));
          PARTY_GHOST (ABED, ENTITY (MOVE_TO (188, 68)));
          PARTY_GHOST (TROY, ENTITY (MOVE_TO (185, 68)));
          PARTY_GHOST (ABED, ENTITY UNFREEZE);
          PARTY_GHOST (TROY, ENTITY UNFREEZE);
          PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
          PARTY_GHOST (TROY, ENTITY (SET_FACING RIGHT));
          STEP (SET_IGNORE_CAMERA_TRIGGERS true);
          STEP (SET_FIXED_CAMERA (177, 61));
          STEP
            (DIALOGUE
               ( "Abed",
                 "Shirley, give us {{blue}} the orb {{white}} and we can save Shirley Island!" ));
          STEP
            (DIALOGUE
               ( "Shirley",
                 "{{blue}} The orb {{white}} can't {{green}} save {{white}} Shirley Island because \
                  Shirley Island {{purple}} is {{blue}} the orb." ));
          STEP
            (DIALOGUE
               ( "Abed",
                 "In a cool way like {{gold}} Keyser Soze {{white}} or in a lame way like \
                  {{magenta}} Jewel of the Nile?" ));
          STEP (WAIT 1.);
          NPC (SHIRLEY, ENTITY (SET_FACING RIGHT));
          STEP (WAIT 1.);
          STEP (SET_FIXED_CAMERA (187, 61));
          STEP
            (DIALOGUE
               ("Shirley", "You tell Buzz Hickey that {{purple}} Shirley Bennett {{white}} said..."));
          STEP (WAIT 1.);
          NPC (SHIRLEY, ENTITY (SET_FACING LEFT));
          STEP
            (DIALOGUE
               ( "Shirley",
                 "... well I don't want to waste your time. Just think of something {{blue}} cool \
                  {{white}} and give me credit." ));
          CURRENT_GHOST (ENTITY (MOVE_TO (62, 95)));
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          CURRENT_GHOST (SET_POSE IDLE);
          ENEMY (LAVA_BRITTA, ENTITY HIDE);
          ENEMY (LAVA_BRITTA_2, ENTITY UNHIDE);
          STEP (SET_FIXED_CAMERA (54, 87));
          STEP (WAIT 1.);
          STEP
            (DIALOGUE
               ( "Jeff",
                 "But the door I'm knocking on is {{green}} your {{white}} home, so if I'm the \
                  floor, it means you're {{red}} dead." ));
          STEP (DIALOGUE ("Britta", "If you're the floor, you're {{red}} already dead."));
          STEP (DIALOGUE ("Jeff", "Just do it right! Knock, knock!"));
          STEP (DIALOGUE ("Britta", "Knock, knock!"));
          ENEMY (LAVA_BRITTA_2, ENTITY UNFREEZE);
        ]
      | LAVA_BRITTA_2 ->
        game.in_boss_fight <- true;
        [
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX 0.));
          ENEMY (LAVA_BRITTA_2, SET_POSE "charge-lunge");
          STEP (WAIT 1.);
          ENEMY (LAVA_BRITTA_2, WALK_TO 46);
          STEP (WAIT 1.);
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_FACING RIGHT));
          CURRENT_GHOST (PARTY (WALK_TO 62));
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          STEP (WAIT 1.);
          CURRENT_GHOST (SET_POSE (PERFORMING (ATTACK LEFT)));
          ENEMY (LAVA_BRITTA_2, SET_POSE "charge-lunge");
          STEP (WAIT 1.);
          ENEMY (LAVA_BRITTA_2, SET_POSE "lunge");
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX config.lava_britta_lunge_vx));
          CURRENT_GHOST (ENTITY (SET_VX (-.config.jeff_lunge_vx)));
          STEP (WAIT 0.3);
          CURRENT_GHOST (SET_POSE (PERFORMING (TAKE_DAMAGE (0, RIGHT))));
          STEP (WAIT 0.1);
          CURRENT_GHOST (ENTITY (SET_VX 0.));
          ENEMY (LAVA_BRITTA_2, SET_POSE "idle");
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX 0.));
          STEP (WAIT 1.);
          CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_FACING LEFT));
          STEP (DIALOGUE ("Britta", "Who's there, bitch? Floor!"));
          STEP (DIALOGUE ("Britta", "Floooooooooor!"));
          STEP (SET_FIXED_CAMERA (65, 90));
          PARTY_GHOST (TROY, ADD_TO_PARTY);
          PARTY_GHOST (TROY, MAKE_CURRENT_GHOST);
          PARTY_GHOST (JEFF, REMOVE_FROM_PARTY);
          PARTY_GHOST (JEFF, ENTITY HIDE);
          NPC (TROY_AND_ABED_IN_A_BUBBLE, ENTITY UNHIDE);
          ENEMY (HICKEY, ENTITY (MOVE_TO (125, 90)));
          ENEMY (HICKEY, ENTITY (SET_FACING LEFT));
          ENEMY (HICKEY, SET_POSE "idle");
          ENEMY (HICKEY, ENTITY UNFREEZE);
          STEP (SET_FIXED_CAMERA (100, 90));
          STEP (DIALOGUE ("Troy and Abed", "Troy and Abed in a {{blue}} bubble!"));
          STEP (SET_FIXED_CAMERA (115, 90));
          STEP (WAIT 1.);
          ENEMY (HICKEY, SET_POSE "provoke");
          STEP (WAIT 1.);
          ENEMY (HICKEY, SET_POSE "provoke-wave");
          STEP (WAIT 1.);
          ENEMY (HICKEY, SET_POSE "walking");
          ENEMY (HICKEY, ENTITY (SET_VX (-.config.bubble_vx)));
          NPC (TROY_AND_ABED_IN_A_BUBBLE, SET_POSE "walking");
          NPC (TROY_AND_ABED_IN_A_BUBBLE, ENTITY (SET_VX config.bubble_vx));
          STEP (WAIT 1.3);
          ENEMY (HICKEY, ENTITY (SET_VX 0.));
          ENEMY (HICKEY, SET_POSE "idle");
          NPC (TROY_AND_ABED_IN_A_BUBBLE, ENTITY (SET_VX 0.));
          NPC (TROY_AND_ABED_IN_A_BUBBLE, SET_POSE "idle");
          STEP (WAIT 0.3);
          NPC (TROY_AND_ABED_IN_A_BUBBLE, ENTITY (SET_FACING LEFT));
          STEP (WAIT 0.3);
          STEP (DIALOGUE ("Troy", "Ha! Hickey, you {{red}} chicken!"));
          STEP (WAIT 0.5);
          ENEMY (HICKEY, ENTITY (SET_FACING RIGHT));
          STEP (WAIT 0.5);
          STEP (PLAY_SOUND_EFFECT "spray");
          STEP (WAIT 2.);
          STEP (DIALOGUE ("Abed", "He {{red}} gutted {{white}} us. Retreat!"));
          STEP (SET_FIXED_CAMERA (145, 90));
          NPC (TROY_AND_ABED_IN_A_BUBBLE, ENTITY (SET_FACING RIGHT));
          NPC (TROY_AND_ABED_IN_A_BUBBLE, SET_POSE "walking");
          NPC (TROY_AND_ABED_IN_A_BUBBLE, ENTITY (SET_VX config.bubble_vx));
          STEP (WAIT 1.78);
          NPC (TROY_AND_ABED_IN_A_BUBBLE, ENTITY UNSET_FLOOR);
          STEP (WAIT 0.6);
          ENEMY (HICKEY, SET_POSE "walking");
          ENEMY (HICKEY, ENTITY (SET_VX config.hickey_walk_vx));
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_FACING RIGHT));
          ENEMY (LAVA_BRITTA_2, ENTITY (MOVE_TO (108, 95)));
          ENEMY (LAVA_BRITTA_2, SET_POSE "with-hickey");
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX config.hickey_walk_vx));
          STEP (WAIT 3.);
          ENEMY (HICKEY, SET_POSE "idle");
          ENEMY (HICKEY, ENTITY (SET_VX 0.));
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX 0.));
          STEP (DIALOGUE ("Britta", "You can't outrun your {{blue}} emotions!"));
          STEP
            (DIALOGUE
               ( "Britta",
                 "I will force you two to {{orange}} grieve properly {{white}} even if it kills us \
                  all!" ));
          ENEMY (HICKEY, SET_POSE "walking");
          ENEMY (HICKEY, ENTITY (SET_VX config.hickey_walk_vx));
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX config.hickey_walk_vx));
          STEP (WAIT 0.5);
          STEP (SET_SCREEN_FADE (make_screen_fade (0, 255) 1.5));
          STEP (WAIT 2.);
        ]
      | HICKEY ->
        [
          ENEMY (HICKEY, ENTITY (SET_VX 0.));
          ENEMY (HICKEY, SET_POSE "dying");
          STEP (WAIT 0.5);
          ENEMY (HICKEY, SET_POSE "dead");
          STEP (WAIT 1.);
          STEP
            (DIALOGUE ("Hickey", "Unbelievable! When this game is over, I'm gonna shove you back."));
          (* TODO Hickey might be off-screen here - maybe find the enemy pos, convert to tile coords,
             and set fixed camera there
          *)
          STEP (HIDE_LAYER "boss-doors");
          STEP (SET_IGNORE_CAMERA_TRIGGERS true);
          STEP (SET_FIXED_CAMERA (72, 24));
          STEP (DIALOGUE ("Troy", "Abed, give me your other hand!"));
          STEP (DIALOGUE ("Abed", "It's down to us. You or Britta will be the winner."));
          STEP
            (DIALOGUE
               ( "Troy",
                 "I'm not leaving, ok? Just... I promise. The floor's not {{red}} lava {{white}} \
                  now. Just give me your hand." ));
          STEP
            (DIALOGUE
               ( "Abed",
                 "I don't think the {{red}} lava's {{white}} here because you're leaving. I think \
                  it's here because I won't {{red}} let go." ));
          STEP (WAIT 2.);
          STEP (DIALOGUE ("Abed", "Sorry."));
          STEP (WAIT 1.);
          STEP (DIALOGUE ("Abed", "Bye."));
          STEP (DIALOGUE ("Troy", "Abed... Abed!"));
          PARTY_GHOST (ABED, ENTITY (SET_VX 0.));
          PARTY_GHOST (ABED, ENTITY (SET_VY 0.));
          PARTY_GHOST (ABED, ENTITY UNFREEZE);
          PARTY_GHOST (ABED, ENTITY (WAIT_UNTIL_LANDED false));
          PARTY_GHOST (ABED, SET_POSE (PERFORMING FOCUS));
          STEP (WAIT 0.5);
          STEP (DIALOGUE ("Troy", "Abed..."));
          STEP (WAIT 0.5);
          STEP (CORNER_TEXT "El está muerto para siempre.");
          STEP (DIALOGUE ("Troy", "He's... He's fake dead. Forever."));
          STEP UNSET_CORNER_TEXT;
          STEP (SET_SCREEN_FADE (make_screen_fade (0, 255) 2.));
          STEP (WAIT 2.);
          PARTY_GHOST (ABED, ENTITY (MOVE_TO (7, 27)));
          PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
          PARTY_GHOST (TROY, ENTITY (MOVE_TO (14, 24)));
          PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
          CURRENT_GHOST (ENTITY (MOVE_TO (11, 24)));
          CURRENT_GHOST (SET_POSE IDLE);
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          STEP (SET_CAMERA_MOTION (LINEAR 16.));
          STEP (SET_FIXED_CAMERA (23, 24));
          STEP (WAIT 1.);
        ]
        @ clear_screen_fade 255 1.
        @ [
            STEP (DIALOGUE ("Britta", "He's not really dead."));
            STEP (DIALOGUE ("Troy", "No, but he's really playing dead and he's not gonna stop."));
            STEP (DIALOGUE ("Troy", "You don't get it. No one gets Abed. I got him a little."));
            STEP (WAIT 1.);
            STEP (DIALOGUE ("Troy", "This is my fault."));
            STEP (WAIT 1.);
            PARTY_GHOST (TROY, WALK_TO 18);
            STEP (DIALOGUE ("Troy", "I don't deserve to fake live."));
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (TROY, WALK_TO 22);
            STEP (DIALOGUE ("Britta", "Wait! No."));
            STEP (WAIT 0.7);
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            STEP (WAIT 0.7);
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Britta", "I can {{green}} fix {{white}} him."));
            PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Troy", "How?"));
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ("Britta", "I don't know. In real life, I don't know, but I can fake fix him."));
            STEP (WAIT 1.);
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP
              (DIALOGUE
                 ( "Britta",
                   "I can {{green}} clone {{white}} him. I'll {{green}} clone {{white}} him." ));
            STEP (WAIT 1.);
            STEP (DIALOGUE ("Troy", "... go on ..."));
            STEP
              (DIALOGUE
                 ( "Britta",
                   "I just need his {{blue}} DNA. {{white}} Let's get his {{blue}} DNA. {{white}} \
                    Don't touch the {{red}} lava." ));
            CURRENT_GHOST (PARTY (WALK_TO 8));
            CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
            STEP
              (DIALOGUE
                 ( "Troy",
                   "You might be onto something... I'm gonna find {{green}} discarded technology \
                    {{white}} from this {{orange}} once-great civilization {{white}} and start a \
                    {{blue}} cloning {{white}} machine." ));
            PARTY_GHOST (TROY, WALK_TO 16);
            PARTY_GHOST (TROY, SET_POSE READING);
            STEP (WAIT 1.);
            CURRENT_GHOST (SET_POSE IDLE);
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP
              (DIALOGUE
                 ( "Britta",
                   "Ok, I'm placing this {{blue}} Cellular Regeneration Sequencer {{white}} on the \
                    spot where he died." ));
            CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            STEP (WAIT 0.5);
            PARTY_GHOST (TROY, SET_POSE IDLE);
            PARTY_GHOST (TROY, WALK_TO 10);
            STEP (DIALOGUE ("Troy", "Here, don't forget this."));
            CURRENT_GHOST (SET_POSE IDLE);
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP (WAIT 1.);
            STEP
              (DIALOGUE
                 ( "Britta",
                   "This is a {{pink}} Laser Guidance System {{white}} that keeps the regeneration \
                    sequence from {{red}} jib-jabbing." ));
            CURRENT_GHOST (PARTY (WALK_TO 5));
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Troy", "{{red}} Jib-jabbing?"));
            STEP (WAIT 0.5);
            CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
            STEP (WAIT 0.5);
            STEP (DIALOGUE ("Britta", "Initiate regeneration sequence."));
            PARTY_GHOST (TROY, SET_POSE (PERFORMING FOCUS));
            STEP (PLAY_SOUND_EFFECT "menu-expand");
            STEP (PLAY_SOUND_EFFECT "menu-close");
            STEP (WAIT 0.2);
            STEP (PLAY_SOUND_EFFECT "menu-close");
            STEP (WAIT 0.2);
            STEP (PLAY_SOUND_EFFECT "menu-expand");
            STEP (WAIT 0.2);
            STEP (PLAY_SOUND_EFFECT "menu-expand");
            STEP (WAIT 2.);
            PARTY_GHOST (TROY, SET_POSE IDLE);
            CURRENT_GHOST (SET_POSE IDLE);
            STEP (WAIT 2.);
          ]
        @
        let game_progress = Progress.get_all_progress state game in
        if game.progress.dreamer_items_found = game_progress.dreamer_items.total then
          [
            PARTY_GHOST (ABED, SET_POSE IDLE);
            STEP (WAIT 1.);
            PARTY_GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP (WAIT 1.);
            PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Troy", "It worked!"));
            STEP (DIALOGUE ("Britta", "We made a {{green}} perfect clone {{white}} of Abed!"));
            PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP (WAIT 1.);
            STEP (DIALOGUE ("Abed", "Actually, Britta's work was sloppy."));
            PARTY_GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Abed", "I'm not an exact replication."));
            STEP
              (DIALOGUE
                 ( "Abed",
                   "I have all of Abed's {{blue}} abilities {{white}} and {{blue}} memories, \
                    {{white}} but I'm missing his {{green}} wild emotionality." ));
            STEP (WAIT 1.);
            PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP (WAIT 1.);
            STEP
              (DIALOGUE
                 ("Abed", "Although I think I may be able to {{red}} let Troy go {{white}} now."));
            STEP (WAIT 1.);
            PARTY_GHOST (TROY, ENTITY (SET_FACING RIGHT));
            STEP (WAIT 1.);
            STEP (DIALOGUE ("Troy", "I don't know..."));
            PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ( "Troy",
                   "I haven't been completely honest. I'm really {{red}} scared {{white}} to go on \
                    my trip." ));
            STEP
              (DIALOGUE ("Abed", "Well you don't have to go. Your {{blue}} clone {{white}} can."));
            STEP (WAIT 1.);
            STEP (DIALOGUE ("Troy", "Right."));
            STEP (WAIT 1.);
            PARTY_GHOST (TROY, WALK_TO 22);
            STEP (WAIT 1.);
            PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
            STEP (WAIT 1.);
            PARTY_GHOST (TROY, ENTITY (SET_FACING RIGHT));
            STEP (WAIT 1.);
          ]
          @ jump_party_ghost ~end_pose:(AIRBORNE (-1.)) TROY RIGHT config.troy_final_jump_vx 0.1
          @ [
              STEP (SET_SCREEN_FADE (make_screen_fade (0, 255) 1.));
              STEP (WAIT 5.);
              STEP STOP_MUSIC;
              STEP (WAIT 3.);
              STEP PLAY_END_CREDITS_MUSIC;
              STEP (WAIT 3.);
              STEP
                (CENTERED_TEXT
                   [
                     "";
                     "";
                     "";
                     "Congratulations.";
                     "Well done on achieving this great feat. You persevered and you triumphed.";
                     "We hope you enjoyed yourself in the world of Hallowdale.";
                     "We'll meet again soon on the road ahead.";
                   ]);
              STEP (WAIT 1.);
              STEP DISABLE_SKIP_INTERACTION;
              (let total_time = Progress.get_total_game_time state.frame.idx in
               let percentage = game_progress.total in
               STEP
                 (CENTERED_TEXT
                    [
                      "";
                      "";
                      "";
                      "Game Completion";
                      "================================";
                      fmt "Percentage: %.02f%s" percentage "%";
                      fmt "Time: %s" total_time;
                    ]));
              STEP (WAIT 4.);
              (* TODO fade out music *)
              STEP RETURN_TO_MAIN_MENU;
            ]
        else
          [
            STEP
              (DIALOGUE
                 ( "Troy",
                   "It didn't work... I think we need to collect more {{blue}} Dreamer Items." ));
            STEP
              (DIALOGUE
                 ( "Britta",
                   "We can use the {{blue}} Cellular Regeneration Sequencer {{white}} to go back \
                    in time to collect the rest." ));
            STEP (SET_SCREEN_FADE (make_screen_fade (0, 255) 2.));
            STEP (WAIT 3.);
            STEP RELOAD_GAME;
          ]
      | _ -> fail ())
    | "dream-nail" -> (
      game.in_boss_fight <- true;
      autosave_pos' := Some game.player.ghost.entity.dest.pos;
      match trigger.name_suffix with
      | "final-sequence" ->
        [
          STEP (SET_SCREEN_FADE (make_screen_fade ~show_ghost:true (0, 255) 0.5));
          STEP (SET_FIXED_CAMERA (210, 61));
          STEP (WAIT 2.);
          STEP (CENTERED_TEXT [ ""; "... No mind to think ..." ]);
          STEP (WAIT 2.);
          STEP (CENTERED_TEXT [ ""; "... No will to break ..." ]);
          STEP (WAIT 2.);
          STEP (CENTERED_TEXT [ ""; "... No dean to cry suffering ..." ]);
          STEP (WAIT 2.);
          STEP
            (SET_SCREEN_FADE
               { starting_alpha = 0; target_alpha = 255; timer = None; show_ghost = false });
        ]
        @ (if game.player.ghost.id <> ABED then [ PARTY_GHOST (ABED, MAKE_CURRENT_GHOST) ] else [])
        @ [
            CURRENT_GHOST (ENTITY (MOVE_TO (194, 69)));
            CURRENT_GHOST (SET_POSE IDLE);
            STEP SET_GHOST_CAMERA;
            PARTY_GHOST (JEFF, ENTITY (UNHIDE_AT (188, 68, 0., 0.)));
            PARTY_GHOST (ANNIE, ENTITY (UNHIDE_AT (186, 68, 0., 0.)));
            PARTY_GHOST (TROY, ENTITY (UNHIDE_AT (190, 68, 0., 0.)));
            PARTY_GHOST (JEFF, SET_POSE IDLE);
            PARTY_GHOST (ANNIE, SET_POSE IDLE);
            PARTY_GHOST (TROY, SET_POSE IDLE);
            PARTY_GHOST (JEFF, ENTITY UNSET_FLOOR);
            PARTY_GHOST (ANNIE, ENTITY UNSET_FLOOR);
            PARTY_GHOST (TROY, ENTITY UNSET_FLOOR);
            STEP (WAIT 3.);
          ]
        @ clear_screen_fade 255 1.
        @ [
            STEP (WAIT 1.);
            PARTY_GHOST (JEFF, ENTITY (SET_FACING LEFT));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING LEFT));
            PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            STEP (DIALOGUE ("Hickey", "Citizens of Shirley Island!"));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (TROY, ENTITY (SET_FACING RIGHT));
            CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Shirley", "What have you brought to my door?"));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING LEFT));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING LEFT));
            PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            ENEMY (HICKEY, ENTITY UNHIDE);
            ENEMY (HICKEY, ENTITY UNFREEZE);
            ENEMY (MANICORN, ENTITY UNHIDE);
            ENEMY (MANICORN, ENTITY UNFREEZE);
            NTH_ENEMY (1, MANICORN, ENTITY (SET_FACING LEFT));
            NTH_ENEMY (3, MANICORN, ENTITY (SET_FACING LEFT));
            (* manicorn idxs:
               0 - hickey
               1 - vicki
               2 - leonard
               3 - garrett
               4 - britta
               5 - big left
               6 - big center
               7 - big right
            *)
            NPC (NEIL, ENTITY (SET_FACING LEFT));
            NPC (NEIL, ENTITY UNFREEZE);
            STEP (SET_CAMERA_MOTION (LINEAR 16.));
            STEP (SET_FIXED_CAMERA (31, 10));
            STEP (WAIT 2.);
            STEP (DIALOGUE ("Neil", "Stay back!"));
            ENEMY (HICKEY, SET_POSE "scream");
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            NTH_ENEMY (0, MANICORN, SET_POSE "punch");
            NPC (NEIL, SET_POSE "take-damage");
            NPC (NEIL, ENTITY (SET_VX config.npc_death_vx));
            STEP (WAIT 0.3);
            NPC (NEIL, ENTITY UNSET_FLOOR);
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            NPC (NEIL, ENTITY HIDE);
            NPC (SHIRLEY, ENTITY (MOVE_TO (178, 68)));
            STEP (SET_FIXED_CAMERA (182, 68));
            STEP (DIALOGUE ("Shirley", "My God... it's Hickey! And he's got {{red}} Chairwalkers!"));
            STEP
              (DIALOGUE
                 ( "Hickey",
                   "Come out with your feet on the floor and there will be no need for {{red}} \
                    nudging {{white}} or {{red}} jostling." ));
            STEP
              (DIALOGUE ("Shirley", "I did not skip my son's birthday for {{blue}} second place!"));
            NPC (SHIRLEY, ENTITY (SET_FACING RIGHT));
            STEP (SHAKE_SCREEN 2.);
            ENEMY (LAVA_BRITTA, ENTITY UNHIDE);
            ENEMY (LAVA_BRITTA, SET_POSE "megaphone");
            ENEMY (MANICORN_BOSS, ENTITY UNHIDE);
            STEP (WAIT 2.);
            STEP (WAIT 2.);
            STEP
              (DIALOGUE
                 ("Britta", "I want to say something to you guys about {{blue}} mental health."));
            STEP (WAIT 1.);
            PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Jeff", "Is that Britta? Is she alive?"));
            STEP (DIALOGUE ("Shirley", "Why did you think she was dead?"));
            STEP (DIALOGUE ("Troy", "We, kind of... {{red}} left {{white}} her?"));
            STEP (DIALOGUE ("Annie", "Left her... for {{red}} dead?"));
            STEP
              (DIALOGUE
                 ( "Abed",
                   "Sounds {{red}} bad {{white}} when you put it that way. Can you put it a way \
                    that sounds {{green}} good?" ));
            STEP (SET_FIXED_CAMERA (53, 79));
            STEP (WAIT 1.);
            STEP
              (DIALOGUE
                 ( "Britta",
                   "You do realize this isn't just a pile of chairs, right? This is a crib, and \
                    you're curled up inside there, sucking your thumb because you're too scared to \
                    say \"good-bye\"." ));
            STEP
              (DIALOGUE
                 ( "Britta",
                   "Well, it's time to {{blue}} grow up. {{white}} The adults are here and we're \
                    going to tear down your fort." ));
            STEP (DIALOGUE ("Britta", "Chairwalkers, attack!"));
            NPC (VICKI, ENTITY (SET_FACING RIGHT));
            STEP (SET_FIXED_CAMERA (120, 50));
            STEP (WAIT 1.);
            STEP (DIALOGUE ("Vicki", "My name was Vicki! Tell my story!"));
            STEP (WAIT 1.);
            NTH_ENEMY (1, MANICORN, SET_POSE "punch");
            NPC (VICKI, ENTITY (SET_VX (-.config.npc_death_vx)));
            NPC (VICKI, ENTITY UNSET_FLOOR);
            NPC (VICKI, SET_POSE "take-damage");
            STEP (WAIT 0.3);
            NPC (VICKI, ENTITY UNSET_FLOOR);
            STEP (WAIT 0.3);
            STEP (SET_FIXED_CAMERA (115, 42));
            STEP
              (DIALOGUE
                 ("Garrett", "These are my {{red}} only pants! {{white}} I can't get them dirty!"));
            NPC (VICKI, ENTITY HIDE);
            NTH_ENEMY (3, MANICORN, SET_POSE "punch");
            NPC (GARRETT, SET_POSE "take-damage");
            NPC (GARRETT, ENTITY (SET_VX (-.config.npc_death_vx)));
            STEP (WAIT 0.35);
            NPC (GARRETT, ENTITY UNSET_FLOOR);
            STEP (WAIT 1.);
            NPC (GARRETT, ENTITY HIDE);
            PARTY_GHOST (JEFF, ENTITY (MOVE_TO (79, 70)));
            STEP SET_GHOST_CAMERA;
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Annie", "Do you have any rolling chairs?"));
            STEP (DIALOGUE ("Shirley", "Behind the curtain."));
            STEP (SET_FIXED_CAMERA (140, 42));
            STEP (DIALOGUE ("Leonard", "I knew this day would come. I'm out of here."));
            NTH_ENEMY (2, MANICORN, SET_POSE "punch");
            NPC (LEONARD, SET_POSE "take-damage");
            NPC (LEONARD, ENTITY (SET_VX config.npc_death_vx));
            STEP (WAIT 0.2);
            NPC (LEONARD, ENTITY UNSET_FLOOR);
            STEP (WAIT 1.);
            NPC (LEONARD, ENTITY HIDE);
            STEP (SET_FIXED_CAMERA (60, 80));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Jeff", "Hey, seat feet! {{blue}} Chair {{white}} to dance?"));
            STEP (SET_IGNORE_CAMERA_TRIGGERS true);
            PARTY_GHOST (JEFF, SET_POSE (PERFORMING (ATTACK LEFT)));
            PARTY_GHOST (ANNIE, ENTITY (MOVE_TO (71, 62)));
            PARTY_GHOST (ANNIE, ENTITY UNSET_FLOOR);
            PARTY_GHOST (ANNIE, SET_POSE (AIRBORNE (-1.)));
            PARTY_GHOST (ANNIE, MAKE_CURRENT_GHOST);
            PARTY_GHOST (JEFF, REMOVE_FROM_PARTY);
            PARTY_GHOST (ABED, REMOVE_FROM_PARTY);
            PARTY_GHOST (TROY, REMOVE_FROM_PARTY);
            CURRENT_GHOST (ENTITY (WAIT_UNTIL_LANDED false));
            STEP (UNHIDE_LAYER "boss-doors");
            STEP SET_GHOST_CAMERA;
            PARTY_GHOST (JEFF, ENTITY HIDE);
            ENEMY (MANICORN_BOSS, ENTITY UNFREEZE);
            ENEMY (HICKEY, ENTITY FREEZE);
          ]
      | _ -> fail ())
    | _ -> failwithf "unknown interaction prefix: %s" trigger.name_prefix
  in

  let steps' = get_interaction_steps trigger in
  let steps =
    match followup with
    | None -> steps'
    | Some followup' -> steps' @ get_interaction_steps followup'
  in
  [ STEP (INITIALIZE_INTERACTIONS { remove_nail = !remove_nail; autosave_pos = !autosave_pos' }) ]
  @ steps
  @ [ STEP (CONCLUDE_INTERACTIONS trigger) ]
