open Utils
open Types
open Types.Interaction

let cutscene_finished
    (progress_by_room : (string * Json_t.room_progress) list)
    (cutscene_name : string) : bool =
  let all_finished_interactions =
    List.map snd progress_by_room
    |> List.map (fun (r : Json_t.room_progress) -> r.finished_interactions)
    |> List.flatten
  in
  List.mem (fmt "cutscene:%s" cutscene_name) all_finished_interactions

let trigger_name trigger : string = String.maybe_trim_before '|' trigger.full_name

let get_steps
    state
    game
    ?(autosave_pos = None)
    ?(followup : trigger option = None)
    (trigger : trigger) : step list =
  let ability_text_outline x y =
    (* TODO move these to config *)
    let w, h = (150., 60.) in
    { pos = { x = x *. w; y = y *. h }; w; h }
  in
  let remove_nail = ref true in

  let get_interaction_steps (trigger : trigger) : step list =
    let fade_screen =
      [
        STEP
          (SET_SCREEN_FADE { target_alpha = 160; timer = Some (make_timer 1.5); show_ghost = false });
        STEP (WAIT 1.5);
      ]
    in
    let fade_screen_with_dramatic_pause steps =
      [ STEP (WAIT 0.5); CURRENT_GHOST (SET_POSE (PERFORMING FOCUS)); STEP (WAIT 1.) ]
      @ fade_screen
      @ steps
      @ [ STEP CLEAR_SCREEN_FADE; STEP (WAIT 0.2); CURRENT_GHOST (SET_POSE IDLE) ]
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

    let jump_party_ghost ?(end_pose = IDLE) ghost_id direction vx =
      [
        PARTY_GHOST (ghost_id, JUMP (direction, vx));
        (* this wait time is arbitrary and specific to the uses in the chang cutscene *)
        STEP (WAIT 1.1);
        PARTY_GHOST (ghost_id, SET_POSE end_pose);
      ]
    in
    let fail () =
      failwithf "unknown '%s' interaction: %s" trigger.name_prefix trigger.name_suffix
    in

    let unhide_and_unfreeze ?(direction : direction = LEFT) ghost_id x =
      [
        PARTY_GHOST (ghost_id, ENTITY (UNHIDE_AT (x, 67, 0., 0.)));
        PARTY_GHOST (ghost_id, ENTITY (SET_FACING direction));
        PARTY_GHOST (ghost_id, SET_POSE IDLE);
      ]
    in
    let read_sign = [ CURRENT_GHOST (SET_POSE READING); STEP (WAIT 0.4) ] in

    let opening_poem : step list =
      [
        CURRENT_GHOST (SET_POSE (AIRBORNE (-1.)));
        CURRENT_GHOST (ENTITY FREEZE);
        (* TODO center this text box *)
        STEP (WAIT 1.);
        STEP (TEXT [ "Give me some rope, tie me to dream." ]);
        STEP
          (TEXT [ "Give me some rope, tie me to dream."; "Give me the hope to run out of steam." ]);
        STEP
          (TEXT
             [
               "Give me some rope, tie me to dream.";
               "Give me the hope to run out of steam.";
               "Somebody said it could be here.";
             ]);
        STEP
          (TEXT
             [
               "Give me some rope, tie me to dream.";
               "Give me the hope to run out of steam.";
               "Somebody said it could be here.";
               "We could be roped up, tied up, dead in a year.";
             ]);
        STEP
          (TEXT
             [
               "Give me some rope, tie me to dream.";
               "Give me the hope to run out of steam.";
               "Somebody said it could be here.";
               "We could be roped up, tied up, dead in a year.";
               " - excerpt from \"At Least It Was Here\" by The 88";
             ]);
        STEP (WAIT 1.);
        CURRENT_GHOST (ENTITY UNFREEZE);
        STEP CLEAR_SCREEN_FADE;
      ]
    in

    (* TODO add dialogue for Shirley Island npcs
       HILDA -> "I live in the village. I love {{orange}} Abed.",
    *)
    match trigger.name_prefix with
    | "warp" -> [ STEP (WARP trigger.kind) ]
    | "door-warp" -> [ STEP (DOOR_WARP trigger.kind) ]
    | "weapon" ->
      (* TODO center this text *)
      fade_screen_with_dramatic_pause [ CURRENT_GHOST (ADD_ITEM (WEAPON trigger.name_suffix)) ]
    | "purple-pen" ->
      remove_nail := false;
      [ STEP (PURPLE_PEN_TEXT (get_lore ())) ]
    | "key" ->
      [
        STEP (WAIT 0.5);
        CURRENT_GHOST (SET_POSE (PERFORMING DIVE_COOLDOWN));
        STEP (WAIT 1.);
        STEP (TEXT [ fmt "Found a key: {{blue}} %s" trigger.name_suffix; get_lore () ]);
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
    | "talk" -> (
      match trigger.name_suffix with
      | "annies-boobs" -> (
        let rewards : (int * reward) list =
          [
            (20, INCREASE_MAX_SOUL);
            ( 30,
              ABILITY
                ("Soul Catcher", "increases the amount of LIFE VAPOR gained when striking an enemy.")
            );
            (40, ABILITY ("Quick Focus", "increases the speed of focusing LIFE VAPOR."));
            (50, INCREASE_MAX_SOUL);
            ( 60,
              ABILITY
                ("Soul Catcher", "increases the amount of LIFE VAPOR gained when striking an enemy.")
            );
            ( 70,
              ABILITY
                ( "Dream Wielder",
                  "charge the Honda Nail faster and collect more LIFE VAPOR when striking foes." )
            );
            (80, INCREASE_MAX_SOUL);
            ( 90,
              ABILITY
                ("Soul Catcher", "increases the amount of LIFE VAPOR gained when striking an enemy.")
            );
            (100, ABILITY ("Deep Focus", "heals two masks when focusing."));
            (110, ABILITY ("Shaman Stone", "increases the power of spells."));
            (120, ABILITY ("Spell Twister", "reduces the LIFE VAPOR cost of casting spells."));
          ]
        in
        let purple_pens_found = List.length game.progress.purple_pens_found in
        let last_upgrade = game.progress.last_upgrade_claimed in
        match List.find_opt (fun (amount, reward) -> amount > last_upgrade) rewards with
        | None -> [ STEP (DIALOGUE ("Annie's Boobs", "I have no more rewards for you.")) ]
        | Some (next_upgrade_amount, reward) ->
          if purple_pens_found >= next_upgrade_amount then
            [
              CURRENT_GHOST (SET_POSE (PERFORMING DIVE_COOLDOWN));
              STEP (WAIT 1.);
              CURRENT_GHOST (CLAIM_REWARD (next_upgrade_amount, reward));
              STEP (TEXT [ "Claimed a reward:"; Show.reward reward ]);
            ]
          else
            [
              STEP
                (DIALOGUE
                   ( "Annie's Boobs",
                     fmt
                       "You have {{purple}} %d purple pens {{white}} - come back when you have \
                        {{purple}} %d."
                       purple_pens_found next_upgrade_amount ));
            ])
      | name -> [ STEP (DIALOGUE (name, get_lore ())) ])
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
                     "Hold (A)";
                     "to focus LIFE VAPOR and HEAL.";
                   ] ));
            STEP CLEAR_SCREEN_FADE;
          ]
      | "true-form" ->
        read_sign
        @ [
            STEP
              (TEXT
                 [
                   "Human Beings, these words are for you alone.";
                   "";
                   "Within our lands do not hide your true form. For only this campus could \
                    produce ones such as you.";
                 ]);
          ]
      | "dean-and-creator" ->
        read_sign
        @ [
            STEP
              (TEXT
                 [
                   (* TEXT is left-aligned by default so this line looks weird, but it can't be manually centered with spaces
                      because Render.get_lines trims them
                   *)
                   "Human Beings, these words are for you alone.";
                   "";
                   "Beyond this point you enter the land of Dean and Creator. Step across this \
                    threshold and obey our laws.";
                 ]);
            STEP (WAIT 0.4);
            STEP
              (TEXT
                 [
                   "Bear witness to the last and only civilisation, the eternal Campus.";
                   "";
                   "Hallowdale";
                 ]);
          ]
      | lore -> read_sign @ [ STEP (TEXT [ get_lore () ]) ])
    | "ability" -> (
      let quote = Some (get_lore ()) in
      match trigger.name_suffix with
      | "scootenanny" ->
        get_ability_steps "mothwing_cloak" 0. 2. ~quote
          [ "Taken the"; "Scootenanny Chair." ]
          [
            "Press [ZR] to scootenanny forwards.";
            "Use the chair to scootenanny quickly along the ground or through the air.";
          ]
      | "double-bouncies" ->
        get_ability_steps "monarch_wings" 0. 4. ~quote
          [ "Consumed the"; "Double Bouncies." ]
          [
            "Press (B) while in the air to double bounce.";
            "Use the ethereal bounce to sail above enemies and discover new paths.";
          ]
      | "reverse-danny-thomas" ->
        get_ability_steps "mantis_claw" 0. 8. ~quote
          [ "Learned the"; "Reverse Danny Thomas." ]
          [
            "Press (B) while sliding against a wall to jump again.";
            "Jump from wall to wall to reach new areas.";
          ]
      | "computer-heart" ->
        get_ability_steps "crystal_heart" 0. 5. ~quote
          [ "Consumed the"; "Computer Heart." ]
          [
            "Hold [ZL] while on the ground or clinging to a wall to concentrate the force.";
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
            "Press [ZR] to scootenanny forwards, cloaked in hologram.";
            "Use the hologram to scootenanny through enemies and their attacks without taking \
             damage.";
          ]
      | "monkey-gas" ->
        get_ability_steps "howling_wraiths" 0. 6. ~quote
          [ "Consumed the"; "Monkey Knockout Gas." ]
          [ "Tap (A) while holding UP"; "to unleash the Knockout Gas." ]
      | "honda-nail" ->
        get_ability_steps "dream_nail" 0. 9. [ "Taken the"; "Honda Nail." ] ~quote
          [
            "Hold (X) to charge and slash with the nail.";
            "Cut through the veil between dreams and waking.";
          ]
      | "sealy-select" ->
        get_ability_steps "shade_soul" 0. 1. ~quote
          [ "Consumed the"; "Hypoallergenic Sealy Select." ]
          [
            "Tap (A)";
            "to unleash a more powerful Cushion.";
            "This spell consumes the same amount of LIFE VAPOR,";
            "with increased power.";
          ]
      | "torvins-flesh-of-fire" ->
        get_ability_steps "descending_dark" 0. 3. ~quote
          [ "Learned"; "Torvin's Flesh of Fire." ]
          [ "Tap (A) while holding DOWN"; "to strike the earth with a burst of white-hot flame." ]
      | "vapors-of-magmarath" ->
        get_ability_steps "abyss_shriek" 0. 6. ~quote
          [ "Consumed the"; "Vapors of Magmarath." ]
          [ "Tap (A) while holding UP"; "to unleash the Vapors." ]
      | _ -> fail ())
    | "boss-fight" -> (
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
                 "You stay back, Britta! I'm not afraid to push a girl into make-believe lava! In \
                  fact, it's been my primary strategy." ));
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
          STEP (DIALOGUE ("Britta", "They're setting a terrible example for today's young women."));
          STEP
            (DIALOGUE ("Duncan", "Well I'm sorry Britta, but it's either you or me. And I'm me."));
          STEP (UNHIDE_LAYER "boss-doors");
          ENEMY (DUNCAN, START_ACTION "jumping");
          ENEMY (DUNCAN, ENTITY (SET_VX Config.interactions.duncan_initial_jump_vx));
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
          ENEMY (BORCHERT, ENTITY (SET_VY 300.));
          STEP (WAIT 1.7);
          ENEMY (BORCHERT, ENTITY (SET_VY 0.));
          STEP (WAIT 2.);
          STEP (UNHIDE_LAYER "boss-doors");
          ENEMY (BORCHERT, SET_POSE "charge-shoot");
          STEP (WAIT 1.);
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
          (* TODO camera moves too fast here *)
          STEP (SET_CAMERA_MOTION (LINEAR 16.));
          STEP (SET_FIXED_CAMERA (127, 28));
          STEP (DIALOGUE ("Annie", "Mama mahogany, feast your feet on that stack of sticks."));
          STEP (WAIT 1.6);
          STEP SET_GHOST_CAMERA;
          STEP (WAIT 0.7);
          STEP (DIALOGUE ("Jeff", "Eh, too easy - could be a trap..."));
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
          STEP (SET_CAMERA_MOTION (LINEAR Config.interactions.shirley_island_camera_motion));
          (* TODO might be a problem that steps continue running without waiting for SET_FIXED_CAMERA to get to the destination
             - maybe could look at the motion speed and estimate how long it will take, then add a new_wait step
             - that's probably more work than it's worth though
          *)
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
          (* this isn't unhiding, just using it to move *)
          CURRENT_GHOST (ENTITY (UNHIDE_AT (177, 67, 0., 0.)));
          CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
        ]
        @ unhide_and_unfreeze TROY ~direction:RIGHT 174
        @ unhide_and_unfreeze JEFF 196
        @ unhide_and_unfreeze ANNIE 187
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
                    dream of standing on a table and pissing in a jar." ));
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
                   "You're not really playing, Shirley. You're a merchant, and more power to you. \
                    But don't withhold power from others just to make {{green}} money." ));
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
      | _ -> fail ())
    | "boss-killed" -> (
      game.in_boss_fight <- false;
      remove_nail := false;
      match Enemy.parse_name "boss-killed interaction" trigger.name_suffix with
      | LAVA_BRITTA ->
        [
          STEP (WAIT 1.);
          NPC (SHIRLEY, ENTITY (MOVE_TO (195, 68)));
          NPC (SHIRLEY, ENTITY (SET_FACING LEFT));
          PARTY_GHOST (ABED, ENTITY (MOVE_TO (192, 68)));
          PARTY_GHOST (TROY, ENTITY (MOVE_TO (190, 68)));
          PARTY_GHOST (ABED, ENTITY UNFREEZE);
          PARTY_GHOST (TROY, ENTITY UNFREEZE);
          PARTY_GHOST (ABED, ENTITY (SET_FACING RIGHT));
          PARTY_GHOST (TROY, ENTITY (SET_FACING RIGHT));
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
          ENEMY (LAVA_BRITTA, ENTITY HIDE);
          ENEMY (LAVA_BRITTA_2, ENTITY UNHIDE);
          CURRENT_GHOST (ENTITY (MOVE_TO (62, 95)));
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          CURRENT_GHOST (SET_POSE IDLE);
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
        [
          STEP (WAIT 1.);
          ENEMY (LAVA_BRITTA_2, WALK_TO 46);
          STEP (WAIT 1.);
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_FACING RIGHT));
          CURRENT_GHOST (PARTY (WALK_TO 62));
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          CURRENT_GHOST (SET_POSE (PERFORMING (ATTACK LEFT)));
          ENEMY (LAVA_BRITTA_2, SET_POSE "charge-lunge");
          STEP (WAIT 1.);
          ENEMY (LAVA_BRITTA_2, SET_POSE "lunge");
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX 2000.));
          STEP (WAIT 0.3);
          CURRENT_GHOST (SET_POSE (PERFORMING (TAKE_DAMAGE (0, RIGHT))));
          STEP (WAIT 0.1);
          ENEMY (LAVA_BRITTA_2, SET_POSE "idle");
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_VX 0.));
          STEP (WAIT 1.);
          CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
          STEP (SET_FIXED_CAMERA (54, 87));
          PARTY_GHOST (TROY, ADD_TO_PARTY);
          PARTY_GHOST (TROY, MAKE_CURRENT_GHOST);
          PARTY_GHOST (JEFF, REMOVE_FROM_PARTY);
          ENEMY (LAVA_BRITTA_2, ENTITY (SET_FACING LEFT));
          STEP (DIALOGUE ("Britta", "Who's there, bitch? Floor!"));
          STEP (DIALOGUE ("Britta", "Floooooooooor!"));
          PARTY_GHOST (JEFF, ENTITY HIDE);
        ]
      | MANICORN_3 ->
        [
          STEP (WAIT 1.);
          ENEMY (MANICORN_3, ENTITY HIDE);
          CURRENT_GHOST (PARTY (WALK_TO 68));
          CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
          STEP (DIALOGUE ("Annie", "Yes! Ha ha ha!"));
          STEP (SPAWN_VENGEFUL_SPIRIT (RIGHT, 65, 94));
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
                 "I'm not gonna say \"who's there?\" because someone on the {{red}} floor is \
                  knocking." ));
          STEP
            (DIALOGUE
               ("Jeff", "Well that's {{orange}} lame. {{white}} You have to say \"who's there?\""));
          STEP (DIALOGUE ("Britta", "Floor!"));
          STEP (DIALOGUE ("Jeff", "What?"));
          STEP (DIALOGUE ("Britta", "That's who's there!"));
          STEP (DIALOGUE ("Jeff", "Yeah, but it's {{red}} for you!"));
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
        let current_ghost_name =
          match game.player.ghost.id with
          | TROY -> "Troy"
          | ABED -> "Abed"
          | JEFF -> "Jeff"
          | ANNIE -> "Annie"
          | BRITTA -> "Britta"
        in
        [
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (WAIT 0.7);
          STEP
            (DIALOGUE
               ( current_ghost_name,
                 "Oh my god! Joshua was {{red}} racist! {{white}} That came out of nowhere!" ));
          STEP (HIDE_LAYER "boss-doors");
          CURRENT_GHOST (ENTITY UNSET_FLOOR);
          ENEMY (JOSHUA, ENTITY UNSET_FLOOR);
        ]
      | DUNCAN ->
        [
          ENEMY (DUNCAN, ENTITY (SET_VX 0.));
          CURRENT_GHOST (PARTY (WALK_TO 52));
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          ENEMY (DUNCAN, WALK_TO 30);
          ENEMY (DUNCAN, SET_POSE "idle");
          ENEMY (DUNCAN, ENTITY (SET_FACING RIGHT));
          STEP (WAIT 0.6);
          STEP (SPAWN_VENGEFUL_SPIRIT (RIGHT, 32, 25));
          STEP (WAIT 0.3);
          ENEMY (DUNCAN, ENTITY (SET_VX Config.interactions.duncan_chair_jump_vx));
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
              "Tap (A)";
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
          STEP (HIDE_LAYER "boss-doors");
          STEP (WAIT 0.5);
          CURRENT_GHOST (SET_POSE IDLE);
          (* PARTY_GHOST (JEFF, WALK_TO 153);
           * PARTY_GHOST (ANNIE, WALK_TO 155); *)
          CURRENT_GHOST (PARTY (WALK_TO 160));
          (* PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT)); *)
          PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
          PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
          (* TODO center this text box so it doesn't overlap the ghosts or chang *)
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
          STEP (WAIT 1.);
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
                 "You used that bench to upset the balance. By the {{orange}} Vapors of Magmarath, \
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
            Config.interactions.troy_dive_jump_vx
        @ [ STEP (WAIT 0.3); NPC (CHANG, ENTITY HIDE); STEP (WAIT 0.3) ]
        @ get_ability_steps "desolate_dive" 0. 3.
            [ "Consumed the"; "Troy and Abed Intimidation Stance." ]
            [
              "Tap (A) while";
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
            NPC (HICKEY, ENTITY UNHIDE);
            NPC (HICKEY, ENTITY UNFREEZE);
            NPC (HICKEY, ENTITY (SET_FACING LEFT));
            STEP (SET_FIXED_CAMERA (215, 17));
            STEP (WAIT 1.1);
            STEP (DIALOGUE ("Hickey", "I'm criminology professor Buzz Hickey."));
            STEP
              (DIALOGUE ("Hickey", "And this... this is just a little something I threw together."));
            NPC (HICKEY, SET_POSE "walking");
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            STEP (SHAKE_SCREEN 1.);
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
            PARTY_GHOST (JEFF, WALK_TO 166);
            PARTY_GHOST (JEFF, SET_POSE (AIRBORNE (-1.)));
            STEP (WAIT 0.7);
            PARTY_GHOST (ANNIE, ENTITY HIDE);
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
        @ jump_party_ghost BRITTA RIGHT Config.interactions.britta_trash_can_jump_vx
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
            STEP (WAIT 0.7);
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
                   "Sorry Britta, Abed knows best. But I'll always remember you as kinda slowing \
                    us down and complaining a lot." ));
            PARTY_GHOST (BRITTA, SET_POSE CRAWLING);
            PARTY_GHOST (TROY, WALK_TO 124);
            PARTY_GHOST (TROY, ENTITY HIDE);
            CURRENT_GHOST (PARTY (WALK_TO 84));
            STEP (WAIT 0.1);
          ]
      | _ -> fail ())
    | "dream-nail" -> (
      match trigger.name_suffix with
      | "final-sequence" ->
        let other_ghosts =
          List.filter_map
            (fun (p : party_ghost) ->
              if p.ghost.id = game.player.ghost.id || not p.in_party then None else Some p.ghost.id)
            game.party
        in
        [
          (* start

          *)
          STEP
            (SET_SCREEN_FADE
               { target_alpha = 255; timer = Some (make_timer 0.5); show_ghost = true });
          STEP (SET_FIXED_CAMERA (210, 61));
          STEP (WAIT 2.);
          STEP (TEXT [ "... No mind to think ..." ]);
          STEP (WAIT 2.);
          STEP (TEXT [ "... No will to break ..." ]);
          STEP (WAIT 2.);
          STEP (TEXT [ "... No dean to cry suffering ..." ]);
          STEP (WAIT 2.);
          STEP
            (SET_SCREEN_FADE
               { target_alpha = 255; timer = Some (make_timer 0.5); show_ghost = false });
          STEP (WAIT 1.);
        ]
        (* switch current ghost

        *)
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
            STEP (WAIT 1.);
            STEP CLEAR_SCREEN_FADE;
            STEP (WAIT 1.);
            PARTY_GHOST (JEFF, ENTITY (SET_FACING LEFT));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING LEFT));
            PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
            CURRENT_GHOST (ENTITY (SET_FACING LEFT));
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            (* Hickey

          *)
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
            NPC (HICKEY, ENTITY UNHIDE);
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
            (* Death of Neil

          *)
            NPC (NEIL, ENTITY (SET_FACING LEFT));
            NPC (NEIL, ENTITY UNFREEZE);
            STEP (SET_CAMERA_MOTION (LINEAR 16.));
            STEP (SET_FIXED_CAMERA (31, 10));
            STEP (WAIT 2.);
            STEP (DIALOGUE ("Neil", "Stay back!"));
            NPC (HICKEY, SET_POSE "walking");
            NPC (NEIL, SET_POSE "take-damage");
            STEP (SHAKE_SCREEN 1.);
            STEP (WAIT 1.);
            NTH_ENEMY (0, MANICORN, SET_POSE "punch");
            NPC (NEIL, ENTITY (SET_VX 300.));
            STEP (WAIT 0.6);
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
            STEP (DIALOGUE ("Shirley", "I did not skip my son's birthday for second place!"));
            NPC (SHIRLEY, ENTITY (SET_FACING RIGHT));
            STEP (SHAKE_SCREEN 2.);
            (* return of Britta

          *)
            ENEMY (LAVA_BRITTA, ENTITY UNHIDE);
            ENEMY (LAVA_BRITTA, SET_POSE "megaphone");
            ENEMY (MANICORN_3, ENTITY UNHIDE);
            STEP (WAIT 2.);
            STEP (SET_FIXED_CAMERA (47, 80));
            STEP (WAIT 2.);
            STEP
              (DIALOGUE
                 ("Britta", "I want to say something to you guys about {{blue}} mental health."));
            STEP (SET_FIXED_CAMERA (182, 68));
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
            STEP (SET_FIXED_CAMERA (47, 80));
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
            STEP (SET_FIXED_CAMERA (120, 56));
            STEP (WAIT 1.);
            (* Death of other characters

          *)
            STEP (DIALOGUE ("Vicki", "My name was Vicki! Tell my story!"));
            STEP (WAIT 1.);
            NTH_ENEMY (1, MANICORN, SET_POSE "punch");
            NPC (VICKI, ENTITY (SET_VX (-500.)));
            NPC (VICKI, ENTITY UNSET_FLOOR);
            NPC (VICKI, SET_POSE "take-damage");
            STEP (WAIT 0.3);
            NPC (VICKI, ENTITY UNSET_FLOOR);
            STEP (WAIT 0.3);
            STEP (SET_FIXED_CAMERA (115, 48));
            STEP
              (DIALOGUE
                 ("Garrett", "These are my {{red}} only pants! {{white}} I can't get them dirty!"));
            NPC (VICKI, ENTITY HIDE);
            NTH_ENEMY (3, MANICORN, SET_POSE "punch");
            NPC (GARRETT, SET_POSE "take-damage");
            NPC (GARRETT, ENTITY (SET_VX (-500.)));
            STEP (WAIT 0.35);
            NPC (GARRETT, ENTITY UNSET_FLOOR);
            STEP (WAIT 1.);
            NPC (GARRETT, ENTITY HIDE);
            PARTY_GHOST (JEFF, ENTITY (MOVE_TO (79, 70)));
            STEP SET_GHOST_CAMERA;
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Annie", "Do you have any rolling chairs?"));
            STEP (DIALOGUE ("Shirley", "Behind the curtain."));
            STEP (SET_FIXED_CAMERA (140, 48));
            STEP (DIALOGUE ("Leonard", "I knew this day would come. I'm out of here."));
            NTH_ENEMY (2, MANICORN, SET_POSE "punch");
            NPC (LEONARD, SET_POSE "take-damage");
            NPC (LEONARD, ENTITY (SET_VX 500.));
            STEP (WAIT 0.2);
            NPC (LEONARD, ENTITY UNSET_FLOOR);
            STEP (WAIT 1.);
            NPC (LEONARD, ENTITY HIDE);
            (* Boss fight

            *)
            STEP (SET_FIXED_CAMERA (60, 80));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING LEFT));
            STEP (DIALOGUE ("Jeff", "Hey, seat feet! {{blue}} Chair {{white}} to dance?"));
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
            ENEMY (MANICORN_3, ENTITY UNFREEZE);
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
  [ STEP (INITIALIZE_INTERACTIONS { remove_nail = !remove_nail; autosave_pos }) ]
  @ steps
  @ [ STEP (CONCLUDE_INTERACTIONS trigger) ]
