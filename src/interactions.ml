open Utils
open Types
open Types.Interaction

let get_steps ?(increase_health = false) state game (triggers : trigger list) : step list =
  let ability_text_outline x y =
    (* TODO move these to config *)
    let w, h = (150., 60.) in
    { pos = { x = x *. w; y = y *. h }; w; h }
  in
  let remove_nail = ref true in

  let get_interaction_steps (trigger : trigger) : step list =
    let fade_screen_with_dramatic_pause steps =
      [
        STEP (WAIT 0.5);
        CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
        STEP (WAIT 1.);
        (* TODO gradually fade screen out to black *)
        STEP FADE_SCREEN_OUT;
        STEP (WAIT 0.8);
      ]
      @ steps
      @ [ STEP FADE_SCREEN_IN; STEP (WAIT 0.2); CURRENT_GHOST (SET_POSE IDLE) ]
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
      match StringMap.find_opt trigger.name_suffix state.global.lore with
      | None -> failwithf "lore name '%s' not found in lore.json" trigger.name_suffix
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
        (* PARTY_GHOST (ghost_id, ENTITY UNFREEZE); *)
        PARTY_GHOST (ghost_id, SET_POSE IDLE);
      ]
    in
    let read_sign = [ CURRENT_GHOST (SET_POSE READING); STEP (WAIT 0.4) ] in

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
    | "increase-health" ->
      [
        STEP (WAIT 0.5);
        CURRENT_GHOST (SET_POSE (PERFORMING DIVE_COOLDOWN));
        STEP (WAIT 1.);
        CURRENT_GHOST (INCREASE_HEALTH_TEXT "Max health increased.");
      ]
    | "d-nail-item" -> (
      match trigger.name_suffix with
      | "dreamnailitem" -> [ STEP (TEXT [ "Give me some rope, tie me to dream." ]) ]
      | _ -> fail ())
    | "dreamer" ->
      fade_screen_with_dramatic_pause
        [ CURRENT_GHOST (ADD_ITEM (DREAMER (trigger.name_suffix, get_lore ()))) ]
    | "info" -> (
      match trigger.name_suffix with
      | "focus" ->
        read_sign
        @ [
            STEP FADE_SCREEN_OUT;
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
            STEP FADE_SCREEN_IN;
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
      match trigger.name_suffix with
      | "scootenanny" ->
        get_ability_steps "mothwing_cloak" 0. 2.
          [ "Taken the"; "Scootenanny Chair." ]
          [
            "Press [ZR] to scootenanny forwards.";
            "Use the chair to scootenanny quickly along the ground or through the air.";
          ]
          ~quote:
            (Some
               "While I normally don't condone climbing on furniture, Troy and Abed's friendship \
                has been such a special and magical part of Greendale, we owe it to ourselves to \
                honor it.")
      | "double-bouncies" ->
        get_ability_steps "monarch_wings" 0. 4.
          [ "Consumed the"; "Double Bouncies." ]
          [
            "Press (B) while in the air to double bounce.";
            "Use the ethereal bounce to sail above enemies and discover new paths.";
          ]
          ~quote:
            (Some
               "At the apex of each bounce, there is a moment outside of time, outside of words, \
                outside of everything - a perfect moment. A silent moment. I call it the {{blue}} \
                World's Whisper.")
      | "reverse-danny-thomas" ->
        get_ability_steps "mantis_claw" 0. 8.
          [ "Learned the"; "Reverse Danny Thomas." ]
          [
            "Press (B) while sliding against a wall to jump again.";
            "Jump from wall to wall to reach new areas.";
          ]
          ~quote:(Some "Do you think this game's gotten a little out of hand?")
      | "computer-heart" ->
        get_ability_steps "crystal_heart" 0. 5.
          [ "Consumed the"; "Computer Heart." ]
          [
            "Hold [ZL] while on the ground or clinging to a wall to concentrate the force.";
            "Release the button to blast forwards and fly through the air.";
          ]
          ~quote:(Some "Without an emotional component, computers will strip us of all humanity.")
        @ [ STEP (HIDE_LAYER "temporary-floors") ]
      | "vaughns-tear" ->
        get_ability_steps "ismas_tear" 0. 10.
          [ "Consumed the"; "Vaughn's Tear." ]
          [ "Acid shall be repelled."; "Swim in acidic waters without coming to any harm." ]
          ~quote:(Some "Everything is connected. Rocks. Eagles. Hats.")
      | "pierce-hologram" ->
        get_ability_steps "shade_cloak" 0. 7.
          [ "Consumed the"; "Pierce Hologram." ]
          [
            "Press [ZR] to scootenanny forwards, cloaked in hologram.";
            "Use the hologram to scootenanny through enemies and their attacks without taking \
             damage.";
          ]
          ~quote:
            (Some
               "Take it from a man with no legal right to be there: you're in a {{blue}} special \
                {{white}} place.")
      | "monkey-gas" ->
        get_ability_steps "howling_wraiths" 0. 6.
          [ "Consumed the"; "Monkey Knockout Gas." ]
          [ "Tap (A) while holding UP"; "to unleash the Knockout Gas." ]
          ~quote:(Some "Some kind of {{red}} gas {{white}} that knocks out monkeys.")
      | "honda-nail" ->
        get_ability_steps "dream_nail" 0. 9. [ "Taken the"; "Honda Nail." ]
          [
            "Hold (X) to charge and slash with the nail.";
            "Cut through the veil between dreams and waking.";
          ]
          ~quote:(Some "The power of dreams.")
      | _ -> fail ())
    | "cutscene" -> (
      match trigger.name_suffix with
      | "opening-poem" ->
        [
          (* TODO center this text box *)
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
          STEP FADE_SCREEN_IN;
        ]
      | "kp-landing" ->
        [ CURRENT_GHOST (SET_POSE (PERFORMING FOCUS)); STEP (SHAKE_SCREEN 1.); STEP (WAIT 2.0) ]
      | "fight-duncan" ->
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
          STEP (WAIT 0.7);
          STEP (DIALOGUE ("Britta", "They're setting a terrible example for today's young women."));
          STEP
            (DIALOGUE ("Duncan", "Well I'm sorry Britta, but it's either you or me. And I'm me."));
          STEP UNHIDE_BOSS_DOORS;
          ENEMY (DUNCAN, START_ACTION "jumping");
          ENEMY (DUNCAN, SET_VX (-350.));
          ENEMY (DUNCAN, ENTITY UNFREEZE);
          STEP (WAIT 0.7);
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
      | "fight-locker-boys" ->
        [
          ENEMY (LOCKER_BOY, ENTITY HIDE);
          CURRENT_GHOST (PARTY (WALK_TO 131));
          STEP (WAIT 0.7);
          CURRENT_GHOST (SET_POSE READING);
          STEP (WAIT 0.7);
          STEP UNHIDE_BOSS_DOORS;
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
      | "arrive-at-shirley-island" ->
        [
          CURRENT_GHOST (SET_POSE IDLE);
          CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
          NPC (NEIL, ENTITY (SET_FACING LEFT));
          STEP (WAIT 1.);
          STEP (DIALOGUE ("Neil", "Welcome to {{purple}} Shirley Island."));
          NPC (NEIL, ENTITY (SET_FACING RIGHT));
          STEP (SET_CAMERA_MOTION (LINEAR 3.));
          (* TODO might be a problem that steps continue running without waiting for SET_FIXED_CAMERA to get to the destination
             - maybe could look at the motion speed and estimate how long it will take, then add a new_wait step
             - that's probably more work than it's worth though
          *)
          (* STEP (SET_FIXED_CAMERA (75, 75)); *)
          STEP (SET_FIXED_CAMERA (125, 48));
          (* STEP (WAIT 1.); *)
          STEP
            (DIALOGUE
               ( "Neil",
                 "No furniture beyond this point. Leave your weapons at the door, and any spare \
                  doors at the entrance." ));
          (* STEP (WAIT 3.); *)
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
        @ unhide_and_unfreeze JEFF 202
        @ unhide_and_unfreeze ANNIE 198
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
                    But don't withhold power from others just to make money." ));
            PARTY_GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
            PARTY_GHOST (JEFF, ENTITY (SET_FACING RIGHT));
            CURRENT_GHOST (PARTY (WALK_TO 201));
            STEP (DIALOGUE ("Abed", "We want {{blue}} the orb."));
            STEP (DIALOGUE ("Troy", "Abed..."));
            PARTY_GHOST (TROY, WALK_TO 203);
            PARTY_GHOST (TROY, ENTITY (SET_FACING LEFT));
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
      remove_nail := false;
      match trigger.name_suffix with
      | "DUNCAN" ->
        [
          CURRENT_GHOST (PARTY (WALK_TO 52));
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          ENEMY (DUNCAN, WALK_TO 30);
          ENEMY (DUNCAN, SET_POSE "idle");
          ENEMY (DUNCAN, ENTITY (SET_FACING RIGHT));
          STEP (WAIT 0.6);
          STEP (SPAWN_VENGEFUL_SPIRIT (RIGHT, 32, 25));
          STEP (WAIT 0.3);
          ENEMY (DUNCAN, SET_VX 150.);
          ENEMY (DUNCAN, START_ACTION "jumping");
          STEP (WAIT 1.2);
          ENEMY (DUNCAN, SET_POSE "scavenging");
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
          ENEMY (DUNCAN, SET_POSE "idle");
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
            ENEMY (DUNCAN, WALK_TO 8);
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
            STEP HIDE_BOSS_DOORS;
            PARTY_GHOST (JEFF, ENTITY HIDE);
            PARTY_GHOST (ANNIE, ENTITY HIDE);
          ]
      | "LOCKER_BOY" ->
        [
          PARTY_GHOST (ANNIE, ENTITY (UNHIDE_AT (157, 31, 0., 0.)));
          PARTY_GHOST (JEFF, ENTITY (UNHIDE_AT (156, 33, 0., 0.)));
          STEP HIDE_BOSS_DOORS;
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
          STEP (WAIT 0.5);
          STEP (DIALOGUE ("Chang", "What a delicious {{blue}} combination!"));
          STEP (DIALOGUE ("Britta", "What are we getting from this extra level of commitment?"));
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
        @ jump_party_ghost ~end_pose:(PERFORMING (CAST DESOLATE_DIVE)) TROY RIGHT 900.
        @ [ NPC (CHANG, ENTITY HIDE); STEP (WAIT 0.3) ]
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
            (* TODO maybe add SHAKE_SCREEN step *)
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
            (* PARTY_GHOST (TROY, MAKE_CURRENT_GHOST); *)
            (* STEP SET_GHOST_CAMERA; *)
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
        @ jump_party_ghost BRITTA RIGHT 300.
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
    | _ -> failwithf "unknown interaction prefix: %s" trigger.name_prefix
  in

  let steps = List.concat_map get_interaction_steps triggers in

  (* SET_GHOST_CAMERA to reset the camera if it changed *)
  [ STEP (INITIALIZE_INTERACTIONS !remove_nail) ]
  @ steps
  @ [
      STEP
        (SET_CAMERA_MOTION (SMOOTH (Config.window.camera_motion.x, Config.window.camera_motion.y)));
      STEP SET_GHOST_CAMERA;
      STEP RESET_TEXT;
    ]
