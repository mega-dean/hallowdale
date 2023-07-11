open Types
open Types.Interaction

[@@@ocaml.warning "-26-27-32"]

let get_steps ?(increase_health = false) state game (full_interaction_name : string) : step list =
  let ability_text_outline x y =
    (* TODO move these to config *)
    let w, h = (150., 60.) in
    { pos = { x = x *. w; y = y *. h }; w; h }
  in
  let remove_nail = ref true in

  let interaction_steps : step list =
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

    let get_ability_steps ability_name outline_x outline_y top_lines bottom_lines =
      fade_screen_with_dramatic_pause
        [
          CURRENT_GHOST (ADD_ITEM (ABILITY ability_name));
          STEP
            (ABILITY_TEXT
               ( ability_text_outline outline_x outline_y,
                 top_lines @ [ "-----------------------------" ] @ bottom_lines ));
        ]
    in

    let matches regex = Str.string_match (Str.regexp regex) full_interaction_name 0 in

    let interaction_prefix, interaction_name = Utils.split_at_first '_' full_interaction_name in

    let get_lore () : string =
      match List.assoc_opt interaction_name state.global.lore with
      | None -> failwithf "lore name '%s' not found in lore.json" interaction_name
      | Some lore -> lore
    in

    let get_locker_boys_ghosts () : ghost_id * ghost_id =
      let is_available (_id, ghost) = ghost.in_party in
      let other_ghosts = List.filter is_available game.ghosts in
      if List.length other_ghosts <> 2 then
        failwithf "got %d other_ghosts" (List.length other_ghosts)
      else
        (List.nth other_ghosts 0 |> fst, List.nth other_ghosts 1 |> fst)
    in

    let jump_ghost ?(end_pose = IDLE) ghost_id direction vx =
      [
        GHOST (ghost_id, JUMP (direction, vx));
        (* this wait time is arbitrary and specific to the uses in the chang cutscene *)
        STEP (WAIT 1.1);
        GHOST (ghost_id, SET_POSE end_pose);
      ]
    in

    (* TODO-2 add dialogue for Shirley Island npcs *)
    match interaction_prefix with
    | "warp" ->
      [ STEP (WARP interaction_name) ]
    | "door-warp" ->
      [ STEP (DOOR_WARP interaction_name) ]
    | "purple-pen" ->
      remove_nail := false;
      [
        STEP
          (PURPLE_PEN_TEXT [ "Found a purple pen with a note:"; fmt "{{purple}} %s" (get_lore ()) ]);
      ]
    | "health" ->
      [
        CURRENT_GHOST (SET_POSE READING);
        CURRENT_GHOST (INCREASE_HEALTH_TEXT (increase_health, get_lore ()));
      ]
    | "dreamer" ->
      fade_screen_with_dramatic_pause
        [ CURRENT_GHOST (ADD_ITEM (DREAMER (interaction_name, get_lore ()))) ]
    | "weapon" ->
      (* TODO center this text *)
      [
        CURRENT_GHOST (SET_POSE (PERFORMING FOCUS));
        CURRENT_GHOST (ADD_ITEM (WEAPON interaction_name));
      ]
    | _ -> (
      match full_interaction_name with
      | "info_opening-poem" ->
        [
          (* TODO add STEP SHOW_RECT and STEP HIDE_RECT to uncover the lines of the poem one-at-a-time
             - probably add something like `black_rects : rect list` to Interaction.t
             - this doesn't work because render doesn't have access to game.interaction.black_rects
          *)
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
      | "ability_scootenanny" ->
        get_ability_steps "mothwing_cloak" 0. 2.
          [ "Taken the"; "Scootenanny Chair." ]
          [
            "Press [ZR]";
            "to scootenanny forwards.";
            "Use the chair to scootenanny quickly along the ground or through the air.";
          ]
        (* TODO this only hides the chair when it is picked up, but it shows up again when the room is re-entered
           - probably can fix this when adding indicators for interactions
        *)
        @ [ STEP (HIDE_LAYER "bg-iso2") ]
      | "ability_double-bouncies" ->
        get_ability_steps "monarch_wings" 0. 4.
          [ "Consumed the"; "Double Bouncies." ]
          [
            "Press (B) while in the air";
            "to double bounce.";
            "At the apex of each bounce, there is a moment outside of time, outside of words, \
             outside of everything - a perfect moment. A silent moment. I call it the {{blue}} \
             World's Whisper.";
          ]
      | "ability_claw" ->
        get_ability_steps "mantis_claw" 0. 4. [ "Taken the"; "Mantis Claw." ] [ "TODO claw" ]
      | "ability_starburns-dash" ->
        get_ability_steps "crystal_heart" 0. 5.
          [ "Consumed the"; "Starburns Crystal." ]
          [
            "Hold [ZL] while on the ground or clinging to a wall";
            "to concentrate the force.";
            "Yeah, he uses some kind of crystal instead of deodorant.";
          ]
      | "info_focus" ->
        [
          CURRENT_GHOST (SET_POSE READING);
          STEP (WAIT 0.4);
          STEP FADE_SCREEN_OUT;
          STEP
            (FOCUS_ABILITY_TEXT
               ( [
                   "Human Beings, these words are for you alone.";
                   "";
                   "";
                   "Your great strength marks you already accepted. Focus your life vapor and you \
                    shall achieve feats of which others can only dream.";
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
      | "info_dean-and-creator" ->
        [
          CURRENT_GHOST (SET_POSE READING);
          STEP (WAIT 0.4);
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
                 "Bear witness to the last and only civilisation, the eternal Kingdom.";
                 "";
                 "Hallowdale";
               ]);
        ]
      | "cutscene_arrive-at-shirley-island" ->
        [
          STEP (DIALOGUE ("Neil", "Welcome to {{purple}} Shirley Island."));
          STEP (SET_FIXED_CAMERA (19, 29));
          STEP (WAIT 3.);
          STEP SET_GHOST_CAMERA;
          STEP
            (DIALOGUE
               ( "Neil",
                 "No furniture beyond this point. Leave your weapons at the door, and any spare \
                  doors at the entrance" ));
        ]
      | "cutscene_fight-duncan" ->
        [
          ENEMY (DUNCAN, ENTITY UNHIDE);
          ENEMY (DUNCAN, ENTITY FREEZE);
          ENEMY (DUNCAN, SET_POSE "scavenging");
          CURRENT_GHOST (SET_POSE IDLE);
          STEP (DIALOGUE ("Britta", "Professor Duncan?"));
          STEP (SET_FIXED_CAMERA (19, 29));
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
          ENEMY (DUNCAN, SET_ACTION "interaction-jumping");
          ENEMY (DUNCAN, ENTITY UNFREEZE);
          STEP (WAIT 0.7);
        ]
      | "boss-killed_DUNCAN" ->
        [
          CURRENT_GHOST (WALK_TO 21);
          CURRENT_GHOST (ENTITY (SET_FACING LEFT));
          ENEMY (DUNCAN, WALK_TO 12);
          ENEMY (DUNCAN, SET_POSE "idle");
          ENEMY (DUNCAN, ENTITY (SET_FACING RIGHT));
          STEP (WAIT 0.3);
          STEP (SPAWN_VENGEFUL_SPIRIT (RIGHT, 13, 31));
          STEP (WAIT 0.6);
          ENEMY (DUNCAN, SET_VX 200.);
          ENEMY (DUNCAN, SET_ACTION "interaction-jumping");
          STEP (WAIT 1.);
          ENEMY (DUNCAN, SET_POSE "scavenging");
          ENEMY (DUNCAN, ENTITY FREEZE);
          STEP (HIDE_LAYER "bg-iso2");
          STEP (UNHIDE_LAYER "bg-iso3");
          GHOST (ANNIE, ENTITY (UNHIDE_AT (6, 32, 0., 0.)));
          GHOST (JEFF, ENTITY (UNHIDE_AT (7, 31, 12., 0.)));
          GHOST (ANNIE, ENTITY FREEZE);
          GHOST (JEFF, ENTITY FREEZE);
          STEP (WAIT 1.5);
          STEP (SET_FIXED_CAMERA (14, 50));
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
            ENEMY (DUNCAN, WALK_TO 1);
            STEP (WAIT 0.2);
            STEP (DIALOGUE ("Annie", "Britta, there you are."));
            GHOST (JEFF, SET_POSE CRAWLING);
            STEP
              (DIALOGUE
                 ( "Jeff",
                   "Sweet, sweet portable chairs. {{gold}} Plastic gold, {{white}} four-legged \
                    diamonds!" ));
            GHOST (JEFF, SET_POSE IDLE);
            STEP (DIALOGUE ("Jeff", "You claimin' this?"));
            GHOST (JEFF, SET_POSE (PERFORMING (ATTACK RIGHT)));
            GHOST (ANNIE, SET_POSE (PERFORMING (ATTACK RIGHT)));
            STEP (DIALOGUE ("Jeff", "Lava joust?"));
            STEP (DIALOGUE ("Britta", "Did you all hit your heads on each other's heads?"));
            GHOST (JEFF, SET_POSE IDLE);
            GHOST (ANNIE, SET_POSE IDLE);
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
            (* TODO select current ghost in a menu *)
            STEP (TEXT [ "Press DEBUG_1 to switch ghost." ]);
            GHOST (JEFF, ADD_TO_PARTY);
            GHOST (ANNIE, ADD_TO_PARTY);
            STEP (WAIT 0.4);
            STEP SET_GHOST_CAMERA;
            STEP (WAIT 0.4);
            ENEMY (DUNCAN, ENTITY HIDE);
            STEP HIDE_BOSS_DOORS;
            GHOST (JEFF, ENTITY HIDE);
            GHOST (ANNIE, ENTITY HIDE);
          ]
      | "cutscene_mama-mahogany" ->
        let other_ghost_1, other_ghost_2 = get_locker_boys_ghosts () in
        [
          GHOST (other_ghost_1, ENTITY (UNHIDE_AT (8, 7, 10., 5.)));
          GHOST (other_ghost_2, ENTITY (UNHIDE_AT (7, 7, 10., 5.)));
          GHOST (other_ghost_1, SET_POSE IDLE);
          GHOST (other_ghost_2, SET_POSE IDLE);
          GHOST (other_ghost_1, ENTITY (SET_FACING RIGHT));
          GHOST (other_ghost_2, ENTITY (SET_FACING RIGHT));
          ENEMY (LOCKER_BOY, ENTITY HIDE);
          CURRENT_GHOST (SET_POSE IDLE);
          CURRENT_GHOST (ENTITY (SET_FACING RIGHT));
          STEP (WAIT 0.7);
          STEP (DIALOGUE ("Annie", "Mama mahogany, feast your feet on that stack of sticks."));
          (* TODO camera moves too fast here *)
          STEP (SET_FIXED_CAMERA (36, 16));
          STEP (WAIT 3.0);
          STEP SET_GHOST_CAMERA;
          STEP (WAIT 0.7);
          STEP (DIALOGUE ("Jeff", "Eh, too easy - could be a trap..."));
        ]
      | "cutscene_lockers" ->
        [
          (* TODO make it possible to walk while this text is on the screen
             - probably keep it entirely separate from existing interaction_text, and make a new floating_text field
          *)
          CURRENT_GHOST (ENTITY FREEZE);
          STEP (TEXT [ "... lockers ..." ]);
        ]
      | "cutscene_lockers-lockers-lockers" ->
        [ CURRENT_GHOST (ENTITY FREEZE); STEP (TEXT [ "... lockers, lockers, lockers ..." ]) ]
      | "cutscene_fight-locker-boys" ->
        [
          ENEMY (LOCKER_BOY, ENTITY HIDE);
          CURRENT_GHOST (WALK_TO 35);
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
      | "boss-killed_LOCKER_BOY" ->
        let other_ghost_1, other_ghost_2 = get_locker_boys_ghosts () in

        [
          GHOST (other_ghost_1, ENTITY (UNHIDE_AT (50, 19, 0., 0.)));
          GHOST (other_ghost_2, ENTITY (UNHIDE_AT (50, 19, 0., 0.)));
          STEP HIDE_BOSS_DOORS;
          STEP (WAIT 0.5);
          GHOST (ANNIE, WALK_TO 49);
          GHOST (JEFF, WALK_TO 48);
          GHOST (BRITTA, WALK_TO 47);
          GHOST (ANNIE, ENTITY (SET_FACING RIGHT));
          GHOST (JEFF, ENTITY (SET_FACING RIGHT));
          GHOST (BRITTA, ENTITY (SET_FACING RIGHT));
          STEP (DIALOGUE ("Chang", "Winger, Perry, and Edison..."));
          NPC (CHANG, ENTITY UNHIDE);
          NPC (CHANG, ENTITY (SET_FACING LEFT));
          NPC (CHANG, WALK_TO 55);
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
          GHOST (TROY, ENTITY (UNHIDE_AT (51, 10, 0., 0.)));
          GHOST (ABED, ENTITY (UNHIDE_AT (50, 10, 0., 0.)));
          GHOST (TROY, SET_POSE (AIRBORNE (-1.)));
          GHOST (ABED, SET_POSE (AIRBORNE (-1.)));
          GHOST (TROY, ENTITY FREEZE);
          GHOST (ABED, ENTITY FREEZE);
          GHOST (ABED, ADD_TO_PARTY);
          GHOST (TROY, ADD_TO_PARTY);
          STEP (DIALOGUE ("Abed", "Stand down or meet your doom, Chang."));
          GHOST (TROY, ENTITY UNFREEZE);
          GHOST (ABED, ENTITY UNFREEZE);
          STEP (WAIT 1.);
          GHOST (TROY, SET_POSE IDLE);
          GHOST (ABED, SET_POSE IDLE);
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
          STEP (SET_FIXED_CAMERA (56, 16));
          NPC (CHANG, SET_POSE "attack-with-locker-boys");
          STEP (WAIT 0.7);
          STEP (DIALOGUE ("Chang", "Earn your M&M's!"));
          STEP (DIALOGUE ("Troy", "Troy and Abed Intimidation Stance!"));
          NPC (CHANG, SET_POSE "take-damage");
        ]
        @ jump_ghost ~end_pose:(PERFORMING (CAST DESOLATE_DIVE)) TROY RIGHT 300.
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
            GHOST (TROY, SET_POSE IDLE);
            STEP (DIALOGUE ("Hickey", "Hiya kids."));
            GHOST (TROY, ENTITY (SET_FACING LEFT));
            GHOST (ABED, ENTITY (SET_FACING LEFT));
            GHOST (ANNIE, ENTITY (SET_FACING LEFT));
            GHOST (BRITTA, ENTITY (SET_FACING LEFT));
            GHOST (JEFF, ENTITY (SET_FACING LEFT));
            STEP (WAIT 0.9);
            (* TODO maybe add SHAKE_SCREEN step *)
            NPC (HICKEY, ENTITY UNHIDE);
            NPC (HICKEY, ENTITY UNFREEZE);
            STEP (SET_FIXED_CAMERA (16, 8));
            STEP (WAIT 1.1);
            STEP (DIALOGUE ("Hickey", "I'm criminology professor Buzz Hickey."));
            STEP
              (DIALOGUE ("Hickey", "And this... this is just a little something I threw together."));
            NPC (HICKEY, SET_POSE "walking");
            GHOST (BRITTA, ENTITY (SET_FACING RIGHT));
            STEP (WAIT 1.1);
            STEP (SET_FIXED_CAMERA (56, 16));
            STEP (WAIT 1.1);
            STEP
              (DIALOGUE
                 ( "Abed",
                   "Jeff, Annie, get to {{purple}} Shirley Island! {{white}} We'll meet you there!"
                 ));
            GHOST (ANNIE, WALK_TO 53);
            GHOST (JEFF, WALK_TO 53);
            STEP (WAIT 0.7);
            GHOST (ANNIE, ENTITY HIDE);
            GHOST (JEFF, ENTITY HIDE);
            GHOST (ANNIE, REMOVE_FROM_PARTY);
            GHOST (JEFF, REMOVE_FROM_PARTY);
            GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Troy", "Abed, save yourself!"));
            GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE
                 ( "Britta",
                   "Abed, before troy {{red}} dies in lava, {{white}} you can save yourself \
                    {{blue}} emotionally {{white}} by honestly experiencing the pain of him \
                    leaving Hallowdale." ));
            STEP (WAIT 0.4);
            GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP (WAIT 0.9);
            GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP (WAIT 0.4);
            STEP
              (DIALOGUE ("Abed", "We can do this in three steps. Britta, jump to that trash can."));
          ]
        @ jump_ghost BRITTA RIGHT 140.
        @ [
            STEP (WAIT 0.18);
            GHOST (BRITTA, ENTITY FREEZE);
            GHOST (ABED, ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Abed", "Now Troy, start inchworming."));
            GHOST (TROY, WALK_TO 54);
            STEP (WAIT 0.7);
            GHOST (ABED, WALK_TO 53);
            STEP (DIALOGUE ("Britta", "What's the third step?"));
            GHOST (TROY, MAKE_CURRENT_GHOST);
            STEP SET_GHOST_CAMERA;
            GHOST (ABED, ENTITY (SET_FACING LEFT));
            STEP
              (DIALOGUE ("Abed", "The third step is {{blue}} survival. {{white}} Good luck, Britta."));
            STEP (SET_FIXED_CAMERA (56, 16));
            STEP (DIALOGUE ("Britta", "Seriously!?"));
            GHOST (BRITTA, ENTITY (SET_FACING LEFT));
            STEP (WAIT 0.5);
            GHOST (BRITTA, ENTITY (SET_FACING RIGHT));
            STEP (DIALOGUE ("Britta", "Troy!"));
            STEP SET_GHOST_CAMERA;
            GHOST (ABED, WALK_TO 49);
            GHOST (TROY, WALK_TO 50);
            GHOST (TROY, ENTITY (SET_FACING RIGHT));
            STEP
              (DIALOGUE
                 ( "Troy",
                   "Sorry Britta, Abed knows best. But I'll always remember you as kinda slowing \
                    us down and complaining a lot." ));
            GHOST (TROY, ENTITY (SET_FACING LEFT));
            GHOST (ABED, WALK_TO 39);
            GHOST (ABED, ENTITY HIDE);
            GHOST (BRITTA, ENTITY HIDE);
            GHOST (BRITTA, REMOVE_FROM_PARTY);
          ]
      | "ability_monkey-gas" ->
        get_ability_steps "howling_wraiths" 0. 6.
          [ "Consumed the"; "Monkey Knockout Gas." ]
          [
            "Tap (A) while holding UP";
            "to unleash the Knockout Gas.";
            "Some kind of gas that knocks out monkeys.";
          ]
      | "ability_shade-cloak" ->
        get_ability_steps "shade_cloak" 0. 7.
          [ "Consumed the"; "Shade Cloak." ]
          [
            "Press [ZR] to scootenanny forwards, cloaked in shadow.";
            "Use the cloak to scootenanny through enemies and their attacks without taking damage.";
          ]
      | "cutscene_kp-drop" ->
        (* TODO starting an interaction resets ghost.v to 0, which looks a little weird when
           jumping into the trigger *)
        [ STEP (HIDE_LAYER "floors3"); CURRENT_GHOST UNSET_FLOOR ]
      | other -> failwithf "unrecognized interaction name: %s" other)
  in
  (* SET_GHOST_CAMERA to reset the camera if it changed *)
  [ STEP (INITIALIZE_INTERACTIONS !remove_nail) ] @ interaction_steps @ [ STEP SET_GHOST_CAMERA ]
