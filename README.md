
## Hallowdale

An homage to Hollow Knight.

[itch.io page](https://mega-dean.itch.io/hallowdale)

![main menu screenshot](./assets/main-menu-screenshot.png)
![world map screenshot](./assets/npcs/shared/world-map.png)


### Instructions

The game can either be cloned from github, or downloaded/unzipped from itch.io. The executables are
located in `dist/`.

#### Linux

This is built on Ubuntu and has been tested on Pop!_OS, so it probably works on most Debian-based
distributions. It may work on other distributions that have OpenGL installed since it depends on
Raylib, but I haven't tested this.

#### MacOS

I've tested on an M1 Macbook Air and it works, but the controls feel pretty sluggish. Also, the
executable isn't notarized by Apple so it can't be run directly. After attempting to run it and
dismissing the "this may be malware" warning, you can go to the Security page in System Preferences
to whitelist it specifically.

#### Windows

Untested - please create an Issue if you run into any problems.

### Game modes

#### Classic

This is like normal Hollow Knight gameplay, and has cutscenes that follow the episode "Geothermal
Escapism". It is disabled until the map can be updated to handle both game modes (currently it's
just a bunch of purple pen jars everywhere).

#### Steel Sole

This is a "New Game +" mode where you start with all abilities, but standing on any floor will
cause a hazard respawn (although you can land in water/acid safely). Cutscenes are disabled and
abilities/lore are removed, so the only objective is to collect all 120 purple pens.

C-dashing makes this game mode much easier, so for an additional challenge you can try to traverse
the map without them. All of the purple pens can be collected without using them, except for some in
the Computer Wing area, so c-dashes that are started in that area aren't counted.

#### Demo

This is a "New Game +" mode like Steel Sole but without the damage respawns, so you can play this if
you just want to run around the map and find the purple pens.

### Configuration

There is currently no menu for rebinding keys, but they can be overridden by placing a file at
`config/keybinds.json` and restarting the game. See `config/keybinds.json.example` for an example.

Only keyboard input is supported, but gamepad support will be added once I get a gamepad to test with.

None of the other settings (like fps) are configurable yet.

### Development

#### Setup

1. install `opam`
2. use `opam` to install `ocaml` version 4.13
3. use `opam` to install dependencies: `opam install dune yojson atdgen raylib ppx_expect ocamlformat`

#### Scripts

##### build and run

```
dune exec hallowdale
```

##### regenerate and replace .atd

```
rm src/json_* && atdgen -t src/json.atd && atdgen -j src/json.atd
```

### License

[MIT for the code, CC BY_NC 4.0 for the assets](LICENSE.md)

- Most of the assets were taken from the [original Journey to the Center of Hawkthorne game](https://github.com/hawkthorne/hawkthorne-journey/tree/master/src/images), with some modifications made.

### Acknowledgements

- creators of original Hawkthorne game
- creators of Hollow Knight
- creators of Community
