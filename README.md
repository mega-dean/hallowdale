
## Hallowdale

An homage to Hollow Knight.

[itch.io page](https://mega-dean.itch.io/hallowdale)

![main menu screenshot](./assets/main-menu-screenshot.png)
![world map screenshot](./assets/npcs/shared/world-map.png)

### Instructions

- Download the archive from the latest Github release, or from https://mega-dean.itch.io/hallowdale
- Unzip the archive
- Run the executable in the `dist/` folder for your operating system

### Verification

The executable is unsigned, so for Windows and MacOS you will need to manually allow the program to
run. A sha256 checksum of the `.tar.gz` file is generated during the build process and included in
`./dist/` so you can verify the executable is built from the code in this repo.

### Game modes

#### Classic

This is like normal Hollow Knight gameplay, and has cutscenes that follow the episode "Geothermal
Escapism". This game mode is disabled until the map can be updated to handle both game modes.

#### Steel Sole

This is a "New Game +" mode where you start with all abilities, but standing on any floor will
cause a hazard respawn (although you can land in water/acid safely). Cutscenes are disabled and
abilities/lore are removed, so the only objective is to collect all 120 purple pens.

C-dashing makes this game mode much easier, so for an additional challenge you can try to traverse
the map without them. All of the purple pens can be collected without using them, except for some in
the Computer Wing area, so c-dashes that are started in that area are allowed and not counted towards
the total.

### Controls

- Most of the controls match the default Hollow Knight controls, but there are a few differences:
-- no Inventory button
-- additional Interact button, instead of pressing up/down
-- cannot tap Focus to cast
- Select menu options with the Jump key/button

#### Keyboard

- Jump - Z
- Attack - X
- Dash - C
- Focus - A
- C-dash - S
- Dream nail - D
- Quick cast - F
- Interact - left shift
- Open map - tab
- Pause - Escape
- Movement - arrow keys

#### Gamepad

The game uses Playstation controller names for buttons, but any gamepad supported by Raylib should work.

- Jump - x
- Attack - square
- Dash - R2
- Focus - R1
- C-dash - L2
- Dream nail - triangle
- Quick cast - circle
- Interact - L1
- Open map - select
- Pause - start
- Movement - d-pad or left stick

### Development

#### Setup

1. install `opam`
2. use `opam` to install `ocaml` version 5.1: `opam switch create 5.1.0`
3. use `opam` to install dependencies: `opam install --deps-only .`
4. use `opam` to install dev dependencies: `opam install ocamlformat merlin ocp-indent ocp-index down omod landmarks landmarks-ppx`

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
- @raysan5 for making [Raylib](https://github.com/raysan5/raylib)
- @tjammer for making [raylib-ocaml](https://github.com/tjammer/raylib-ocaml)
- @bjorn for making [Tiled](https://github.com/mapeditor/tiled)
