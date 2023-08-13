
## Hallowdale

An homage to Hollow Knight.

![main menu screenshot](./assets/main-menu-screenshot.png)
![world map screenshot](./assets/world-map.png)

### Project goals

- doable
- passable

### Setup

1. install `opam`
2. use `opam` to install `ocaml` version 4.13
3. use `opam` to install dependencies: `opam install dune yojson atdgen raylib ppx_expect ocamlformat`

### Scripts

#### build and run

```
dune exec hallowdale
```

#### run tests

```
dune runtest
```

#### promote test output

```
dune promote
```

#### regenerate and replace .atd

```
rm src/json_* && atdgen -t src/json.atd && atdgen -j src/json.atd
```

#### generate world map

- `world-map` layer must be visible in every room in the map for this to work
- the `tmxrasterizer` binary comes with Tiled, so you'll need to use the path where it is installed

```
path/to/tmxrasterizer --show-layer world-map assets/tiled/rooms/Deepnest_East.world world-map.png
path/to/tmxrasterizer --show-layer world-map --show-layer world-map-labels assets/tiled/rooms/Deepnest_East.world world-map-with-notes.png
```

#### create release

TBD, but seems like it is something along the lines of `dune-release distrib` then `gh release create`

### License

[MIT for the code, CC BY_NC 4.0 for the assets](LICENSE.md)

- Most of the assets were taken from the [original Journey to the Center of Hawkthorne game](https://github.com/hawkthorne/hawkthorne-journey/tree/master/src/images), with some modifications made.

### Acknowledgements

- creators of original Hawkthorne game
- creators of Hollow Knight
- creators of Community
