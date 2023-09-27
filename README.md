
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

#### create release

Trigger the github actions manually.

### License

[MIT for the code, CC BY_NC 4.0 for the assets](LICENSE.md)

- Most of the assets were taken from the [original Journey to the Center of Hawkthorne game](https://github.com/hawkthorne/hawkthorne-journey/tree/master/src/images), with some modifications made.

### Acknowledgements

- creators of original Hawkthorne game
- creators of Hollow Knight
- creators of Community
