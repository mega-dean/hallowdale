
// this sets some properties based on layer name

/// <reference types="@mapeditor/tiled-api" />

/*
 * find-layer-by-id.js
 *
 * This extension adds a 'Select Layer by ID' (Ctrl+Shift+L) action to the
 * Layer menu, which can be used to quickly jump to and select a layer when
 * you know its ID.
 */

/* global tiled */

function findLayerById(thing, id) {
  for (let i = thing.layerCount - 1; i >= 0; i--) {
    const layer = thing.layerAt(i);
    if (layer.id == id) {
      return layer;
    }

    if (layer.isGroupLayer) {
      const l = findLayerById(layer, id);
      if (l) {
        return l;
      }
    }
  }

  return null;
}


let setDefaultLayerProperties = tiled.registerAction("SetDefaultLayerProperties", function(/* action */) {
  const map = tiled.activeAsset;
  if (!map.isTileMap) {
    tiled.alert("Not a tile map!");
    return;
  }
  console.log('running');

  // let id = tiled.prompt("Please enter a layer ID:");
  // if (id == "") {
  //   return;
  // }

  // id = Number(id);

  let parallaxProps = {
    "ref:camera": 0.0,
    "auto:walls": 0.9,
    "auto:iso": 0.9,
    "bg-walls": 0.9,
    "bg-iso": 0.9,
    // TODO fg layers
  };

  for (let i = map.layerCount - 1; i >= 0; i--) {
    const layer = map.layerAt(i);
    // propsByLayer[layer.name] = props[layer.name] || {};
    if (parallaxProps[layer.name] !== undefined) {
      // tiled.alert("layer: " + layer.name);
      layer.parallaxFactor.x = parallaxProps[layer.name];
      layer.parallaxFactor.y = parallaxProps[layer.name];
    }
  }

});
setDefaultLayerProperties.text = "Set Default Layer Properties";

tiled.extendMenu("Layer", [
  { action: "SetDefaultLayerProperties", before: "SelectPreviousLayer" },
]);
