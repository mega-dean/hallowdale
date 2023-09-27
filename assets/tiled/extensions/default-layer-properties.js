
// this sets some properties based on layer name


let setDefaultLayerProperties = tiled.registerAction("SetDefaultLayerProperties", function(/* action */) {
  const map = tiled.activeAsset;
  if (!map.isTileMap) {
    tiled.alert("Not a tile map!");
    return;
  }

  let parallaxProps = {
    "ref:camera": 0.0,
    "auto:walls": 0.9,
    "auto:iso": 0.9,
    "bg-walls": 0.9,
    "bg": 0.9,
    "bg-iso": 0.9,
    "bg-iso-walls": 0.9,
    // TODO fg layers
  };

  let layersToLock = [
    "ref:camera",
    // TODO fg layers
  ];

  for (let i = map.layerCount - 1; i >= 0; i--) {
    const layer = map.layerAt(i);

    let layerName = layer.name.replace(/\d$/, '');

    if (parallaxProps[layerName] !== undefined) {
      // tiled.alert("layer: " + layer.name);
      layer.parallaxFactor.x = parallaxProps[layerName];
      layer.parallaxFactor.y = parallaxProps[layerName];
    }

    if (layersToLock.includes(layerName)) {
      layer.locked = true;
    }
  }

});
setDefaultLayerProperties.text = "Set Default Layer Properties";


let OnlyShowWorldMap = tiled.registerAction("OnlyShowWorldMap", function(/* action */) {
  const map = tiled.activeAsset;

  for (let i = map.layerCount - 1; i >= 0; i--) {
    const layer = map.layerAt(i);

    if (layer.name === "world-map") {
      layer.visible = true;
      map.currentLayer = layer;
    } else {
      layer.visible = false;
    }
  }
});
OnlyShowWorldMap.text = "Only Show World Map";
OnlyShowWorldMap.shortcut = "Ctrl+Alt+W";

tiled.extendMenu("Layer", [
  { action: "SetDefaultLayerProperties", before: "SelectPreviousLayer" },
  { action: "OnlyShowWorldMap", before: "SelectPreviousLayer" },
]);

let OnlyShowHK = tiled.registerAction("OnlyShowHK", function(/* action */) {
  for (let i = 0; i <= tiled.openAssets.length; i++) {
    const map = tiled.openAssets[i];

    for (let j = map.layerCount - 1; j >= 0; j--) {
      const layer = map.layerAt(j);

      if (layer.name === "ref:hk") {
        layer.visible = true;
        map.currentLayer = layer;
      } else {
        layer.visible = false;
      }
    }
  }
});
OnlyShowHK.text = "Only Show HK Reference";
OnlyShowHK.shortcut = "Meta+S";

tiled.extendMenu("Layer", [
  { action: "SetDefaultLayerProperties", before: "SelectPreviousLayer" },
  { action: "OnlyShowHK", before: "SelectPreviousLayer" },
]);


// let RepeatedAutomap = tiled.registerAction("RepeatedAutomap", function(/* action */) {
//   let count = tiled.prompt("Please enter the number of times to automap:");
//   if (count == "") {
//     return;
//   }
//
//   const map = tiled.activeAsset;
//   count = Number(count);
//   for (let i = 0; i < count; i++) {
//     map.autoMap();
//   }
// });
// RepeatedAutomap.text = "Repeated Automap";
// // RepeatedAutomap.shortcut = "Ctrl+Alt+A";
// //
// tiled.extendMenu("Layer", [
//   { action: "SetDefaultLayerProperties", before: "SelectPreviousLayer" },
//   { action: "RepeatedAutomap", before: "SelectPreviousLayer" },
// ]);
//
//

let RepeatedAutomap = tiled.registerAction("RepeatedAutomap", function(/* action */) {
  const map = tiled.activeAsset;
  for (let i = 0; i < 10; i++) {
    map.autoMap();
  }
});
RepeatedAutomap.text = "Repeated Automap";
RepeatedAutomap.shortcut = "Ctrl+Alt+A";

tiled.extendMenu("Layer", [
  { action: "SetDefaultLayerProperties", before: "SelectPreviousLayer" },
  { action: "RepeatedAutomap", before: "SelectPreviousLayer" },
]);







/*
 * this is adapted from the find-layer-by-id.js example in the Tiled repo
 */

function findLayerByName(thing, name) {
  for (let i = thing.layerCount - 1; i >= 0; i--) {
    const layer = thing.layerAt(i);
    if (layer.name === name) {
      return layer;
    }

    if (layer.isGroupLayer) {
      const l = findLayerByName(layer, name);
      if (l) {
        return l;
      }
    }
  }

  return null;
}

let selectLayerByName = tiled.registerAction("SelectLayerByName", function(/* action */) {
  const map = tiled.activeAsset;
  if (!map.isTileMap) {
    tiled.alert("Not a tile map!");
    return;
  }

  let name = tiled.prompt("Please enter a layer name:");
  if (name == "") {
    return;
  }

  const layer = findLayerByName(map, name);
  if (!layer) {
    tiled.alert("Failed to find a layer with Name " + name);
    return;
  }

  map.currentLayer = layer;
});
selectLayerByName.text = "Select Layer by Name";
selectLayerByName.shortcut = "Ctrl+L";

tiled.extendMenu("Layer", [
  { action: "SelectLayerByName", before: "SelectPreviousLayer" },
]);
