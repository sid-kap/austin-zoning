// import * as L from "leaflet";
import L = require("leaflet")
import $ = require("jquery")

var map = L.map("map");
map.setView(L.latLng(30.284301, -97.744734), 19);
// var layer = L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', { maxZoom: 18, attribution: "attribution test" })
var layer = L.tileLayer('https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.{ext}', {
	attribution: 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
	subdomains: 'abcd',
	minZoom: 0,
	maxZoom: 20,
	ext: 'png'
});
layer.addTo(map);

// // add marker
// var marker = L.marker(L.latLng(51.5, -0.09));
// marker.addTo(map).bindPopup("<b>Hello world!</b><br />I am a popup.").openPopup();

// add circle
// var circle = L.circle(L.latLng(51.508, -0.11), 500, {
//   		color: 'red',
// 			fillColor: '#f03',
// 			fillOpacity: 0.5
// 		}).addTo(map).bindPopup("I am a circle.");

// // add polygon
// var latlongs : L.LatLng[];
// latlongs= [
//     L.latLng(51.509, -0.08),
//     L.latLng(51.503, -0.06),
//     L.latLng(51.51, -0.047)
// ];
// var polygon = L.polygon(latlongs).addTo(map).bindPopup("I am a polygon.");

// // popup on mapclick
// var popup = L.popup();

// function onMapClick(e) {
// 	popup
// 		.setLatLng(e.latlng)
// 		.setContent("You clicked the map at " + e.latlng.toString())
// 		.openOn(map);
// }
var layerGroup = L.layerGroup([]);
layerGroup.addTo(map);

var apiUrl = window.location.protocol + "//" + window.location.hostname + ":" + window.location.port + "/area";

function reloadZoningRegions() {
    var bounds = map.getBounds();
    var data = JSON.stringify(
        { "_xmin": bounds.getEast()
        , "_xmax": bounds.getWest()
        , "_ymin": bounds.getNorth()
        , "_ymax": bounds.getSouth()
        });
    $.post(apiUrl,
        data,
           (result) => {
               console.log(result);
               layerGroup.clearLayers();
               for (let entry of result.results) {
                   // if (entry.geometryType == "esriGeometryPoint") {
                   //     var marker = L.marker(L.latLng(entry.geometry.y, entry.geometry.x))
                   //         .bindPopup(entry.value)
                   //     layerGroup.addLayer(marker);
                   // }
                   if (entry.geometryType == "esriGeometryPolygon") {
                       var poly = L.polygon(entry.geometry.rings[0].map(arr => [arr[1],arr[0]]))
                       layerGroup.addLayer(poly);
                       poly.bindTooltip(entry.attributes.ZONING_ZTYPE, {permanent: true, direction: "center"})
                           .openTooltip()
                   }
               }
          });
}

map.on('zoomend', _ => reloadZoningRegions);
map.on('moveend', _ => reloadZoningRegions);

// run once on page load
reloadZoningRegions();
