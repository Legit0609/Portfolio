// ============================================
// CHIRPS Monatliche Niederschläge
// Rio Pardo, 2024
// ============================================

// Hilfsfunktion für zweistellige Monatszahl (GEE nutzt ES5)
function padMonth(m) {
  return m < 10 ? '0' + m : '' + m;
}

// 1. Einzugsgebiet laden
var aoi = ee.FeatureCollection('users/leonhaberberger/bacia_Pardo_and_Paraiso_clean_GEE');
Map.centerObject(aoi, 9);
Map.addLayer(aoi, {color: 'blue'}, 'Einzugsgebiet');

// 2. CHIRPS Tagesdaten 2024 laden
var chirps_daily = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY')
  .filterDate('2024-01-01', '2024-12-31')
  .filterBounds(aoi);

// 3. Monatliche Summen berechnen
var months = ee.List.sequence(1, 12);

var monthlyPrecip = months.map(function(m) {
  var monthly = chirps_daily
    .filter(ee.Filter.calendarRange(m, m, 'month'))
    .sum()
    .clip(aoi);
  return monthly.set('month', m);
});

// 4. Export jeden Monat als separates GeoTIFF
for (var i = 1; i <= 12; i++) {
  var img = ee.Image(monthlyPrecip.get(i-1));
  Export.image.toDrive({
    image: img,
    description: 'CHIRPS_2024_' + padMonth(i),
    folder: 'GEE_Exports/CHIRPS_2024',
    fileNamePrefix: 'precip_' + padMonth(i),
    region: aoi.geometry(),
    scale: 5566,
    crs: 'EPSG:32723',
    maxPixels: 1e9,
    fileFormat: 'GeoTIFF'
  });
}

print('✓ 12 Exports gestartet – Tasks Tab prüfen!');

// 5. Visualisierung Jahresniederschlag
var annualPrecip = chirps_daily.sum().clip(aoi);
Map.addLayer(annualPrecip, {
  min: 500, max: 1500,
  palette: ['white', 'lightblue', 'blue', 'darkblue']
}, 'Jahresniederschlag 2024 (mm)');