// var errors = UPLOAD ONE OF THE *_errors.gpkg IN THE RESULTS FOLDER HERE
var location_id = 2766977;

var error = errors.filter(ee.Filter.eq("location_i", location_id));
var error = ee.Feature(error.first())
print("From: ", error.get('from'), "To: ", error.get("to"))

var pt = error.geometry();

var buffer = 30;
var roi = pt.buffer(buffer*1).bounds();
var geometry = pt.buffer(buffer*1).bounds();

// Display map layers 
Map.centerObject(geometry, 15);
Map.addLayer(geometry)

// Define training and monitoring period
var start_monitoring = '2016-01-01';
var end_monitoring = '2018-12-31';


// Select and filter collection for S2, filter clouds
var s2_collection = ee.ImageCollection('COPERNICUS/S2')
  .filterDate(ee.Date(start_monitoring), ee.Date(end_monitoring))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 10))
  .filterBounds(roi);

// Clip to ROI
var s2_collection = s2_collection.map(function(image){
  return image.clip(roi);
});
  
  
// Simple cloudMask
var func1 = function cloudMask(im) {
  var condition = im.select('B2').gte(2800)
                  .or(im.select('B10').gte(30))
                  .or(im.select('QA60').gte(1024))
  var mask1 = ee.Image(0).where(condition, 1).not()
  return im.updateMask(mask1);
}
 
// remove clouds for all images
var s2_collection = s2_collection.map(func1)

//Derive NDVI and MNDWI
var addvariables = function(image) {
  var SWIR = image.select('B11')
  var NIR = image.select('B8')
  var RED = image.select('B4')
  var GREEN = image.select('B3')
  var ndvi = NIR.subtract(RED).divide(NIR.add(RED)).rename('NDVI')
  var mndwi = GREEN.subtract(SWIR).divide(GREEN.add(SWIR)).rename('MNDWI')
 
  return image.addBands([ndvi, mndwi]);
};

var s2_collection = s2_collection.map(addvariables)

var s2_collection = s2_collection.select(['NDVI', 'MNDWI'])

// vegetation indices together

var chart = ui.Chart.image.series({
  imageCollection: s2_collection,
  region: roi,
  reducer: ee.Reducer.mean(),
  scale: 10
  })
  .setOptions({
    title: 'Sentinel MNDWI and NDVI over time',
    vAxis: {title: 'Index Value'},
    hAxis: {title: 'Date'},
    lineWidth: 1,
    series: {
    0: {color: 'blue'}, // NDVI series color
    1: {color: 'green'} // EVI series color
  }
  });
// Display the chart in the console.
print(chart);

// Load the Landsat 8 collection and filter it
var landsat = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
  .filterBounds(roi)
  .filterDate(ee.Date(start_monitoring), ee.Date(end_monitoring));

var maskClouds = function(image) {
  // Bits 3 and 5 are cloud shadow and cloud, respectively.
  var cloudBitMask = (1 << 3);
  var cloudShadowBitMask = (1 << 5);
  
  // Get the pixel QA band.
  var qa = image.select('QA_PIXEL');
  
  // Both flags should be set to zero, indicating clear conditions.
  var clear = qa.bitwiseAnd(cloudBitMask).eq(0)
    .and(qa.bitwiseAnd(cloudShadowBitMask).eq(0));
  
  // Return the masked image, scaled to reflectance, without the QA bands.
  return image.updateMask(clear).divide(10000)
    .select("SR_B[0-9]*")
    .copyProperties(image, ["system:time_start"]);
};

// Apply the cloud mask function to the collection.
var landsat = landsat.map(maskClouds);

// Compute NDVI (Normalized Difference Vegetation Index)
function addNDVI(image) {
  var ndvi = image.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDVI');
  return image.addBands(ndvi);
}

// Map the NDVI function over the collection
var landsatNDVI = landsat.map(addNDVI);

// Define a function to calculate EVI (Enhanced Vegetation Index)
function addEVI(image) {
  var evi = image.expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
      'NIR': image.select('SR_B5'),
      'RED': image.select('SR_B4'),
      'BLUE': image.select('SR_B2')
  }).rename('EVI');
  return image.addBands(evi);
}

// Map the EVI function over the collection
var landsatEVI = landsatNDVI.map(addEVI);

// Compute MNDWI (Modified Normalized Difference Water Index)
function addMNDWI(image) {
  var mndwi = image.normalizedDifference(['SR_B3', 'SR_B6']).rename('MNDWI');
  return image.addBands(mndwi);
}

var landsatMNDWI = landsatEVI.map(addMNDWI);

// Compute MNDBI (Modified Normalized Difference Bare-land Index)
function addMNDBI(image) {
  var mndbi = image.normalizedDifference(['SR_B7', 'SR_B2']).rename('MNDBI');
  return image.addBands(mndbi);
}

var landsatMNDBI = landsatMNDWI.map(addMNDBI);

// Compute NBR (Normalized Burn Area index)
function addNBR(image) {
  var nbr = image.normalizedDifference(['SR_B5', 'SR_B7']).rename('NBR');
  return image.addBands(nbr);
}

var landsatNBR = landsatMNDBI.map(addNBR);

var landsatVI = landsatNBR.select(['NDVI', 'MNDWI', 'MNDBI', 'NBR'])

// Create a chart to visualize NDVI, EVI, MNDWI, MNDBI, and NBR time series
var chart = ui.Chart.
image.series({
  imageCollection: landsatVI,
  region: roi,
  reducer: ee.Reducer.mean(),
  scale: 30
}).setOptions({
  title: 'Time Series of Landsat NDVI, EVI, MNDWI, MNDBI, and NBR',
  vAxis: {title: 'Index Value'},
  hAxis: {title: 'Date'},
  lineWidth: 2,
  series: {
    0: {color: 'blue'},
    1: {color: 'green'},
    2: {color: 'red'},
    3: {color: 'brown'},
    4: {color: 'orange'}
  }
});

// Display the chart
print(chart);
