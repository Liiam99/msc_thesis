// Pick the targeted date range for the surface reflectance extraction.
var start_date = '2015-01-01';
var end_date = '2019-12-31';

// Creates one composite with all unique burned pixels for the given time range.
var burned_area = ee.ImageCollection("MODIS/061/MCD64A1");
var burned_area_composite = burned_area
  .filterDate(start_date, '2018-12-31')
  .select("BurnDate")
  .reduce(ee.Reducer.countDistinct())
  
// var brazil = https://code.earthengine.google.com/?asset=projects/thesis-liam-adam/assets/brazil

// Calculates for each point if they intersect with any of the burned pixels.
var results = burned_area_composite.reduceRegions({
  collection: brazil,
  reducer: ee.Reducer.anyNonZero(),
  scale: 30
});

// Retrieves the points that were not labelled burned.
var brazil_not_burned = results.filter(ee.Filter.eq('any', 0))

var exportParams = {
  collection: brazil_not_burned,
  description: "lol",
  assetId: "users/liam99adam/lol"
}

Export.table.toAsset(exportParams)

var brazil_filtered = brazil_not_burned.filter(
  ee.Filter.and(
    ee.Filter.neq("COUNT_2016", 1), 
    ee.Filter.neq("COUNT_2017", 1),
    ee.Filter.neq("COUNT_2018", 1),
    ee.Filter.neq("CLASS_2016", 27),
    ee.Filter.neq("CLASS_2017", 27),
    ee.Filter.neq("CLASS_2018", 27)
  )
);

// Maps the numbers of the Mapbiomas legend to the classes of the global reference data set.
// https://staging-brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/08/Legenda-Colecao-8-LEGEND-CODE.pdf
var class_map = ee.Dictionary({
  3: "tree",
  4: "grassland",
  5: "wetland_herbaceous",
  6: "wetland_herbaceous",
  9: "tree",
  11: "wetland_herbaceous",
  12: "grassland",
  13: "shrub",
  15: "grassland",
  19: "crops",
  20: "crops",
  23: "bare",
  24: "urban_built_up",
  25: "bare",
  29: "bare",
  30: "bare",
  31: "water",
  32: "wetland_herbaceous",
  33: "water",
  36: "crops",
  50: "wetland_herbaceous"
})

// Custom function to remap values based on class map
var remapProperties = function(feature) {
  // Iterate over each year property
  var years = ['2016', '2017', '2018'];
  years.forEach(function(year) {
    // Get the property name for the current year
    var propertyName = 'CLASS_' + year;
    // Get the value of the property
    var propertyValue = feature.get(propertyName);
    // Check if the property value exists in the class map
    if (class_map.contains(propertyValue)) {
      // Get the new class from the class map
      var newClass = class_map.get(propertyValue);
      // Update the property value with the new class
      feature = feature.set(propertyName, newClass);
    }
  });
  
  return feature;
};

// Apply the custom function to each feature in the FeatureCollection
var brazil_reclassified = brazil_filtered.map(remapProperties);

var addChangeFlag = function(feature) {
  return feature.set('is_change', 1);
};

// Filters where classes changed in the targeted years and adds a "Change" value.
var brazil_changes = brazil_reclassified.filter(
  ee.Filter.or(
    ee.Filter.notEquals({leftField: "CLASS_2016", rightField:"CLASS_2017"}),
    ee.Filter.notEquals({leftField: "CLASS_2017", rightField:"CLASS_2018"})
  )
).map(addChangeFlag);

// Filters where classes did not change in the targeted years.
var brazil_no_changes = brazil_reclassified.filter(
  ee.Filter.and(
    ee.Filter.equals({leftField: "CLASS_2016", rightField:"CLASS_2017"}),
    ee.Filter.equals({leftField: "CLASS_2017", rightField:"CLASS_2018"}),
    ee.Filter.eq("COUNT_2016", 3), 
    ee.Filter.eq("COUNT_2017", 3),
    ee.Filter.eq("COUNT_2018", 3)
  )
);

var addNoChangeFlag = function(feature) {
  return feature.set('is_change', 0);
};  

// Adds a column with "No Change" to the no-changes.
brazil_no_changes = brazil_no_changes
  .map(addNoChangeFlag);
  
var brazil_reference_data = brazil_changes.merge(brazil_no_changes);

// Shrinks the number of features
var brazil_reference_data = brazil_reference_data.randomColumn('random', 123);
var brazil_reference_data = brazil_reference_data.filter(ee.Filter.lt('random', 0.3));

// Reference data with change flags attached.
Export.table.toDrive(brazil_reference_data, "brazil_reference_data", "thesis");

// Drops all columns except the identifier.
var brazil_point_data = brazil_reference_data.select(['TARGETID']);



// SURFACE REFLECTANCE EXTRACTION
/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
    
    Copyright 2021 Dainius Masiliunas
*/

// Extract time series of Landsat 8 over Brazil validation points.
// Aggregates to the 100 m polygons and merges observations on the same day.

var Prefix = "Brazil_LC";

var UTMZone = "";

var MySubset = brazil_point_data

var ExportBands = ["SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7"];

// Define dates
var dataset = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
    .filterDate(start_date, end_date).filterBounds(MySubset)
    .sort("system:time_start"); // Sort by Date

// Mask clouds
var qa = dataset.select("QA_PIXEL");

// Merge all images taken on the same date
function temporalCollection(collection, start, count, interval, units) {
  // Create a sequence of numbers, one for each time interval.
  var sequence = ee.List.sequence(0, ee.Number(count).subtract(1));

  var originalStartDate = ee.Date(start);

  return ee.ImageCollection(sequence.map(function(i) {
    // Get the start date of the current sequence.
    var startDate = originalStartDate.advance(ee.Number(interval).multiply(i), units);

    // Get the end date of the current sequence.
    var endDate = originalStartDate.advance(
      ee.Number(interval).multiply(ee.Number(i).add(1)), units);

    return collection.filterDate(startDate, endDate).mean()
        .set('system:time_start', startDate.millis(),
             'system:time_end', endDate.millis(),
             "system:index", startDate.format("YYYY-MM-dd"),
             "system:id", startDate.format("YYYY-MM-dd"));
  }));
}

function DropNullImages(collection) {
  collection = collection.map(function (image){
    return ee.Image(image.set('bandCount',image.bandNames().size()));
  });
  
  return collection.filterMetadata("bandCount","not_equals",0);
}

// Export bands one by one
function ExtractFromBand(bandname) {
  var cloudfree = dataset.map(function(image) {
    var initial = image.select(bandname).toUint16().mask(image.select('QA_PIXEL') // NB: only for bands that are Uint16!
      .remap([5440,21824,21888,21952,22080,22144,30048,54596,54852],[1,1,1,1,1,1,1,1,1],0)); // 5440,21824=clear -> 1 = not masked
    return initial.copyProperties(initial,ee.List(['system:time_start']));
  });
  
  // Make a 16-day mean composite. Yes, Landsat is already 16-day,
  // but sometimes the days cross over, which results in a table of mostly NAs.
  // Force the result to have 192 columns.
  
  cloudfree = temporalCollection(cloudfree, start_date, 192, 16, 'day');//mosaicByDate(cloudfree);
  cloudfree = DropNullImages(cloudfree);
  
  // Scale = 30 m because points were validated with 30 m x 30 m imagery.
  var point = cloudfree.toBands()
    .reduceRegions({collection: MySubset, reducer: ee.Reducer.mean(), scale: 30, tileScale: 2});
  //print(point);
  Export.table.toDrive(point, Prefix + "_" + bandname, "GEE_extracted_points");
}
ExportBands.map(ExtractFromBand);
