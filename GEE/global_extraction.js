var burned_area = ee.ImageCollection("MODIS/061/MCD64A1");

// Pick the targeted date range.
var start_date = '2015-01-01';
var end_date = '2018-12-31';

// Creates one composite with all unique burned pixels for the given time range.
var burned_area_composite = burned_area
  .filterDate(start_date, end_date)
  .select("BurnDate")
  .reduce(ee.Reducer.countDistinct())

// var IIASAChange = CONTACT AUTHORS FOR DATA
// var WURChange = CONTACT AUTHORS FOR DATA

// Calculates for each point if they intersect with any of the burned pixels.
var results = burned_area_composite.reduceRegions({
  collection: IIASAChange, //WURChange
  reducer: ee.Reducer.anyNonZero(),
  scale: 100
});

// Retrieves the points that overlap with one of the burned pixels.
var burned_points = results.filter(ee.Filter.gt('any', 0));

// Exports the ids of burned locations.
var export_table = burned_points.select(['sample_id']); //WUR: location_id
var export_options = {
  collection: export_table,
  description: 'burned_location_ids',  // Set your desired file name
  fileFormat: 'CSV'
};
Export.table.toDrive(export_options);
