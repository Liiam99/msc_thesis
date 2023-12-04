# Credit:
# https://github.com/GreatEmerald/supervised-bfast/blob/main/src/utils/utils.r

library(dplyr)
library(parsedate)
library(zoo)

# Get all data.frame columns that have a date in it (according to the pattern)
datecols = function(object, pattern="X????.??.??*")
{
  grep(glob2rx(pattern), names(object))
}

# Extract the time series in matrix format from an SF object by date
SFToMatrix = function(object, pattern="X????.??.??*", rowname.source="location_id")
{
  cols = datecols(object, pattern)
  result = as.matrix(st_drop_geometry(object)[cols])
  rownames(result) = object[[rowname.source]]
  return(result)
}

# Parse a matrix into a zoo object
MatrixToZoo = function(object)
{
  datenames = colnames(object)
  dates = as.Date(datenames, "X%Y.%m.%d")
  return(as.zooreg(zoo(t(object), dates), 16))
}

# Utility for both
SFToZoo = function(object, ...)
{
  MatrixToZoo(SFToMatrix(object, ...))
}

# Reintegrate a zoo object into an SF object (used as a template)
ZooToSF = function(zobj, sfobj)
{
  st_crs(sfobj) = 4326
  
  stopifnot(nrow(sfobj) == ncol(zobj))
  stopifnot(length(datecols(sfobj)) == nrow(zobj))
  
  sfobj[datecols(sfobj)] = t(zobj)
  return(sfobj)
}

# Rescale predictions so that they add up to 100%
ScalePredictions = function(Predictions, LeaveZeroes = TRUE)
{
  Predictions = as.matrix(Predictions)
  Predictions[Predictions < 0] = 0
  Predictions = Predictions / rowSums(Predictions) * 100
  # There is a possibility that all classes have been predicted as 0, so we can't normalise.
  # In that case we just keep them as 0%. It won't add up to 100%. Alternatively we can set it to 1/nclass.
  Predictions[is.nan(Predictions)] = if (LeaveZeroes) 0 else 100/ncol(Predictions)
  return(as.data.frame(Predictions))
}

filter_by_dates = function(SRs, earliest, latest) {
  earliest_date <- as.Date(earliest)
  latest_date <- as.Date(latest)
  
  # The first band acts as a template.
  SR = SRs[[1]]
  
  # First three columns and last two columns are not dates.
  dates_indexes <- 4:(ncol(SR) - 2)
  dates <- names(SR)[dates_indexes]
  col_dates <- as.Date(parse_date(dates))
  
  filtered_dates_indexes <- (col_dates >= earliest_date) & (col_dates <= latest_date)
  selected_dates_indexes <- dates_indexes[filtered_dates_indexes]
  
  # Combines indexes to select all indexes of dates in range or non-date columns.
  all_selected_indexes <- c(1:3, selected_dates_indexes, (ncol(SR) - 1):ncol(SR))
  
  # Selects for each band the non-date columns and the date columns in range.
  SRs <- lapply(SRs, select, all_of(all_selected_indexes))
}
