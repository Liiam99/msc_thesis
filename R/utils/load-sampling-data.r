# Credit:
# https://github.com/GreatEmerald/supervised-bfast/blob/main/src/utils/load-sampling-data.r

library(reshape2)
library(sf)
library(pbapply)

source("./R/utils/covariate-names.r")
source("./R/utils/utils.r")

# Updates the dominant_lc column based on the classes desired
UpdateDominantLC = function(df, classes = GetCommonClassNames())
{
  ClassProportions = df[,classes[classes %in% names(df)]]
  DominantClasses = apply(ClassProportions, 1, which.max)
  df$dominant_lc = factor(classes[DominantClasses])
  return(df)
}

# Remove rows with NAs and drop covariates with too few observations
TidyData = function(df, classes = GetCommonClassNames(), drop.cols=NULL)
{
  # Remove rows that have NA in some key columns.
  # NA in elevation means that the point is not actually in Africa. Remove those.
  # NA in amplitude1 means that we have no time series over the area, so remove these (though likely it's bare soil)
  # NA in evi means that we didn't have an image from the summer of year 2016. That's a lot of points to remove; but the alternative is dropping those covars altogether.
  # NA in soil or climate covars means it's over water.
  df = if ("validation_id" %in% colnames(df)) df[!is.na(df$validation_id), ] else df[!is.na(df$point_id), ]
  
  Before = nrow(df)
  if (is.character(drop.cols))
  {
    DropRows = apply(df[,drop.cols], 1, function(x){any(!is.finite(x))})
    #DropRows = apply(df[,GetAllPixelCovars()], 1, function(x){any(is.na(x))})
    df = df[!DropRows,]
    After = nrow(df)
    print(paste("Dropped NAs, data frame size reduced from", Before, "to", After))
    Before = After
    
    stopifnot(all(apply(df[,drop.cols], 2, function(x){sum(is.na(x))}) / nrow(df) * 100 == 0))
  }
  
  # Recalculate dominant classes based on all classes
  df = UpdateDominantLC(df, classes)
  # Drop those dominated by "not_sure"
  df = df[df$dominant_lc != "not_sure",]
  
  # Reclassify rare classes to common ones
  df = ReclassifyAndScale(df)
  
  After = nrow(df)
  print(paste("Reclassified and rescaled small classes, data frame size reduced from", Before, "to", After))
  
  # Also drop the level, otherwise sampling would try to sample from 0 points
  df = UpdateDominantLC(df, classes)
  
  return(df)
}

ReclassifyAndScale = function(df, output.classes=GetCommonClassNames())
{
  # Some classes are merged to other classes. Put the values into the bigger classes
  ClassMap = c(burnt="grassland",
               fallow_shifting_cultivation="crops",
               flooded="wetland_herbaceous",
               lichen_and_moss="grassland",
               lichen="grassland",
               fl.grass="wetland_herbaceous",
               fl.lichen="wetland_herbaceous",
               snow_and_ice="bare",
               snow="bare")
  
  for (class in 1:length(ClassMap))
  {
    if (names(ClassMap[class]) %in% names(df))
      df[[ClassMap[class]]] = df[[ClassMap[class]]] + df[[names(ClassMap[class])]]
  }
  
  # Scale relevant classes to 100%; that way we get rid of influences from not_sure and snow_and_ice
  RelevantClasses = df[, output.classes]
  ClassSums = rowSums(RelevantClasses)
  ZeroRows = ClassSums == 0

  if (any(ZeroRows))
  {
    print(paste("Dropping", sum(ZeroRows), "samples because all their relevant fractions are zero"))
    RelevantClasses = RelevantClasses[!ZeroRows,]
    df = df[!ZeroRows,]
    ClassSums = ClassSums[!ZeroRows]
  }
  
  df[,output.classes] = RelevantClasses / (ClassSums / 100)
  stopifnot(all(round(rowSums(df[,output.classes])) == 100))
  return(df)
}

# Rename the columns of the reference dataset to match those in the IIASA 2015 dataset
RenameReferenceData = function(df)
{
  NameMap = data.frame(from=c("epoch", "subpix_mean_x", "subpix_mean_y", "trees", "grass", "urban"),
                       to=c("reference_year", "centroid_x", "centroid_y", "tree", "grassland", "urban_built_up"))
  NewNames = names(df)
  for (i in 1:nrow(NameMap))
    if (NameMap[i,"from"] %in% names(df))
      NewNames[names(df) == NameMap[i,"from"]] = NameMap[i, "to"]
  
  names(df) = NewNames
  return(df)
}

# Convert a GPKG file into the same format as the reference data
# (long format with one column per class)
FlattenGPKG = function(filename)
{
  Classes = st_layers(filename)$name
  
  MeltClass = function(Class)
  {
    ClassSF = st_read(filename, Class)
    Result = melt(ClassSF, id.vars = "location_id", measure.vars = datecols(ClassSF), variable.name="timestamp.x", value.name=Class)
    Result[["timestamp.x"]] = as.Date(Result[["timestamp.x"]], "X%Y.%m.%d")
    return(Result)
  }
  ResultList = pblapply(Classes, MeltClass, cl=length(Classes))
  
  # Result = Reduce(merge, ResultList) # Slow option
  
  Cbind = function(df1, df2)
  {
    stopifnot(all(df1$timestamp.x == df2$timestamp.x))
    stopifnot(all(df1$location_id == df2$location_id))
    cbind(df1, df2[3])
  }
  Result = Reduce(Cbind, ResultList)
  Result = Result[complete.cases(Result),]
  return(Result)
}