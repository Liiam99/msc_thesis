# Credit: 
# https://github.com/GreatEmerald/supervised-bfast/blob/main/src/010_preprocessing/postprocess-gee-download.r

# Script to convert the GEE extracted points output CSVs into a compact GeoPackage
library(sf)

InputDir = file.path("..", "..", "data", "brazil", "raw", "brazil_bands_data_2015-2018")
DataFile = file.path("..", "..", "data", "brazil", "raw", "brazil_reference_data.csv")
OutFile = file.path("..", "..", "data", "brazil", "raw", "BrazilChange20152018_Landsat8_TS.gpkg")
Bands = c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7") # For Landsat 8
#Bands = c("water", "trees", "grass", "flooded_vegetation", "crops", "shrub_and_scrub", "built", "bare", "snow_and_ice") # For Dynamic World
id = "TARGETID" # "sample_id" for validation data
xycols = c("LON", "LAT") # x/y column names in the DataFile, "subpix_mean_x", "subpix_mean_y" for validation
integers = TRUE # Whether the data type is integers (Landsat) or floats (Dynamic World)

# We need the original data to make it spatial so we can put it into a .gpkg
OriginalData = st_read(DataFile, options=c(paste0("X_POSSIBLE_NAMES=", xycols[1]), paste0("Y_POSSIBLE_NAMES=",xycols[2])))
st_crs(OriginalData) = 4326
UniqueData = OriginalData[!duplicated(OriginalData[[id]]),]
# Keep only the coordinates and id
UniqueData = UniqueData[,c(xycols[1], xycols[2], id)]

ListBands = function(Band) list.files(InputDir, pattern = glob2rx(paste0("*", Band, ".csv")), full.names = TRUE)
InputFiles = lapply(Bands, ListBands)

ProcessBand = function(Filenames)
{
    SingleBand = NULL
    for (Filename in Filenames)
    {
        SingleFile = read.csv(Filename, stringsAsFactors=FALSE)
        SingleBand[setdiff(names(SingleFile), names(SingleBand))] <- NA
        SingleFile[setdiff(names(SingleBand), names(SingleFile))] <- NA
        SingleBand = rbind(SingleBand, SingleFile)
    }
    # Remove GEE-specific columns
    SingleBand = SingleBand[,! names(SingleBand) %in% c(".geo", "system.index")]
    # Explicitly use integers
    NumericCols = grep(glob2rx("X????.??.??_*"), names(SingleBand))
    SingleBand[, NumericCols] = if (integers) {
        as.integer(round(as.matrix(SingleBand[,NumericCols])))
    } else {
        # Convert to integers 0-100
        as.integer(round(as.matrix(SingleBand[,NumericCols])*100))
    }
    # Sort columns by date
    SingleBand = SingleBand[,order(names(SingleBand))]
    SpatialBand = merge(UniqueData, SingleBand, by=id)
    names(SpatialBand)[names(SpatialBand) == id] = "location_id"

    BandMatch = which(sapply(Bands, function(Band) length(grep(Band, Filenames[1])) > 0))
    BandName = Bands[BandMatch]

    st_write(SpatialBand, OutFile, layer=BandName, append=TRUE)
}

lapply(InputFiles, ProcessBand)
