# Credit:
# https://github.com/GreatEmerald/supervised-bfast/blob/main/src/015_preprocess_dense/00_CalcTemporalIndices.r

# Calculate temporal indices (harmonics, VIs, etc. over time).
# AKA 1D convolutions.
# Input: Geopackage of Landsat surface reflectance time series from GEE.
# Output: A (new) Geopackage with one layer (table) per feature in the same schema as input.

library(sf)
library(zoo)
library(parallel)

source("../utils/utils.r")

SR_GPKG = "../../data/global/WURChange20152019_Landsat8_TS.gpkg"
Feature_GPKG = "../../data/wur_validation_features/"

# Read all layers into a single list "SR"
SRNames = st_layers(SR_GPKG)$name
SR = lapply(SRNames, function(name) st_read(SR_GPKG, layer=name))

# Save a template for writing, with consistent column names
OutTemplate = SR[[1]]
names(OutTemplate)[datecols(OutTemplate)] = strtrim(names(OutTemplate)[datecols(OutTemplate)], 11)

# Convert into a zoo matrix
SRZ = lapply(lapply(SR, SFToMatrix), MatrixToZoo)
names(SRZ) = SRNames

# Calculate all indices of interest
NDVI  = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR=NULL)
    {return((NIR-RED)/(NIR+RED))}
NIRv  = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR=NULL)
    {return(((NIR-RED) / (NIR+RED))*NIR)}
NDMI  = function(BLUE=NULL, GREEN=NULL, RED=NULL, NIR,  SWIR)
    {return((NIR-SWIR)/(NIR+SWIR))}
EVI   = function(BLUE     , GREEN=NULL, RED     , NIR,  SWIR=NULL)
    {return(2.5*((NIR-RED)/((NIR + 6*RED - 7.5*BLUE)+1)))}
MNDWI = function(BLUE=NULL, GREEN,      RED=NULL, NIR=NULL,  SWIR)
    {return((GREEN-SWIR)/(GREEN+SWIR))}

OutFile = paste(Feature_GPKG, "NDVI.gpkg", sep="_")
if (!file.exists(OutFile))
{
    NDVIz = NDVI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    NDVIsf = ZooToSF(NDVIz, OutTemplate)
    st_write(NDVIsf, OutFile)
    rm(NDVIsf, NDVIz)
}

OutFile = paste(Feature_GPKG, "NIRv.gpkg", sep="_")
if (!file.exists(OutFile))
{
  NIRvz = NIRv(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
  NIRvsf = ZooToSF(NIRvz, OutTemplate)
  st_write(NIRvsf, OutFile)
  rm(NIRvsf, NIRvz)
}

OutFile = paste(Feature_GPKG, "NDMI.gpkg", sep="_")
if (!file.exists(OutFile))
{
  NDMIz = NDMI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
  NDMIsf = ZooToSF(NDMIz, OutTemplate)
  st_write(NDMIsf, OutFile)
  rm(NDMIsf, NDMIz)
}

OutFile = paste(Feature_GPKG, "EVI.gpkg", sep="_")
if (!file.exists(OutFile))
{
    EVIz = EVI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    EVIsf = ZooToSF(EVIz, OutTemplate)
    st_write(EVIsf, OutFile)
    rm(EVIsf, EVIz)
}

OutFile = paste(Feature_GPKG, "MNDWI.gpkg", sep="_")
if (!file.exists(OutFile))
{
  MNDWIz = MNDWI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
  MNDWIsf = ZooToSF(MNDWIz, OutTemplate)
  st_write(MNDWIsf, OutFile)
  rm(MNDWIsf, MNDWIz)
}

# Calculate convolutions: 1-year, 3-year
# A window is centred on the observation, so intervals are:
# 3 year = 68
# 1 year = 23
# Quarter= 6
# A mean would be affected by outliers, so take the median (unless we perform extra filtering)
VIs = c("NDVI", "NIRv", "NDMI", "EVI", "MNDWI")
WinSizes = c(yearly=23, threeyear=68)
Stats = c("quant05", "median", "quant95", "IQR")

# Get a missing data mask
VI = st_read(paste0(Feature_GPKG, "_", VIs[1], ".gpkg"))
VIz = SFToZoo(VI)
MissingData = is.na(VIz)
rm(VIz)

# Calculate rolling statistics over a given window size for a vegetation index
# VI layer name, window size in observations (16-day units), statistic function,
# zoo object to apply to, what file to write to
CalcRollingStat = function(VIname, WinSize, Stat, OutFile = Feature_GPKG)
{
    OutName = paste0(OutFile, "_", paste(VIname, WinSize, Stat, sep="_"), ".gpkg")
    if (file.exists(OutName))
        return() # If the layer already exists, don't do anything
    VIzoo = SFToZoo(st_read(paste0(OutFile, "_", VIname, ".gpkg")))
    StatFun = switch(Stat,
                     quant05=function(...) quantile(..., probs=0.05),
                     quant95=function(...) quantile(..., probs=0.95),
                     get(Stat))
    VI_stat = rollapply(VIzoo, WinSize, StatFun, na.rm = TRUE, partial = TRUE)
    VI_stat = as.matrix(VI_stat) # Workaround for a bug in zoo
    VI_stat[MissingData] = NA
    VI_stat[!is.finite(VI_stat)] = NA
    st_write(ZooToSF(VI_stat, VI), OutName)
    rm(VI_stat)
    return()
}

l <- expand.grid(VIname=VIs, WinSize=WinSizes, Stat=Stats, stringsAsFactors=FALSE)
RollingStats = do.call(mcmapply, c(FUN=CalcRollingStat, as.list(l), mc.cores=5))
