# Credit:
# https://github.com/GreatEmerald/supervised-bfast/blob/main/src/015_preprocess_dense/00_CalcTemporalIndices.r

# Calculates temporal indices over time.
# AKA 1D convolutions.
# Input: Geopackage of Landsat surface reflectance time series from GEE.
# Output: A (new) Geopackage with one layer (table) per feature in the same schema as input.

library(sf)
library(zoo)

source("./R/utils/utils.r")

calc_temporal_indices = function(SRs) {
  # Saves a template for writing, with consistent column names.
  OutTemplate = SRs[[1]]
  names(OutTemplate)[datecols(OutTemplate)] = strtrim(names(OutTemplate)[datecols(OutTemplate)], 11)
  
  # Converts into a zoo matrix.
  SRZ = lapply(lapply(SRs, SFToMatrix), MatrixToZoo)
  
  Feature_GPKG = "./data/global/processed/temporal_indices/"
  
  # Calculates all indices of interest.
  NDVI  = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR=NULL)
      {return((NIR-RED)/(NIR+RED))}
  NIRv  = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR=NULL)
      {return(((NIR-RED) / (NIR+RED))*NIR)}
  NDMI  = function(BLUE=NULL, GREEN=NULL, RED=NULL, NIR,  SWIR)
      {return((NIR-SWIR)/(NIR+SWIR))}
  EVI   = function(BLUE     , GREEN=NULL, RED     , NIR,  SWIR=NULL)
      {return(2.5*((NIR-RED)/((NIR + 6*RED - 7.5*BLUE) + 1)))}
  MNDWI = function(BLUE=NULL, GREEN,      RED=NULL, NIR=NULL,  SWIR)
      {return((GREEN-SWIR)/(GREEN+SWIR))}
  
  OutFile = paste0(Feature_GPKG, "NDVI.gpkg")
  if (!file.exists(OutFile))
  {
      NDVIz = NDVI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
      NDVIsf = ZooToSF(NDVIz, OutTemplate)
      st_write(NDVIsf, OutFile)
      rm(NDVIsf, NDVIz)
  }
  
  OutFile = paste0(Feature_GPKG, "NIRv.gpkg")
  if (!file.exists(OutFile))
  {
    NIRvz = NIRv(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    NIRvsf = ZooToSF(NIRvz, OutTemplate)
    st_write(NIRvsf, OutFile)
    rm(NIRvsf, NIRvz)
  }
  
  OutFile = paste0(Feature_GPKG, "NDMI.gpkg")
  if (!file.exists(OutFile))
  {
    NDMIz = NDMI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    NDMIsf = ZooToSF(NDMIz, OutTemplate)
    st_write(NDMIsf, OutFile)
    rm(NDMIsf, NDMIz)
  }
  
  OutFile = paste0(Feature_GPKG, "EVI.gpkg")
  if (!file.exists(OutFile))
  {
      EVIz = EVI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
      EVIsf = ZooToSF(EVIz, OutTemplate)
      st_write(EVIsf, OutFile)
      rm(EVIsf, EVIz)
  }
  
  OutFile = paste0(Feature_GPKG, "MNDWI.gpkg")
  if (!file.exists(OutFile))
  {
    MNDWIz = MNDWI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    MNDWIsf = ZooToSF(MNDWIz, OutTemplate)
    st_write(MNDWIsf, OutFile)
    rm(MNDWIsf, MNDWIz)
  }
}
