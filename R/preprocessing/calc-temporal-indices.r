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
  
  # Calculates all indices used in Xu et al. (2022).
  NDVI  = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR1=NULL, SWIR2=NULL)
      {return((NIR - RED)/(NIR + RED))}
  NIRv  = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR1=NULL, SWIR2=NULL)
      {return(((NIR - RED) / (NIR + RED))*NIR)}
  NDMI  = function(BLUE=NULL, GREEN=NULL, RED=NULL, NIR,  SWIR1,      SWIR2=NULL)
      {return((NIR - SWIR1)/(NIR + SWIR1))}
  EVI   = function(BLUE     , GREEN=NULL, RED     , NIR,  SWIR1=NULL, SWIR2=NULL)
      {return(2.5*((NIR-RED)/((NIR + 6*RED - 7.5*BLUE) + 1)))}
  MNDWI = function(BLUE=NULL, GREEN,      RED=NULL, NIR=NULL,  SWIR1, SWIR2=NULL)
      {return((GREEN - SWIR1)/(GREEN + SWIR1))}
  
  # Extra indices.
  # Reasoning for DVI: https://www.tandfonline.com/doi/full/10.1080/15481603.2020.1846948
  DVI   = function(BLUE=NULL, GREEN=NULL, RED,      NIR,  SWIR1=NULL, SWIR2=NULL)
      {return(NIR - RED)}
  # https://www.mdpi.com/2220-9964/7/12/453 MNDBI
  MNDBI = function(BLUE,      GREEN=NULL, RED=NULL, NIR=NULL,  SWIR1=NULL, SWIR2)
      {return((SWIR2 - BLUE)/(SWIR2 + BLUE))}
  TCB   = function(BLUE,      GREEN,      RED,      NIR,  SWIR1,      SWIR2)
      {return(0.3037*BLUE + 0.2793*GREEN + 0.4743*RED + 0.5585*NIR + 0.5082*SWIR1 + 0.1863*SWIR2)}
  TCG   = function(BLUE,      GREEN,      RED,      NIR,  SWIR1,      SWIR2)
      {return(-0.2941*BLUE - 0.243*GREEN - 0.5424*RED + 0.7276*NIR + 0.0713*SWIR1 - 0.1608*SWIR2)}
  TCW   = function(BLUE,      GREEN,      RED,      NIR,  SWIR1,      SWIR2)
      {return(0.1511*BLUE + 0.1973*GREEN + 0.3283*RED + 0.3407*NIR - 0.7117*SWIR1 - 0.4559*SWIR2)}

  indices_formulae = list(NDVI=NDVI, NIRv=NIRv, NDMI=NDMI, EVI=EVI, MNDWI=MNDWI, 
                          DVI=DVI, MNDBI=MNDBI, TCB=TCB, TCG=TCG, TCW=TCW)
  
  # Writes the indices' values to disk.
  for (i in seq_along(indices_formulae)) {
    index_formula = indices_formulae[[i]]
    OutFile = paste0(Feature_GPKG, names(indices_formulae)[i], ".gpkg")
    Iz = index_formula(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]], SRZ[["SR_B7"]])
    Isf = ZooToSF(Iz, OutTemplate)
    st_write(Isf, OutFile, append=F)
  }
  
  # https://www.mdpi.com/2220-9964/7/12/453 TCWVI
  TCBz = TCB(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]], SRZ[["SR_B7"]])
  TCGz = TCG(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]], SRZ[["SR_B7"]])
  TCWVIz = (TCBz - TCGz)/(TCBz + TCGz)
  TCWVIsf = ZooToSF(TCWVIz, OutTemplate)
  st_write(TCWVIsf, paste0(Feature_GPKG, "TCWVI.gpkg"), append=F)
}
