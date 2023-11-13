source("../postprocessing/load-sampling-data.r")

IIASA_path <- file.path("..", "..", "data", "global", "IIASA_reference_data.csv")
IIASA_burned_path <- file.path("..", "..", "data", "global", "IIASA_burned_sample_ids.csv")

WUR_path <- file.path("..", "..", "data", "global", "WUR_reference_data.csv")
WUR_burned_path <- file.path("..", "..", "data", "global", "WUR_burned_location_ids.csv")

IIASA <- read.csv(IIASA_path)
IIASA <- TidyData(IIASA)

WUR <- read.csv(WUR_path)
WUR <- RenameReferenceData(WUR)
WUR <- TidyData(WUR)
WUR <- WUR[WUR$dataYear != 2019, ]

IIASA_burned <- read.csv(IIASA_burned_path)
IIASA <- IIASA[!IIASA$sample_id %in% IIASA_burned$sample_id, ]

WUR_burned <- read.csv(WUR_burned_path)
WUR <- WUR[!WUR$location_id %in% WUR_burned$location_id, ]
