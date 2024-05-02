library(dplyr)
library(ggplot2)

visualise_obs_counts <- function(preds, time_series) {
  vis_preds <- preds %>%
    mutate(
      type=case_when(
        obs == "Change" & pred == "Change" ~ "TP",
        obs == "NoChange" & pred == "Change" ~ "FP",
        obs == "NoChange" & pred == "NoChange" ~ "TN",
        obs == "Change" & pred == "NoChange" ~ "FN"
      )
    )
  
  FNs <- vis_preds[vis_preds$type == "FN", ]
  TPs <- vis_preds[vis_preds$type == "TP", ]
  FPs <- vis_preds[vis_preds$type == "FP", ]
  TNs <- vis_preds[vis_preds$type == "TN", ]
  
  FNs_time_series <- time_series[, names(time_series) %in% FNs$location_id]
  FNs_obs <- !apply(FNs_time_series, 1, is.na)
  FNs_n_obs <- apply(FNs_obs, 1, sum)

  TPs_time_series <- time_series[, names(time_series) %in% TPs$location_id]
  TPs_obs <- !apply(TPs_time_series, 1, is.na)
  TPs_n_obs <- apply(TPs_obs, 1, sum)

  TNs_time_series <- time_series[, names(time_series) %in% TNs$location_id]
  TNs_obs <- !apply(TNs_time_series, 1, is.na)
  TNs_n_obs <- apply(TNs_obs, 1, sum)

  FPs_time_series <- time_series[, names(time_series) %in% FPs$location_id]
  FPs_obs <- !apply(FPs_time_series, 1, is.na)
  FPs_n_obs <- apply(FPs_obs, 1, sum)

  data <- data.frame(
    Type = c(rep("TP", length(TPs_n_obs)),
             rep("FP", length(FPs_n_obs)),
             rep("TN", length(TNs_n_obs)),
             rep("FN", length(FNs_n_obs))),
    Count = c(TPs_n_obs, FPs_n_obs, TNs_n_obs, FNs_n_obs)
  )

  ggplot(data, aes(x = Type, y = Count, fill = Type)) +
    geom_boxplot() +
    labs(title = "Counts of cloud-free regional observations per prediction type",
         x = "Prediction type",
         y = "Number of cloud-free regional observations") +
    scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#C77CFF")) +
    theme_minimal(base_size=27)
}
