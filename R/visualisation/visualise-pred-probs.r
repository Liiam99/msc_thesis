library(dplyr)
library(ggplot2)

visualise_pred_probs <- function(preds, title="Distribution of prediction values per result type") {
  vis_preds <- preds %>%
    mutate(
      type=case_when(
        obs == "Change" & pred == "Change" ~ "TP",
        obs == "NoChange" & pred == "Change" ~ "FP",
        obs == "NoChange" & pred == "NoChange" ~ "TN",
        obs == "Change" & pred == "NoChange" ~ "FN"
      )
    )
  
  group_counts <- vis_preds %>%
    group_by(Change, type) %>%
    summarise(count = n())
  
  # Calculate relative frequency per group
  group_counts <- group_counts %>%
    group_by(type) %>%
    mutate(rel_freq = count / sum(count))
  
  # Plot the data
  ggplot(group_counts, aes(x = Change, y = rel_freq, fill = type)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
    scale_fill_manual(values = c("red", "blue", "green", "purple")) +
    labs(title = title, x = "Change Prediction", y = "Relative Frequency") +
    theme_minimal(base_size=27)
}
