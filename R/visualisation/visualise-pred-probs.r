library(dplyr)
library(ggplot2)

visualise_pred_probs <- function(preds, title="Distribution of prediction values per result type") {
  vis_preds <- preds %>%
    mutate(
      Type=case_when(
        obs == "Change" & pred == "Change" ~ "TP",
        obs == "NoChange" & pred == "Change" ~ "FP",
        obs == "NoChange" & pred == "NoChange" ~ "TN",
        obs == "Change" & pred == "NoChange" ~ "FN"
      )
    )
  
  group_counts <- vis_preds %>%
    group_by(Change, Type) %>%
    summarise(count = n())
  
  print(vis_preds %>%
          group_by(Type) %>%
          summarise(count = n()))
  
  # Calculate relative frequency per group
  group_counts <- group_counts %>%
    group_by(Type) %>%
    mutate(rel_freq = count / sum(count))
  
  # Plot the data
  ggplot(group_counts, aes(x = Change, y = rel_freq, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 1) +
    scale_fill_manual(values = c("red", "blue", "green", "purple")) +
    labs(title = title, x = "Change probability prediction", y = "Relative frequency") +
    theme_minimal(base_size=35)
}
