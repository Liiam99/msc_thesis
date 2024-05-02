source("./R/preprocessing/old-calc-base-features.r")

# Calculating average NAs per segment for old method.
base_features <- calc_base_features(reference_data_condensed, START, END)
base_features <- split_matrix(base_features)
means <- colMeans(base_features$NIRv)

# Convert means to a dataframe
means_df <- data.frame(Column = names(means), Mean = means)

# Calculate the difference between the mean and 6 for each column
difference <- 6 - means_df$Mean

# Create a new dataframe for the difference bars
difference_df <- data.frame(Column = means_df$Column, Difference = difference)

# Combine means_df and difference_df
combined_df <- merge(means_df, difference_df, by = "Column")

# Create barplot using ggplot2
ggplot(combined_df, aes(x = Column, y = Mean)) +
  geom_bar(aes(y = Mean + Difference), data = combined_df, stat = "identity", fill = "#F8766D") +
  geom_bar(stat = "identity", fill = "#00BA38") +
  labs(x = "Segments", y = "Mean number of observations", title = "Mean number of complete and missing observations per segment") +
  ylim(0,6) +
  theme_minimal(base_size=27)



# Number of NAs per prediction type
visualise_obs_counts(global_preds, global_indices_ts$NDVI)
visualise_obs_counts(brazil_preds, brazil_indices_ts$NDVI)



# Combined SHAP feature contribution
df1 <- brazil_shaps
df2 <- global_shaps

mean <- colMeans(abs(df1))
df <- data.frame(variable = factor(names(mean)), importance = as.vector(mean))
df$variable <- reorder(df$variable, df$importance * ifelse(TRUE, 
                                                           1, -1))
brazil_shaps_df <- df[order(df$importance, decreasing = TRUE)[1:5], 
]
brazil_shaps_df$scale <- "regional"

mean <- colMeans(abs(df2))
df <- data.frame(variable = factor(names(mean)), importance = as.vector(mean))
df$variable <- reorder(df$variable, df$importance * ifelse(TRUE, 
                                                           1, -1))
global_shaps_df <- df[order(df$importance, decreasing = TRUE)[1:5], 
]
global_shaps_df$scale <- "global"

shaps_df <- bind_rows(global_shaps_df, brazil_shaps_df)
shaps_df <- shaps_df[order(shaps_df$importance, decreasing = TRUE)[1:10], 
]

p <- ggplot(shaps_df, aes(x = variable, y = importance, fill = scale)) + 
  geom_bar(stat = "identity", position = "identity")

p + coord_flip() + 
  theme_drwhy_vertical() + 
  ylab("mean(|SHAP value|)") + 
  xlab("") + 
  labs(title = "Feature importance", subtitle = "SHAP values for global and regional prediction errors") + 
  scale_y_continuous(labels = scales::comma) + 
  theme(legend.position = "none") +
  theme_minimal(base_size=27) + 
  scale_fill_manual(values = c(
    "global" = "#77bf77",
    "regional" = "#d4a75e"
  ))



# Feature importance for wetlands..
wetland_ids <- reference_data_condensed[reference_data_condensed$from_lcc == "wetland" | reference_data_condensed$to_lcc == "wetland", "location_id"]
wetland_ids <- na.omit(wetland_ids)
wetland_rows <- lapply(full_rf_results, function(result, features) {
  val_features <- features[result$val_idx, ]
  lol <- val_features$location_id %in% wetland_ids$location_id
  return(lol)
}, full_features)

wetland_rows_idx <- do.call(c, wetland_rows)

wetland_shaps <- global_shaps[wetland_rows_idx, ]
plot_feature_importance_mod(wetland_shaps)
