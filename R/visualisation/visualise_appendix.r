source("./R/preprocessing/old-calc-base-features.r")
source("./R/utils/utils.r")

#### Calculating average NAs per segment for old method. ####
base_features <- old_calc_base_features(reference_data_condensed, START, END)
means <- colMeans(base_features$NIRv)

# Retrieves the begin date and end date of each segment to label them.
names(means) <- rollapply(global_indices_ts$NIRv[, 1], width=6, function(x) { 
  dates <- names(x)
  begin_date <- dates[1]
  end_date <- dates[length(dates)]
  date_range <- gsub("X", "", paste(begin_date, "-", end_date))
  return(date_range)
  }, by=6, partial=TRUE, align="left")

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
  coord_flip() +
  geom_bar(aes(y = Mean + Difference), data = combined_df, stat = "identity", fill = "#F8766D") +
  geom_bar(stat = "identity", fill = "#00BA38") +
  labs(x = "Segments", y = "Mean number of observations", title = "") +
  ylim(0,6) +
  theme_minimal(base_size=27) +
  scale_x_discrete(limits=rev)



#### Number of NAs per prediction type ####
visualise_obs_counts(global_preds, global_indices_ts$NDVI)
visualise_obs_counts(brazil_preds, brazil_indices_ts$NDVI)



#### Combined SHAP feature contribution ####
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
