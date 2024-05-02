# CREDIT: https://github.com/ModelOriented/treeshap/blob/master/R/plot_feature_importance.R

library(ggplot2)

plot_feature_importance_mod <- function (shaps, desc_sorting=TRUE, max_vars=ncol(shaps), 
                                         title="Feature Importance", subtitle=NULL) {
  if (!is.logical(desc_sorting)) {
    stop("desc_sorting is not logical.")
  }
  if (!is.numeric(max_vars)) {
    stop("max_vars is not numeric.")
  }
  if (max_vars > ncol(shaps)) {
    warning("max_vars exceeded number of explained variables. All variables will be shown.")
    max_vars <- ncol(shaps)
  }
  mean <- colMeans(abs(shaps))
  df <- data.frame(variable = factor(names(mean)), importance = as.vector(mean))
  df$variable <- reorder(df$variable, df$importance * ifelse(desc_sorting, 
                                                             1, -1))
  df <- df[order(df$importance, decreasing = TRUE)[1:max_vars], 
  ]
  p <- ggplot(df, aes(x = variable, y = importance)) + 
    geom_bar(stat = "identity")
  
  p + coord_flip() + 
    theme_drwhy_vertical() + 
    ylab("mean(|SHAP value|)") + 
    xlab("") + 
    labs(title = title, subtitle = subtitle) + 
    scale_y_continuous(labels = scales::comma) + 
    theme(legend.position = "none") +
    theme_minimal(base_size=27)
}
