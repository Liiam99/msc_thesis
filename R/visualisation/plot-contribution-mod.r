# CREDIT TO: https://github.com/ModelOriented/treeshap/blob/master/R/plot_contribution.R

library(ggplot2)

plot_contribution_mod <- function(shap,
                                  x,
                                  prediction,
                                  max_vars = 5,
                                  min_max = NA,
                                  digits = 3,
                                  title = "SHAP Break-Down",
                                  subtitle = "") {
  
  
  # Argument checks.
  if (max_vars > ncol(shap)) {
    warning("max_vars exceeds number of variables. All variables will be shown.")
    max_vars <- ncol(shap)
  }
  if (nrow(shap) != 1) {
    warning("Only 1 observation can be plotted. Plotting 1st one.")
    shap <- shap[1, ]
  }
  
  x <- x[, colnames(shap)]

  df <- data.frame(variable = colnames(shap), contribution = as.numeric(shap))
  
  # setting variable names to showing their value
  df$variable <- paste0(df$variable, " = ", as.character(x))
  
  # selecting max_vars most important variables
  is_important <- order(abs(df$contribution), decreasing = TRUE)[1:max_vars]
  other_variables_contribution_sum <- sum(df$contribution[-is_important])
  df <- df[is_important, ]
  df$position <- 2:(max_vars + 1)
  if (max_vars < ncol(shap)) {
    df <- rbind(df, data.frame(variable = "+ all other variables",
                               contribution = other_variables_contribution_sum,
                               position = max(df$position) + 1))
  }
  
  # adding "prediction" bar
  df <- rbind(df, data.frame(variable = "prediction",
                             contribution = prediction,
                             position = max(df$position) + 1))
  
  df$sign <- ifelse(df$contribution >= 0, "1", "-1")
  
  # adding "intercept" bar
  df <- rbind(df, data.frame(variable = "intercept",
                             contribution = prediction - sum(as.numeric(shap)),
                             position = 1,
                             sign = "X"))
  
  # ordering
  df <- df[order(df$position), ]
  
  # adding columns needed by plot
  df$cumulative <- cumsum(df$contribution)
  df$prev <- df$cumulative - df$contribution
  df$text <- as.character(round(df$contribution, digits))
  df$text[df$contribution > 0] <- paste0("+", df$text[df$contribution > 0])
  
  # intercept bar corrections:
  df$prev[1] <- df$contribution[1]
  df$text[1] <- as.character(round(df$contribution[1], digits))
  
  # prediction bar corrections:
  df$prev[nrow(df)] <- df$contribution[1]
  df$cumulative[nrow(df)] <- df$cumulative[max_vars + 2]
  df$sign[nrow(df)] <- "X"
  df$text[nrow(df)] <- as.character(round(df$contribution[nrow(df)], digits))
  
  # reversing postions to sort bars decreasing
  df$position <- rev(df$position)
  
  # base plot
  p <- ggplot(df, aes(x = position + 0.5,
                      y = pmax(cumulative, prev),
                      xmin = position + 0.15, xmax = position + 0.85,
                      ymin = cumulative, ymax = prev,
                      fill = sign,
                      label = text))
  
  # add rectangles and hline
  p <- p +
    geom_errorbarh(data = df[-nrow(df), ],
                   aes(xmax = position - 0.85,
                       xmin = position + 0.85,
                       y = cumulative), height = 0,
                   color = "#371ea3") +
    geom_rect(alpha = 0.9)
  
  # add adnotations
  drange <- diff(range(df$cumulative))
  p <- p + geom_text(aes(y = pmax(cumulative,  cumulative - contribution)),
                     vjust = 0.5,
                     nudge_y = drange * 0.05,
                     hjust = 0,
                     color = "#371ea3")
  
  # set limits for contributions
  if (any(is.na(min_max))) {
    x_limits <- scale_y_continuous(expand = c(0.05, 0.15), name = "", labels = scales::comma)
  } else {
    x_limits <- scale_y_continuous(expand = c(0.05, 0.15), name = "", limits = min_max, labels = scales::comma)
  }
  
  p <- p + x_limits +
    scale_x_continuous(labels = df$variable, breaks = df$position + 0.5, name = "") +
    scale_fill_manual(values = colors_breakdown_drwhy())
  
  # add theme
  p + coord_flip() + theme_drwhy_vertical() +
    theme(legend.position = "none") +
    labs(title = title, subtitle = subtitle)
}