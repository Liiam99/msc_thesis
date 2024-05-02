# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggplot2)
library(dplyr) 

visualise_class_changes <- function(data) {
  data <- make_long(data, from, to)
  
  class_counts <- data %>%
    group_by(x, node) %>%
    summarise(count = n())

  data <- left_join(data, class_counts, by = c("x", "node"))
  
  ggplot(data, aes(x = x, 
                   next_x = next_x, 
                   node = node, 
                   next_node = next_node,
                   fill = factor(node),
                   label = paste0(node, " (", count, ")"))) + # Use count for label
    geom_sankey() +
    geom_sankey_label() +
    theme_sankey(base_size = 20) +
    geom_sankey_label(size=5) +
    theme(legend.position = "none") + 
    scale_fill_manual(values = c(
      "bare" = "#d4a75e",
      "crops" = "#035162",
      "grassland" = "#77bf77",
      "tree" = "#4b7a4b",
      "urban_built_up" = "#5b95c2",
      "shrub" = "#9e9e2b",
      "wetland_herbaceous" = "#83c0c2",
      "water" = "#3876d9"
    )) +
    labs(x = "Regional Land Cover Changes")
}
