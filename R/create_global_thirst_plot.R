##' @title
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom dplyr desc
#' @importFrom ggplot2 ggplot aes theme_minimal
#' labs element_blank theme ggsave
create_global_thirst_plot <- function(data) {
  data %>%
    ggplot(aes(x = ptds, y = reorder(global_thirst_factor, desc(global_thirst_factor)),
               colour = global_thirst_factor)) +
    geom_beeswarm(groupOnX = FALSE, alpha = 0.5, size = 2) +
    theme_minimal() +
    labs(
      y = element_blank(),
      x = "\nPTDS-5 (5-15; higher scores indicate worse thirst discomfort)"
    ) +
    theme(
      legend.position = "none",
      plot.title.position = "plot"
    )
  ggsave(device = "png", filename = here("manuscript/figures/global-thirst.png"),
         width = 174, units = "mm")
}
