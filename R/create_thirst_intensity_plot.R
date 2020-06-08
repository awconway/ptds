
##' @title
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom here here
#' @importFrom ggplot2 ggplot aes geom_smooth theme_minimal labs theme
#' ggsave
create_thirst_intensity_plot <- function(data) {
  data %>%
    # mutate(thirst_intensity = as_factor(thirst_intensity)) %>%
    ggplot(aes(x = ptds, y = factor(thirst_intensity),
               colour = factor(thirst_intensity))) +
    geom_beeswarm(alpha = 0.5, size = 2, groupOnX = FALSE) +
    theme_minimal() +
    labs(
      y = "\nThirst intensity (0-10; higher scores indicate greater thirst intensity)",
      x = "PTDS-5 (5-15; higher scores indicate worse thirst discomfort)\n"
    ) +
    theme(
      legend.position = "none",
      plot.title.position = "plot"
    )

  ggsave(device = "png", filename = here("manuscript/figures/thirst-intensity.png"),
         width = 174, units = "mm")
}
