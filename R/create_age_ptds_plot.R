
##' @title
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom ggplot2 ggplot aes geom_jitter
#' geom_smooth theme_minimal labs theme ggsave
create_age_ptds_plot <- function(data) {
  data %>%
    ggplot(aes(y = ptds, x = age)) +
    geom_jitter(alpha = 0.5) +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(
      x = "\nAge",
      y = "5-item peri-operative thirst discomfort scale (PTDS-5)\n",
      title = "Association between PTDS-5 and age",
      subtitle = "PTDS-5 score ranges from 5 to 15 with higher scores indicating worse thirst discomfort"
    ) +
    theme(
      legend.position = "none",
      plot.title.position = "plot"
    )

  ggsave(device = "png", filename = here("manuscript/figures/age.png"),
         width = 174, units = "mm")
}
