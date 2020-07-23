##' @title
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom here here
#' @importFrom ggplot2 ggplot aes geom_smooth theme_minimal labs theme
#' ggsave
#' @importFrom ggtext geom_richtext

create_intensity_plot <- function(data,
                                  intensity_corr) {

  tau <- round(intensity_corr$tau, 2)
  tau_low <- round(intensity_corr$CI_low, 2)
  tau_high <- round(intensity_corr$CI_high, 2)

  cor <- tibble(
    ptds = 9.5,
    thirst_intensity = 2,
    # text label containing r^2 value
    label = glue("*tau* = {tau}; (95% CI = {tau_low} to {tau_high})")
  )


  data %>%
    # mutate(thirst_intensity = as_factor(thirst_intensity)) %>%
    ggplot(aes(x = ptds, y = factor(thirst_intensity),
               colour = factor(thirst_intensity))) +
    geom_beeswarm(alpha = 0.5, size = 2, groupOnX = FALSE) +
    labs(
      y = "\nThirst intensity (0-10; higher scores indicate greater thirst intensity)",
      x = "PTDS-5 (0-10; higher scores indicate worse thirst discomfort)\n"
    ) +
    theme(
      legend.position = "none",
      plot.title.position = "plot"
    ) +
    geom_richtext(
      data = cor,
      aes(label = label),
      hjust = 1, vjust = 2
    )+
    scale_x_continuous(
      breaks = seq(0, 10, by = 2)
    )
}
