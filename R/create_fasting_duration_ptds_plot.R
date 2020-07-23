##' @title
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom ggplot2 ggplot aes geom_jitter geom_smooth
#' theme_minimal labs theme ggsave
#' @importFrom patchwork plot_layout
#' @importFrom ggtext element_markdown geom_richtext
#' @importFrom dplyr summarize mutate
#' @importFrom glue glue
#' @importFrom tibble tibble

create_fasting_duration_ptds_plot <- function(data = data_ptds,
                                              fluids_corr, food_corr) {

  rho <- round(fluids_corr$rho, 2)
  rho_low <- round(fluids_corr$CI_low, 2)
  rho_high <- round(fluids_corr$CI_high, 2)

  ptds_fluids_cor <- tibble(
  # location of each text label in data coordinates
  ptds = 10,
  fluids_duration = 20,
  # text label containing rho value
  label = glue("*rho* = {rho}
      (95% CI = {rho_low} to {rho_high})")
  )

  fluids <- data %>%
    ggplot(aes(y = ptds, x = fluids_duration)) +
    geom_jitter(alpha = 0.5,
                colour = "dodgerblue") +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(
      x = "\nDuration of fasting from <span style='color:dodgerblue'>fluids</span> in hours",
      y = "PTDS-5\n"
    ) +
    theme(
      legend.position = "none",
      plot.title.position = "plot",
      axis.title.x = element_markdown()

    ) +
    ggtext::geom_richtext(
      data = ptds_fluids_cor,
      aes(label = label),
      hjust = 1, vjust = 2
    ) +
    scale_y_continuous(
      breaks = seq(0, 10, by = 2)
    )

  rho <- round(food_corr$rho, 2)
  rho_low <- round(food_corr$CI_low, 2)
  rho_high <- round(food_corr$CI_high, 2)

  ptds_food_cor <- tibble(
  # location of each text label in data coordinates
  ptds = 10,
  food_duration = 25,
  # text label containing rho value
  label = glue("*rho* = {rho}
      (95% CI = {rho_low} to {rho_high})")
  )

  food <- data %>%
    ggplot(aes(y = ptds, x = food_duration)) +
    geom_jitter(alpha = 0.5,
                colour = "orangered") +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(
      x = "\nDuration of fasting from <span style='color:orangered'>food</span> in hours",
      y = "PTDS-5\n"
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_markdown(),
      plot.title.position = "plot"
    ) +
    ggtext::geom_richtext(
      data = ptds_food_cor,
      aes(label = label),
      hjust = 1, vjust = 2
    ) +
    scale_y_continuous(
      breaks = seq(0, 10, by = 2)
    )


  fluids / food

}
