#' @title
#' @return
#' @author Aaron Conway
#' @export
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom dplyr desc
#' @importFrom ggplot2 ggplot aes theme_minimal
#' labs element_blank theme ggsave
#' @importFrom ggtext geom_richtext

create_global_thirst_plot <- function(data
                                      # global_thirst_corr
                                      ) {

  # tau <- round(global_thirst_corr$tau, 2)
  # tau_low <- round(global_thirst_corr$CI_low, 2)
  # tau_high <- round(global_thirst_corr$CI_high, 2)
  #
  # cor <- tibble(
  #   ptds = 8,
  #   global_thirst_factor = "Extremely uncomfortable",
  #   # text label containing r^2 value
  #   label = glue("*tau* = {tau}; (95% CI = {tau_low} to {tau_high})")
  # )

  data %>%
    ggplot(aes(
      x = ptds, y = reorder(global_thirst_factor, desc(global_thirst_factor)
      ),
      colour = global_thirst_factor
    )) +
    geom_beeswarm(groupOnX = FALSE, alpha = 0.5, size = 2) +
    labs(
      y = element_blank(),
      x = "\nPTDS-5"
    ) +
    theme_minimal()+
    theme(
      legend.position = "none",
      plot.title.position = "plot",
      text = element_text(size=9)
    ) +
    # geom_richtext(
    #   data = cor,
    #   aes(label = label),
    #   hjust = 1, vjust = 2
    # ) +
    scale_x_continuous(
      breaks = seq(0, 10, by = 2)
    )
}
