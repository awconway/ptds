#' @title create_clarity_plot
#' @return
#' @author Aaron Conway
#' @export
#' @importFrom dplyr count
#' @importFrom ggplot2 ggplot aes geom_col
#' labs theme ggsave
#'

create_clarity_plot <- function(data_ptds) {
  clarity_plot <- data_ptds %>%
    count(instructions_factor) %>%
    drop_na() %>%
    ggplot(aes(x = n, y = instructions_factor)) +
    geom_col(aes(fill = instructions_factor, color = instructions_factor)) +
    labs(
      x = "Number of participants",
      y = element_blank()
    ) +
    theme(
      panel.background = element_blank(),
      legend.position = "none",
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      text = element_text(family = "BioRhyme")
    )
}