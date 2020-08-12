#' @title
#' @return
#' @export
#' @importFrom ggplot2 ggplot aes theme_minimal scale_x_continuous
#' labs element_blank theme ggsave geom_histogram
create_distribution_plot <- function(data_ptds) {
    distribution_plot <- data_ptds %>%
        ggplot(aes(x = ptds)) +
        geom_histogram(binwidth = 0.5, fill = "#2a6ebb") +
        labs(
            y = "Count",
            x = "\nPTDS-5"
        ) +
        scale_x_continuous(
            breaks = seq(0, 10, by = 1)
        ) +
      theme_minimal()+
        theme(
            #panel.background = element_blank(),
            legend.position = "none",
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(size=9)
        )
}
