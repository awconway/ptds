#' @title
#' @return
#' @export
#' @importFrom ggplot2 ggplot aes theme_minimal scale_x_continuous
#' labs element_blank theme ggsave geom_histogram
create_distribution_plot <- function(data_ptds) {
    data_ptds %>%
        ggplot(aes(x = ptds)) +
        geom_histogram(binwidth = 0.5) +
        theme_minimal() +
        labs(
            y = "Count",
            x = "\nPTDS-5 (range is 5 to 15; higher scores indicate worse thirst discomfort)"
        ) +
        scale_x_continuous(
            breaks = c(seq(5, 15, by = 1))
        )

    ggsave(
        device = "png", filename = here("manuscript/figures/distribution.png"),
        width = 174, units = "mm"
    )
}