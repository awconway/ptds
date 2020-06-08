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
create_fasting_duration_ptds_plot <- function(data) {

  ptds_fluids_cor <- data %>%
    drop_na(ptds, fluids_duration) %>%
    summarize(r_square = cor(ptds, fluids_duration)^2) %>%
    mutate(
      # location of each text label in data coordinates
      ptds = 14,
      fluids_duration = 20,
      # text label containing r^2 value
      label = glue("*r*<sup>2</sup> = {round(r_square, 2)}")
    )

  fluids <- data %>%
    ggplot(aes(y = ptds, x = fluids_duration)) +
    geom_jitter(alpha = 0.5,
                colour = "dodgerblue") +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(
      x = "\nDuration of fasting from <span style='color:dodgerblue'>fluids</span> in hours",
      y = "PTDS-5 (5-15; higher scores indicate worse thirst discomfort)\n"
    ) +
    theme(
      legend.position = "none",
      plot.title.position = "plot",
      axis.title.x = element_markdown()

    ) +
    ggtext::geom_richtext(
      data = ptds_fluids_cor,
      aes(label = label),
      hjust = 1, vjust = 1
    )

  ptds_food_cor <- data %>%
    drop_na(ptds, food_duration) %>%
    summarize(r_square = cor(ptds, food_duration)^2) %>%
    mutate(
      # location of each text label in data coordinates
      ptds = 14,
      food_duration = 25,
      # text label containing r^2 value
      label = glue("*r*<sup>2</sup> = {round(r_square, 2)}")
    )

 food <-  data %>%
   ggplot(aes(y = ptds, x = food_duration)) +
   geom_jitter(alpha = 0.5,
               colour = "orangered") +
   geom_smooth(method = "lm") +
   theme_minimal() +
   labs(
     x = "\nDuration of fasting from <span style='color:orangered'>food</span> in hours",
     y = "PTDS-5 (5-15; higher scores indicate worse thirst discomfort)\n"
   ) +
   theme(
     legend.position = "none",
     axis.title.x = element_markdown(),
     plot.title.position = "plot"
   ) +
   ggtext::geom_richtext(
     data = ptds_food_cor,
     aes(label = label),
     hjust = 1, vjust = 1
   )


 fluids + food


  ggsave(device = "png", filename = here("manuscript/figures/fasting.png"),
         width = 174, units = "mm")
}
