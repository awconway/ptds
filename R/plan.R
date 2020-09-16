#' @title drake plan
#' @rdname get_analysis_plan
#' @description Targets and functions for analyses
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr select
#' @importFrom here here
#' @importFrom drake drake_plan knitr_in
#' @importFrom ggplot2 ggsave
#'

get_analysis_plan <- function() {
  drake_plan(
    original_data = read_csv(here("data/ThirstDiscomfortScal_DATA_2020-03-24_2016.csv")),
    # Label original data
    data_labelled = label_data(data = original_data),
    participation_df = preprocess_df(data_labelled),
    ptds7 = mokken_variables(participation_df),
    corr_plot = make_corr_plot(ptds7),
    aisp = moscales_lowerbounds(x = ptds7),
    ptds6 = ptds7 %>%
      select(-bad_taste),
    H_values_ptds7 = create_H_values(data = ptds7),
    H_values_ptds7_table = create_H_values_ptds7_table(H_values_ptds7),
    H_values_ptds6 = create_H_values(data = ptds6),
    monotonicity_df_ptds6 = create_monotonicity_df(data = ptds6),
    monotonicity_df_ptds6_table = create_monotonicity_df_ptds6_table(monotonicity_df_ptds6),
    iio_ptds6 = create_iio(data = ptds6),
    iio_ptds6_table = create_iioptds6_table(iio_ptds6),
    ptds5 = ptds6 %>%
      select(-dry_throat),
    iio_ptds5 = create_iio(data = ptds5),
    iio_ptds5_table = create_iioptds5_table(iio_ptds5),
    accuracy_ptds5 = round(iio_ptds5$HT, 3),
    reliability_ptds5 = create_reliability(ptds5),
    H_values_ptds5 = create_H_values(data = ptds5),
    item_hierarchy_df_ptds5 = create_item_hierarchy_df(
      data = ptds5,
      Hvalues = H_values_ptds5
    ),
    data_ptds = add_ptds_total(ptds5, participation_df), # data with total ptds score added
    summary_table = create_summary_table(data_ptds),
    global_thirst_plot = create_global_thirst_plot(
      data = data_ptds
      # global_thirst_corr = global_thirst_corr
    ),
    global_thirst_plot_png = ggsave(
      plot = global_thirst_plot,
      device = "png", filename = here("manuscript/figures/global-thirst.png"),
      width = 174, units = "mm"
    ),
    global_thirst_plot_pdf = ggsave(
      plot = global_thirst_plot,
      device = "pdf", filename = here("manuscript/figures/global-thirst.pdf"),
      width = 174, units = "mm"
    ),
    thirst_intensity_plot = create_intensity_plot(
      data = data_ptds,
      intensity_corr = intensity_corr
    ),

    thirst_intensity_plot_png = ggsave(
      plot = thirst_intensity_plot,
      device = "png",
      filename = here("manuscript/figures/thirst-intensity.png"),
      width = 174, units = "mm"
    ),

    fasting_duration_ptds_plot = create_fasting_duration_ptds_plot(
      data_ptds
      # fluids_corr, food_corr
    ),

    fasting_duration_ptds_plot_png = ggsave(
      plot = fasting_duration_ptds_plot,
      device = "png",
      filename = here("manuscript/figures/fasting.png"),
      width = 174, units = "mm"
    ),

    age_plot = create_age_ptds_plot(data_ptds),
    DT = create_DT(data_ptds),
    distribution_plot = create_distribution_plot(data_ptds),
    distribution_plot_png = ggsave(
      plot = distribution_plot,
      device = "png",
      filename = here("manuscript/figures/distribution.png"),
      width = 174,
      units = "mm"
    ),
    distribution_plot_pdf = ggsave(
      plot = distribution_plot,
      device = "pdf",
      filename = here("manuscript/figures/distribution.pdf"),
      width = 174,
      units = "mm"
    ),

    # correlations

    global_thirst_corr = create_global_thirst_corr(data_ptds),
    intensity_corr = create_intensity_corr(data_ptds),
    fluids_corr = create_fluids_corr(data_ptds),
    food_corr = create_food_corr(data_ptds),
    pain_corr = create_pain_corr(data_ptds),

    # deviations

    #coded_descriptions = read.csv(
    #  here::here("Analysis/fasting_descriptions_complete.csv")
    #),

    #deviation_plot = create_deviation_plot(
    #  data_ptds = data_ptds,
    #  coded_descriptions = coded_descriptions
    #),
    #deviation_plot_png = ggsave(
    #  plot = deviation_plot,
    #  filename = here("plots", "fasting-deviation-plot.png"),
    #  height = 15,
    #  width = 12,
    #  units = "in",
    #  device = "png"
    #),
    #deviation_plot_svg = ggsave(
    #  plot = deviation_plot,
    #  filename = here("plots", "fasting-deviation-plot.svg"),
    #  height = 15,
    #  width = 12,
    #  units = "in",
    #  device = "svg"
    #),
    #
    ## instructions
    #
    #clarity_plot = create_clarity_plot(data_ptds),
    #clarity_plot_png = ggsave(
    #  plot = clarity_plot,
    #  filename = here("plots", "clarity-plot.svg"),
    #  device = "svg"
    #),
    #instructions_plot = create_instruction_plot(
    #  data_ptds = data_ptds,
    #  coded_descriptions = coded_descriptions
    #),
    #instructions_plot_svg = ggsave(
    #  plot = instructions_plot,
    #  filename = here("plots", "instructions_plot.svg"),
    #  device = "svg"
    #),

    # manuscript = target(
    #   command = {
    #     rmarkdown::render(knitr_in("manuscript/index.Rmd"))
    #     file_out("manuscript/index.docx")
    #   }
    # ),
    manuscript_pdf = target(
      command = {
        rmarkdown::render(knitr_in("manuscript/manuscript.Rmd"))
        file_out("manuscript/manuscript.pdf")
      }
    )
  )
}
