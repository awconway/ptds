#' @title drake plan
#' @rdname get_analysis_plan
#' @description Targets and functions for analyses
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr select
#' @importFrom here here
#' @importFrom drake drake_plan knitr_in
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
    global_thirst_plot = create_global_thirst_plot(data_ptds),
    thirst_intensity_plot = create_thirst_intensity_plot(data_ptds),
    fasting_duration_ptds_plot = create_fasting_duration_ptds_plot(data_ptds),
    age_plot = create_age_ptds_plot(data_ptds),
    DT = create_DT(data_ptds),

    manuscript = target(
      command = {
        rmarkdown::render(knitr_in("manuscript/index.Rmd"))
        file_out("manuscript/index.docx")
      }
    ),
  )
}
