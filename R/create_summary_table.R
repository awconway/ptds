##' @title
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom dplyr select
#' @importFrom gtsummary tbl_summary italicize_labels as_flextable
create_summary_table <- function(data_ptds) {
  data_ptds %>%
    select(age, sex_factor, procedure_factor, fluids_duration, food_duration) %>%
    droplevels() %>%
    tbl_summary(
      missing = "no",
      label = list(
        sex_factor ~ "Gender",
        procedure_factor ~ "Procedure",
        fluids_duration ~ "Time since last clear fluids (hours)",
        food_duration ~ "Time since last food (hours)"
      )
    )
}
