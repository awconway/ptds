#' @title Correlation ptds and fluids fasting
#' @return
#' @author Aaron Conway
#' @export
#' @importFrom correlation correlation
#' @importFrom dplyr mutate select

create_fluids_corr <- function(data_ptds) {
  data_ptds %>%
    select(ptds, fluids_duration) %>%
    correlation(method = "spearman")
}