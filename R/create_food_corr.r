#' @title Correlation ptds and food fasting
#' @return
#' @author Aaron Conway
#' @export
#' @importFrom correlation correlation
#' @importFrom dplyr mutate select

create_food_corr <- function(data_ptds) {
  data_ptds %>%
    select(ptds, food_duration) %>%
    correlation(method = "spearman")
}