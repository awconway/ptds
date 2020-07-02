#' @title Correlation ptds and global thirst
#' @param data_ptds dataframe
#' @return
#' @author Aaron Conway
#' @export
#' @importFrom correlation correlation
#' @importFrom forcats fct_rev
#' @importFrom dplyr mutate select

create_intensity_corr <- function(data_ptds) {
  data_ptds %>%
    select(ptds, thirst_intensity) %>%
    correlation(method = "kendall")
}