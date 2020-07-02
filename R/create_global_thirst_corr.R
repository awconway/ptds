#' @title Correlation ptds and global thirst
#' @param data_ptds dataframe
#' @return
#' @author Aaron Conway
#' @export
#' @importFrom correlation correlation
#' @importFrom forcats fct_rev
#' @importFrom dplyr mutate select

create_global_thirst_corr <- function(data_ptds) {
  data_ptds %>%
    mutate(global = as.numeric(fct_rev(global_thirst_factor))) %>%
    select(ptds, global) %>%
    correlation(method = "kendall")
}