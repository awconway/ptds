#' @title Correlation ptds and pain
#' @return
#' @author Aaron Conway
#' @export
#' @importFrom correlation correlation
#' @importFrom dplyr mutate select

create_pain_corr <- function(data_ptds) {
    data_ptds %>%
        select(ptds, pain) %>%
        correlation(method = "spearman")
}