##' @title
##' @param data dataframe
##' @importFrom mokken check.monotonicity
##' @author Aaron Conway
##' @export
create_monotonicity_df <- function(data) {
  summary(check.monotonicity(as.matrix(data)))
}
