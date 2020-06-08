##' @title
##' @param data dataframe
##' @return
##' @author Aaron Conway
##' @importFrom mokken check.iio

##' @export
create_iio <- function(data) {
  summary(check.iio(as.matrix(data), item.selection = FALSE))
}
