
##' @title
#' @importFrom mokken check.reliability

##' @return
##' @author Aaron Conway
##' @export
create_reliability <- function(data) {
  round(check.reliability(as.matrix(data))$MS, 2)
}
