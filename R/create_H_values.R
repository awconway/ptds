##' @title
##' @param data dataframe
##' @return
##' @author Aaron Conway
##' @importFrom mokken coefH

##' @export
create_H_values <- function(data = ptds6) {
  coefH(as.matrix(data))
}
