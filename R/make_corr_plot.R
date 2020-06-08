
##' @title Make a correlation plot
##' @param ptds7 data frame
##' @return
##' @author Aaron Conway
##' @importFrom ggcorrplot ggcorrplot
##' @export
make_corr_plot <- function(ptds7) {
  ggcorrplot(cor(ptds7),
    colors = c("#E46726", "white", "#6D9EC1")
  )
}
