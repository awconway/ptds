
##' @title

##' @export
#' @importFrom dplyr rowwise c_across everything summarise
add_ptds_total <- function(ptds_df, original_df) {
  ptds_total <- ptds_df %>%
    rowwise() %>%
    summarise(ptds = sum(c_across(everything())))

  original_df %>% cbind(ptds_total)
}
