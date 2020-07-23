##' @title
##' @return
##' @export
#' @importFrom dplyr select starts_with ends_with rename_all mutate_if
#' @importFrom stringr str_remove_all
mokken_variables <- function(participation_df) {

  # variables for mokken scaling
  participation_df %>%
    select(starts_with("ptds")) %>%
    select(!ends_with("factor")) %>%
    rename_all(.funs = ~ str_remove_all(., "ptds_")) %>%
    #rename_all(.funs = ~ str_remove_all(., "_factor")) %>%
    #mutate_if(is.factor, ~ as.numeric(.x)) %>%
    as.data.frame()
}
