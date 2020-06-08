##' @title

##' @return
##' @author Aaron Conway
##' @export
#' @importFrom dplyr select starts_with ends_with
create_DT <- function(data) {
  data %>%
    select(
      -starts_with("redcap"), -ends_with("complete"), -thirst_change,
      -starts_with("screen"),
      -ends_with("complete_factor"),
      -studyid, -`1`
    )
}
