##' @title

##' @return
##' @author Aaron Conway
##' @importFrom dplyr as_tibble select everything mutate

##' @export
create_monotonicity_df_ptds6_table <- function(monotonicity_df_ptds6) {
  monotonicity_df_ptds6 %>%
    as_tibble() %>%
    mutate(item = c(
      "My mouth is dry",
      "My lips are dry",
      "My tongue is thick",
      "My saliva is thick",
      "My throat is dry",
      "I want to drink water"
    )) %>%
    select(item, everything())
}
