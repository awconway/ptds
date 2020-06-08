##' @title
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom dplyr as_tibble select everything mutate

create_iioptds6_table <- function(iio_ptds6) {
  iio_ptds6$item.summary %>%
    as_tibble() %>%
    mutate(item = c("I want to drink water",
                    "My mouth is dry",
                    "My lips are dry",
                    "My throat is dry",
                    "My saliva is thick",
                    "My tongue is thick")) %>%
    select(item, everything())

}
