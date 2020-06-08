##' @title
##' @return
##' @author Aaron Conway
##' @export

#' @importFrom dplyr summarise across everything mutate
#' tribble arrange case_when
#' @importFrom stringr str_remove_all
#' @importFrom tidyr pivot_longer
create_item_hierarchy_df <- function(data, Hvalues) {
  Hi <- Hvalues$Hi
  scalability <- data %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "item", values_to = "mean"
    ) %>%
    mutate(mean = round(mean, 2))

  Hi_df <- tribble(
    ~Hi, ~se,
    Hi[1], Hi[6],
    Hi[2], Hi[7],
    Hi[3], Hi[8],
    Hi[4], Hi[9],
    Hi[5], Hi[10]
  )

  scalability <- scalability %>% cbind(Hi_df)

  scalability <- scalability %>%
    mutate(se = str_remove_all(se, "\\(")) %>%
    mutate(se = str_remove_all(se, "\\)")) %>%
    mutate(se = as.numeric(se)) %>%
    mutate(Hi = round(as.numeric(Hi), 2)) %>%
    arrange(-mean)

  scalability  %>%
    mutate(item = case_when(item == "water" ~ "I want to drink water",
                            item == "dry_mouth" ~ "My mouth is dry",
                            item == "dry_lips" ~ "My lips are dry",
                            item == "thick_saliva" ~ "My saliva is thick",
                            item == "thick_tongue" ~ "My tongue is thick"))
}
