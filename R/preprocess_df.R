##' @title Preprocessing
##' @param data_labelled labelled data from redcap
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom dplyr filter starts_with mutate select
#' ends_with rename_all mutate_if
#' @importFrom stringr str_detect str_remove_all
#' @importFrom tidyr drop_na
preprocess_df <- function(data_labelled) {
  rel1 <- data_labelled %>%
    filter(str_detect(id, "R")) %>% # remove screening data
    filter(
      redcap_event_name != "t2_arm_2",
      redcap_event_name != "t3_arm_2"
    ) # get participant data for this arm
  participation_df <- data_labelled %>%
    filter(str_detect(id, "R") | str_detect(id, "V")) %>% # remove screening data
    filter(redcap_event_name != "t1_arm_2") %>% # remove additional timepoints for test-retest (only n=5)
    filter(redcap_event_name != "t2_arm_2") %>%
    coalesce_join(rel1, by = "id") %>% # join participant data from t1 for reliability arm
    drop_na(starts_with("ptds")) %>% # remove one participant who missed one item
    mutate(fluids_duration = round(
      as.numeric(
        difftime(date_time, fluids, units = "hours")
      )
    ), 1) %>% # time since last fluids
    mutate(food_duration = round(
      as.numeric(
        difftime(date_time, food, units = "hours")
      )
    ), 1)
}
