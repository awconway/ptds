library(drake)
library(tidyverse)
library(here)
library(mokken)
library(gtsummary)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

source("R/functions.R")

plan <- drake_plan(
  original_data = readr::read_csv(here("data/ThirstDiscomfortScal_DATA_2020-03-24_2016.csv")),
  data_labelled = label_data(original_data), # Label original data
  rel1 = data_labelled %>%
    filter(str_detect(id, 'R')) %>% #remove screening data
    filter(redcap_event_name!="t2_arm_2", redcap_event_name!="t3_arm_2"),# get participant data for this arm
  participation_df = data_labelled %>%
    filter(str_detect(id, 'R')|str_detect(id, 'V')) %>% #remove screening data
    filter(redcap_event_name!="t1_arm_2") %>% # remove additional timepoints for test-retest (only n=5)
    filter(redcap_event_name!="t2_arm_2") %>%
    coalesce_join(rel1, by = "id") %>% # join participant data from t1 for reliability arm
    drop_na(starts_with("ptds")) %>% # remove one participant who missed one item
    mutate(fluids_duration = round(as.numeric(difftime(date_time,fluids, units = "hours"))),1) %>% #time since last fluids
    mutate(food_duration = round(as.numeric(difftime(date_time,food, units = "hours"))),1),
  #mokken scaling
  ptds_factor = participation_df %>%
    select(starts_with("ptds")) %>%
    select(ends_with("factor"))  %>%
    rename_all(.funs = ~str_remove_all(., "ptds_")) %>%
    rename_all(.funs = ~str_remove_all(., "_factor")),
  ptds7  =  ptds_factor %>%
    mutate_if(is.factor, ~ as.numeric(.x)),
  corr_plot = ggcorrplot::ggcorrplot(cor(ptds7),
                                     colors = c( "#E46726", "white","#6D9EC1")),
  aisp = moscales.for.lowerbounds(x= as.data.frame( ptds7)),
  ptds6 = ptds7 %>%
    select(-bad_taste),
  H_values_ptds6 = create_H_values(data = ptds6),
  monotonicity_df_ptds6 = create_monotonicity_df( data = ptds6),
  iio_ptds6 = create_iio(data = ptds6),
  ptds5 = ptds6 %>%
    select(-dry_throat),
  iio_ptds5 = create_iio(data = ptds5),
  accuracy_ptds5 = round(summary(iio_ptds5)$HT, 3),
  reliability_ptds5 = round(check.reliability(as.matrix(ptds5))$MS, 2),
  H_values_ptds5 = create_H_values(data = ptds5),
  item_hierarchy_df_ptds5 = create_item_hierarchy_df(data = ptds5, Hvalues = H_values_ptds5),
  data_ptds = add_ptds_total(ptds5, participation_df), #data with total ptds score added
  summary_table = create_summary_table(data_ptds),
  global_thirst_plot = create_global_thirst_plot(data_ptds),
  thirst_intensity_plot = create_thirst_intensity_plot(data_ptds),
  fluids_duration_ptds_plot = create_fluids_duration_ptds_plot(data_ptds),
  DT = create_DT(data_ptds),
  mokken_report = callr::r(
    function(...) rmarkdown::render(...),
    args = list(
      input = knitr_in(here::here("Analysis/mokken.Rmd")),
      output_file = "mokken.html")
  )
)

make(plan)

vis_drake_graph(plan)
