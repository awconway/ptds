#' @title
#' @export

create_instruction_plot <- function(coded_descriptions,
                                    data_ptds) {
  coded_descriptions <- coded_descriptions %>%
    mutate(fasting_type = case_when(
      npo_midnight == 1 ~ 1,
      npo_morning == 1 ~ 2,
      npo_hrsprior == 1 ~ 3,
      npo_dinner == 1 ~ 1,
      separate_fooddrink == 1 ~ 4,
      unclear_patient == 1 ~ 6,
      missing == 1 ~ 7
    )) %>%
    mutate(fasting_type_text = case_when(
      fasting_type %in% 1 ~ "\"No food or drink after midnight\"",
      fasting_type == 2 ~ "\"No food or drink after __ AM\"",
      fasting_type == 3 ~ "\"No food or fluids __ hours prior\"",
      fasting_type == 4 ~ "\"No food __ hours prior and no\ndrink after __ AM this morning\"",
      fasting_type == 6 ~ "Unclear instructions",
      fasting_type == 7 ~ "Missing"
    ))

  # Join coded data to original data
  coded_descriptions$id <- as.character(coded_descriptions$id)
  coded_data <- data_ptds %>%
    inner_join(coded_descriptions %>%
      select(id, fasting_type, fasting_type_text), by = c("id" = "id"))

  # Compute food and fluids fasting deviations
  coded_data <- coded_data %>% mutate(
    food_dur = as.double(date_time - food),
    fluids_dur = as.double(date_time - fluids)
  )

  # Filter out screening arm and select only relevant columns
  coded_data <- coded_data %>%
    #   filter(redcap_event_name == "t3_arm_1") %>%
    select(id, fasting_type, fasting_type_text, food_dur, fluids_dur, procedure, procedure_factor)

  coded_data %>%
    group_by(procedure_factor) %>%
    mutate(department = ifelse(procedure < 5, "Cardiac Cath Lab", no = "Interventional Radiology")) %>%
    group_by(department) %>%
    count(fasting_type_text) %>%
    drop_na() %>%
    filter(n > 1) %>%
    ggplot2::ggplot(aes(
      y = fasting_type_text,
      x = n,
      fill = fasting_type_text
    )) +
    geom_col() +
    facet_wrap(~department, nrow = 2) +
    theme(
      panel.background = element_blank(),
      legend.position = "none",
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(
      y = element_blank(),
      x = "Number of participants"
    ) +
    theme(
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 12,
        color = "#ffffff", fill = "#2a6ebb", box.color = "#4A618C",
        halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
      )
    )
}