#' @param data dataframe from csv downloaded from redcap

#' @export
label_data <- function(data){

#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name_factor = factor(data$redcap_event_name,levels=c("t3_arm_1","t1_arm_2","t2_arm_2","t3_arm_2","screening_arm_3"))
data$datetime_v1_complete_factor = factor(data$datetime_v1_complete,levels=c("0","1","2"))
data$sex_factor = factor(data$sex,levels=c("1","2","3"))
data$procedure_factor = factor(data$procedure,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13"))
data$instructions_factor = factor(data$instructions,levels=c("1","2","3","4","5","6"))
data$baseline_v1_complete_factor = factor(data$baseline_v1_complete,levels=c("0","1","2"))
data$ptds_dry_mouth_factor = factor(data$ptds_dry_mouth,levels=c("0","1","2"))
data$ptds_dry_lips_factor = factor(data$ptds_dry_lips,levels=c("0","1","2"))
data$ptds_thick_tongue_factor = factor(data$ptds_thick_tongue,levels=c("0","1","2"))
data$ptds_thick_saliva_factor = factor(data$ptds_thick_saliva,levels=c("0","1","2"))
data$ptds_dry_throat_factor = factor(data$ptds_dry_throat,levels=c("0","1","2"))
data$ptds_bad_taste_factor = factor(data$ptds_bad_taste,levels=c("0","1","2"))
data$ptds_water_factor = factor(data$ptds_water,levels=c("0","1","2"))
data$global_thirst_factor = factor(data$global_thirst,levels=c("1","2","3","4","5","6"))
data$thirst_intensity_factor = factor(data$thirst_intensity,levels=c("0","1","2","3","4","5","6","7","8","9","10"))
data$pain_factor = factor(data$pain,levels=c("0","1","2","3","4","5","6","7","8","9","10"))
data$o2_therapy_factor = factor(data$o2_therapy,levels=c("1","0"))
data$perioperative_thirst_discomfort_scale_v1_complete_factor = factor(data$perioperative_thirst_discomfort_scale_v1_complete,levels=c("0","1","2"))
data$thirst_change_factor = factor(data$thirst_change,levels=c("1","2","3","4","5"))
data$thirst_change_v1_complete_factor = factor(data$thirst_change_v1_complete,levels=c("0","1","2"))
data$screenprocedure_factor = factor(data$screenprocedure,levels=c("1","2"))
data$screenage_factor = factor(data$screenage,levels=c("1","2"))
data$screenemergency_factor = factor(data$screenemergency,levels=c("1","2"))
data$screenenglish_factor = factor(data$screenenglish,levels=c("1","2"))
data$screensufficient_factor = factor(data$screensufficient,levels=c("1","2"))
data$screen_eligible_factor = factor(data$screen_eligible,levels=c("1","2"))
data$screeninvited_factor = factor(data$screeninvited,levels=c("1","2"))
data$screenconsented_factor = factor(data$screenconsented,levels=c("1","2"))
data$screening_complete_factor = factor(data$screening_complete,levels=c("0","1","2"))

levels(data$redcap_event_name_factor)=c("T3 (Arm 1: Validity)","T1 (Arm 2: Reliability and responsiveness)","T2 (Arm 2: Reliability and responsiveness)","T3 (Arm 2: Reliability and responsiveness)","Screening (Arm 3: Screening)")
levels(data$datetime_v1_complete_factor)=c("Incomplete","Unverified","Complete")
levels(data$sex_factor)=c("Male","Female","Prefer not to say")
levels(data$procedure_factor)=c("Angiogram or Percutaneous Coronary Intervention (PCI)","Cardiac Implantable Electronic Device (CIED)","Electrophysiology Study (EPS)","Structural heart intervention","Vascular angiography or intervention","Biopsy","Port-a-cath","Gall bladder stone removal","Endovascular aneurysm repair (EVAR)","Radiofrequency ablation of a tumour (RFA)","Fistula repair/de-clotting","Vascular embolization (e.g., renal)","Other procedure")
levels(data$instructions_factor)=c("Extremely easy","Very Easy","Easy","Difficult","Very Difficult","Extremely difficult")
levels(data$baseline_v1_complete_factor)=c("Incomplete","Unverified","Complete")
levels(data$ptds_dry_mouth_factor)=c("Not bothered","Slightly bothered","Very bothered")
levels(data$ptds_dry_lips_factor)=c("Not bothered","Slightly bothered","Very bothered")
levels(data$ptds_thick_tongue_factor)=c("Not bothered","Slightly bothered","Very bothered")
levels(data$ptds_thick_saliva_factor)=c("Not bothered","Slightly bothered","Very bothered")
levels(data$ptds_dry_throat_factor)=c("Not bothered","Slightly bothered","Very bothered")
levels(data$ptds_bad_taste_factor)=c("Not bothered","Slightly bothered","Very bothered")
levels(data$ptds_water_factor)=c("Not bothered","Slightly bothered","Very bothered")
levels(data$global_thirst_factor)=c("Extremely uncomfortable","Very uncomfortable","Mildly uncomfortable","Comfortable","Very comfortable","Extremely comfortable")
levels(data$thirst_intensity_factor)=c("0","1","2","3","4","5","6","7","8","9","10")
levels(data$pain_factor)=c("0- No pain","1- Very mild","2- Discomforting","3- Tolerable","4- Distressing","5- Very distressing","6- Intense","7- Very intense","8- Utterly horrible","9- Excrutiating/ unbearable","10- Unimaginable/ unspeakable")
levels(data$o2_therapy_factor)=c("Yes","No")
levels(data$perioperative_thirst_discomfort_scale_v1_complete_factor)=c("Incomplete","Unverified","Complete")
levels(data$thirst_change_factor)=c("1- A lot worse","2- A little worse","3- Exactly the same","4- A little better","5- A lot better")
levels(data$thirst_change_v1_complete_factor)=c("Incomplete","Unverified","Complete")
levels(data$screenprocedure_factor)=c("Cardiac Cath Lab","Interventional Radiology")
levels(data$screenage_factor)=c("Yes","No")
levels(data$screenemergency_factor)=c("Yes","No")
levels(data$screenenglish_factor)=c("Yes","No")
levels(data$screensufficient_factor)=c("Yes","No")
levels(data$screen_eligible_factor)=c("Yes","No")
levels(data$screeninvited_factor)=c("Yes","No")
levels(data$screenconsented_factor)=c("Yes","No")
levels(data$screening_complete_factor)=c("Incomplete","Unverified","Complete")

Hmisc::label(data$id)="Record ID"
Hmisc::label(data$redcap_event_name)="Event Name"
Hmisc::label(data$date_time)="Date/time"
Hmisc::label(data$datetime_v1_complete)="Complete?"
Hmisc::label(data$age)="Age"
Hmisc::label(data$sex_factor)="Sex"
Hmisc::label(data$procedure_factor)="Type of procedure"
Hmisc::label(data$other_procedure)="Specify other type of procedure"
Hmisc::label(data$food)="Last food"
Hmisc::label(data$fluids)="Last clear fluids"
Hmisc::label(data$instructions_factor)="How easy was it for you to understand the instructions you received about fasting before your procedure ?"
Hmisc::label(data$fasting_description)="Can you please briefly describe what instructions you received about when to stop eating and drinking before your procedure?"
Hmisc::label(data$baseline_v1_complete)="Complete?"
Hmisc::label(data$ptds_dry_mouth_factor)="My mouth is dry"
Hmisc::label(data$ptds_dry_lips_factor)="My lips are dry"
Hmisc::label(data$ptds_thick_tongue_factor)="My tongue is thick"
Hmisc::label(data$ptds_thick_saliva_factor)="My saliva is thick"
Hmisc::label(data$ptds_dry_throat_factor)="My throat is dry"
Hmisc::label(data$ptds_bad_taste_factor)="I have a bad taste in my mouth"
Hmisc::label(data$ptds_water_factor)="I want to drink water"
Hmisc::label(data$global_thirst_factor)="Please rate your current level of thirst discomfort"
Hmisc::label(data$thirst_intensity_factor)="Please rate the intensity of your thirst on a scale of 0 to 10, where 0 is no thirst and 10 is the most intense thirst you could imagine."
Hmisc::label(data$pain_factor)="Please rate your pain on a scale of 0-10"
Hmisc::label(data$o2_therapy_factor)="Are you on oxygen therapy? "
Hmisc::label(data$perioperative_thirst_discomfort_scale_v1_complete)="Complete?"
Hmisc::label(data$thirst_change_factor)="How has your thirst-discomfort changed since you were last asked"
Hmisc::label(data$thirst_change_v1_complete)="Complete?"
Hmisc::label(data$screendate)="Date of screening"
Hmisc::label(data$screenprocedure)="Procedure location"
Hmisc::label(data$screenage)="Under 16 years of age"
Hmisc::label(data$screenemergency)="Undergoing an emergency procedure"
Hmisc::label(data$screenenglish)="Not able to understand or speak English"
Hmisc::label(data$screensufficient)="Insufficient time prior to procedure"
Hmisc::label(data$screen_eligible)="Eligible?"
Hmisc::label(data$screeninvited)="Invitation to participate provided?"
Hmisc::label(data$screenconsented)="Provided consent?"
Hmisc::label(data$studyid)="Study ID"
Hmisc::label(data$screening_complete)="Complete?"


data
}


#' @export
#'
create_item_hierarchy_df <- function(data, Hvalues){
  Hi <- Hvalues$Hi
  scalability <- data %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(cols = everything(),
                 names_to = "item", values_to = "mean") %>%
    mutate(mean = round(mean, 2))

  Hi_df <- tribble(~Hi, ~se,
                   Hi[1], Hi[6],
                   Hi[2], Hi[7],
                   Hi[3], Hi[8],
                   Hi[4], Hi[9],
                   Hi[5], Hi[10])

  scalability <- scalability %>% cbind(Hi_df)

  scalability <- scalability %>%
    mutate(se = str_remove_all(se, "\\(")) %>%
    mutate(se = str_remove_all(se, "\\)")) %>%
    mutate(se = as.numeric(se)) %>%
    mutate(Hi = round(as.numeric(Hi), 2)) %>%
    arrange(-mean)
}


add_ptds_total <- function(ptds_df, participation_df){

ptds_total <- ptds_df %>%
  rowwise() %>%
  summarise(ptds = sum(c_across(everything())))

participation_df %>% cbind(ptds_total)

}


create_summary_table <- function(participation_df){

participation_df %>%
  select(age, sex_factor, procedure_factor,fluids_duration, food_duration) %>%
  tbl_summary(missing = "no",
    label = list(sex_factor~"Gender",
                                      procedure_factor~"Procedure",
                                      fluids_duration~"Time since last clear fluids (hours)",
                                      food_duration~"Time since last food (hours)")) %>%
  italicize_labels()
}


create_global_thirst_plot <- function(data){

data %>%
  ggplot(aes(x = ptds, y = global_thirst_factor, colour = global_thirst_factor))+
  ggbeeswarm::geom_beeswarm(groupOnX=FALSE,alpha = 0.5, size = 2)+
  theme_minimal()+
  labs(y = element_blank(),
       x = "\n5-item peri-operative thirst discomfort scale (PTDS-5)",
       title = "Association between PTDS-5 and global thirst rating",
       subtitle = "PTDS-5 score ranges from 5 to 15 with higher scores indicating worse thirst discomfort")+
  theme(legend.position = "none",
        plot.title.position = "plot")

}

create_thirst_intensity_plot <- function(data){
data %>%
  ggplot(aes(y = ptds, x = thirst_intensity))+
  ggbeeswarm::geom_beeswarm(alpha = 0.5, size = 2, groupOnX = TRUE)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(x = "\nThirst intensity (0-10; higher scores indicate greater thirst intensity)",
       y = "5-item peri-operative thirst discomfort scale (PTDS-5)\n",
       title = "Association between PTDS-5 and thirst intensity",
       subtitle = "PTDS-5 score ranges from 5 to 15 with higher scores indicating worse thirst discomfort")+
  theme(legend.position = "none",
        plot.title.position = "plot")
}


create_fluids_duration_ptds_plot <- function(data){
data %>%
  ggplot(aes(y = ptds, x = fluids_duration))+
  geom_jitter(alpha = 0.5)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(x = "\nDuration of fasting from fluids in hours",
       y = "5-item peri-operative thirst discomfort scale (PTDS-5)\n",
       title = "Association between PTDS-5 and duration of fluid fasting",
       subtitle = "PTDS-5 score ranges from 5 to 15 with higher scores indicating worse thirst discomfort")+
  theme(legend.position = "none",
        plot.title.position = "plot")
}

moscales.for.lowerbounds <- function( x, lowerbounds=seq(from=0.05,to=0.80,by=0.05) )
{
  ret.value <- NULL;
  for( lowerbound in lowerbounds )
  {
    tmp <- aisp( x,  lowerbound=lowerbound );
    if( is.null(ret.value) )
    {
      ret.value <- data.frame( "Item"=rownames(tmp), "Scales."=tmp[,1] );
    }
    else
    {
      ret.value <- cbind( ret.value, "Scales."=tmp[,1] );
    }
    names(ret.value)[ncol(ret.value)] <- sprintf("%.2f",lowerbound);
  }
  rownames(ret.value) <- NULL;
  ret.value;
}


create_H_values <- function(data){
  coefH(as.matrix(data))
}

create_monotonicity_df <- function(data){
  summary(check.monotonicity(as.matrix(data)))
}

create_iio <- function(data){
  check.iio(as.matrix(data), item.selection = FALSE)
}


create_DT <- function(data){
  data %>%
    select(-starts_with("redcap"), -ends_with("complete"), -thirst_change,
           -starts_with("screen"),
           -ends_with("complete_factor"),
           -studyid, -`1`)
}

coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}
