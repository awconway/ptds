##' @title Label data
##' @param data dataframe from csv downloaded from redcap
##' @return
##' @author Aaron Conway
##' @export

label_data <- function(data) {

  # Setting Units


  # Setting Factors(will create new variable for factors)
  data$redcap_event_name_factor <- factor(data$redcap_event_name,
    levels = c("t3_arm_1",
    "t1_arm_2",
    "t2_arm_2",
    "t3_arm_2", 
    "screening_arm_3")
  )

  data$sex_factor <- factor(data$sex, levels = c("1", "2", "3"))
  data$procedure_factor <- factor(data$procedure,
    levels = c("1", "2", "3", "4", "5", 
    "6", "7", "8", "9", "10", "11", "12", "13")
  )
  data$instructions_factor <- factor(data$instructions,
    levels = c("1", "2", "3", "4", "5", "6")
  )
  data$ptds_dry_mouth_factor <- factor(data$ptds_dry_mouth,
    levels = c("0", "1", "2")
  )
  data$ptds_dry_lips_factor <- factor(data$ptds_dry_lips,
    levels = c("0", "1", "2")
  )
  data$ptds_thick_tongue_factor <- factor(data$ptds_thick_tongue,
    levels = c("0", "1", "2")
  )
  data$ptds_thick_saliva_factor <- factor(data$ptds_thick_saliva,
    levels = c("0", "1", "2")
  )
  data$ptds_dry_throat_factor <- factor(data$ptds_dry_throat,
    levels = c("0", "1", "2")
  )
  data$ptds_bad_taste_factor <- factor(data$ptds_bad_taste,
    levels = c("0", "1", "2")
  )
  data$ptds_water_factor <- factor(data$ptds_water,
    levels = c("0", "1", "2")
  )
  data$global_thirst_factor <- factor(data$global_thirst,
    levels = c("1", "2", "3", "4", "5", "6")
  )
  data$thirst_intensity_factor <- factor(data$thirst_intensity,
    levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  )
  data$pain_factor <- factor(data$pain,
    levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  )
  data$o2_therapy_factor <- factor(data$o2_therapy,
    levels = c("1", "0")
  )
  data$screenprocedure_factor <- factor(data$screenprocedure,
    levels = c("1", "2")
  )
  data$screenage_factor <- factor(data$screenage,
    levels = c("1", "2")
  )
  data$screenemergency_factor <- factor(data$screenemergency,
    levels = c("1", "2")
  )
  data$screenenglish_factor <- factor(data$screenenglish,
    levels = c("1", "2")
  )
  data$screensufficient_factor <- factor(data$screensufficient,
    levels = c("1", "2")
  )
  data$screen_eligible_factor <- factor(data$screen_eligible,
    levels = c("1", "2")
  )
  data$screeninvited_factor <- factor(data$screeninvited,
    levels = c("1", "2")
  )
  data$screenconsented_factor <- factor(data$screenconsented,
    levels = c("1", "2")
  )

  levels(data$redcap_event_name_factor) <- c(
    "T3 (Arm 1: Validity)",
    "T1 (Arm 2: Reliability and responsiveness)",
    "T2 (Arm 2: Reliability and responsiveness)",
    "T3 (Arm 2: Reliability and responsiveness)",
    "Screening (Arm 3: Screening)"
  )
  levels(data$sex_factor) <- c("Male", "Female", "Prefer not to say")
  levels(data$procedure_factor) <- c(
    "Angiogram or Percutaneous Coronary Intervention (PCI)",
    "Cardiac Implantable Electronic Device (CIED)",
    "Electrophysiology Study (EPS)",
    "Structural heart intervention",
    "Vascular angiography or intervention",
    "Biopsy",
    "Port-a-cath",
    "Gall bladder stone removal",
    "Endovascular aneurysm repair (EVAR)",
    "Radiofrequency ablation of a tumour (RFA)",
    "Fistula repair/de-clotting",
    "Vascular embolization (e.g., renal)",
    "Other procedure"
  )
  levels(data$instructions_factor) <- c(
    "Extremely easy",
    "Very Easy",
    "Easy",
    "Difficult",
    "Very Difficult",
    "Extremely difficult"
  )

  levels(data$ptds_dry_mouth_factor) <- c(
    "Not bothered",
    "Slightly bothered",
    "Very bothered"
  )
  levels(data$ptds_dry_lips_factor) <- c(
    "Not bothered",
    "Slightly bothered",
    "Very bothered"
  )
  levels(data$ptds_thick_tongue_factor) <- c(
    "Not bothered",
    "Slightly bothered",
    "Very bothered"
  )
  levels(data$ptds_thick_saliva_factor) <- c(
    "Not bothered",
    "Slightly bothered",
    "Very bothered"
  )
  levels(data$ptds_dry_throat_factor) <- c(
    "Not bothered",
    "Slightly bothered", "Very bothered"
  )
  levels(data$ptds_bad_taste_factor) <- c(
    "Not bothered",
    "Slightly bothered",
    "Very bothered"
  )
  levels(data$ptds_water_factor) <- c(
    "Not bothered",
    "Slightly bothered", "Very bothered"
  )
  levels(data$global_thirst_factor) <- c(
    "Extremely uncomfortable",
    "Very uncomfortable",
    "Mildly uncomfortable",
    "Comfortable",
    "Very comfortable",
    "Extremely comfortable"
  )
  levels(data$thirst_intensity_factor) <- c(
    "0", "1", "2", "3", "4",
    "5", "6", "7", "8", "9", "10"
  )
  levels(data$pain_factor) <- c(
    "0- No pain",
    "1- Very mild",
    "2- Discomforting",
    "3- Tolerable",
    "4- Distressing",
    "5- Very distressing",
    "6- Intense",
    "7- Very intense",
    "8- Utterly horrible",
    "9- Excrutiating/ unbearable",
    "10- Unimaginable/ unspeakable"
  )
  levels(data$o2_therapy_factor) <- c("Yes", "No")

  levels(data$screenprocedure_factor) <- c(
    "Cardiac Cath Lab",
    "Interventional Radiology"
  )
  levels(data$screenage_factor) <- c("Yes", "No")
  levels(data$screenemergency_factor) <- c("Yes", "No")
  levels(data$screenenglish_factor) <- c("Yes", "No")
  levels(data$screensufficient_factor) <- c("Yes", "No")
  levels(data$screen_eligible_factor) <- c("Yes", "No")
  levels(data$screeninvited_factor) <- c("Yes", "No")
  levels(data$screenconsented_factor) <- c("Yes", "No")

  attr(data$id, "label") <- "Record ID"
  attr(data$redcap_event_name, "label") <- "Event Name"
  attr(data$date_time, "label") <- "Date/time"
  attr(data$age, "label") <- "Age"
  attr(data$sex_factor, "label") <- "Sex"
  attr(data$procedure_factor, "label") <- "Type of procedure"
  attr(data$other_procedure, "label") <- "Specify other type of procedure"
  attr(data$food, "label") <- "Last food"
  attr(data$fluids, "label") <- "Last clear fluids"
  attr(data$instructions_factor, "label") <- "How easy was it for you to understand the instructions you received about fasting before your procedure ?"
  attr(data$fasting_description, "label") <- "Can you please briefly describe what instructions you received about when to stop eating and drinking before your procedure?"
  attr(data$ptds_dry_mouth_factor, "label") <- "My mouth is dry"
  attr(data$ptds_dry_lips_factor, "label") <- "My lips are dry"
  attr(data$ptds_thick_tongue_factor, "label") <- "My tongue is thick"
  attr(data$ptds_thick_saliva_factor, "label") <- "My saliva is thick"
  attr(data$ptds_dry_throat_factor, "label") <- "My throat is dry"
  attr(data$ptds_bad_taste_factor, "label") <- "I have a bad taste in my mouth"
  attr(data$ptds_water_factor, "label") <- "I want to drink water"
  attr(data$global_thirst_factor, "label") <- "Please rate your current level of thirst discomfort"
  attr(data$thirst_intensity_factor, "label") <- "Please rate the intensity of your thirst on a scale of 0 to 10, where 0 is no thirst and 10 is the most intense thirst you could imagine."
  attr(data$pain_factor, "label") <- "Please rate your pain on a scale of 0-10"
  attr(data$o2_therapy_factor, "label") <- "Are you on oxygen therapy? "
  attr(data$screendate, "label") <- "Date of screening"
  attr(data$screenprocedure, "label") <- "Procedure location"
  attr(data$screenage, "label") <- "Under 16 years of age"
  attr(data$screenemergency, "label") <- "Undergoing an emergency procedure"
  attr(data$screenenglish, "label") <- "Not able to understand or speak English"
  attr(data$screensufficient, "label") <- "Insufficient time prior to procedure"
  attr(data$screen_eligible, "label") <- "Eligible?"
  attr(data$screeninvited, "label") <- "Invitation to participate provided?"
  attr(data$screenconsented, "label") <- "Provided consent?"
  attr(data$studyid, "label") <- "Study ID"

  return(data)
}
