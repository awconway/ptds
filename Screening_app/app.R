library(shiny)
library(shinydashboard)
library(redcapAPI)
library(tidyverse)
library(stringr)
library(lubridate)
library(flexdashboard)
library(ggpubr)
library(bizdays)
library(qwraps2)
library(purrr)
library(kableExtra)
library(Hmisc)
library(plotly)
library(waiter)





###########################  UI  ###############

ui <- 
  
  
  dashboardPage(
    dashboardHeader(title = "Thirst discomfort scale"),
    dashboardSidebar(
      sidebarMenu(
        br(),
        # menu item tabs
        menuItem("Recruitment", tabName="recruitment", icon=icon("search")),
        menuItem("Fasting duration", tabName="fasting-duration", icon=icon("exclamation-circle")),
        menuItem("Thirst discomfort", tabName="thirst-discomfort", icon=icon("chart-area"))
      )
       
    ),
    dashboardBody(
      
      use_waiter(include_js = FALSE), # do not include js
      
      tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
      
      tabItems(
        tabItem(tabName="recruitment",
          fluidRow(
              column(width = 4,
               flexdashboard::valueBoxOutput("sysdate"),
               
               
               flexdashboard::valueBoxOutput("Screened"),
               
               
               flexdashboard::valueBoxOutput( "Eligible"),
               
               
               flexdashboard::valueBoxOutput("Consented"),
               
               
               flexdashboard::valueBoxOutput("estdatecompletion")
               
              ),
              column(width = 4,
               box( width = NULL,
                    flexdashboard::gaugeOutput("gaugeEligibility"), height = 150, title = "Eligibility Rate", solidHeader = TRUE,
                    status = "primary"),
               
               box( width = NULL,
                    flexdashboard::gaugeOutput("gaugeRecruitment"),height = 150,title = "Recruitment Rate", solidHeader = TRUE,
                    status = "primary"),
               
               box(width = NULL,
                   flexdashboard::gaugeOutput("gaugeLeftToTarget"), height = 150,title = "Left to target in total", solidHeader = TRUE,
                   status = "primary"),
               
               box(width = NULL,
                   flexdashboard::gaugeOutput("gaugelefttotarget_rel"), height = 150,title = "Left to target for test-retest reliability", solidHeader = TRUE,
                   status = "primary"),
               
               box(width = NULL,
                   flexdashboard::gaugeOutput("gaugelefttotarget_val"), height = 150,title = "Left to target for validity", solidHeader = TRUE,
                   status = "primary")
               
               
                  ),
              column(width = 4, 
                tabBox(width = NULL,
                      title = "",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "tabset1", height = "700px",
                      tabPanel("Exclusions", plotOutput("exclusionsplot")),
                      tabPanel("Participants", tableOutput("tribkable")),
                      tabPanel("Milestones", flexdashboard::valueBoxOutput("starttrial"),
                               flexdashboard::valueBoxOutput("durationtrial")),
                      tabPanel("IR Recruitment", plotOutput("recruit_ir_plot")),
                      tabPanel("CCL Recruitment", plotOutput("recruit_cardiac_plot"))
                      
                      
                      
                  ),
               
               box(width = NULL, height = 125, align = "center",
                   tags$a(href="https://aconway.dev/rstudio/files/thirst-discomfort/Protocol/Protocol.html", "View the full protocol for more information about this study"),
                   br(),
                   br(),
                   tags$img(src='https://aconway.dev/images/UHN.jpg')
                   )
                )#end column 4 of recruitment page
              ) #end of fuid row for recruitment page
          ), #end of tabitem for recruitment page
        tabItem(tabName = "thirst-discomfort",
                fluidRow(align="center",
                         column(12,
                                h4("Relationship between perioperative thirst discomfort scale scores and fluid fasting duration"),
                                plotlyOutput("thirst_intensity_plot"),
                                br(),
                                h4("Relationship between global thirst discomfort and fluid fasting duration"),
                                br(),
                                plotlyOutput("global_thirst_plot"),
                                h4("Relationship between global thirst discomfort and perioperative thirst discomfort scale scores"),
                                br(),
                                plotlyOutput("global_ptds_plot")
                                )#end of column
                          )# end of fluid row
        
                ),#end of tabitem for thirst discomfort page
        tabItem(tabName = "fasting-duration",
                fluidRow(align="center",
                         column(12,
                                plotlyOutput("fluid_duration_plot"),
                                br(),
                                plotlyOutput("food_duration_plot")
                         )#end of column
                )# end of fluid row
                
        )#end of tabitem for fasting duration page
      )#end tabItems
      ,show_waiter_on_load(spin_fading_circles()) )# dashboad body
    
    # place at the bottom
    
  )#dashboard page

###########################################      
#Server

server <- function(input, output) {
  #############redcapAPI###############
  
  rcon <- redcapConnection(url="https://redcap.thetacollaborative.ca/api/", token="CABFAFF8E4259517B23F344818EE682F")
  
  
  myData <- exportRecords(rcon, labels = FALSE)
  
  #################Calculations for valueBoxes and gauges###############
  screen <- myData %>%
    filter(str_detect(id, 'S'))
  
  
  screen <- screen %>% 
    select(id, starts_with("screen"))
  
  
  screentotal <- nrow(screen)
  
  
  consented <- nrow(filter(screen, screenconsented=="Yes"))
  
  
  eligible <- nrow(filter(screen, screen_eligible=="Yes"))
  
  
  
  eligibilityRate <- round(eligible/(screentotal)*100, 0)
  
  
  recruitmentRate <- round((consented/eligible)*100, 0)
  
  ##################Partcipation###############
  
  val <- myData %>%
    filter(str_detect(id, 'V'))
  
  rel <- myData %>%
    filter(str_detect(id, 'R'))
  
  all <- myData %>% 
    filter(str_detect(id, 'R')|str_detect(id, 'V'))
  
  enrolled_val <- nrow(filter(all, redcap_event_name=="t3_arm_1"))
  
  enrolled_rel <- nrow(filter(all, redcap_event_name=="t1_arm_2"))
  
  enrolled <- nrow(filter(all, redcap_event_name=="t1_arm_2" | redcap_event_name=="t3_arm_1"))
  
  trialdates <- c(first(screen$screendate), max(screen$screendate, na.rm=TRUE))
  create.calendar(name = "mycal", weekdays = c('saturday', 'sunday'))
  daysoftrial <- bizdiff(trialdates, "mycal")
  
  lefttotarget_val <- 150-enrolled_val
  
  lefttotarget_rel <- 50-enrolled_rel
  
  lefttotarget_total <- 200-enrolled
  
  
  daystotarget <- lefttotarget_total/(enrolled/daysoftrial)
  
  
  estdatecompletion <- offset(Sys.Date(), round(daystotarget,0), "mycal")
  
  
  datacomplete <- all %>% 
    filter(baseline_v1_complete == "Complete") %>% 
    filter(perioperative_thirst_discomfort_scale_v1_complete == "Complete") %>% 
    filter(thirst_change_v1_complete == "Complete" )
  
  datacomplete <- nrow(datacomplete)
  
  
  #################################Milestones###############
  starttrial <- first(screen$screendate)
  
  endtrial  <- last(screen$screendate)
  
  durationtrial <- paste(as.character(round(difftime(as.POSIXct(Sys.Date()), first(screen$screendate), units = "weeks"),0)), "weeks")
  
  screenedperday_ir <- screen %>% 
    filter(screenprocedure=="Interventional Radiology") %>% 
    group_by(screendate) %>% 
    summarise(screensperday=n())  
  
  eligibleperday_ir <- screen %>% 
    filter(screenprocedure=="Interventional Radiology") %>% 
    group_by(screendate) %>% 
    filter(screen_eligible=="Yes") %>% 
    summarise(eligibleperday=n()) 
  
  recruitperday_ir <- screen %>% 
    filter(screenprocedure=="Interventional Radiology") %>% 
    group_by(screendate) %>% 
    filter(screenconsented=="Yes") %>% 
    summarise(recruitperday=n()) 
  
  
  recruitmentdf_ir <- screenedperday_ir %>% 
    right_join(eligibleperday_ir) %>% 
    right_join(recruitperday_ir) %>% 
    pivot_longer(cols = -screendate, names_to = "step", values_to = "total") %>%
    mutate(step = as.factor(step)) %>% 
    mutate(step = fct_recode(step, Screened = "screensperday")) %>% 
    mutate(step = fct_recode(step, Eligible = "eligibleperday")) %>% 
    mutate(step = fct_recode(step,  Recruited = "recruitperday")) 
  
  
  
  
  screenedperday_cardiac <- screen %>% 
    filter(screenprocedure=="Cardiac Cath Lab") %>% 
    group_by(screendate) %>% 
    summarise(screensperday=n())
  
  eligibleperday_cardiac <- screen %>% 
    filter(screenprocedure=="Cardiac Cath Lab") %>% 
    group_by(screendate) %>% 
    filter(screen_eligible=="Yes") %>% 
    summarise(eligibleperday=n()) 
  
  recruitperday_cardiac <- screen %>% 
    filter(screenprocedure=="Cardiac Cath Lab") %>% 
    group_by(screendate) %>% 
    filter(screenconsented=="Yes") %>% 
    summarise(recruitperday=n()) 
  
  recruitmentdf_cardiac <- screenedperday_cardiac %>% 
    right_join(eligibleperday_cardiac) %>% 
    right_join(recruitperday_cardiac) %>% 
    pivot_longer(cols = -screendate, names_to = "step", values_to = "total") %>%
    mutate(step = as.factor(step)) %>% 
    mutate(step = fct_recode(step, Screened = "screensperday")) %>% 
    mutate(step = fct_recode(step, Eligible = "eligibleperday")) %>% 
    mutate(step = fct_recode(step,  Recruited = "recruitperday")) 
  
  
  
  #########################Exclusions plot###############
  
  exclusions_emergency <- screen %>% 
    filter(screenemergency == "Yes") %>% 
    summarise(total = n())
  
  exclusions_age <- screen %>% 
    filter(screenage == "Yes") %>% 
    summarise(total = n())
  
  exclusions_sufficient <- screen %>% 
    filter(screensufficient == "Yes") %>% 
    summarise(total = n())
  
  exclusions_english <- screen %>% 
    filter(screenenglish == "Yes") %>% 
    summarise(total = n())
  
  exclusions <- rbind(exclusions_age, exclusions_emergency, exclusions_english, exclusions_sufficient)
  
  exclusionnames <- c("screenage", "screenemergency", "screenenglish", "screensufficient")
  
  exclusions <- cbind(exclusionnames, exclusions)
  
  
  exclusionnames2 <- c("Aged below 16 years", "Undergoing an emergency procedure", "Unable to understand or speak English",
                       "Insufficient time prior to procedure")
  
  exclusionsdf <- exclusions %>% 
    mutate(exclusionnames= exclusionnames2)
  
  
  
  ################ Fasting duration ###############################
  
  
  # fasting_duration_food <- glue::glue(as.numeric(round(mean(difftime(all$date_time,all$food, units = "hours"), na.rm = TRUE), 2)), " hours")
  # 
  # fasting_duration_fluids <- glue::glue(as.numeric(round(mean(difftime(all$date_time,all$fluids, units = "hours"), na.rm = TRUE), 2)), " hours")
  # 
  #                  
  
  #################PTDS###############
  ptds <- all %>% 
    select(starts_with("ptds"))
  str(all$ptds)
  
  key <- list(ptds=c(names(ptds)
  ))
  my.scales <- psych::scoreItems(key, all)
  all <- all %>% 
    mutate(ptds= my.scales$scores)
  
  #############Outputs#################
  
  hide_waiter()
  
  
  output$sysdate <- flexdashboard::renderValueBox({
    shinydashboard::valueBox(
      value = Sys.Date(),
      subtitle = "As at",
      color = "light-blue",
      width = 12
    )
  })
  
  output$Screened <- flexdashboard::renderValueBox({
    shinydashboard::valueBox(
      value = Sys.Date(),
      subtitle = "Screened",
      icon = icon("list"),
      color = "light-blue",
      width = 12
    )
  })
  
  output$Screened <- flexdashboard::renderValueBox({
    shinydashboard::valueBox(
      value = screentotal,
      subtitle = "Screened",
      icon = icon("list"),
      color = "light-blue",
      width = 12
    )
  })
  
  output$Eligible <- flexdashboard::renderValueBox({
    shinydashboard::valueBox(
      value = eligible,
      subtitle = "Eligible",
      icon = icon("check"),
      color = "light-blue",
      width = 12
    )
  }) 
  
  output$Consented <- flexdashboard::renderValueBox({
    shinydashboard::valueBox(
      value = consented,
      subtitle = "Consented",
      icon = icon("check-double"),
      color = "light-blue",
      width = 12
    )
  }) 
  
  output$estdatecompletion <- flexdashboard::renderValueBox({
    shinydashboard::valueBox(
      value = estdatecompletion,
      subtitle = "Estimated completion date",
      icon = icon("calendar"),
      color = "light-blue",
      width = 12
    )
  }) 
  
  
  
  
  output$gaugeEligibility <- flexdashboard::renderGauge({
    gauge(eligibilityRate, min=0, max = 100, symbol = '%',  gaugeSectors(
      success = c(90,100), warning = c(50,89), danger = c(0,49)
    ))
  })
  
  
  output$gaugeRecruitment <- flexdashboard::renderGauge({
    gauge(recruitmentRate, min=0, max = 100, symbol = '%', gaugeSectors(
      success = c(90,100), warning = c(50,89), danger = c(0,49)
    ))
  })
  
  output$gaugeLeftToTarget <- flexdashboard::renderGauge({
    gauge(lefttotarget_total, min=0, max = 200,  gaugeSectors(
      success = c(0,0), danger = c(1,150)
    ))
  })
  
  output$gaugelefttotarget_rel <- flexdashboard::renderGauge({
    gauge(lefttotarget_rel, min=0, max = 50,  gaugeSectors(
      success = c(150,150), danger = c(0,49)
    ))
  })  
    
    output$gaugelefttotarget_val <- flexdashboard::renderGauge({
      gauge(lefttotarget_val, min=0, max = 150,  gaugeSectors(
        success = c(150,150), danger = c(0,149)
      ))
  })
  
  #Tab pabel in third column
  
  output$exclusionsplot <- renderPlot({
   ggdotchart(exclusionsdf, x = "exclusionnames", y = "total",
                                 color = "steelblue",
                                 sorting = "descending",
                                 add = "segments",
                                 rotate = TRUE,
                                 dot.size = 6,
                                 label = exclusionsdf$total,
                                 font.label = list(color = "white", size = 9, 
                                                   vjust = 0.5),
                                 ggtheme = theme_pubr())+
      labs( x= "", y = "", title = "Reasons for exclusion")+
      theme(plot.title = element_text(size = 20, face = "bold", margin = margin(t = 0, r=0, b=30, l=0)))  })
  
  output$tribkable  <- function() {

    #################Participants summary table###############
    
    
    
    
    tribtable <- tribble(
      ~"Characteristic", ~"Mean (SD) or Frequency (%)",
      "Age", mean_sd(all$age, denote_sd = "paren", na_rm = TRUE),
      "Female",n_perc0(all$sex=="Female", na_rm = TRUE),
      "Coronary procedure", n_perc0(all$procedure == "Angiogram or Percutaneous Coronary Intervention (PCI)", na_rm = TRUE),
      "Cardiac Implantable Electronic Device", n_perc0(all$procedure=="Cardiac Implantable Electronic Device (CIED)", na_rm = TRUE),
      "Electrophysiology Study", n_perc0(all$procedure=="Electrophysiology Study (EPS)", na_rm = TRUE),
      "Structural heart intervention", n_perc0(all$procedure == "Structural heart intervention", na_rm = TRUE),
      "Peripheral angiography or intervention", n_perc0(all$procedure == "Vascular angiography or intervention", na_rm = TRUE),
      "Biopsy", n_perc0(all$procedure == "Biopsy", na_rm = TRUE),
      "Port-a-cath", n_perc0(all$procedure == "Port-a-cath", na_rm = TRUE),
      "Gall bladder stone removal", n_perc0(all$procedure == "Gall bladder stone removal", na_rm = TRUE),
      "Endovascular aneurysm repair",  n_perc0(all$procedure == "Endovascular aneurysm repair (EVAR)", na_rm = TRUE),
      "Radiofrequency ablation of a tumour",  n_perc0(all$procedure == "Radiofrequency ablation of a tumour (RFA)", na_rm = TRUE),
      "Fistula repair/de-clotting",  n_perc0(all$procedure == "Fistula repair/de-clotting", na_rm = TRUE),
      "Other procedure",  n_perc0(all$procedure == "Other procedure", na_rm = TRUE)
    )
    
    kable(tribtable) %>%
      group_rows("Demographics", 1, 2) %>%
      group_rows("Procedures", 3, 14) %>%
      row_spec(0, bold = T, color = "white", background = "#3c8dbc") %>%
      kable_styling(bootstrap_options = "hover") %>%
      scroll_box(width = "100%", height = "580px")
    
    
      }
  
  output$durationtrial <- flexdashboard::renderValueBox({
    shinydashboard::valueBox(
      value = durationtrial,
      subtitle = "Duration of study to date",
      icon = icon("duration"),
      color = "light-blue",
      width = 12
    )
  }) 
  
  output$starttrial <- flexdashboard::renderValueBox({
    shinydashboard::valueBox(
      value = starttrial,
      subtitle = "Start Date",
      icon = icon("start"),
      color = "light-blue",
      width = 12
    )
  })
  
  output$recruit_ir_plot <- renderPlot({
    
    recruitmentdf_ir %>% 
      mutate(screendate = as.character(screendate)) %>% 
      ggplot(x=screendate, y=total)+
      geom_linerange(aes(x=screendate, ymin=0, ymax=total, colour = step),
                     position = position_dodge(width = 0.05))+
      geom_point(aes(x=screendate, y=total, colour = step), position = position_dodge(width = 0.05))+
      theme_pubclean()+
      # theme(
      #   axis.text.y = element_blank(),
      #   axis.ticks = element_blank()) +
      labs( x= "", y = "", title = "")+
      expand_limits(y = 0)+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 90),
            plot.title = element_text(size = 20, face = "bold", margin = margin(t = 0, r=0, b=30, l=0)))
    
    
  })
  
  output$recruit_cardiac_plot <- renderPlot({
    
    
    
    recruitmentdf_cardiac %>% 
      mutate(screendate = as.factor(screendate)) %>% 
      ggplot(x=screendate, y=total)+
      geom_linerange(aes(x=screendate, ymin=0, ymax=total, colour = step),
                     position = position_dodge(width = 0.05))+
      geom_point(aes(x=screendate, y=total, colour = step), position = position_dodge(width = 0.05))+
      theme_pubclean()+
      # theme(
      #   axis.text.y = element_blank(),
      #   axis.ticks = element_blank()) +
      labs( x= "", y = "", title = "")+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 90),
            plot.title = element_text(size = 20, face = "bold", margin = margin(t = 0, r=0, b=30, l=0)))+
      expand_limits(y = 0)
    
    
  })
  
  
  output$fluid_duration_plot <- renderPlotly({
    
    Hmisc::histboxp(x=as.numeric(difftime(all$date_time,all$fluids, units = "hours")),
                    xlab = "Fluids fasting duration")    
    
      })
  
  output$food_duration_plot <- renderPlotly({
    
    Hmisc::histboxp(x=as.numeric(difftime(all$date_time,all$food, units = "hours")),
                    xlab = "Food fasting duration")
  })
  
  
  output$thirst_intensity_plot <- renderPlotly({
    
    all %>% 
      mutate(fluids_fasting_duration = as.numeric(difftime(all$date_time,all$fluids, units = "hours"))) %>% 
      ggplot(aes(x=ptds, y=fluids_fasting_duration)) +
      geom_point(aes(colour=all$procedure))+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 30),
            legend.title = element_blank())+
      labs(x="Perioperative thirst discomfort scale score", y="Fluid fasting duration")
    
    })
  
  
  output$global_thirst_plot <- renderPlotly({
    
    all %>% 
      mutate(fluids_fasting_duration = as.numeric(difftime(all$date_time,all$fluids, units = "hours"))) %>% 
      ggplot(aes(x=global_thirst, y=fluids_fasting_duration, colour=all$procedure)) +
      geom_jitter(width=0.2, height=0)+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 30),
            legend.title = element_blank())+
      labs(x="Thirst discomfort", y="Fluid fasting duration")
    
    })
  
  output$global_ptds_plot <- renderPlotly({
    all %>% 
      mutate(fluids_fasting_duration = as.numeric(difftime(all$date_time,all$fluids, units = "hours"))) %>% 
      ggplot(aes(x=global_thirst, y=ptds, colour=all$procedure)) +
      geom_jitter(width=0.2, height=0)+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 30),
            legend.title = element_blank())+
      labs(x="Thirst discomfort", y="Perioperative thirst discomfort scale")
    })
  
  
  
}

###########################################



shinyApp(ui, server)




