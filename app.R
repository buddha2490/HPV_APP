### Shiny app
library(shiny)
library(dplyr)
library(shinythemes)
library(DT)
library(shinydashboard)
library(rsconnect)
library(AzureStor)
library(stringr)
library(ggplot2)
library(gridExtra)
library(rdrop2)
library(shinyjs)
library(shinyalert)
library(RSQLite)
library(rhandsontable)
library(dbplyr)
library(parallel)
library(grid)
library(plotly)
rm(list=ls())



# December 1st 2020
# App authors:  Brian Carter, Becky Hodge
# Note to reviewers:  The authors are not responsible for any positive or negative
#  side effects that stem from reading or reviewing this app.  This includes
#  any physical or emotional trauma, acute and post-acute, that you may experience.
#  Read this app at your own risk, the authors relinquish all responsibility.
#  The app works, nothing else matters.



# Production URLs
logout <- "https://interventionsandimplementation.shinyapps.io/HPV_iSASI/__logout__/"

# Source files for my functions
source("Functions.R")


# Ui ----------------------------------------------------------------------

ui = dashboardPage(
  title = "HPV Vaccination Systems and Strategies Inventory 2021",
  header = dashboardHeader(title = textOutput("username")),
 
  
  # Sidebar layout ----------------------------------------------------------
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      type = "hidden",
      menuItem("Instructions", tabName = "instructions", icon = icon("home")),
      menuItem(
        "Baseline Submission",
        tabName = "baseline",
        icon = icon("hourglass-start")
      ),
      menuItem("Final Submission", tabName = "finalupdate", icon = icon("hourglass-end")),
      menuItem("Optional Data", tabName="optional", icon=icon("chart-bar")), # Add optional data page
      menuItem(
        "Baseline Dashboard",
        tabName = "dashboard",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Final Dashboard",
        tabName = "fuDashboard",
        icon = icon("chart-bar")
      ),
      menuItem("Log Out",
               href = logout,
               icon=icon("user-cog"))
      ),
      tags$head(tags$style(".mybutton{background-color:black;} .skin-black .sidebar .mybutton{color: white;}"))
    ),

  body = dashboardBody(
    shinyjs::useShinyjs(),
    # need this so we can do the session$reload() thing
    shinyalert::useShinyalert(),
    # need this for those lovely pop-ups
    
    tags$style(
      HTML(
        ".box.box-solid.box-success>.box-header {
                                      background:#fca500
                                      }
                                      .box.box-solid.box-success{
                                      border-bottom-color:#fca500;
                                      border-left-color:#fca500;
                                      border-right-color:#fca500;
                                      border-top-color:#fca500
                                      }"
      )
    ),
    # this is to change the 'status=success' box color to yellow instead of green
    # Make the error messages red when we use the validate function
    tags$style(HTML(
      ".shiny-output-error-red_warnings {
                          color: red;
                          }
                          "
    )),
    tags$style(
      HTML(
        ".shiny-input-container:not(.shiny-input-container-inline) {
              width: 100%;
              }"
      )
    ),
    tabItems(
      
      # Instructions page  -------------------------------------------
      
      tabItem(
        "instructions",
        div(
          imageOutput(
            'HPVlogo',
            inline = TRUE,
            height = '75px',
            width = '300px'
          ),
          h1("HPV Vaccination Systems and Strategies Inventory 2021", align =
               "center"),
        ),
        # end box with logos and title
        HTML("<br><br>"),
        h3("Instructions and Dates", align = "center"),
        HTML("<br>"),
        helpText(
          "The Systems and Strategies Inventory (SASI) is designed to be completed by American Cancer Society (ACS) staff and the health
           system partner quality improvement team for each year of HPV vaccination partnership. This tool will guide you as you assemble
          a team, make a plan based on your data and previous activities, engage your system, track your progress, and measure its impact."
        ),
        HTML("<br>"),
        helpText("The Inventory contains ", strong("two sections:")),
        tags$ol(
          tags$li(strong("Baseline: "), "due March 31, 2021"),
          tags$li(strong("Final Project Update: "), "due January 31, 2022")
        ),
        HTML("<br>"),
        helpText(
          "If your HPV vaccination project with ACS continues for more than one year, ",
          strong("you will complete a new inventory for each project year.")
        ),
        HTML("<br>", "<br>"),
        
        
        splitLayout(
          box(width = '12',
              align = 'center',
              verticalLayout(
                h4(strong("Systems and Strategies Inventory")),
                helpText("Complete this section to start your project ", strong("1/1/2021-3/31/2021")),
                actionButton("click_baseline", "Baseline Health System Information",
                             class = "btn-inverse btn-lg"),
                HTML("<br>"),
                helpText("Complete this section after your project ",  strong("1/1/2022-1/31/2022")),
                actionButton("click_finalupdate", "Final Project Update", class = "btn-inverse btn-lg")
              )),
          
          
          box(width = '12',
              align = 'center',
              h4(strong("Data Monitoring")),
              verticalLayout(
                helpText("Click here to visualize your ", strong("baseline "), "vaccination rates"),
                actionButton("click_dashboard", "Baseline Dashboard", class =
                               "btn-inverse btn-lg"),
                HTML("<br>"),
                helpText("Click here to visualize your ", strong("final "), "vaccination rates"),
                actionButton("click_fuDashboard", "Final Dashboard", class =
                               "btn-inverse btn-lg")
              )
          )),
        
      ),    # end instructions page
      
      
      # Baseline information page -----------------------------------------------
      
      tabItem(
        "baseline",
        verticalLayout(splitLayout(
          h1("Baseline Information", align = "center")
          #valueBox(textOutput("uniqueID"), "Health System ID", icon=icon("building")) # Display unique health system identifier once the necessary information has been input
        )),
        
        
        # Demographic section -----------------------------------------------------
        
        tabBox(
          width = NULL,
          id = "tabbox_baseline",
          
          tabPanel(
            h4("Demographics"),
            value = "p1_demo",
            valueBoxOutput("date_demo", width=12),
                h3("Demographics"),
            helpText(
              "First, let's get more information about your clinic and its demographics"
            ),
            HTML("<br>"),
            verticalLayout(
              # --- Lines 1-2 of the pdf ---- #
              uiOutput("HealthSystem"),
              uiOutput("Q1_DBA"),
              splitLayout(
                uiOutput("Q2"), uiOutput("Q2_other")
              ),
              uiOutput("Q3"),
              textOutput("error_Q3"),
              HTML("<br>"),
              # ---- Line 3 in the pdf- address info  ----- #
              splitLayout(
                uiOutput("Q4a"), uiOutput("Q4b"), uiOutput("Q4c")
              ),
              HTML("<br>"),
              splitLayout(
                uiOutput("Q5"), uiOutput("Q5_Email")
              ),
              uiOutput("Q6"),
              uiOutput("Q7"),
              uiOutput("Q8"),
              uiOutput("Q8_Email"),
              uiOutput("Q9"),
              fluidRow(
                column(width = 6,
                       uiOutput("Q10"),),
                column(
                  width = 4,
                  offset = 1,
                  uiOutput("Q10_other"),
                  uiOutput("error_Q10_other"),
                  uiOutput("error_Q10")
                  
                )
              ),
              verticalLayout(
                uiOutput("Q11"),
                uiOutput("Q11_other")
              ),
              verticalLayout(
                uiOutput("Q12"),
                uiOutput("Q12_other")
              ),
              uiOutput("Q13"),
              splitLayout(
                uiOutput("Q13_amount"), uiOutput("Q13_source"),  uiOutput("Q13_date"),  uiOutput("Q13_length")
              ),
              splitLayout(
                textOutput("error_Q13_amount"), textOutput("error_Q13_source"), textOutput("error_Q13_date"), textOutput("error_Q13_length")
              ),
              splitLayout(uiOutput("Q14"),
                          uiOutput("Q14_other")),
              HTML("<br>"),
              
              p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),

              actionButton("button_demo", "Save and finish later", class =
                             "btn-primary")
            ) #end vertical layout
            
          ),
          # end demographics tab panel,
          
          
          
        # Systems background page -------------------------------------------------------
          
          
          tabPanel(
            h4("System Background"),
            value = "p1_data",
            valueBoxOutput("date_systems", width=12),
            h3("System Background"),
            helpText(
              "Now, you'll provide information on the data systems your health system uses.
               Please complete this information to the best of your knowledge."
            ),
            HTML("<br>"),
            verticalLayout(
              uiOutput("Q15"),
              uiOutput("Q15_other"),
              uiOutput("Q15_EHRversion"), 
              uiOutput("Q16"),
              HTML("<br>"),
              uiOutput("Q17"),
              HTML("<br>"),
              uiOutput("Q18"),
              uiOutput("Q19"),
              HTML("<br>"),
              uiOutput("Q20"),
              uiOutput("Q20_notes", inline = TRUE),
              uiOutput("Q20_orders"),
              uiOutput("Q20_other"),
              uiOutput("Q21"),
              uiOutput("Q21_text"),
              uiOutput("Q22"),
              uiOutput("Q22_notes"),
              uiOutput("Q23"),
              uiOutput("Q23_notes", inline = TRUE),
              uiOutput("Q24"),
              uiOutput("Q24_years", inline = TRUE),
              uiOutput("Q25"),
              uiOutput("Q25_other"),
              HTML("<br>"),
              uiOutput("Q26"),
              uiOutput("Q26_other"),
              HTML("<br>"),
              p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),
              actionButton("button_systems", "Save and finish later", class = "btn-primary"),
              HTML("<br>"),
              uiOutput("ReviewData"),
              uiOutput("saveBaseline"),
              uiOutput("saveError")
            )
          ),
      
      
        # Vaccination rates tab ---------------------------------------------------

          # TEXT STUFF
          tabPanel(
            h4("Vaccination Rates"),
            value = "p1_rates",
            valueBoxOutput("date_brates", width=12),
            h3("Vaccination Rates"),
            HTML("<br>"),
            h4("INSTRUCTIONS:"),
            helpText("This section collects baseline vaccination rates for your health system. Vaccination rates 
                     should be calculated for active medical patients ages 9-13 at participating clinic sites who were up-to-date 
                     with HPV, Tdap, and Meningococcal vaccines in 2020. The definitions and tips listed below will help you
                      calculate your vaccination rates. Data entry tables will appear after you answer the prompts to the questions
                      below."),
            HTML("<br>"),
            h4("DEFINITIONS:"),
            tags$ul(
              tags$li(
                em("HPV vaccine initiation"), "number should include patients who have ",
                strong("ever"), " received 1 dose of the HPV vaccine. (This number will include patients who received both their 1st and 2nd dose.)"
              ),
              tags$li(
                em("HPV vaccination series completion "), "includes patients who have received 2 doses of HPV vaccine separated by at least 5 months"
              ),
              tags$li(
                em("Active medical patients"),
                " are defined as those who were seen at least once during ",
                strong("the time period you specify below"),
                " (12 months, 18 months, 24 months). Medical visits do not include dental or other non-medical visits. Medical visits",
                em(" do "), "include well-child visits and sick visits. Please note that a longer time period allows more patients to be 
                included in the rate calculation to account for the infrequenty medical viits for patients ages 9-13."
              ),
              tags$li(
                "The ", em("reporting period "), "is used to define patient age to determine which active patients were, for example,
                 age 13 during the baseline year. The reporting period for baseline is January 1, 2020 to December 31, 2020. Patients included in the age 13 group turned 13 during the reporting period"
              ),
              tags$li(
                em("Up-to-date "), "is defined as active medical patients in the relevant age and sex categories who have ",
                strong("ever "), "received the specified vaccine (or dose). They may have received the vaccine prior to the reporting period
                and should still be counted. This means that the date of service is not relevant to the calculation of vaccination rates for this project.
                Use the following table to identify ", 
                strong("up-to-date "), "patients in 2020."
              )
            ),
            
            HTML("<br>"),
            tableOutput("static_table"),
            
            h4("TIPS ON DATA QUALITY:"),
            helpText(p("You will see ", strong("red notes ", style="color:red"), "appear if entered data includes the following data quality flags:")),
            tags$ul(
              tags$li(
                "The number of patients receiving a vaccination exceeds the number of active patients"
              ),
              tags$li(
                "Completion rates exceed initiation rates (every child that completed the series must have been initiated)"
              )
            ),
            helpText("Other quality tips you will not be notified for but you should pay attention to include:"),
            tags$ul(
              tags$li(
                "Be sure to enter data for the correct age into each table"
              ),
              tags$li(
                "The active patient population should not be the same for different age groups or sex (this would be highly unlikely)"
              ),
              tags$li(
                "Do any of the numbers seem really high or really low? Is the number of patients who received one or two doses for HPV a lot higher than Tdap and Meningococcal?
                Are more patients age 9-10 vaccinated than those age 13? These questions could highlight potential issues in data quality."
              )
            ),
            HTML("<br>"),
            h4("CUSTOMIZING YOUR VACCINATION RATE TABLES:"),
            helpText("We strongly encourage you to report vaccination rates separated by sex and specific age groupings (9-10, 11-12, and 13).
                      We recognize this may not always be possible. Use the following question prompts to customize the data entry tables you need. ",
                     strong("Once you respond to all the following prompts, your data table(s) will appear below.")
                     ),
            HTML("<br>"),
            
            # These go AFTER the instructions but before the rates
            splitLayout(
              uiOutput("Q27"), uiOutput("Q27_other")
            ),
            uiOutput("Q28"), 
            fluidRow(
              column(8,
                     uiOutput("Q28_other")),
              column(4,
                     uiOutput("Q28_other_message"))
            ),
            #uiOutput("Q28_other"),
            #uiOutput("Q28_other_message"),
            uiOutput("Q29"),
            HTML("<br>"),
            
            # Rates tables
            uiOutput("brates_9_10"),
            uiOutput("brates_11_12"),
            uiOutput("brates_13"),
            
            verticalLayout(
              HTML("<br><br>"),
              
              box(
                width = '100%',
                span(
                  uiOutput("Q30"),
                ),
                textOutput("error_rates"),
                uiOutput("Q31"),
                uiOutput("Q31_other"),
                uiOutput("Q32"),
                splitLayout(uiOutput("Q32_details"),
                            uiOutput("Q32_other"))
              ),
              
              fluidRow(
                column(6,  uiOutput("Q33")),
                column(6, uiOutput("Q33_other"))),
              uiOutput("Q34")
            ), # End vertical arrangement
            
            HTML("<br>"),
            p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),

            actionButton("button_rates", "Save and finish later", class =
                           "btn-primary")
          ),
          # end rates tabbox
          
          
        # Project activity plan ---------------------------------------------------
          
          tabPanel(
            h4("Project Activity Plan"),
            value = "p1_plan",
            valueBoxOutput("date_activity", width=12),
            helpText("This last page will prompt you about your project activity plan"),
            HTML("<br>"),
            verticalLayout(
              h4(strong("Goal Setting"), align = "center"),
              fluidRow(
                column(8,
                       uiOutput("Q35")),
                column(4,
                       uiOutput("Q35_other"))
              ),
              HTML("<br>"),
              box(
                width = 12,
                uiOutput("Q36a"),
                uiOutput("Q36b"),
                uiOutput("Q36c"),
                uiOutput("Q36d"),
                uiOutput("Q36e"),
                HTML("<br>"),
                box(
                  title = h4(
                    "36f.  Now, combine the above into a single ",
                    strong("Aim"),
                    " statement:"
                  ),
                  solidHeader = TRUE,
                  width = 12,
                  status = "success",
                  uiOutput("Q36f")
                ) # end this mini box
              ),  #end the other box
              HTML("<br>"),
              uiOutput("Q37"),
              uiOutput("Q38"),
              uiOutput("Q38_other"),
              uiOutput("Q39"),
              uiOutput("Q39_other"),
              uiOutput("Q40"),
              uiOutput("Q41"),
              uiOutput("Q42"),
              
              HTML("<br>"),
              h5(
                strong(
                  "18.  What specific actions will we take to implement the above interventions and trainings to meet our goal?"
                )
              ),
              helpText(em("This question can be used as a QI action plan")),
              HTML("<br>"),
              box(
                width = 12,
                splitLayout(
                  uiOutput("act1"), uiOutput("time1"), uiOutput("ppl1")
                ),
                splitLayout(
                  uiOutput("act2"), uiOutput("time2"), uiOutput("ppl2")
                ),
                splitLayout(
                  uiOutput("act3"), uiOutput("time3"), uiOutput("ppl3")
                ),
                splitLayout(
                  uiOutput("act4"), uiOutput("time4"), uiOutput("ppl4")
                ),
                splitLayout(
                  uiOutput("act5"), uiOutput("time5"), uiOutput("ppl5")
                ),
                splitLayout(
                  uiOutput("act6"), uiOutput("time6"), uiOutput("ppl6")
                ),
                splitLayout(
                  uiOutput("act7"), uiOutput("time7"), uiOutput("ppl7")
                ),
                splitLayout(
                  uiOutput("act8"), uiOutput("time8"), uiOutput("ppl8")
                ),
                splitLayout(
                  uiOutput("act9"), uiOutput("time9"), uiOutput("ppl9")
                ),
                splitLayout(
                  uiOutput("act10"), uiOutput("time10"), uiOutput("ppl10")
                )
              ),# end box
              HTML("<br>"),
              p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),

              actionButton("button_activities", "Save and finish later", class =
                             "btn-primary")
              
            ) # end vertical layout
            
          ), # end activity plan tabitem
        
        
        tabPanel(
            h4("Baseline Data Submission"),
            value = "p1_submission",
            h4("Yay! You've completed the baseline data entry!", style="color:blue"),
            HTML("<br>"),
            helpText("Now that you've entered your baseline data for all four sections, please download your baseline report to check your answers, and also as a copy for your records.
                      Any responses left blank will appear blank."),
            HTML("<br>"),
            downloadButton("baseline_report", "Download baseline report"),
            HTML("<br>"),
            HTML("<br>"),
            h4("Once you've reviewed your data..."),
            helpText(p("Hit the submit button below to send the finalized data to the ACS Interventions & Implementations Team (formerly VACs).", style="color:red")),
            helpText("If you have any questions, please contact ", a("Jennifer Isher-Witt", href = "mailto:Jennifer.Ish@cancer.org")),
                    actionButton("button_baseline_final", "SUBMIT FINAL DATA", class="btn-danger"),
                    HTML("<br>"),
                    helpText(em("*Please note that it is possible to return to this page and resubmit if need be. Any changes made after hitting the above button will not be conveyed to the ACS interventions team
                                 unless you hit this button again and resubmit the data.")),
            HTML("<br>")
            
        ) #end baseline submission tab
        ) # end baseline tabbox
      ), # end baseline's page (with all the nested tabboxes)
      
      
      # Final Rates section ----------------------------------------------------
      
      
      # Note:  They have several "Progress Updates" that are otherwise identical in the page.
      # As I understand, this project only has the single update, which is why that's all that's programmed here
      # We can expand if necessary.
      
      tabItem(
        "finalupdate",
        h1("Final Project Update", align = "center"),
        
        
        
        # All text stuff
        tabBox(
          width = NULL,
          id = "tabbox_final",
          tabPanel(
            h4("Updated Rates"),
            value = "p2_rates",
            valueBoxOutput("date_fwup_rates", width=12),
            h3("Final Project Updates"),
            HTML("<br>"),
            h4("INSTRUCTIONS:"),
            helpText("This section collects final vaccination rates for your health system. Vaccination rates 
                     should be calculated for active medical patients ages 9-13 at participating clinic sites who were up-to-date 
                     with HPV, Tdap, and Meningococcal vaccines in 2021. The definitions and tips listed below will help you
                      calculate your vaccination rates. Data entry tables will appear after you answer the prompts to the questions
                      below."),
            HTML("<br>"),
            h4("DEFINITIONS:"),
            tags$ul(
              tags$li(
                em("HPV vaccine initiation"), "number should include patients who have ",
                strong("ever"), " received 1 dose of the HPV vaccine. (This number will include patients who received both their 1st and 2nd dose.)"
              ),
              tags$li(
                em("HPV vaccination series completion "), "includes patients who have received 2 doses of HPV vaccine separated by at least 5 months"
              ),
              tags$li(
                em("Active medical patients"),
                " are defined as those who were seen at least once during ",
                strong("the time period you specify below"),
                " (12 months, 18 months, 24 months). Medical visits do not include dental or other non-medical visits. Medical visits",
                em(" do "), "include well-child visits and sick visits. Please note that a longer time period allows more patients to be 
                included in the rate calculation to account for the infrequenty medical viits for patients ages 9-13."
              ),
              tags$li(
                "The ", em("reporting period "), "is used to define patient age to determine which active patients were, for example,
                 age 13 during the baseline year. The reporting period for the final update is January 1, 2021 to December 31, 2021. Patients included in the age 13 group turned 13 during the reporting period"
              ),
              tags$li(
                em("Up-to-date "), "is defined as active medical patients in the relevant age and sex categories who have ",
                strong("ever "), "received the specified vaccine (or dose). They may have received the vaccine prior to the reporting period
                and should still be counted. This means that the date of service is not relevant to the calculation of vaccination rates for this project.
                Use the following table to identify ", 
                strong("up-to-date "), "patients in 2021."
              )
            ),
            
            HTML("<br>"),
            tableOutput("static_table_fwup"),
            
            h4("TIPS ON DATA QUALITY:"),
            helpText(p("You will see ", strong("red notes ", style="color:red"), "appear if entered data includes the following data quality flags:")),
            tags$ul(
              tags$li(
                "The number of patients receiving a vaccination exceeds the number of active patients"
              ),
              tags$li(
                "Completion rates exceed initiation rates (every child that completed the series must have been initiated)"
              )
            ),
            helpText("Other quality tips you will not be notified for but you should pay attention to include:"),
            tags$ul(
              tags$li(
                "Be sure to enter data for the correct age into each table"
              ),
              tags$li(
                "The active patient population should not be the same for different age groups or sex (this would be highly unlikely)"
              ),
              tags$li(
                "Do any of the numbers seem really high or really low? Is the number of patients who received one or two doses for HPV a lot higher than Tdap and Meningococcal?
                Are more patients age 9-10 vaccinated than those age 13? These questions could highlight potential issues in data quality."
              )
            ),
            HTML("<br>"),
            h4("CUSTOMIZING YOUR VACCINATION RATE TABLES:"),
            helpText(strong("We strongly recommend you use the same active patient population, ages, and data sources as your baseline data. "),
                     "For baseline data, we encouraged you to report vaccination rates separated by sex and specific age groupings (9-10, 11-12, and 13).
                      We recognize this may not always be possible. Use the following question prompts to customize the data entry tables you need.",
                     strong("Once you respond to all the following prompts, your data table(s) will appear below.")
                     ),
            HTML("<br>"),
            # These go AFTER the instructions but before the rates
            splitLayout(
              uiOutput("Q1FU"), uiOutput("Q1FU_other")
            ),
            splitLayout(
              uiOutput("Q2FU"), uiOutput("Q2FU_other")
            ),
            uiOutput("Q3FU"),
            HTML("<br>"),
            
            uiOutput("furates_9_10"),
            uiOutput("furates_11_12"),
            uiOutput("furates_13"),
            
            # ages 9-10
            verticalLayout(
            HTML("<br>"),
            HTML("<br>"),
            uiOutput("Q4FU"),
            HTML("<br>"),
            p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),
            actionButton("button_fwup_rates", "Save and finish later", class =
                           "btn-primary"),
            textOutput("error_rates_fwup")
          )),
          # end tab on rates
          
      # Additional information page ---------------------------------------------
          
          tabPanel(
            h4("Additional Info"),
            value = "p2_additional",
            valueBoxOutput("date_fwup_additional", width=12),
            h3("Additional Info"),
            HTML("<br>"),
            verticalLayout(
              uiOutput("Q5FU"),
              uiOutput("Q5FU_text"),
              uiOutput("Q6FU"),
              uiOutput("Q6FU_text"),
              HTML("<br>"),
              uiOutput("Q7FU"),
              uiOutput("Q7FU_other"),
              uiOutput("Q8FU"),
              uiOutput("Q8FU_other"),
              uiOutput("Q9FU"),
              uiOutput("Q9FU_other"),
              HTML("<br>"),
              verticalLayout(
                uiOutput("Q10FU"),
                uiOutput("Q11FU"),
                uiOutput("Q12FU")
              ),
              uiOutput("Q13FU"),
              uiOutput("Q13FU_other"),
              uiOutput("Q14FU"),
              uiOutput("Q14FU_other"),
              uiOutput("Q15FU"),
              uiOutput("Q16FU"),
              uiOutput("Q17FU"),
              uiOutput("Q18FU"),
              uiOutput("Q18FU_more")
            ),
            HTML("<br>"),
            p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),

            actionButton("save_followup", "Save and finish later", class =
                           "btn-primary")
            
          ), # end tab on other
      
      tabPanel(
            h4("Final Data Submission"),
            value = "p2_submission",
            
            h4(p("Yay! You've completed your final data entry.", style="color:blue")),
            HTML("<br>"),
            helpText("Now that you've entered your fina data, please download your final report to check your answers, and also as a copy for your records.
                      Any responses left blank will appear blank."),
            HTML("<br>"),
            downloadButton("final_report", "Download final report"),
            HTML("<br>"),
            HTML("<br>"),
            h4("Once you've reviewed your data..."),
            helpText(p("Hit the submit button below to send the finalized data to the ACS Interventions & Implementations team.", style="color:red")),
                    helpText("If you have any questions, please contact ", a("Jennifer Isher-Witt", href = "mailto:Jennifer.Ish@cancer.org")),
                    actionButton("button_fwup_final", "SUBMIT FINAL DATA", class="btn-danger"),
                    HTML("<br>"),
                    helpText(em("*Please note that it is possible to return to this page and resubmit if need be. Any changes made after hitting the above button will not be conveyed to the ACS interventions team
                                 unless you hit this button again and resubmit the data.")),
            
            HTML("<br>")
            
      ) #end final submit tab
        )# end tabbox
        
        
        
      ),
      # end final update tab
      
      
      # Optional data page -----------------------------
      tabItem("optional",
              verticalLayout(
                fluidRow(
                  column(12, offset = 2,
                         box(title = NULL, width = 8,
                             h3("Remember your goals:", align = "center"),
                             uiOutput("remembergoal3"),
                             background = "olive"))
                ),
              tabBox(width = NULL,
                     tabPanel(title = "Monthly data entry",
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              h2("Monthly data entry"),
                              HTML("<br>"),
                              helpText("You can use this dashboard to enter monthly data updates and track your progress through the year."),
                              HTML("<br>"),
                              box(width=4, align="center", background = "light-blue", solidHeader=TRUE,
                                  verticalLayout(
                                    
                                    # Select Age groups
                                    selectInput(inputId = "mu_ages",
                                                label = "What ages are you reporting?",
                                                choices = c("9-10","11-12", "13"),
                                                selected = NULL),
                                    
                                    # Select Sex
                                    selectInput(inputId = "mu_sex",
                                                label = "sex",
                                                choices = c("Males and females seperately",
                                                            "Males and females combined"),
                                                selected = "Males and females combined"),
                                    
                                    # Choose starting month
                                    uiOutput("start_month")
                                  )), # end box
                              
                              box(width=8, solidHeader=TRUE,
                                  tags$ol(
                                    tags$li("Each row is a new month"),
                                    tags$li("You can decide the type of visit you'd like to include (i.e., well visit and/or Other Medical Visit (focused/sick/acute))"),
                                    tags$li("Data are for sites that are participating in the HPV VACs QI project only (exclude other sites in the system that are not participating in the project)"),
                                    tags$li("Select your starting month from the drop down menu and whether you want to enter data for males and females separately or together"),
                                    tags$li("If you provide sex-specific data, the combined rates will be calculated for you"),
                                    tags$li("First provide the total number in the cells for total patients seen in each month"),
                                    tags$li("Enter the number of patients recieving at least one or both HPV doses"),
                                    tags$li("For patients aged 11+, you may also include the number of Meningococcal and Tdap vaccinations."),
                                  )
                              ), # end box
                              
                              verticalLayout(
                                HTML("<br>"),
                                HTML("<br>"),
                                uiOutput("rates_9_10"),
                                uiOutput("rates_11_12"),
                                uiOutput("rates_13"),
                                actionButton("save_monthly", "Save data", class="success")
                              ),
                              helpText("Please save after entering data in each table so you 
                                           can follow your progress throughout the year."),
                              HTML("<br><br>"),
                              plotOutput("allPlots", height = "600px")
                              
                     ), # end tab panel 1 (monthly updates)
                     
                     
                     tabPanel(title="Clinic-level data entry",
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              h2("Clinic-level data entry"),
                              HTML("<br>"),
                              helpText("You can use this dashboard to enter clinic-level data updates and track your progress through the year."),
                              HTML("<br>"),
                              
                              box(width=4, align="center", background = "light-blue", solidHeader=TRUE,
                                  verticalLayout(
                                    
                                    # Select Age groups
                                    selectInput(inputId = "site_ages",
                                                label = "What ages are you reporting?",
                                                choices = c("9-10","11-12", "13"),
                                                selected = NULL),
                                    
                                    # Select Sex
                                    selectInput(inputId = "site_sex",
                                                label = "sex",
                                                choices = c("Males and females seperately",
                                                            "Males and females combined"),
                                                selected = "Males and females combined"),
                                    
                                    selectInput(inputId = "num_sites",
                                                label = "Select number of sites or providers",
                                                choices = paste(1:12, "Site/Provider"),
                                                selected = "1 Site/Provider")
                                  )), # end box
                              box(width = 8, solidHeader = TRUE,
                                h4("Conversation Corner"),
                                HTML("<br>"),
                                tags$head(tags$style(
                                  HTML("pre { white-space: pre-wrap; word-break: keep-all; }")
                                )),
                                htmlOutput("siteMessage"),
                                align = "center",
                                background = "light-blue"
                              ),
                              
                              verticalLayout(
                                HTML("<br>"),
                                HTML("<br>"),
                                uiOutput("site_9_10"),
                                uiOutput("site_11_12"),
                                uiOutput("site_13"),
                                actionButton("save_site", "Save data", class="success")
                              ),
                              helpText("Please save after entering data in each table so you 
                                           can follow your progress throughout the year.")
                     )# end clinic-level data entry
              ) # end tab box
              ) # end vertical layout
      ), # end tab home
      

      
      
      
      
      # Final dashboard ------------------------------------------------------
      tabItem(
        "fuDashboard",
        h2("HPV Vaccination Data Tracker: Final Data", align = "center"),
        helpText(
          "This section is designed to help ACS staff and partners track and visualize data collected
                 during HPV vaccination quality improvement projects.  Additional tabs are provided for monthly
                  monitoring under the Optional Data button in the left-hand menu."
        ),
        HTML("<br><br>"),
        verticalLayout(
          fluidRow(
            column(12, offset = 2,
                   box(title = NULL, width = 8,
                       h3("Remember your goals:", align = "center"),
                       uiOutput("remembergoal2"),
                       background = "olive"))
          ),
          
        
        # Within the fuDashboard tabItem container, I want to define another container of tabs
        tabBox(
          width = NULL,
          tabPanel(
            title = "Final Updates",
            h3("Instructions"),
            tags$ol(
              tags$li(
                "The data presented here are the same as the baseline ",
                strong("Systems and Strategies Inventory (SASI)")
              ),
              tags$li(
                "This tab is intended to monitor the final update data entered in the final survey."
              ),
              tags$li(
                "Each of the following charts will aggregate all final data for each health system"
              ),
              tags$li("You can drag each chart to your desktop and share with your partner!")
            ),
            HTML("<br><br>"),
            h4("Charts"),
            helpText(
              "Consider sharing this back with each health systems following the submission of
                                 your final data. Use this to see if there are any changes the system can make prior
                                 to the start of vaccine season in June."
            ),
            HTML("<br><br>"),
            verticalLayout(
              h4("Vaccination rates from baseline to final update", align = "center"),
              helpText(
                "These charts use the baseline and final data you have provided throughout
                                   the data collection process and are aggregated for each health system and update
                                   period.  Data are presented for males, females, and combined."
              ),
              HTML("<br><br>"),
              HTML("<br><br>"),
              
              
              # Data one - 1 HPV dose - change in time
              verticalLayout(
                box(
                  h3(strong("Conversation Corner")),
                  HTML("<br>"),
                  tags$head(tags$style(
                    HTML("pre { white-space: pre-wrap; word-break: keep-all; }")
                  )),
                  htmlOutput("FUconversationCorner1"),
                  width = 12,
                  align = "center",
                  background = "light-blue"
                ),
                h5(
                  strong("HPV Initiation Rates (1+ doses) - baseline to final update"),
                  align = "center"
                ),
                h5(strong("Males and females combined"), align = "center"),
                
                splitLayout(
                  DT::dataTableOutput("fuTable1"),
                  plotOutput("fuFigure1", height = "600px")
                )
              ),
              HTML("<br><br>"),
              HTML("<br><br>"),
              # Data two - Complete HPV dose - change in time
              verticalLayout(
                h5(
                  strong("HPV Completion Rates (2 doses) - baseline to final update"),
                  align = "center"
                ),
                h5(strong("Males and females combined"), align = "center"),
                splitLayout(
                  DT::dataTableOutput("fuTable2"),
                  plotOutput("fuFigure2", height = "600px")
                )
              ),
              HTML("<br><br>"),
              HTML("<br><br>"),
              
              verticalLayout(
                box(
                  h3(strong("Conversation Corner")),
                  HTML("<br>"),
                  tags$head(tags$style(
                    HTML("pre { white-space: pre-wrap; word-break: keep-all; }")
                  )),
                  htmlOutput("FUconversationCorner3"),
                  width = 12,
                  align = "center",
                  background = "light-blue"
                ),
                splitLayout(
                  plotOutput("fuFigure3", height = "600px"),
                  plotOutput("fuFigure4", height = "600px")
                )
              )
            ) # end verticalLayout
          ) # end of the first SASI Updates tab
          
        ) # end the tabBox container within fuDashboard
        ) # end vertical alignment
        
      ),
      # end fuDashboard tabItem
      
      
      
      # Baseline dashboard ------------------------------------------------------
      tabItem(
        "dashboard",
        h2("HPV Vaccination Data Tracker: Baseline Data", align = "center"),
        HTML("<br>"),
        helpText(
          "This section is designed to help ACS staff and partners track and visualize
           data collected during HPV vaccination quality improvement projects.
           This version is designed for systems collecting baseline data.
           See the other tabs below for updates."
        ),
        HTML("<br><br>"),
        verticalLayout(
          fluidRow(
            column(12, offset = 2,
                   box(title = NULL, width = 8,
                       h3("Remember your goals:", align = "center"),
                       uiOutput("remembergoal1"),
                       background = "olive"))
          ),
        tabBox(
          width = NULL,
          tabPanel(
            title = "Baseline Overview",
            h3(strong("Monitoring Charts"), align = "center"),
            helpText(
              "These charts are based on the data you have entered for your health system.  You
               may see how your health system compares to others."
            ),
            HTML("<br>"),
            h4(strong(
              "Baseline Vaccination Rates:  Males and Females"
            ), align = "center"),
            helpText(
              "Consider sharing this soon after the start of the project,
                                     when only baseline data has been collected, so your partner
                                     can see their rates visualized."
            ),
            HTML("<br><br>"),
            # Figure 1
            fluidRow(
              column(7,
                     plotOutput("baseVacRates1")),
              column(5,
                     verticalLayout(
                       HTML("<br><br><br>"),
                       box(
                         h4("Conversation Corner"),
                         HTML("<br>"),
                         tags$head(tags$style(
                           HTML("pre { white-space: pre-wrap; word-break: keep-all; }")
                         )),
                         htmlOutput("conversationCorner1"),
                         width = 12,
                         align = "center",
                         background = "light-blue"
                       )  
                     ))
            ),

            HTML("<br><br>"),
            
            # Figure 2
            h4(strong("Baseline HPV vaccination rates compared to other vaccines"),
              align = "center"
            ),
            fluidRow(
              column(7,
                     plotOutput("baseVacRates2", height = "600px")),
              column(5,
                     verticalLayout(
                     HTML("<br><br><br><br>"),
                     box(
                       h4("Conversation Corner"),
                       HTML("<br>"),
                       htmlOutput("conversationCorner2"),
                       width = 12,
                       align = "center",
                       background = "light-blue"
                     )))
            ),
            # Figure 3
            HTML("<br><br>"),
            h4(strong("Baseline HPV vaccination rates by age"), align = "center"),
            fluidRow(
              column(7,
                     plotOutput("baseVacRates3")),
             column(5,
                    verticalLayout(
                      HTML("<br><br><br>"),
                      box(
                        h4("Conversation Corner"),
                        HTML("<br>"),
                        #tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                        htmlOutput("conversationCorner3"),
                        width = 12,
                        align = "center",
                        background = "light-blue"
                      )
                    )) 
            ),
            # Figure 4
            HTML("<br><br>"),
            h4(strong("How does your health system compare to others?"), align = "center"),
            fluidRow(
              column(6,
                     selectInput(inputId = "selectRegion",
                                 label = "Please select a region",
                                 choices = c("All regions combined",
                                             "West",
                                             "North",
                                             "South",
                                             "North Central",
                                             "Southeast",
                                             "Northeast"),
                                 selected = "All regions combined")),
              column(6, 
                     selectInput(inputId = "selectSystemType",
                                 label = "Please select a system type",
                                 choices = c("All system types",
                                             "IDS", 
                                             "FQHC")))
                     ),
            plotOutput("baseVacRates4")

          )
          
        ) # End TabBox
        ) # end verticalLayout
      ) # End Tab Item
      
    ) #end all tabItems listing
    
  ),# end dashboard Body
  skin = "black"
) # End UI section



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  
# File paths to Azure: ----------------------------------------------------
  
  
  # Pull my inputData
  inputData <- dbConnect(SQLite(), "inputData.DB")
  
  # User directory
  #shortUser <- stringr::str_replace(session$user, pattern = "[[@]].*", "")
  shortUser <- "bcarter6"
  ## Define the endpoint and container
  # One primary containers: DataSrc
  endpoint <- storage_endpoint(
    dbReadTable(inputData, "azureDat")$myFileshare,
    dbReadTable(inputData, "azureDat")$mykey
  )
  DataSrc <- storage_container(endpoint, "hpv/DataSrc")

  # Within each container, each user gets a folder for their temporary data
  baseFiles <- dplyr::filter(list_storage_files(DataSrc), isdir == F)$name  # list of names or NULL

  # Create the filename for each user and initialize the SQLite
  userFilename <- paste0(shortUser,".DB")
  
  #  This will initialize the database from an initial null database
  if (!userFilename %in% baseFiles)  firstDB(DataSrc, userFilename, session$user)


  
  
  # Home page ---------------------------------------------------------------
  
  # Get the mergeID
  output$username <- renderText({
    session$user
  })
  
  # Display the logos
  output$HPVlogo <- renderImage({
    list(src = "Logos/ACS-Mission HPV_RGB.png",
         width = 300,
         alt = "ACS Interventions & Implementations team")
  }, deleteFile = FALSE)
  
  
  # When click the buttons on the first page, get taken to the relevant tabs
  observeEvent(input$click_baseline, {
    updateTabsetPanel(session, "tabs", selected = "baseline")
  })
  observeEvent(input$click_finalupdate, {
    updateTabsetPanel(session, "tabs", selected = "finalupdate")
  })
  observeEvent(input$click_dashboard, {
    updateTabsetPanel(session, "tabs", selected = "dashboard")
  })
  observeEvent(input$click_fuDashboard, {
    updateTabsetPanel(session, "tabs", selected = "fuDashboard")
  })
  
  # ------------------------------- Baseline information section ---------------------------------------- #
  # Demographics ------------------------------------------------------------
  

  demographics <- getRecentData(DataSrc, userFilename, "demographics")
  
  DemoComments <- demographics$DemoComments

    
    foo11 <- c(demographics$Q11_1, demographics$Q11_2, demographics$Q11_3, demographics$Q11_4, demographics$Q11_5)
    foo11 <- foo11[!is.na(foo11)]
    
    foo12 <- with(demographics, c(Q12_1, Q12_2, Q12_3, Q12_4,  Q12_5, Q12_6))
    foo12 <- foo12[!is.na(foo12)]  
    
    foo14 <- with(demographics, c(Q14_1, Q14_2, Q14_3))
    foo14 <- foo14[!is.na(foo14)]
    
    
    output$HealthSystem <- renderUI({
      span(
        textInput(
          inputId = "HealthSystem",
          label = "1.  System Name  (Please write out your full system name, no abbreviations)",
          value = demographics$HealthSystem,
          width =  '100%',
        ),
        style = "color:red"
      )
    })
    output$Q1_DBA <- renderUI({
      textInput(
        inputId = "Q1_DBA",
        label = "1a. Doing Business as (DBA)",
        value = demographics$Q1_DBA,
        width = '100%')
    })
    output$Q2 <- renderUI({
      selectInput(
        inputId = "Q2",
        label = "2.  Health System type",
        choices = c("", "FQHC", "IDS", "Other (specify)"),
        width = '100%',
        selected <- demographics$Q2,
        selectize = FALSE)
    })
    output$Q2_other <- renderUI({
      validate(need(input$Q2, ""))
      if ("Other (specify)" %in% input$Q2) {
        textInput("Q2_other",
                  label = "Other:",
                  value = demographics$Q2_other,
                  width = "100%")
      }
    })
    output$Q3 <- renderUI({
      numericInput(
        inputId = "Q3",
        label = "3.  Total number of clinic sites in system",
        value = demographics$Q3,
        min = 0,
        max = 100,
        step = 1,
        width = '100%'
      )
    })
    output$Q4a <- renderUI({
      textInput(
        inputId = "Q4a",
        label = "4a. City",
        value = demographics$Q4a,
        placeholder = c("ex: Atlanta")
      )
    })
    output$Q4b <- renderUI({
      selectInput(
        inputId = "Q4b",
        label = "4b. State",
        choices = c("",
          "AL","AK","AZ","AR","CA","CO","CT","DC","DE",
          "FL","GA","GU","HI", "ID","IL","IN","IA","KS","KY",
          "LA","ME","MD","MA","MI","MN","MS","MO","MT",
          "NE","NV","NH","NJ","NM","NY","NC","ND","OH",
          "OK","OR","PA","PR","RI","SC","SD","TN","TX","UT",
          "VT","VA","WA","WV","WI", "WY"),
        selected = demographics$Q4b,
        selectize = F
      )
    })
    output$Q4c <- renderUI({
      textInput(
        inputId  = "Q4c",
        label = "4c. Zip code",
        value = demographics$Q4c,
        placeholder = "ex: 90210"
      )
    })   
    output$Q5 <- renderUI({
      textInput(
        inputId = "Q5",
        label = "5.  Name and title of Health System project lead",
        value = demographics$Q5,
        width = '100%'
      )
    })
    output$Q5_Email <- renderUI({
      validate(need(input$Q5, ""))
      span(
        textInput(
          inputId = "Q5_Email",
          label = "Health System project lead e-mail address",
          value = demographics$Q5_Email,
          width = "100%",
          placeholder = "name@domain.com"
        ),
        style = "color:red"
      )
    })
    output$Q6 <- renderUI({
      textInput(
        inputId = "Q6",
        label = "6.  Name and title of Health System QI lead",
        value = demographics$Q6,
        width = '100%'
      )
    })
    output$Q7 <- renderUI({
      textInput(
        inputId = "Q7",
        label = "7.  Name and title of project clinical champion",
        value = demographics$Q7,
        width = '100%'
      )
    })
    output$Q8 <- renderUI({
      selectInput(
        inputId = "Q8",
        label = "8. Select or enter the name of an ACS lead",
        choices = c("", dbReadTable(inputData, "roster")$Name),
        selected = demographics$Q8,
        selectize = F,
        width = "100%"
      )
    })
    
    
    output$Q8_Email <- renderUI({
      validate(need(input$Q8, ""))
      # Read in the staff roster data
      roster <- dbReadTable(inputData, "roster") %>%
        filter(Name == input$Q8)
      span(
        textInput(
          inputId = "Q8_Email",
          label = "8a. ACS Lead Email address",
          width = "100%",
          value = roster$Email,
        ),
        style = "color:red"
      )
    })
    output$Q9 <- renderUI({
      textInput(
        inputId = "Q9",
        label = "9.  Other ACS staff involved in project",
        value = demographics$Q9,
        width = '100%'
      )
    })
    output$Q10 <- renderUI({
      selectInput(
        inputId = "Q10",
        label = "10.  Are all clinic sites in the system participating in this project?",
        choices = c("", "No", "Yes"),
        selected = demographics$Q10,
        selectize = T
      )
    })
    output$Q10_other <- renderUI({
      validate(need(input$Q10, ""))
      if (input$Q10 == "No") {
        numericInput(
          inputId = "Q10_other",
          label = "10a. How many clinic sites are participating in this project?",
          min = 0,
          max = 100,
          value = demographics$Q10_other,
          step = 1
        )
      }
    })
    
    # Q11 - checkbox group
    output$Q11 <- renderUI({
      checkboxGroupInput(
        inputId = "Q11",
        label = p("11.  Which service lines are participating in this HPV vaccination project? ",
                  em("(check all that apply)")
        ),
        choices = c(
          "Pediatric",
          "Internal Medicine",
          "Family Medicine",
          "Dental",
          "Other (specify)"
        ),
        selected = foo11,
        inline =  TRUE
      )
    })
    output$Q11_other <- renderUI({
      validate(need(input$Q11, ""))
      if ("Other (specify)" %in% input$Q11) {
        textInput("Q11_other",
                  "Please specify",
                  value = demographics$Q11_other,
                  width = "100%")
      }
    })
    
    # Q12 - checkbox group
    output$Q12 <- renderUI({
      checkboxGroupInput(
        inputId = "Q12",
        label = p("12.  Are any of the following types of clinics participating in this HPV vaccination project? ",
                  em("(check all that apply)")
        ),
        choices = c(
          "Urban clinics",
          "Suburban clinics",
          "Rural clinics",
          "School-based clinics",
          "Mobile clinics",
          "Other (specify)"
        ),
        selected =  foo12,
        inline =  TRUE)
    })
    output$Q12_other <- renderUI({
      validate(need(input$Q12, ""))
      if ("Other (specify)" %in% input$Q12) {
        textInput(inputId = "Q12_other", 
                  label = "Please specify", 
                  value = demographics$Q12_other)
      }
    })
    output$Q13 <- renderUI({
      selectInput(
        inputId = "Q13",
        label = "13.  Does this project have any project-specific funding?",
        choices = c("", "No", "Yes"),
        selected = demographics$Q13,
        selectize = T)
    })
    output$Q13_amount <- renderUI({
      validate(need(input$Q13, ""))
      if (input$Q13 != "Yes")
        return("")
      else{
        textInput("Q13_amount",
                  "13a. Amount",
                  value = demographics$Q13_amount,
                  width = "100%")
      }
    })
    output$Q13_source <- renderUI({
      validate(need(input$Q13, ""))
      if (input$Q13 != "Yes")
        return("")
      else{
        textInput(
          "Q13_source",
          "13b. Please describe the funding source",
          value = demographics$Q13_source,
          width = "100%"
        )
      }
    })
    output$Q13_date <- renderUI({
      validate(need(input$Q13, ""))
      if (input$Q13 != "Yes")
        return()
      else{
        dateInput(
          "Q13_date",
          "13c. When did you first receive this funding?",
          width = "100%",
          format = "mm/dd/yy",
          value = as.Date(demographics$Q13_date, origin = "1970-01-01")
        )
      }
    })
    output$Q13_length <- renderUI({
      validate(need(input$Q13, ""))
      if (input$Q13 != "Yes")
        return("")
      else{
        numericInput(
          "Q13_length",
          "13d. For how many months was this funding?",
          value = demographics$Q13_length,
          min = 0,
          max = 36,
          step = 1,
          width = "100%"
        )
      }
    })
    
    # Q14 - checkbox group
    output$Q14 <- renderUI({
      checkboxGroupInput(
        inputId = "Q14",
        label = p("14.  Is this project part of a", em("(check all that apply)")),
        choices = c("ECHO", "Learning collaborative", "Other (specify)"),
        selected = foo14,
        inline = TRUE
      )
    })
    output$Q14_other <- renderUI({
      validate(need(input$Q14, ""))
      if (input$Q14 != "Other (specify)")
        return("")
      else{
        textInput(inputId = "Q14_other",
                  label = "Please specify",
                  value = demographics$Q14_other)
      }
    })
    

  
  # Demographics - error sections
  # These sections will work regardless of the previous entered data
  # So they exist outside the control structure.
  observe({
    output$error_Q3 <- renderText({
      validate(need(input$Q3,
                      "Please enter a numeric value for the total number of clinics in your health system"
        ), errorClass = "red_warnings"
        )
    })
  })
  

  output$error_Q10 <- renderUI({
    validate(need(input$Q10, ""))
    validate(need(input$Q10_other, ""))
    if (input$Q10 == "No" & input$Q10_other == 0) {
      helpText(strong("You must enter a numeric value.  If you don't know, please enter 1", style = ("color:red")))
    }
  })
  
 output$error_Q10_other <- renderUI({
   validate(need(input$Q10_other, ""))
   validate(need(input$Q10, ""))
   validate(need(input$Q3, ""))
   Q3 <- ifelse(is.na(input$Q3), 0, input$Q3)
   Q10_other <- ifelse(is.na(input$Q10_other), 0, input$Q10_other)
   condition <- ifelse(input$Q10 == "No" & Q10_other > Q3, 1, 0)
   if (condition == 1) {
     helpText(strong("The number of participating sites must be fewer than the total number of sites in the health system (Question 3)",
                     style = ("color:red")))
   }
 })
   
  observe({
    validate(need(input$Q13, ""))
    
    output$error_Q13_amount <- renderText({
      if (input$Q13=="Yes"){
      validate(need(input$Q13_amount,
                      "Please answer this question"
        ), errorClass = "red_warnings"
      )} else { return("")}
    })
    
    output$error_Q13_source <- renderText({
      if (input$Q13=="Yes"){
      validate(need(input$Q13_source,
                      "Please answer this question"
        ), errorClass = "red_warnings"
        ) } else { return("")}
    })
    
    output$error_Q13_date <- renderText({
      if (input$Q13=="Yes"){
      validate(need(input$Q13_date,
                      "Please answer this question"
        ), errorClass = "red_warnings"
        ) } else { return("")}
    })
    
    output$error_Q13_length <- renderText({
      if (input$Q13 == "Yes") {
        validate(need(input$Q13_length,
                      "Please answer this question"
        ), errorClass = "red_warnings"
        )
      } else { return("")}
    })
  })
  
  
  # Gather the data and make the temporary save before
  # Moving to the systems background section
  observeEvent(input$button_demo, {
    
    if (input$HealthSystem == "") {
      shinyalert(title = "Please enter a valid system name (Q1)", type = "warning")
     }   else if (is.na(input$Q3)) {
       shinyalert(title = "Please enter a numeric value for the total number of clinics in your health system (Q3)", type = "warning")
    } else {
      
      # Save a temporary copy of the dataset
      username <- session$user
      
      #username <- "bcarter6@me.com"
      HealthSystem  <- input$HealthSystem
      
      # Demographics section
      Q1_DBA <- input$Q1_DBA
      Q2 <- input$Q2
      Q2_other <- ifelse(is.null(input$Q2_other), "", input$Q2_other)
      Q3 <- as.numeric(input$Q3)
      Q4a <- input$Q4a
      Q4b <- input$Q4b
      Q4c <- input$Q4c
      Q5 <- input$Q5
      Q5_Email <- ifelse(is.null(input$Q5_Email), "", input$Q5_Email)
      Q6 <- input$Q6
      Q7 <- input$Q7
      Q8 <- input$Q8
      Q8_Email <- ifelse(is.null(input$Q8_Email), "", input$Q8_Email)
      Q9 <- input$Q9
      Q10 <- input$Q10
      Q10_other <- as.numeric(ifelse(is.null(input$Q10_other), NA, input$Q10_other))
      
      # Q11 - checkbox group
      Q11 <- ifelse(is.null(input$Q11), "", paste(input$Q11, collapse = ", "))
      Q11_1 <- ifelse(length(grep("Pediatric", input$Q11, fixed = T))==1, "Pediatric", NA)
      Q11_2 <- ifelse(length(grep("Internal Medicine", input$Q11, fixed = T))==1, "Internal Medicine", NA)
      Q11_3 <- ifelse(length(grep("Family Medicine", input$Q11, fixed = T))==1, "Family Medicine", NA)
      Q11_4 <- ifelse(length(grep("Dental", input$Q11, fixed = T))==1, "Dental", NA)
      Q11_5 <- ifelse(length(grep("Other (specify)", input$Q11, fixed = T))==1, "Other (specify)", NA)
      Q11_other <- ifelse(is.null(input$Q11_other), "", input$Q11_other)
      
      # Q12 - checkbox group
      Q12 <- ifelse(is.null(input$Q12), "", paste(input$Q12, collapse = ", "))
      Q12_1 <- ifelse(length(grep("Urban clinics", Q12))==1, "Urban clinics", NA)
      Q12_2 <- ifelse(length(grep("Suburban clinics", Q12))==1, "Suburban clinics", NA)
      Q12_3 <- ifelse(length(grep("Rural clinics", Q12))==1, "Rural clinics", NA)
      Q12_4 <- ifelse(length(grep("School-based clinics", Q12))==1, "School-based clinics", NA)
      Q12_5 <- ifelse(length(grep("Mobile clinics", Q12, fixed = T))==1, "Mobile clinics", NA)
      Q12_6 <- ifelse(length(grep("Other (specify)", Q12, fixed = T))==1, "Other (specify)", NA)
      Q12_other <-  ifelse(is.null(input$Q12_other), "", input$Q12_other)
      
      Q13 <- input$Q13
      Q13_amount <- ifelse(is.null(input$Q13_amount), "", input$Q13_amount)
      Q13_source <- ifelse(is.null(input$Q13_source), "", input$Q13_source)
      Q13_date <- as.Date(ifelse(is.null(input$Q13_date), NA, input$Q13_date), origin = "1970-01-01")
      Q13_length <- as.numeric(ifelse(is.null(input$Q13_length), NA, input$Q13_length))
      
      # Q14 - checkbox group
      Q14 <- ifelse(is.null(input$Q14), "", paste(input$Q14, collapse = ", "))
      Q14_1 <- ifelse(length(grep("ECHO",Q14, fixed = T))==1, "ECHO", NA)
      Q14_2 <- ifelse(length(grep("Learning collaborative",Q14, fixed = T))==1, "Learning collaborative", NA)
      Q14_3 <- ifelse(length(grep("Other (specify)",Q14, fixed = T))==1, "Other (specify)", NA)
      Q14_other <- ifelse(is.null(input$Q14_other), "", input$Q14_other)
      
      demographics <- data.frame(
        Username = username, demographicsDate = as.numeric(Sys.time()), HealthSystem,
        Q1_DBA, Q2, Q2_other, Q3,
        Q4a, Q4b, Q4c, Q5, Q5_Email,
        Q6, Q7, Q8, Q8_Email, Q9,
        Q10, Q10_other, 
        Q11, Q11_1, Q11_2, Q11_3, Q11_4, Q11_5,
        Q11_other,
        Q12,  Q12_1, Q12_2, Q12_3, Q12_4,  Q12_5,
        Q12_6, Q12_other, Q13, Q13_amount,
        Q13_source,  Q13_date, Q13_length,
        Q14, Q14_1, Q14_2, Q14_3, Q14_other, DemoComments=DemoComments)
      
      updateDB(loc = DataSrc, filename = userFilename, "demographics", demographics)
      updateTabsetPanel(session, "tabbox_baseline", selected = "p1_data")
    }

  })
  
  output$date_demo <- renderValueBox({
    if (is.na(demographics$demographicsDate)){
    valueBox(tags$p("Data entry started", style="font-size: 75%"), 
             tags$p(Sys.Date(), style="font-size: 150%;"), 
             icon=icon("horse"),
             color="blue")   
    } else {
    valueBox(tags$p("Last Submitted", style="font-size: 75%"), 
             tags$p(as.POSIXct(demographics$demographicsDate, origin = "1970-01-01"), style="font-size: 150%;"), 
             icon=icon("horse"),
             color="blue")      
    }

  })
  
  
  # Systems background ------------------------------------------------------------
  
  
  systems <- getRecentData(DataSrc, userFilename, "systems")

  SystemsComments <- systems$SystemsComments
  
    # Multi group input vectors
    foo17 <- with(systems, c(Q17_1, Q17_2, Q17_3, Q17_4, Q17_5))
    foo17 <- foo17[!is.na(foo17)]
    
    foo19 <- with(systems, c(Q19_1, Q19_2, Q19_3, Q19_4, Q19_5))
    foo19 <- foo19[!is.na(foo19)]
    
    foo25 <- with(systems, c(Q25_1, Q25_2, Q25_3, Q25_4, Q25_5))
    foo25 <- foo25[!is.na(foo25)]
    
    foo26 <- with(systems, c(Q26_1, Q26_2, Q26_3, Q26_4, Q26_5, Q26_6, Q26_7, Q26_8, Q26_9))
    foo26 <- foo26[!is.na(foo26)]
    
    output$Q15 <- renderUI({
      selectInput(
        inputId = "Q15",
        label = "15.  EHR System",
        choices =c("",
                   "AIIScripts",
                   "Athena",
                   "Cerner",
                   "eClinicalWorks",
                   "Epic",
                   "GE Centricity",
                   "Greenway",
                   "Indian Health Services",
                   "MEDITECH",
                   "NextGen",
                   "Other (specify)"),
        selected = systems$Q15,
        selectize =  FALSE,
        width = '100%')
    })
    output$Q15_other <- renderUI({
      if ("Other (specify)" %in% input$Q15) {
        textInput("Q15_other",
                  "Please specify",
                  value = systems$Q15_other,
                  width = '100%')
      }
    })
     output$Q15_EHRversion <- renderUI({
        textInput(inputId = "Q15_EHRversion",
                  label = "15a. EHR version",
                  value = systems$Q15_EHRversion,
                  width = '100%')
    })
    output$Q16 <- renderUI({
      textInput(
        inputId = "Q16",
        label = "16.  Population Management system and version",
        value = systems$Q16,
        width = '100%')
    })
    
    # Q17 - multiCheckBox
    
    output$Q17 <- renderUI({
      checkboxGroupInput(
        inputId = "Q17",
        label = p("17.  Our system is currently set up to: ",
                  em("(check all that apply)")),
        choices = c("Automatically determine which immunizations are due for each patient at every visit",
                    "Alert providers that a patient is due for HPV vaccination",
                    "Provide a report of patients who are not up-to-date on HPV vaccination or have not completed all doses",
                    "Provide a report with provider-specific HPV vaccination rates",
                    "Provide a missed opportunity report that identifies patients who had an appointment, were due for HPV vaccination, but did not receive a vaccine dose"),
        selected = foo17,
        width = "100%")
    })
    output$Q18 <- renderUI({
      selectInput(
        inputId = "Q18",
        label = "18.  At this moment, our EHR system has:",
        choices = c("",
                    "Bidirectional interface with the State Immunization Registry",
                    "Unidirectional interface with the State Immunization Registry",
                    "No interface with the State Immunization Registry"),
        selected = systems$Q18,
        width = "100%")
    })
    
    # Q19 - multiCheckBox
    output$Q19 <- renderUI({
      checkboxGroupInput(
        inputId = "Q19",
        label = p("19.  At this moment, our State Immunization Registry: ",
                  em("(check all that apply)")),
        choices = c("Has current and accurate data",
                    "Is used daily to verify patient vaccination status",
                    "Provides data we use to track HPV vaccination rates",
                    "Is not useful to our HPV vaccination work",
                    "May have functions we could use, but have not explored"),
        selected = foo19,
        width = '100%')
    })
    output$Q20 <- renderUI({
      selectInput(
        inputId = "Q20",
        label = "20.  Does your system have standing orders for HPV vaccination?",
        choices = c("", "Yes", "No"),
        selected = systems$Q20)
    })
    output$Q20_notes <- renderUI({
      validate(need(input$Q20, ""))
      if (input$Q20 == "No")
        return(NULL)
      else{
        textAreaInput(inputId =  "Q20_notes",
                      label =   "20a. Please indicate who vaccinates besides a provider, how often, and whether a 
                                copy of standing orders is available for review",
                      placeholder = "",
                      value = systems$Q20_notes,
                      rows = 4
        )
      }
    })
    output$Q20_orders <- renderUI({
      validate(need(input$Q20, ""))
      if (input$Q20 == "No")
        return(NULL)
      else{
        selectInput(inputId = "Q20_orders",
                    label =  "20b. Are standing orders implemented fully and systematically?",
                    choices = c("", "Yes", "No"),
                    selected = systems$Q20_orders)
      }
    })
    output$Q20_other <- renderUI({
      validate(need(input$Q20_orders, ""))
      if (input$Q20_orders == "No") {
        textAreaInput(inputId = "Q20_other",
                      label = "20c. Why haven't standing orders been implemented fully and systematically?",
                      rows = 4,
                      value = systems$Q20_other)
      } else
        return(NULL)
    })
    
    output$Q21 <-   renderUI({
      selectInput(inputId = "Q21",
                  label = "21.  Are patients scheduled for the next HPV vaccination visit before they leave the office?",
                  choices = c("", "Yes", "No"),
                  selected = systems$Q21)
    })
    output$Q21_text <- renderUI({
      validate(need(input$Q21, ""))
      if (input$Q21 != "No")
        return(NULL)
      else{
        textAreaInput(inputId = "Q21_text",
                      label = "21a. Why not?",
                      value = systems$Q21_text,
                      rows = 4)
      }
    })
    
    output$Q22 <- renderUI({
      selectInput(inputId = "Q22",
                  label = "22.  If the HPV vaccine is declined, is it system policy to recommend it again at future visits?",
                  choices = c("", "Yes", "No"),
                  selected = systems$Q22)
    })
    output$Q22_notes <- renderUI({
      validate(need(input$Q22, ""))
      if (input$Q22 != "No") {
        return(NULL)
      } else {
        textAreaInput(inputId = "Q22_notes", 
                      label = "22a. Why isn't this the current policy?",
                      value = systems$Q22_notes,
                      rows = 4)
      }
    })
    output$Q23 <- renderUI({
      selectInput(inputId = "Q23",
                  label = "23.  Has your system used quality improvement to increase HPV vaccination rates in the past?",
                  choices = c("", "Yes", "No"),
                  selected = systems$Q23)
    })
    output$Q23_notes <- renderUI({
      validate(need(input$Q23, ""))
      if (input$Q23 == "No") {
        return(NULL)
      } else {
        textAreaInput(inputId =  "Q23_notes",
                      label = "23a. Please describe your system's quality improvement efforts",
                      value = systems$Q23_notes,
                      rows = 4)
      }
    })
    
    output$Q24 <- renderUI({
      selectInput(inputId = "Q24",
                  label = "24.  Has your system completed a HPV VACs Systems and Strategies Inventory (SASI) with ACS in the past?",
                  choices = c("", "Yes", "No"),
                  selected = systems$Q24)
    })
    output$Q24_years <- renderUI({
      validate(need(input$Q24, ""))
      if (input$Q24 == "No") {
        return(NULL)
      } else {
        numericInput( inputId = "Q24_years",
                      label = p("24a. If yes, how many years has", strong("your system"), "previously participated in a VACs QI project and submitted a SASI?"),
                      value = systems$Q24_years,
                      min = 1,
                      max = 5)
      }
    })
    
    # Q25 checkboxGroup
    output$Q25 <- renderUI({
      checkboxGroupInput(inputId = "Q25",
                         label = p("25.  Which of the following ",
                                   strong("training and education activities ", style = "color:blue"),
                                   "has your system already conducted to increase HPV vaccination rates? ",
                                   em("(With or without support from ACS)."), " Select all that apply"),
                         choices = c("Educated staff on HPV vaccination as cancer prevention",
                                     "Educated staff on strategies to improve HPV vaccination rates",
                                     "Identified HPV vaccination champions",
                                     "Trained providers on making an effective HPV vaccine recommendation",
                                     "Other (specify)"),
                         selected = foo25,
                         width = "100%")
    })
    output$Q25_other <- renderUI({
      validate(need(input$Q25, ""))
      if ("Other (specify)" %in% input$Q25) {
        textInput(inputId = "Q25_other",
                  label = "Please specify",
                  value = systems$Q25_other,
                  width = "100%")
      }
    })
    
    # Q26 checkboxGroup
    output$Q26 <- renderUI({
      checkboxGroupInput(inputId = "Q26",
                         label = p( "26.  Which of the following ",
                                    strong("interventions ", style = "color:blue"),
                                    "to increase HPV vaccination has your system already implemented? ",
                                    em("(With or without support from ACS)."), " Select all that apply"),
                         choices = c("Client reminders",
                                     "Extended hours",
                                     "Modified EHR",
                                     "Offered in alternative settings like schools or mobile units",
                                     "Parent/patient education",
                                     "Provider assessment & feedback",
                                     "Provider prompts/reminders",
                                     "Standing orders",
                                     "Other (specify)"),
                         selected =  foo26,
                         width = '100%')
    })
    output$Q26_other <- renderUI({
      validate(need(input$Q26, ""))
      if ("Other (specify)" %in% input$Q26) {
        textInput(inputId = "Q26_other",
                  label = "Please specify",
                  value = systems$Q26_other,
                  width = "100%")
      }
    })
    
  observeEvent(input$button_systems, {
    
 
    username <- session$user
    
    # Data systems
    Q15 <- input$Q15
    Q15_other <- ifelse(is.null(input$Q15_other), "", input$Q15_other)
    Q15_EHRversion <- input$Q15_EHRversion
    Q16 <- input$Q16
    
    
    # Q17 - multigroup
    Q17 <-  ifelse(is.null(input$Q17), "", paste(input$Q17, collapse = ", "))
    answers <- c("Automatically determine which immunizations are due for each patient at every visit",
                 "Alert providers that a patient is due for HPV vaccination",
                 "Provide a report of patients who are not up-to-date on HPV vaccination or have not completed all doses",
                 "Provide a report with provider-specific HPV vaccination rates",
                 "Provide a missed opportunity report that identifies patients who had an appointment, were due for HPV vaccination, but did not receive a vaccine dose")
    Q17_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q17, fixed = T)) == 1, x, NA)
    })
    Q17_1 <- as.character(Q17_responses[1])
    Q17_2 <- as.character(Q17_responses[2])
    Q17_3 <- as.character(Q17_responses[3])
    Q17_4 <- as.character(Q17_responses[4])
    Q17_5 <- as.character(Q17_responses[5])
    rm(Q17_responses, answers)
    
    Q18 <- input$Q18
    
    # Q19 - multigroup
    Q19 <- ifelse(is.null(input$Q19), "", paste(input$Q19, collapse = ", "))
    answers <- c("Has current and accurate data",
                 "Is used daily to verify patient vaccination status",
                 "Provides data we use to track HPV vaccination rates",
                 "Is not useful to our HPV vaccination work",
                 "May have functions we could use, but have not explored")
    Q19_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q19, fixed = T)) == 1, x, NA)
    })
    Q19_1 <- as.character(Q19_responses[1])
    Q19_2 <- as.character(Q19_responses[2])
    Q19_3 <- as.character(Q19_responses[3])
    Q19_4 <- as.character(Q19_responses[4])
    Q19_5 <- as.character(Q19_responses[5])
    rm(Q19_responses, answers)
    
    
    Q20 <- ifelse(is.null(input$Q20), "", input$Q20)
    Q20_notes <- ifelse(is.null(input$Q20_notes), "", input$Q20_notes)
    Q20_orders <-ifelse(is.null(input$Q20_orders), "", input$Q20_orders)
    Q20_other <- ifelse(is.null(input$Q20_other), "", input$Q20_other)
    Q21 <- ifelse(is.null(input$Q21), "", input$Q21)
    Q21_text <- ifelse(is.null(input$Q21_text), "", input$Q21_text)
    Q22 <- ifelse(is.null(input$Q22), "", input$Q22)
    Q22_notes <-  ifelse(is.null(input$Q22_notes), "", input$Q22_notes)
    Q23 <- ifelse(is.null(input$Q23), "", input$Q23)
    Q23_notes <- ifelse(is.null(input$Q23_notes), "", input$Q23_notes)
    Q24 <- ifelse(is.null(input$Q24), "", input$Q24)
    Q24_years <- as.numeric(ifelse(is.null(input$Q24_years), NA, input$Q24_years))
    
    # Q25 - multigroup
    Q25 <- ifelse(is.null(input$Q25), "", paste(input$Q25, collapse = ", "))
    answers <-  c("Educated staff on HPV vaccination as cancer prevention",
                  "Educated staff on strategies to improve HPV vaccination rates",
                  "Identified HPV vaccination champions",
                  "Trained providers on making an effective HPV vaccine recommendation",
                  "Other (specify)")
    Q25_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q25, fixed = T)) == 1, x, NA)
    })
    Q25_1 <- as.character(Q25_responses[1])
    Q25_2 <- as.character(Q25_responses[2])
    Q25_3 <- as.character(Q25_responses[3])
    Q25_4 <- as.character(Q25_responses[4])
    Q25_5 <- as.character(Q25_responses[5])
    rm(Q25_responses, answers)
    Q25_other <- ifelse(is.null(input$Q25_other), "", input$Q25_other)
    
    
    Q26 <- ifelse(is.null(input$Q26), "", paste(input$Q26, collapse = ", "))
    answers <-c("Client reminders",
                "Extended hours",
                "Modified EHR",
                "Offered in alternative settings like schools or mobile units",
                "Parent/patient education",
                "Provider assessment & feedback",
                "Provider prompts/reminders",
                "Standing orders",
                "Other (specify)")
    Q26_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q26, fixed = T)) == 1, x, NA) %>%
        as.character()
    })
    Q26_1 <- as.character(Q26_responses[1])
    Q26_2 <- as.character(Q26_responses[2])
    Q26_3 <- as.character(Q26_responses[3])
    Q26_4 <- as.character(Q26_responses[4])
    Q26_5 <- as.character(Q26_responses[5])
    Q26_6 <- as.character(Q26_responses[6])
    Q26_7 <- as.character(Q26_responses[7])
    Q26_8 <- as.character(Q26_responses[8])
    Q26_9 <- as.character(Q26_responses[9])
    rm(Q26_responses, answers)
    Q26_other <- ifelse(is.null(input$Q26_other), "", input$Q26_other)
    
    systems <- data.frame(Username = username, systemsDate = as.numeric(Sys.time()), Q15, Q15_other, Q15_EHRversion, Q16, Q17, Q17_1, Q17_2, Q17_3, Q17_4, Q17_5, 
                           Q18, Q19, Q19_1, Q19_2, Q19_3, Q19_4, Q19_5, Q20, Q20_notes, 
                           Q20_orders, Q20_other, Q21, Q21_text, Q22, Q22_notes, Q23, Q23_notes, 
                           Q24, Q24_years, Q25, Q25_1, Q25_2, Q25_3, Q25_4, Q25_5, Q25_other, Q26, 
                           Q26_1, Q26_2, Q26_3, Q26_4, Q26_5, Q26_6, Q26_7, Q26_8, Q26_9, Q26_other, SystemsComments=SystemsComments)
    updateDB(DataSrc, userFilename, "systems", systems)
    updateTabsetPanel(session, "tabbox_baseline", selected = "p1_rates")
    #}
  })
  
  # Show the last updated date
    output$date_systems<- renderValueBox({
      if (is.na(systems$systemsDate)){
        valueBox(tags$p("Data entry started", style="font-size: 75%"), 
tags$p(Sys.Date(), style="font-size: 150%;"),
                 icon=icon("horse"),
                 color="blue")   
      } else {
     valueBox(tags$p("Last Submitted", style="font-size: 75%"), 
              tags$p(as.POSIXct(systems$systemsDate, origin = "1970-01-01"), style="font-size: 150%;"), 
             icon=icon("horse"),
             color="blue")        
      }

  })
  
  
  # Rates page  -------------------------------------------------------------
  
  savedRates <- getRecentData(DataSrc, userFilename, "savedRates")
    RatesComments <- savedRates$RatesComments
    
    # Checkbox groups
    foo28 <- with(savedRates, c(Q28_1, Q28_2, Q28_3, Q28_4))
    foo28 <- foo28[!is.na(foo28)]
    
    foo32 <- with(savedRates, c(Q32_details1, Q32_details2, Q32_details3, Q32_details4))
    foo32 <- foo32[!is.na(foo32)]
    
    output$Q27 <- renderUI({
      selectInput(
        inputId = "Q27",
        label = "27.  How are you defining your active patient populations?",
        choices = c("",
                    "12 months",
                    "18 months",
                    "24 months",
                    "Other (please specify)"),
        selected = savedRates$Q27,
        selectize = F
      )
    })
    output$Q27_other <- renderUI({
      validate(need(input$Q27, ""))
      if (input$Q27 != "Other (please specify)") {
        return(NULL)
      } else {
        textInput(
          inputId = "Q27_other",
          label = "Please specify",
          value = savedRates$Q27_other,
          placeholder = "",
          width = "150%")
      }
    })
    
    
    # Q28 - CheckBoxGroup
    output$Q28 <- renderUI({
      checkboxGroupInput(
        inputId = "Q28",
        label = "28. For what ages are you reporting?",
        choices = c("9-10",
                    "11-12",
                    "13",
                    "We can't report on these ages (other, please specify)"),
        selected = foo28)
    })
    output$Q28_other <- renderUI({
      validate(need(input$Q28, ""))
      if (("We can't report on these ages (other, please specify)" %in% input$Q28) &
          (!c("9-10", "11-12", "13") %in% input$Q28)) {
        textAreaInput(
          inputId = "Q28_other",
          label = "Please specify",
          value = savedRates$Q28_other,
          placeholder = "",
          height = "100px")
      } else {
        return(NULL)
      }
    })
    
    # Include message about how they should contact Jennifer to resolve this issue
    output$Q28_other_message <- renderUI({
      validate(need(input$Q28, ""))
      if (("We can't report on these ages (other, please specify)" %in% input$Q28) &
          (!c("9-10", "11-12", "13") %in% input$Q28)) {
        HTML("<br>")
        helpText("Please contact Jennifer Isher-Witt at Jennifer.Ish@cancer.org to resolve this issue",
                  style = "color:red")
    }
    })
    
    output$Q29 <- renderUI({
      selectInput(
        inputId = "Q29",
        label = "29.  Can you pull rate data by sex?",
        choices = c("",
                    "Yes, we'll report separate data for male and female patients",
                    "No, we will have combined rate data"),
        selected = savedRates$Q29)
    })


    # This is how you get rid of the boxes
    observe({
      x <- input$Q28
      if (!"9-10" %in% x) {
        output$brates_9_10 <- renderUI(NULL)
      }
      if (!"11-12" %in% x) {
        output$brates_11_12 <- renderUI(NULL)
      }
      if (!"13" %in% x) {
        output$brates_13 <- renderUI(NULL)
      }

    })

    # Create the ages 9-10 boxes
    observeEvent(list(input$Q28, input$Q29), {
      validate(need(input$Q28, ""))
      validate(need(input$Q29, ""))
      

      # Boys and girls - ages 9-10
      if ("9-10" %in% input$Q28 &
          input$Q29 == "Yes, we'll report separate data for male and female patients") {
        
        # Girls ages 9-10 rates 
        output$brates_9_10 <- renderUI({
          verticalLayout(
            splitLayout(
          box(
            h3("Vaccination Rates for Female Patients, Ages 9-10"),
            title = "Vaccination Rates for Female Patients, Ages 9-10",
            collapsible = T,
            solidHeader = T,
            status = "primary",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("female ", style = "color:blue"),
                "medical patients ",
                em("ages 9-10 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "FemAge1_total",
              label = NULL,
              value = savedRates$FemAge1_total,
              min = 0,
              step = 1,
              width = '50%'
            ),
            uiOutput("error_f9_total"),
            #textOutput("error_f9_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge1_dose1",
                label = NULL,
                min = 0,
                value = savedRates$FemAge1_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge1_dose1_rate")
            ),
            uiOutput("error_f9_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge1_dose2",
                label = NULL,
                min = 0,
                value = savedRates$FemAge1_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge1_dose2_rate")
            ),
            uiOutput("error_f9_2plus"),
            uiOutput("error_f9a_2plus")
          ),
          box( # Men aged 9-10
            h3("Males, ages 9-10"),
            title = "Males, ages 9-10",
            collapsible = T,
            solidHeader = T,
            status = "danger",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("male ", style = "color:blue"),
                "medical patients ",
                em("ages 9-10 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "MenAge1_total",
              label = NULL,
              value = savedRates$MenAge1_total,
              min = 0,
              step = 1,
              width = '50%'
            ),
            uiOutput("error_m9_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge1_dose1",
                label = NULL,
                min = 0,
                value =  savedRates$MenAge1_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge1_dose1_rate")
            ),
            uiOutput("error_m9_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge1_dose2",
                label = NULL,
                min = 0,
                value =  savedRates$MenAge1_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge1_dose2_rate")
            ),
            uiOutput("error_m9_2plus"),
            uiOutput("error_m9a_2plus")
          )
            )) # end split and veritical layout
        })
        
        output$error_f9_total <- renderUI({
          if (is.na(input$FemAge1_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_f9_gt1 <- renderUI({
          if (is.na(input$FemAge1_dose1) | is.na(input$FemAge1_total)) {
            return(NULL)
          } else if (input$FemAge1_dose1 > input$FemAge1_total) {
              helpText("This number can't be greater than the number of patients seen",
                       style = "color:red")
            }
        })
        output$error_f9_2plus <- renderUI({if (is.na(input$FemAge1_dose2) | is.na(input$FemAge1_total)) {
          return(NULL)
        } else if (input$FemAge1_dose2 > input$FemAge1_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f9a_2plus <- renderUI({
          if (is.na(input$FemAge1_dose2) | is.na(input$FemAge1_dose1)) {
          return(NULL)
        } else if (input$FemAge1_dose2 >= input$FemAge1_dose1){
              helpText("This number can't be greater than the total number of patients who received the first dose",
                       style = "color:red")
        }
        })
        output$FemAge1_dose1_rate <- renderText({
          validate(need(input$FemAge1_total, ""))
          if (is.na(input$FemAge1_dose1)) {
            return(NULL)
          }
          if (!is.na(input$FemAge1_dose1)) {
            rate <- input$FemAge1_dose1 / input$FemAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        output$FemAge1_dose2_rate <- renderText({
          validate(need(input$FemAge1_total, ""))
          if (is.na(input$FemAge1_dose2)) {
            return(NULL)
          }
          if (!is.na(input$FemAge1_dose2)) {
            rate <- input$FemAge1_dose2 / input$FemAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })

        output$error_m9_total <- renderUI({
          if (is.na(input$MenAge1_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_m9_gt1 <- renderUI({
          if (is.na(input$MenAge1_dose1) | is.na(input$MenAge1_total)) {
            return(NULL)
          } else if (input$MenAge1_dose1 > input$MenAge1_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m9_2plus <- renderUI({if (is.na(input$MenAge1_dose2) | is.na(input$MenAge1_total)) {
          return(NULL)
        } else if (input$MenAge1_dose2 > input$MenAge1_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_m9a_2plus <- renderUI({
          if (is.na(input$MenAge1_dose2) | is.na(input$MenAge1_dose1)) {
            return(NULL)
          } else if (input$MenAge1_dose2 >= input$MenAge1_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$MenAge1_dose1_rate <- renderText({
          validate(need(input$MenAge1_total, ""))
          if (is.na(input$MenAge1_dose1)) {
            return(NULL)
          }
          if (!is.na(input$MenAge1_dose1)) {
            rate <- input$MenAge1_dose1 / input$MenAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        output$MenAge1_dose2_rate <- renderText({
          validate(need(input$MenAge1_total, ""))
          if (is.na(input$MenAge1_dose2)) {
            return(NULL)
          }
          if (!is.na(input$MenAge1_dose2)) {
            rate <- input$MenAge1_dose2 / input$MenAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
      } 
      
      # Ages 9-10, combined
      if ("9-10" %in% input$Q28 &
          input$Q29 == "No, we will have combined rate data") {
        # Create the 'male and females combined' box
        output$brates_9_10 <- renderUI({
          box(
            h3("Vaccination Rates for Male and Female Patients Combined, Ages 9-10"),
            title = "Vaccination Rates for Male and Female Patients Combined, Ages 9-10",
            collapsible = T,
            solidHeader = T,
            status = "success",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active medical patients ",
                em("ages 9-10 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "BothAge1_total",
              label = NULL,
              value = savedRates$BothAge1_total,
              min = 0,
              step = 1,
              width = '50%'
            ),
            uiOutput("error_9_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge1_dose1",
                label = NULL,
                min = 0,
                value = savedRates$BothAge1_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge1_dose1_rate")
            ),
            uiOutput("error_9_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(inputId = "BothAge1_dose2",
                           label = NULL,
                           min = 0,
                           value = savedRates$BothAge1_dose2,
                           step = 1,
                           width = '50%'
              ),
              textOutput("BothAge1_dose2_rate")
            ),
            uiOutput("error_9_2plus"),
            uiOutput("error_9a_2plus")
          )
        })
        output$BothAge1_dose1_rate <- renderText({
          validate(need(input$BothAge1_total, ""))
          if (is.na(input$BothAge1_dose1)) {
            return(NULL)
          }
          if (!is.na(input$BothAge1_dose1)) {
            rate <- input$BothAge1_dose1 / input$BothAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        output$BothAge1_dose2_rate <- renderText({
          validate(need(input$BothAge1_total, ""))
          if (is.na(input$BothAge1_dose2)) {
            return(NULL)
          }
          if (!is.na(input$BothAge1_dose2)) {
            rate <- input$BothAge1_dose2 / input$BothAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        
        output$error_9_total <- renderUI({
          if (is.na(input$BothAge1_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_9_gt1 <- renderUI({
          if (is.na(input$BothAge1_dose1) | is.na(input$BothAge1_total)) {
            return(NULL)
          } else if (input$BothAge1_dose1 > input$BothAge1_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_9_2plus <- renderUI({if (is.na(input$BothAge1_dose2) | is.na(input$BothAge1_total)) {
          return(NULL)
        } else if (input$BothAge1_dose2 > input$BothAge1_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_9a_2plus <- renderUI({
          if (is.na(input$BothAge1_dose2) | is.na(input$BothAge1_dose1)) {
            return(NULL)
          } else if (input$BothAge1_dose2 >= input$BothAge1_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        
      }
      
    }) # end the 9-10 observation
    
    # Create the ages 11-12 boxes
    observeEvent(list(input$Q28, input$Q29), {
      validate(need(input$Q28, ""))
      validate(need(input$Q29, ""))
      
      if ("11-12" %in% input$Q28 &
          input$Q29 == "Yes, we'll report separate data for male and female patients") {
        
        # Girls ages 11-12
        output$brates_11_12 <- renderUI({
          verticalLayout(
            splitLayout(
          box(
            h3("Vaccination Rates for Females Patients, Ages 11-12"),
            title = "Vaccination Rates for Female Patients, Ages 11-12",
            collapsible = T,
            solidHeader = T,
            status = "primary",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("female ", style = "color:blue"),
                "medical patients ",
                em("ages 11-12 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "FemAge2_total",
              label = NULL,
              value = savedRates$FemAge2_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_f11_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge2_dose1",
                label = NULL,
                min = 0,
                value = savedRates$FemAge2_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge2_dose1_rate")
            ),
            uiOutput("error_f11_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge2_dose2",
                label = NULL,
                min = 0,
                value = savedRates$FemAge2_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge2_dose2_rate")
            ),
            uiOutput("error_f11_2plus"),
            uiOutput("error_f11a_2plus"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge2_mening",
                label = NULL,
                min = 0,
                value = savedRates$FemAge2_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge2_mening_rate")
            ),
            uiOutput("error_f11_mening"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge2_tdap",
                label = NULL,
                min = 0,
                value = savedRates$FemAge2_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge2_tdap_rate")
            ),
            uiOutput("error_f11_Tdap"),
          ),
          box( # males aged 11-12
            h3("Males, ages 11-12"),
            title = "Males, ages 11-12",
            collapsible = T,
            solidHeader = T,
            status = "danger",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("male ", style = "color:blue"),
                "medical patients ",
                em("ages 11-12 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "MenAge2_total",
              label = NULL,
              value = savedRates$MenAge2_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_m11_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge2_dose1",
                label = NULL,
                min = 0,
                value = savedRates$MenAge2_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge2_dose1_rate")
            ),
            uiOutput("error_m11_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge2_dose2",
                label = NULL,
                min = 0,
                value = savedRates$MenAge2_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge2_dose2_rate")
            ),
            uiOutput("error_m11_2plus"),
            uiOutput("error_m11a_2plus"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge2_mening",
                label = NULL,
                min = 0,
                value = savedRates$MenAge2_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge2_mening_rate")
            ),
            uiOutput("error_m11_mening"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge2_tdap",
                label = NULL,
                min = 0,
                value = savedRates$MenAge2_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge2_tdap_rate")
            ),
            uiOutput("error_m11_Tdap"),
          )
            )) # end split and vertical layout
        })
        output$FemAge2_dose1_rate <- renderText({
          validate(need(input$FemAge2_total, ""))
          if (is.na(input$FemAge2_dose1)) {
            return(NULL)
          }
          if (!is.na(input$FemAge2_dose1)) {
            rate <- input$FemAge2_dose1 / input$FemAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemAge2_dose2_rate <- renderText({
          validate(need(input$FemAge2_total, ""))
          if (is.na(input$FemAge2_dose2)) {
            return(NULL)
          }
          if (!is.na(input$FemAge2_dose2)) {
            rate <- input$FemAge2_dose2 / input$FemAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemAge2_mening_rate <- renderText({
          validate(need(input$FemAge2_total, ""))
          if (is.na(input$FemAge2_mening)) {
            return(NULL)
          }
          if (!is.na(input$FemAge2_mening)) {
            rate <- input$FemAge2_mening / input$FemAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemAge2_tdap_rate <- renderText({
          validate(need(input$FemAge2_total, ""))
          if (is.na(input$FemAge2_tdap)) {
            return(NULL)
          }
          if (!is.na(input$FemAge2_tdap)) {
            rate <- input$FemAge2_tdap / input$FemAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        output$error_f11_total <- renderUI({
          if (is.na(input$FemAge2_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_f11_gt1 <- renderUI({
          if (is.na(input$FemAge2_dose1) | is.na(input$FemAge2_total)) {
            return(NULL)
          } else if (input$FemAge2_dose1 > input$FemAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f11_2plus <- renderUI({if (is.na(input$FemAge2_dose2) | is.na(input$FemAge2_total)) {
          return(NULL)
        } else if (input$FemAge2_dose2 > input$FemAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_f11a_2plus <- renderUI({
          if (is.na(input$FemAge2_dose2) | is.na(input$FemAge2_dose1)) {
            return(NULL)
          } else if (input$FemAge2_dose2 >= input$FemAge2_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_f11_mening <- renderUI({
          if (is.na(input$FemAge2_mening) | is.na(input$FemAge2_total)) {
            return(NULL)
          } else if (input$FemAge2_mening > input$FemAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f11_Tdap <- renderUI({
          if (is.na(input$FemAge2_tdap) | is.na(input$FemAge2_total)) {
            return(NULL)
          } else if (input$FemAge2_tdap > input$FemAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
        output$MenAge2_dose1_rate <- renderText({
          validate(need(input$MenAge2_total, ""))
          if (is.na(input$MenAge2_dose1)) {
            return(NULL)
          }
          if (!is.na(input$MenAge2_dose1)) {
            rate <- input$MenAge2_dose1 / input$MenAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenAge2_dose2_rate <- renderText({
          validate(need(input$MenAge2_total, ""))
          if (is.na(input$MenAge2_dose2)) {
            return(NULL)
          }
          if (!is.na(input$MenAge2_dose2)) {
            rate <- input$MenAge2_dose2 / input$MenAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenAge2_mening_rate <- renderText({
          validate(need(input$MenAge2_total, ""))
          if (is.na(input$MenAge2_mening)) {
            return(NULL)
          }
          if (!is.na(input$MenAge2_mening)) {
            rate <- input$MenAge2_mening / input$MenAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenAge2_Tdap_rate <- renderText({
          validate(need(input$MenAge2_total, ""))
          if (is.na(input$MenAge2_tdap)) {
            return(NULL)
          }
          if (!is.na(input$MenAge2_tdap)) {
            rate <- input$MenAge2_tdap / input$MenAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        output$error_m11_total <- renderUI({
          if (is.na(input$MenAge2_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_m11_gt1 <- renderUI({
          if (is.na(input$MenAge2_dose1) | is.na(input$MenAge2_total)) {
            return(NULL)
          } else if (input$MenAge2_dose1 > input$MenAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m11_2plus <- renderUI({if (is.na(input$MenAge2_dose2) | is.na(input$MenAge2_total)) {
          return(NULL)
        } else if (input$MenAge2_dose2 > input$MenAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_m11a_2plus <- renderUI({
          if (is.na(input$MenAge2_dose2) | is.na(input$MenAge2_dose1)) {
            return(NULL)
          } else if (input$MenAge2_dose2 >= input$MenAge2_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_m11_mening <- renderUI({
          if (is.na(input$MenAge2_mening) | is.na(input$MenAge2_total)) {
            return(NULL)
          } else if (input$MenAge2_mening > input$MenAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m11_Tdap <- renderUI({
          if (is.na(input$MenAge2_tdap) | is.na(input$MenAge2_total)) {
            return(NULL)
          } else if (input$MenAge2_tdap > input$MenAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
      }

      
      # Ages 11-12 - boys and girls combined
      if ("11-12" %in% input$Q28 &
          input$Q29 == "No, we will have combined rate data") {
        output$brates_11_12 <- renderUI({
          box(
            h3("Vaccination Rates for Male and Female Patients Combined, Ages 11-12"),
            title = "Vaccination Rates for Male and Female Patients Combined, Ages 11-12",
            collapsible = T,
            solidHeader = T,
            status = "success",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active medical patients ",
                em("ages 11-12 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "BothAge2_total",
              label = NULL,
              value = savedRates$BothAge2_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_11_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge2_dose1",
                label = NULL,
                min = 0,
                value = savedRates$BothAge2_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge2_dose1_rate")
            ),
            uiOutput("error_11_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge2_dose2",
                label = NULL,
                min = 0,
                value = savedRates$BothAge2_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge2_dose2_rate")
            ),
            uiOutput("error_11_2plus"),
            uiOutput("error_11a_2plus"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge2_mening",
                label = NULL,
                min = 0,
                value = savedRates$BothAge2_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge2_mening_rate")
            ),
            uiOutput("error_11_mening"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge2_tdap",
                label = NULL,
                min = 0,
                value = savedRates$BothAge2_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge2_tdap_rate")
            ),
            uiOutput("error_11_Tdap"),
          )
        })
        output$BothAge2_dose1_rate <- renderText({
          validate(need(input$BothAge2_total, ""))
          if (is.na(input$BothAge2_dose1)) {
            return(NULL)
          }
          if (!is.na(input$BothAge2_dose1)) {
            rate <- input$BothAge2_dose1 / input$BothAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothAge2_dose2_rate <- renderText({
          validate(need(input$BothAge2_total, ""))
          if (is.na(input$BothAge2_dose2)) {
            return(NULL)
          }
          if (!is.na(input$BothAge2_dose2)) {
            rate <- input$BothAge2_dose2 / input$BothAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothAge2_mening_rate <- renderText({
          validate(need(input$BothAge2_total, ""))
          if (is.na(input$BothAge2_mening)) {
            return(NULL)
          }
          if (!is.na(input$BothAge2_mening)) {
            rate <- input$BothAge2_mening / input$BothAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothAge2_tdap_rate <- renderText({
          validate(need(input$BothAge2_total, ""))
          if (is.na(input$BothAge2_tdap)) {
            return(NULL)
          }
          if (!is.na(input$BothAge2_tdap)) {
            rate <- input$BothAge2_tdap / input$BothAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })

        output$error_11_total <- renderUI({
          if (is.na(input$BothAge2_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_11_gt1 <- renderUI({
          if (is.na(input$BothAge2_dose1) | is.na(input$BothAge2_total)) {
            return(NULL)
          } else if (input$BothAge2_dose1 > input$BothAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_11_2plus <- renderUI({if (is.na(input$BothAge2_dose2) | is.na(input$BothAge2_total)) {
          return(NULL)
        } else if (input$BothAge2_dose2 > input$BothAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_11a_2plus <- renderUI({
          if (is.na(input$BothAge2_dose2) | is.na(input$BothAge2_dose1)) {
            return(NULL)
          } else if (input$BothAge2_dose2 >= input$BothAge2_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_11_mening <- renderUI({
          if (is.na(input$BothAge2_mening) | is.na(input$BothAge2_total)) {
            return(NULL)
          } else if (input$BothAge2_mening > input$BothAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_11_Tdap <- renderUI({
          if (is.na(input$BothAge2_tdap) | is.na(input$BothAge2_total)) {
            return(NULL)
          } else if (input$BothAge2_tdap > input$BothAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
        
      }
      
    }) # end the 11-12 observation
    
    # Age 13 boxes
    observeEvent(list(input$Q28, input$Q29), {
      validate(need(input$Q28, ""))
      validate(need(input$Q29, ""))
      
      if ("13" %in% input$Q28 &
          input$Q29 == "Yes, we'll report separate data for male and female patients") {
        
        # Girls age 13
        output$brates_13 <- renderUI({
          verticalLayout(
            splitLayout(
          box(
            h3("Vaccination Rates for Female Patients, Age 13"),
            title = "Vaccination Rates for Female Patients, Age 13",
            collapsible = T,
            solidHeader = T,
            status = "primary",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("female ", style = "color:blue"),
                "medical patients ",
                em("age 13 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "FemAge3_total",
              label = NULL,
              value = savedRates$FemAge3_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_f13_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge3_dose1",
                label = NULL,
                min = 0,
                value = savedRates$FemAge3_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge3_dose1_rate")
            ),
            uiOutput("error_f13_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge3_dose2",
                label = NULL,
                min = 0,
                value = savedRates$FemAge3_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge3_dose2_rate")
            ),
            uiOutput("error_f13_2plus"),
            uiOutput("error_f13a_2plus"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge3_mening",
                label = NULL,
                min = 0,
                value = savedRates$FemAge3_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge3_mening_rate")
            ),
            uiOutput("error_f13_mening"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemAge3_tdap",
                label = NULL,
                min = 0,
                value = savedRates$FemAge3_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("FemAge3_tdap_rate")
            ),
            uiOutput("error_f13_Tdap"),
          ),
          box( # boys aged 13
            h3("Males, age 13"),
            title = "Males, age 13",
            collapsible = T,
            solidHeader = T,
            status = "danger",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("male ", style = "color:blue"),
                "medical patients ",
                em("age 13 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "MenAge3_total",
              label = NULL,
              value = savedRates$MenAge3_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_m13_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge3_dose1",
                label = NULL,
                min = 0,
                value = savedRates$MenAge3_total,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge3_dose1_rate")
            ),
            uiOutput("error_m13_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge3_dose2",
                label = NULL,
                min = 0,
                value = savedRates$MenAge3_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge3_dose2_rate")
            ),
            uiOutput("error_m13_2plus"),
            uiOutput("error_m13a_2plus"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge3_mening",
                label = NULL,
                min = 0,
                value = savedRates$MenAge3_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge3_mening_rate")
            ),
            uiOutput("error_m13_mening"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenAge3_tdap",
                label = NULL,
                min = 0,
                value = savedRates$MenAge3_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("MenAge3_tdap_rate")
            ),
            uiOutput("error_m13_Tdap"),
          )
            ))
        })
        output$FemAge3_dose1_rate <- renderText({
          validate(need(input$FemAge3_total, ""))
          if (is.na(input$FemAge3_dose1)) {
            return(NULL)
          }
          if (!is.na(input$FemAge3_dose1)) {
            rate <- input$FemAge3_dose1 / input$FemAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemAge3_dose2_rate <- renderText({
          validate(need(input$FemAge3_total, ""))
          if (is.na(input$FemAge3_dose2)) {
            return(NULL)
          }
          if (!is.na(input$FemAge3_dose2)) {
            rate <- input$FemAge3_dose2 / input$FemAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemAge3_mening_rate <- renderText({
          validate(need(input$FemAge3_total, ""))
          if (is.na(input$FemAge3_mening)) {
            return(NULL)
          }
          if (!is.na(input$FemAge3_mening)) {
            rate <- input$FemAge3_mening / input$FemAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemAge3_Tdap_rate <- renderText({
          validate(need(input$FemAge3_total, ""))
          if (is.na(input$FemAge3_tdap)) {
            return(NULL)
          }
          if (!is.na(input$FemAge3_tdap)) {
            rate <- input$FemAge3_tdap / input$FemAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })

        output$error_f13_total <- renderUI({
          if (is.na(input$FemAge3_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_f13_gt1 <- renderUI({
          if (is.na(input$FemAge3_dose1) | is.na(input$FemAge3_total)) {
            return(NULL)
          } else if (input$FemAge3_dose1 > input$FemAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f13_2plus <- renderUI({if (is.na(input$FemAge3_dose2) | is.na(input$FemAge3_total)) {
          return(NULL)
        } else if (input$FemAge3_dose2 > input$FemAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_f13a_2plus <- renderUI({
          if (is.na(input$FemAge3_dose2) | is.na(input$FemAge3_dose1)) {
            return(NULL)
          } else if (input$FemAge3_dose2 >= input$FemAge3_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_f13_mening <- renderUI({
          if (is.na(input$FemAge3_mening) | is.na(input$FemAge3_total)) {
            return(NULL)
          } else if (input$FemAge3_mening > input$FemAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f13_Tdap <- renderUI({
          if (is.na(input$FemAge3_tdap) | is.na(input$FemAge3_total)) {
            return(NULL)
          } else if (input$FemAge3_tdap > input$FemAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
        
        output$MenAge3_dose1_rate <- renderText({
          validate(need(input$MenAge3_total, ""))
          if (is.na(input$MenAge3_dose1)) {
            return(NULL)
          }
          if (!is.na(input$MenAge3_dose1)) {
            rate <- input$MenAge3_dose1 / input$MenAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenAge3_dose2_rate <- renderText({
          validate(need(input$MenAge3_total, ""))
          if (is.na(input$MenAge3_dose2)) {
            return(NULL)
          }
          if (!is.na(input$MenAge3_dose2)) {
            rate <- input$MenAge3_dose2 / input$MenAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenAge3_mening_rate <- renderText({
          validate(need(input$MenAge3_total, ""))
          if (is.na(input$MenAge3_mening)) {
            return(NULL)
          }
          if (!is.na(input$MenAge3_mening)) {
            rate <- input$MenAge3_mening / input$MenAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenAge3_tdap_rate <- renderText({
          validate(need(input$MenAge3_total, ""))
          if (is.na(input$MenAge3_tdap)) {
            return(NULL)
          }
          if (!is.na(input$MenAge3_tdap)) {
            rate <- input$MenAge3_tdap / input$MenAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        
        
        
        output$error_m13_total <- renderUI({
          if (is.na(input$MenAge3_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_m13_gt1 <- renderUI({
          if (is.na(input$MenAge3_dose1) | is.na(input$MenAge3_total)) {
            return(NULL)
          } else if (input$MenAge3_dose1 > input$MenAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m13_2plus <- renderUI({if (is.na(input$MenAge3_dose2) | is.na(input$MenAge3_total)) {
          return(NULL)
        } else if (input$MenAge3_dose2 > input$MenAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_m13a_2plus <- renderUI({
          if (is.na(input$MenAge3_dose2) | is.na(input$MenAge3_dose1)) {
            return(NULL)
          } else if (input$MenAge3_dose2 >= input$MenAge3_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_m13_mening <- renderUI({
          if (is.na(input$MenAge3_mening) | is.na(input$MenAge3_total)) {
            return(NULL)
          } else if (input$MenAge3_mening > input$MenAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m13_Tdap <- renderUI({
          if (is.na(input$MenAge3_tdap) | is.na(input$MenAge3_total)) {
            return(NULL)
          } else if (input$MenAge3_tdap > input$MenAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
      }

      # Boys and girls combined - age 13
      if ("13" %in% input$Q28 &
          input$Q29 == "No, we will have combined rate data") {
        output$brates_13 <- renderUI({
          box(
            h3("Vaccination Rates for Male and Female Patients Combined, Age 13"),
            title = "Vaccination Rates for Male and Female Patients Combined, Age 13",
            collapsible = T,
            solidHeader = T,
            status = "success",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active medical patients ",
                em("age 13 ", style = "color:red"),
                "your clinic saw in 2020?"
              )
            ),
            numericInput(
              "BothAge3_total",
              label = NULL,
              value = savedRates$BothAge3_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_13_total"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge3_dose1",
                label = NULL,
                min = 0,
                value = savedRates$BothAge3_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge3_dose1_rate")
            ),
            uiOutput("error_13_gt1"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge3_dose2",
                label = NULL,
                min = 0,
                value = savedRates$BothAge3_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge3_dose2_rate")
            ),
            uiOutput("error_13_2plus"),
            uiOutput("error_13a_2plus"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge3_mening",
                label = NULL,
                min = 0,
                value = savedRates$BothAge3_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge3_mening_rate")
            ),
            uiOutput("error_13_mening"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothAge3_tdap",
                label = NULL,
                min = 0,
                value = savedRates$BothAge3_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("BothAge3_tdap_rate")
            ),
            uiOutput("error_13_Tdap"),
          )
        })
        output$BothAge3_dose1_rate <- renderText({
          validate(need(input$BothAge3_total, ""))
          if (is.na(input$BothAge3_dose1)) {
            return(NULL)
          }
          if (!is.na(input$BothAge3_dose1)) {
            rate <- input$BothAge3_dose1 / input$BothAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothAge3_dose2_rate <- renderText({
          validate(need(input$BothAge3_total, ""))
          if (is.na(input$BothAge3_dose2)) {
            return(NULL)
          }
          if (!is.na(input$BothAge3_dose2)) {
            rate <- input$BothAge3_dose2 / input$BothAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothAge3_mening_rate <- renderText({
          validate(need(input$BothAge3_total, ""))
          if (is.na(input$BothAge3_mening)) {
            return(NULL)
          }
          if (!is.na(input$BothAge3_mening)) {
            rate <- input$BothAge3_mening / input$BothAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothAge3_tdap_rate <- renderText({
          validate(need(input$BothAge3_total, ""))
          if (is.na(input$BothAge3_tdap)) {
            return(NULL)
          }
          if (!is.na(input$BothAge3_tdap)) {
            rate <- input$BothAge3_tdap / input$BothAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        output$error_13_total <- renderUI({
          if (is.na(input$BothAge3_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_13_gt1 <- renderUI({
          if (is.na(input$BothAge3_dose1) | is.na(input$BothAge3_total)) {
            return(NULL)
          } else if (input$BothAge3_dose1 > input$BothAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_13_2plus <- renderUI({if (is.na(input$BothAge3_dose2) | is.na(input$BothAge3_total)) {
          return(NULL)
        } else if (input$BothAge3_dose2 > input$BothAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_13a_2plus <- renderUI({
          if (is.na(input$BothAge3_dose2) | is.na(input$BothAge3_dose1)) {
            return(NULL)
          } else if (input$BothAge3_dose2 >= input$BothAge3_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_13_mening <- renderUI({
          if (is.na(input$BothAge3_mening) | is.na(input$BothAge3_total)) {
            return(NULL)
          } else if (input$BothAge3_mening > input$BothAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_13_Tdap <- renderUI({
          if (is.na(input$BothAge3_tdap) | is.na(input$BothAge3_total)) {
            return(NULL)
          } else if (input$BothAge3_tdap > input$BothAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
      }
    }) # end the 13 observation
    
    output$Q30 <- renderUI({
      textAreaInput(
        inputId = "Q30",
        label = "30.  Please share anything else about your baseline data you'd like us to know.",
        value =  savedRates$Q30,
        rows = 4)
    })
    
    output$Q31 <- renderUI({
      selectInput(inputId = "Q31",
                  label = "31.  What was the primary data source used to calculate your vaccination rates?",
                  choices =c("",
                             "EHR (preferred)",
                             "Chart Audit",
                             "State Immunization Registry",
                             "Other (specify)"),
                  selectize = F,
                  selected = savedRates$Q31,
                  width = "100%")
    })
    output$Q31_other <- renderUI({
      validate(need(input$Q31, ""))
      if ("Other (specify)" %in% input$Q31) {
        textInput("Q31_other",
                  label = "Please specify",
                  value = savedRates$Q31_other,
                  width = "100%")
      }
    })
    
    output$Q32 <- renderUI({
      selectInput(
        inputId = "Q32",
        label = "32.  Did you use a secondary data source?",
        choices = c("", "Yes", "No"),
        selected = savedRates$Q32,
        selectize = T)
    })
    
    # Q32 - group textbox
    output$Q32_details <- renderUI({
      validate(need(input$Q32, ""))
      if (input$Q32 == "Yes") {
        checkboxGroupInput(
          inputId = "Q32_details",
          label = "32a. What secondary sources were used (select all that apply)?",
          choices = c( "EHR",
                       "Chart Audit",
                       "State Immunization Registry",
                       "Other (specify)"),
          selected = foo32)
      }
    })
    output$Q32_other <- renderUI({
      validate(need(input$Q32_details, ""))
      if ("Other (specify)" %in% input$Q32_details) {
        textInput("Q32_other",
                  label = "Please specify",
                  value = savedRates$Q32_other,
                  width = "100%")
      }
    })
    
    output$Q33 <- renderUI({
      selectInput(
        inputId = "Q33",
        label = "33.  Were you unable to report any of the requested data?",
        choices = c("", "Yes", "No"),
        selected = savedRates$Q33,
        selectize = T)
    })
    output$Q33_other <- renderUI({
      validate(need(input$Q33, ""))
      if (input$Q33 == "Yes") {
        textAreaInput("Q33_other",
                      label = "33a. Describe what you were unable to report and why.",
                      value = savedRates$Q33_other,
                      rows = 4)
      }
    })
    
    output$Q34 <- renderUI({
      selectInput(
        inputId = "Q34",
        label = "34.  Are you working on your reporting capacity and anticipate submitting updated data in the next few months?",
        choices = c("", "Yes", "No"),
        selected = savedRates$Q34,
        selectize = T,
        width = "100%")
    })
    
  
  
  
  # ---- RATES ----- #
  # Create the static table that people will use as reference
  output$static_table <- renderTable({
    q <-
      data.frame(rbind(
        c("AGES", "BORN", "EVER RECEIVED THE FOLLOWING VACCINES"),
        c("9-10", "2010-2011", "At least 1 dose of HPV; 2 doses of HPV"),
        c("11-12","2008-2009", "At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap"),
        c("13", "2007",  "At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap")
      ))
  }, striped = FALSE, align = "l", colnames = FALSE, rownames = FALSE, bordered =
    TRUE, spacing = "m", width = "70%")
  
  
  
  
  # WHEN HIT BUTTON, SAVE DATA AND MOVE TO FINAL PAGE
  observeEvent(input$button_rates, {
    
    username <- session$user
    
    # Vaccination rates
    Q27 <- input$Q27
    Q27_other <- ifelse(is.null(input$Q27_other), "", input$Q27_other)
    
    # Q28 Groupbox text input
    Q28 <-  ifelse(is.null(input$Q28), "", paste(input$Q28, collapse = ", "))
    answers <- c("9-10", "11-12",  "13",
                 "We can't report on these ages (other, please specify)?")
    Q28_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q28, fixed = T)) == 1, x, NA)
    })
    Q28_1 <- as.character(Q28_responses[1])
    Q28_2 <- as.character(Q28_responses[2])
    Q28_3 <- as.character(Q28_responses[3])
    Q28_4 <- as.character(Q28_responses[4])
    rm(Q28_responses, answers)
    
    Q28_other <- ifelse(is.null(input$Q28_other), "", input$Q28_other)
    Q29 <- input$Q29
    Q30 <- input$Q30
    
    # Rate variables
    
    # Girls - ages 9-10
    FemAge1_total <- ifelse(is.null(input$FemAge1_total), NA, input$FemAge1_total)
    FemAge1_dose1 <- ifelse(is.null(input$FemAge1_dose1), NA, input$FemAge1_dose1)
    FemAge1_dose2 <- ifelse(is.null(input$FemAge1_dose2), NA, input$FemAge1_dose2)
    # Boys - ages 9-10
    MenAge1_total <- ifelse(is.null(input$MenAge1_total), NA, input$MenAge1_total)
    MenAge1_dose1 <- ifelse(is.null(input$MenAge1_dose1), NA, input$MenAge1_dose1)
    MenAge1_dose2 <- ifelse(is.null(input$MenAge1_dose2), NA, input$MenAge1_dose2)
    # Both - ages 9-10
    BothAge1_total <- ifelse(is.null(input$BothAge1_total), NA, input$BothAge1_total)
    BothAge1_dose1 <- ifelse(is.null(input$BothAge1_dose1), NA, input$BothAge1_dose1)
    BothAge1_dose2 <- ifelse(is.null(input$BothAge1_dose2), NA, input$BothAge1_dose2)
    
    # Girls - ages 11-12
    FemAge2_total <- ifelse(is.null(input$FemAge2_total), NA, input$FemAge2_total)
    FemAge2_dose1 <- ifelse(is.null(input$FemAge2_dose1), NA, input$FemAge2_dose1)
    FemAge2_dose2 <- ifelse(is.null(input$FemAge2_dose2), NA, input$FemAge2_dose2)
    FemAge2_mening <- ifelse(is.null(input$FemAge2_mening), NA, input$FemAge2_mening)
    FemAge2_tdap <- ifelse(is.null(input$FemAge2_tdap), NA, input$FemAge2_tdap)
    
    # Boys - ages 11-12
    MenAge2_total <- ifelse(is.null(input$MenAge2_total), NA, input$MenAge2_total)
    MenAge2_dose1 <- ifelse(is.null(input$MenAge2_dose1), NA, input$MenAge2_dose1)
    MenAge2_dose2 <- ifelse(is.null(input$MenAge2_dose2), NA, input$MenAge2_dose2)
    MenAge2_mening <- ifelse(is.null(input$MenAge2_mening), NA, input$MenAge2_mening)
    MenAge2_tdap <- ifelse(is.null(input$MenAge2_tdap), NA, input$MenAge2_tdap)
    
    # Both - ages 11-12
    BothAge2_total <- ifelse(is.null(input$BothAge2_total), NA, input$BothAge2_total)
    BothAge2_dose1 <- ifelse(is.null(input$BothAge2_dose1), NA, input$BothAge2_dose1)
    BothAge2_dose2 <- ifelse(is.null(input$BothAge2_dose2), NA, input$BothAge2_dose2)
    BothAge2_mening <- ifelse(is.null(input$BothAge2_mening), NA, input$BothAge2_mening)
    BothAge2_tdap <- ifelse(is.null(input$BothAge2_tdap), NA, input$BothAge2_tdap)
    
    # Girls - ages 13
    FemAge3_total <-  ifelse(is.null(input$FemAge3_total), NA, input$FemAge3_total)
    FemAge3_dose1 <-  ifelse(is.null(input$FemAge3_dose1), NA, input$FemAge3_dose1)
    FemAge3_dose2 <- ifelse(is.null(input$FemAge3_dose2), NA, input$FemAge3_dose2)
    FemAge3_mening <- ifelse(is.null(input$FemAge3_mening), NA, input$FemAge3_mening)
    FemAge3_tdap <-  ifelse(is.null(input$FemAge3_tdap), NA, input$FemAge3_tdap)
    # Boys - ages 13
    MenAge3_total <- ifelse(is.null(input$MenAge3_total), NA, input$MenAge3_total)
    MenAge3_dose1 <- ifelse(is.null(input$MenAge3_dose1), NA, input$MenAge3_dose1)
    MenAge3_dose2 <- ifelse(is.null(input$MenAge3_dose2), NA, input$MenAge3_dose2)
    MenAge3_mening <- ifelse(is.null(input$MenAge3_mening), NA, input$MenAge3_mening)
    MenAge3_tdap <- ifelse(is.null(input$MenAge3_tdap), NA, input$MenAge3_tdap)
    
    # Both - ages 13
    BothAge3_total <- ifelse(is.null(input$BothAge3_total), NA, input$BothAge3_total)
    BothAge3_dose1 <- ifelse(is.null(input$BothAge3_dose1), NA, input$BothAge3_dose1)
    BothAge3_dose2 <- ifelse(is.null(input$BothAge3_dose2), NA, input$BothAge3_dose2)
    BothAge3_mening <- ifelse(is.null(input$BothAge3_mening), NA, input$BothAge3_mening)
    BothAge3_tdap <- ifelse(is.null(input$BothAge3_tdap), NA, input$BothAge3_tdap)
    
    # Create a data frame of rates, impute the BothAgexxx variables if needed
    baselineRates <- data.frame(
      FemAge1_total, FemAge1_dose1, FemAge1_dose2,
      MenAge1_total, MenAge1_dose1, MenAge1_dose2,
      BothAge1_total, BothAge1_dose1,  BothAge1_dose2,
      FemAge2_total, FemAge2_dose1, FemAge2_dose2, FemAge2_mening,  FemAge2_tdap,
      MenAge2_total, MenAge2_dose1, MenAge2_dose2, MenAge2_mening, MenAge2_tdap,
      BothAge2_total, BothAge2_dose1, BothAge2_dose2, BothAge2_mening, BothAge2_tdap,
      FemAge3_total, FemAge3_dose1, FemAge3_dose2, FemAge3_mening, FemAge3_tdap,
      MenAge3_total, MenAge3_dose1,  MenAge3_dose2, MenAge3_mening, MenAge3_tdap,
      BothAge3_total, BothAge3_dose1,  BothAge3_dose2, BothAge3_mening, BothAge3_tdap)
    
    # make sure all are numeric
    baselineRates[, names(baselineRates)] <-
      lapply(baselineRates[, names(baselineRates)], as.numeric)
    
    rm( FemAge1_total, FemAge1_dose1, FemAge1_dose2,
        MenAge1_total, MenAge1_dose1, MenAge1_dose2,
        BothAge1_total, BothAge1_dose1,  BothAge1_dose2,
        FemAge2_total, FemAge2_dose1, FemAge2_dose2, FemAge2_mening,  FemAge2_tdap,
        MenAge2_total, MenAge2_dose1, MenAge2_dose2, MenAge2_mening, MenAge2_tdap,
        BothAge2_total, BothAge2_dose1, BothAge2_dose2, BothAge2_mening, BothAge2_tdap,
        FemAge3_total, FemAge3_dose1, FemAge3_dose2, FemAge3_mening, FemAge3_tdap,
        MenAge3_total, MenAge3_dose1,  MenAge3_dose2, MenAge3_mening, MenAge3_tdap,
        BothAge3_total, BothAge3_dose1,  BothAge3_dose2, BothAge3_mening, BothAge3_tdap)
    
    # Reporting section
    Q31 <- input$Q31
    Q31_other <- ifelse(is.null(input$Q31_other), "", input$Q31_other)
    Q32 <- input$Q32
    
    # Q32 - group textb ox
    Q32_details <- ifelse(is.null(input$Q32_details), "", paste(input$Q32_details, collapse = ", "))
    answers <- c("EHR", "Chart Audit", "State Immunization Registry", "Other (specify)")
    Q32_details_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q32_details, fixed = T)) == 1, x, NA)
    })
    Q32_details1 <- as.character(Q32_details_responses[1])
    Q32_details2 <- as.character(Q32_details_responses[2])
    Q32_details3 <- as.character(Q32_details_responses[3])
    Q32_details4 <- as.character(Q32_details_responses[4])
    rm(Q32_details_responses, answers)
    
    Q32_other <- ifelse(is.null(input$Q32_other), "", input$Q32_other)
    Q33 <- input$Q33
    Q33_other <- ifelse(is.null(input$Q33_other), "", input$Q33_other)
    Q34 <- input$Q34
    
    
    
    
    
    savedRates <- data.frame(Username = session$user,
      baseratesDate = as.numeric(Sys.time()),
      # Rates (questions)
      Q27, Q27_other, Q28, Q28_1, Q28_2, Q28_3, Q28_4, Q28_other,
      Q29, Q30,
      # Rates - numbers
      baselineRates,
      Q31, Q31_other, Q32, Q32_details, Q32_details1, Q32_details2, Q32_details3, Q32_details4,
      Q32_other, Q33, Q33_other, Q34, RatesComments=RatesComments)
    
    updateDB(DataSrc, userFilename, "savedRates", savedRates) 
    updateTabsetPanel(session, "tabbox_baseline", selected = "p1_plan")
  })
  
  # Add latest update box
    output$date_brates <- renderValueBox({
      if (is.na(savedRates$baseratesDate)) {
        valueBox(tags$p("Data entry started", style="font-size: 75%"), 
tags$p(Sys.Date(), style="font-size: 150%;"),
                 icon=icon("horse"),
                 color="blue")   
      } else {
      valueBox(tags$p("Last Submitted", style="font-size: 75%"), 
               tags$p(as.POSIXct(savedRates$baseratesDate, origin = "1970-01-01"), style="font-size: 150%;"), 
             icon=icon("horse"),
             color="blue")        
      }

  })
  
    
  
  # Project activity plan ---------------------------------------------------
  
    savedActivities <- getRecentData(DataSrc, userFilename, "savedActivities")
  
    ActivitiesComments <- savedActivities$ActivitiesComments

    
    foo35 <- with(savedActivities, c(Q35_1, Q35_2, Q35_3, Q35_4))
    foo35 <- foo35[!is.na(foo35)]  
    
    foo37 <- with(savedActivities, c(Q37_1, Q37_2, Q37_3, Q37_4))
    foo37 <- foo37[!is.na(foo37)]
    
    
    foo38 <- with(savedActivities, c(Q38_1, Q38_2, Q38_3, Q38_4, Q38_5, Q38_6, Q38_7, Q38_8, Q38_9))
    foo38 <- foo38[!is.na(foo38)]
    
    foo39 <- with(savedActivities, c(Q39_1, Q39_2, Q39_3, Q39_4, Q39_5))
    foo39 <- foo39[!is.na(foo39)]
    
    
    # Q35 - checkbox group input
    output$Q35 <- renderUI({
      checkboxGroupInput(
        inputId = "Q35",
        label = "35.  What other data do we need to obtain before establishing our goal?",
        choices = c("Provider-level rates", "Clinic-level rates", "We have all the data we need", "Other (specify)"),
        selected = foo35,
        width = '100%'
      )
    })
    output$Q35_other <- renderUI({
      validate(need(input$Q35, ""))
      if ("Other (specify)" %in% input$Q35) {
        textInput("Q35_other",
                  label = "Please specify",
                  value = savedActivities$Q35_other,
                  width = "100%")
      }
    })
    
    output$Q36a <- renderUI({
      numericInput(
        inputId = "Q36a",
        label = "36a.  What is our specific numerical goal?",
        value =  savedActivities$Q36a,
        width = '100%')
    })
    output$Q36b <- renderUI({
      textInput(
        inputId = "Q36b",
        label = "36b.  What is our target population?",
        value = savedActivities$Q36b,
        width = '100%')
    })
    output$Q36c <- renderUI({
      textInput(
        inputId = "Q36c",
        label = "36c.  Where will we do it?",
        value = savedActivities$Q36c,
        width = '100%')
    })
    output$Q36d <- renderUI({
      textInput(inputId = "Q36d",
                label = "36d.  What is the time period to achieve this aim?",
                value = savedActivities$Q36d,
                width = '100%')
    })
    output$Q36e <- renderUI({
      textAreaInput(inputId = "Q36e",
                    label = "36e.  Why is it important to do this now?",
                    value = savedActivities$Q36e,
                    rows = 4)
    })
    output$Q36f <- renderUI({
      textAreaInput(inputId = "Q36f",
                    label = "We aim to...",
                    value = savedActivities$Q36f,
                    rows = 5)
    })
    
    # Q37 - checkbox group
    output$Q37 <- renderUI({
      checkboxGroupInput(inputId = "Q37",
                         label = p("37.  Which of the following HPV vaccination training and education activities will we conduct?",
                                   em(" (check all that apply)")),
                         choices = c("Educate staff on HPV vaccination as cancer prevention",
                                     "Educate staff on strategies to improve HPV vaccination rates",
                                     "Identify HPV vaccination champions",
                                     "Train providers to make an effective HPV vaccine recommendation"),
                         selected = foo37,
                         width = "250%")
    })
    
    # Q38 - checkbox group
    output$Q38 <- renderUI({
      checkboxGroupInput(
        inputId = "Q38",
        label = p("38.  Which of the following interventions to increase HPV vaccination will we implement?",
                  em(" (check all that apply)")),
        choices = c("Client reminders",
                    "Extended hours",
                    "Modified EHR",
                    "Offer in alternative settings, like schools or mobile units",
                    "Parent/patient education",
                    "Provider assessment & feedback",
                    "Provider prompts/reminders",
                    "Standing orders",
                    "Other (specify)"),
        selected = foo38,
        width = "250%")
    })
    output$Q38_other <- renderUI({
      validate(need(input$Q38, ""))
      if ("Other (specify)" %in% input$Q38) {
        textInput("Q38_other",
                  label = "Please specify",
                  value = savedActivities$Q38_other,
                  width = "100%")
      }
    })
    
    # Q39 - checkbox group
    output$Q39 <- renderUI({
      checkboxGroupInput(
        inputId = "Q39",
        label = p("39.  What forms of support would the health system like from the ACS staff partner?",
                  em(" (check all that apply)")),
        choices = c("Member of HPV QI project team",
                    "Provide input on strategy",
                    "Provide materials",
                    "Provide technical assistance",
                    "Other (specify)"),
        width =  '250%',
        selected = foo39)
      
    })
    output$Q39_other <- renderUI({
      validate(need(input$Q39, ""))
      if ("Other (specify)" %in% input$Q39) {
        textInput("Q39_other",
                  label = "Please specify",
                  value = savedActivities$Q39_other,
                  width = "100%")
      }
    })
    
    output$Q40 <- renderUI({
      textAreaInput(
        inputId = "Q40",
        label = "40.  How will your system communicate with providers, staff, and leadership about project kickoff, goal, and updates?",
        rows = 4,
        value <- savedActivities$Q40)
    })
    output$Q41 <- renderUI({
      textAreaInput(
        inputId = "Q41",
        label = "41.  How will your system build and maintain excitement and momentum for the project?",
        rows = 4,
        value = savedActivities$Q41)
    })
    output$Q42 <- renderUI({
      textAreaInput(
        inputId = "Q42",
        label = "42.  How will your system celebrate and share success?",
        rows = 4,
        value = savedActivities$Q42)
    })
    
    # Activity matrix
    output$act1 <- renderUI({
      textInput("act1", strong("Activity"), value = savedActivities$act1)
    })
    output$time1 <- renderUI({
      textInput("time1", strong("Timeline"), value = savedActivities$time1)
    })
    output$ppl1 <- renderUI({
      textInput("ppl1", strong("Person/People Responsible"), value = savedActivities$ppl1)
    })
    
    output$act2 <- renderUI({
      textInput("act2", label = NULL, value = savedActivities$act2)
    })
    output$time2 <- renderUI({
      textInput("time2", label = NULL, value = savedActivities$time2)
    })
    output$ppl2 <- renderUI({
      textInput("ppl2", label = NULL, value = savedActivities$ppl2)
    })
    
    output$act3 <- renderUI({
      textInput("act3", label = NULL, value = savedActivities$act3)
    })
    output$time3 <- renderUI({
      textInput("time3", label = NULL, value = savedActivities$time3)
    })
    output$ppl3 <- renderUI({
      textInput("ppl3", label = NULL, value = savedActivities$ppl3)
    })
    
    output$act4 <- renderUI({
      textInput("act4", label = NULL, value = savedActivities$act4)
    })
    output$time4 <- renderUI({
      textInput("time4", label = NULL, value = savedActivities$time4)
    })
    output$ppl4 <- renderUI({
      textInput("ppl4", label = NULL, value = savedActivities$ppl4)
    })
    
    output$act5 <- renderUI({
      textInput("act5", label = NULL, value = savedActivities$act5)
    })
    output$time5 <- renderUI({
      textInput("time5", label = NULL, value = savedActivities$time5)
    })
    output$ppl5 <- renderUI({
      textInput("ppl5", label = NULL, value = savedActivities$ppl5)
    })
    
    output$act6 <- renderUI({
      textInput("act6", label = NULL, value = savedActivities$act6)
    })
    output$time6 <- renderUI({
      textInput("time6", label = NULL, value = savedActivities$time6)
    })
    output$ppl6 <- renderUI({
      textInput("ppl6", label = NULL, value = savedActivities$ppl6)
    })
    
    output$act7 <- renderUI({
      textInput("act7", label = NULL, value = savedActivities$act7)
    })
    output$time7 <- renderUI({
      textInput("time7", label = NULL, value = savedActivities$time7)
    })
    output$ppl7 <- renderUI({
      textInput("ppl7", label = NULL, value = savedActivities$ppl7)
    })
    
    output$act8 <- renderUI({
      textInput("act8", label = NULL, value = savedActivities$act8)
    })
    output$time8 <- renderUI({
      textInput("time8", label = NULL, value = savedActivities$time8)
    })
    output$ppl8 <- renderUI({
      textInput("ppl8", label = NULL, value = savedActivities$ppl8)
    })
    
    output$act9 <- renderUI({
      textInput("act9", label = NULL, value = savedActivities$act9)
    })
    output$time9 <- renderUI({
      textInput("time9", label = NULL, value = savedActivities$time9)
    })
    output$ppl9 <- renderUI({
      textInput("ppl9", label = NULL, value = savedActivities$ppl9)
    })
    
    output$act10 <- renderUI({
      textInput("act10", label = NULL, value = savedActivities$act10)
    })
    output$time10 <- renderUI({
      textInput("time10", label = NULL, value = savedActivities$time10)
    })
    output$ppl10 <- renderUI({
      textInput("ppl10", label = NULL, value = savedActivities$ppl10)
    })
    
  # Gather and save the data
  observeEvent(input$button_activities, {
    
    
    Q35 <- ifelse(is.null(input$Q35), "", paste(input$Q35, collapse = ", "))
    answers <- c("Provider-level rates", "Clinic-level rates", "We have all the data we need", "Other (specify)")
    Q35_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q35, fixed = T)) == 1, x, NA)
    })
    Q35_1 <- as.character(Q35_responses[1])
    Q35_2 <- as.character(Q35_responses[2])
    Q35_3 <- as.character(Q35_responses[3])
    Q35_4 <- as.character(Q35_responses[4])
    rm(Q35_responses, answers)
    
    Q35_other <- ifelse(is.null(input$Q35_other), "", input$Q35_other)
    Q36a <- input$Q36a
    Q36b <- input$Q36b
    Q36c <- input$Q36c
    Q36d <- input$Q36d
    Q36e <- input$Q36e
    Q36f <- input$Q36f
    
    
    
    Q37 <- ifelse(is.null(input$Q37), "", paste(input$Q37, collapse = ", "))
    answers <- c("Educate staff on HPV vaccination as cancer prevention",
                 "Educate staff on strategies to improve HPV vaccination rates",
                 "Identify HPV vaccination champions",
                 "Train providers to make an effective HPV vaccine recommendation")
    Q37_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q37, fixed = T)) == 1, x, NA)
    })
    Q37_1 <- as.character(Q37_responses[1])
    Q37_2 <- as.character(Q37_responses[2])
    Q37_3 <- as.character(Q37_responses[3])
    Q37_4 <- as.character(Q37_responses[4])
    rm(Q37_responses, answers)
    
    
    Q38 <- ifelse(is.null(input$Q38), "", paste(input$Q38, collapse = ", "))
    answers <- c("Client reminders",
                 "Extended hours",
                 "Modified EHR",
                 "Offer in alternative settings, like schools or mobile units",
                 "Parent/patient education",
                 "Provider assessment & feedback",
                 "Provider prompts/reminders",
                 "Standing orders",
                 "Other (specify)")
    Q38_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q38, fixed = T)) == 1, x, NA)
    })
    Q38_1 <- as.character(Q38_responses[1])
    Q38_2 <- as.character(Q38_responses[2])
    Q38_3 <- as.character(Q38_responses[3])
    Q38_4 <- as.character(Q38_responses[4])
    Q38_5 <- as.character(Q38_responses[5])
    Q38_6 <- as.character(Q38_responses[6])
    Q38_7 <- as.character(Q38_responses[7])
    Q38_8 <- as.character(Q38_responses[8])
    Q38_9 <- as.character(Q38_responses[9])
    rm(Q38_responses, answers)
    
    Q38_other <- ifelse(is.null(input$Q38_other), "", input$Q38_other)
    
    Q39 <- ifelse(is.null(input$Q39), "", paste(input$Q39, collapse = ", "))
    answers <- c("Member of HPV QI project team",
                 "Provide input on strategy",
                 "Provide materials",
                 "Provide technical assistance",
                 "Other (specify)")
    Q39_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q39, fixed = T)) == 1, x, NA)
    })
    Q39_1 <- as.character(Q39_responses[1])
    Q39_2 <- as.character(Q39_responses[2])
    Q39_3 <- as.character(Q39_responses[3])
    Q39_4 <- as.character(Q39_responses[4])
    Q39_5 <- as.character(Q39_responses[5])
    rm(Q39_responses, answers)
    
    Q39_other <- ifelse(is.null(input$Q39_other), "", input$Q39_other)
    Q40 <- input$Q40
    Q41 <- input$Q41
    Q42 <- input$Q42
    
    # Activity grid - should all default to ""
    act <- paste0("act", 1:10)
    time <- paste0("time", 1:10)
    ppl <- paste0("ppl", 1:10)
    activityGrid <- lapply(c(act, time, ppl), function(x) {
      var <- ifelse(is.null(input[[x]]), "", input[[x]])
      foo <- data.frame(var)
      names(foo) <- x
      return(foo)
    }) %>%
      do.call("cbind", .)
    rm(act, time, ppl)
    
    
    
    savedActivities <- data.frame(Username = session$user,
      activitiesDate = as.numeric(Sys.time()),
      Q35, Q35_1, Q35_2, Q35_3, Q35_4, Q35_other,
      Q36a, Q36b, Q36c, Q36d, Q36e, Q36f,
      Q37, Q37_1, Q37_2, Q37_3, Q37_4,
      Q38, Q38_1, Q38_2, Q38_3, Q38_4, Q38_5,
      Q38_6, Q38_7, Q38_8, Q38_9, Q38_other,
      Q39, Q39_1, Q39_2, Q39_3, Q39_4, Q39_5, Q39_other,
      Q40, Q41, Q42, activityGrid, ActivitiesComments=ActivitiesComments)
    
    updateDB(DataSrc, userFilename, "savedActivities", savedActivities)
    updateTabsetPanel(session, "tabbox_baseline", selected = "p1_submission")
    
  })
  
  # Add latest dates
    output$date_activity <- renderValueBox({
      if (is.na(savedActivities$activitiesDate)) {
        valueBox(tags$p("Data entry started", style="font-size: 75%"), 
tags$p(Sys.Date(), style="font-size: 150%;"),
                 icon=icon("horse"),
                 color="blue")   
      } else {
          valueBox(tags$p("Last Submitted", style="font-size: 75%"), 
                   tags$p(as.POSIXct(savedActivities$activitiesDate, origin = "1970-01-01"), style="font-size: 150%;"), 
             icon=icon("horse"),
             color="blue")        
      }

  })
  

  # Final baseline submission page ------------------------------------------

    # Add the final baseline submit button
  observeEvent(input$button_baseline_final, {
    
    nullDB()
    nullData <- dbConnect(SQLite(), "nullDB.DB")
    
    baselineTable <- getBaselineData(loc = DataSrc, filename = userFilename)
    
    # Hard stop errors:
    # 1.  Missing all rate tables
    rates <- c("FemAge1_total","FemAge1_dose1", "FemAge1_dose2",
               "MenAge1_total", "MenAge1_dose1", "MenAge1_dose2",
               "BothAge1_total","BothAge1_dose1", "BothAge1_dose2",
               "FemAge2_total", "FemAge2_dose1", "FemAge2_dose2", "FemAge2_mening", "FemAge2_tdap",
               "MenAge2_total", "MenAge2_dose1", "MenAge2_dose2", "MenAge2_mening", "MenAge2_tdap",
               "BothAge2_total", "BothAge2_dose1", "BothAge2_dose2", "BothAge2_mening", "BothAge2_tdap",
               "FemAge3_total", "FemAge3_dose1", "FemAge3_dose2", "FemAge3_mening", "FemAge3_tdap",
               "MenAge3_total", "MenAge3_dose1", "MenAge3_dose2", "MenAge3_mening", "MenAge3_tdap", 
               "BothAge3_total", "BothAge3_dose1", "BothAge3_dose2", "BothAge3_mening", "BothAge3_tdap")
    sumRates <- apply(baselineTable[,rates], 1, sum, na.rm=T)

    # 2.  Missing systems data
    systems <- dbReadTable(nullData, "systems")
    systems2 <- baselineTable[,names(systems)]
    
    # 3.  Missing activities
    savedActivities <- dbReadTable(nullData, "savedActivities")
    savedActivities2 <- baselineTable[,names(savedActivities)]


    condition <- ifelse(
      identical(systems, systems2), 1, ifelse(
        sumRates == 0, 2, ifelse(
          identical(savedActivities, savedActivities2), 3, 99)))

    if (condition == 1) {
      shinyalert(title="Uh oh",
                 text="It looks like you did not enter data in the System Background tab.  Please return to this tab and input your data",
                 type="error", showConfirmButton = FALSE, showCancelButton = TRUE)

    }
    else if (condition == 2) {
              shinyalert(title="Uh oh",
                         text="It looks like you forgot to enter vaccination rates data.  Please return to the Vaccination Rates
                               tab and input your data",
                          type="error",showConfirmButton = FALSE, showCancelButton = TRUE)

    }
    else if (condition == 3) {
      shinyalert(title="Uh oh",
                 text="It looks like you did not enter data in the Project Activity Plan tab.  Please return to this section and input your data",
                 type="error", showConfirmButton = FALSE, showCancelButton = TRUE)

    } else {
      shinyalert(title="Confirm submit", text="You are about to submit the final information for your health system. Have you checked and confirmed you've answered every question and your answers are correct?",
                 type="warning", showCancelButton=TRUE, showConfirmButton = TRUE, closeOnClickOutside=FALSE,
                 closeOnEsc= FALSE, confirmButtonText = "Submit", cancelButtonText="Cancel (go back to review)",
                 inputId="bline_shinyalert")
       }
   })
  
  observe({
    req(input$bline_shinyalert)
    
    # This will merge all the most recent data files and save it as the finalTable
    
    baselineTable <- getBaselineData(loc = DataSrc, filename = userFilename)
    baselineTable$BaselineSubmitDate <- Sys.time()
    updateDB(loc = DataSrc, filename = userFilename, table = "baselineTable", df = baselineTable)
    session$reload()
  })
  

  ###### ------------------------------ FINAL UPDATES TABS ----------------------------- #######
  
  followupRates <- getRecentData(DataSrc, userFilename, "followupRates")
  
  FollowupRatesComments <- followupRates$FollowupRatesComments


    foo2FU <- with(followupRates, c(Q2FU_1, Q2FU_2, Q2FU_3, Q2FU_4))
    foo2FU <- foo2FU[!is.na(foo2FU)]
    
    
    output$Q1FU <- renderUI({
      selectInput(
        inputId = "Q1FU",
        label = "1.  How are you defining your active patient populations?",
        choices = c("",
                    "12 months",
                    "18 months",
                    "24 months",
                    "Other (please specify)"),
        selected = followupRates$Q1FU,
        selectize = F)
    })
    output$Q1FU_other <- renderUI({
      validate(need(input$Q1FU, ""))
      if (input$Q1FU != "Other (please specify)") {
        return(NULL)
      } else {
        textInput(
          inputId = "Q1FU_other",
          label = "Please specify",
          value = followupRates$Q1FU_other,
          placeholder = "",
          width = "150%"
        )
      }
    })
    output$Q2FU <- renderUI({
      checkboxGroupInput(
        inputId = "Q2FU",
        label = "2. For what ages are you reporting?",
        choices = c("9-10",
                    "11-12",
                    "13",
                    "We can't report on these ages (other, please specify)?"),
        selected = foo2FU)
    })
    output$Q2FU_other <- renderUI({
      validate(need(input$Q2FU, ""))
      if (!"We can't report on these ages (other, please specify)?" %in% input$Q2FU) {
        return(NULL)
      } else {
        textInput(
          inputId = "Q2FU_other",
          label = "Please specify",
          value = followupRates$Q2FU_other,
          placeholder = "",
          width = "150%"
        )
      }
    })    
    output$Q3FU <- renderUI({
      selectInput(
        inputId = "Q3FU",
        label = "3.  Can you pull rate data by sex?",
        choices = c("",
                    "Yes, we'll report separate data for male and female patients",
                    "No, we will have combined rate data"),
        selected = followupRates$Q3FU)
    })
    output$Q4FU <- renderUI({
      textAreaInput(
        inputId = "Q4FU",
        label = "4.  Please share anything else about your final data you'd like us to know.",
        value = followupRates$Q4FU,
        rows = 4)
    })
    
    # This is how you get rid of the boxes
    observe({
      x <- input$Q2FU
      if (!"9-10" %in% x) {
        output$furates_9_10 <- renderUI(NULL)
      }
      if (!"11-12" %in% x) {
        output$furates_11_12 <- renderUI(NULL)
      }
      if (!"13" %in% x) {
        output$furates_13 <- renderUI(NULL)
      }
      
    })
    
    # Create the ages 9-10 boxes
    observeEvent(list(input$Q2FU, input$Q3FU), {
      validate(need(input$Q2FU, ""))
      validate(need(input$Q3FU, ""))
      
      # Boys and girls - ages 9-10
      if ("9-10" %in% input$Q2FU &
          input$Q3FU == "Yes, we'll report separate data for male and female patients") {
        
        # Girls ages 9-10 rates 
        output$furates_9_10 <- renderUI({
          verticalLayout(
            splitLayout(
          box(
            h3("Vaccination Rates for Female Patients, Ages 9-10"),
            title = "Vaccination Rates for Female Patients, Ages 9-10",
            collapsible = T,
            solidHeader = T,
            status = "primary",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("female ", style = "color:blue"),
                "medical patients ",
                em("ages 9-10 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "FemFUAge1_total",
              label = NULL,
              value = followupRates$FemFUAge1_total,
              min = 0,
              step = 1,
              width = '50%'
            ),
            uiOutput("error_f9_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge1_dose1",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge1_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge1_dose1_rate")
            ),
            uiOutput("error_f9_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge1_dose2",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge1_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge1_dose2_rate")
            ),
            uiOutput("error_f9_2plus_fwup"),
            uiOutput("error_f9a_2plus_fwup")
          ),
          box( #boys aged 9-10
            h3("Males, ages 9-10"),
            title = "Males, ages 9-10",
            collapsible = T,
            solidHeader = T,
            status = "danger",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("male ", style = "color:blue"),
                "medical patients ",
                em("ages 9-10 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "MenFUAge1_total",
              label = NULL,
              value = followupRates$MenFUAge1_total,
              min = 0,
              step = 1,
              width = '50%'
            ),
            uiOutput("error_m9_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge1_dose1",
                label = NULL,
                min = 0,
                value =  followupRates$MenFUAge1_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge1_dose1_rate")
            ),
            uiOutput("error_m9_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge1_dose2",
                label = NULL,
                min = 0,
                value =  followupRates$MenFUAge1_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge1_dose2_rate")
            ),
            uiOutput("error_m9_2plus_fwup"),
            uiOutput("error_m9a_2plus_fwup")
          )
            )) # end split and vertical layout
        })
        output$FemFUAge1_dose1_rate <- renderText({
          validate(need(input$FemFUAge1_total, ""))
          if (is.na(input$FemFUAge1_dose1)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge1_dose1)) {
            rate <- input$FemFUAge1_dose1 / input$FemFUAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        output$FemFUAge1_dose2_rate <- renderText({
          validate(need(input$FemFUAge1_total, ""))
          if (is.na(input$FemFUAge1_dose2)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge1_dose2)) {
            rate <- input$FemFUAge1_dose2 / input$FemFUAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        output$error_f9_total_fwup <- renderUI({
          if (is.na(input$FemFUAge1_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_f9_gt1_fwup <- renderUI({
          if (is.na(input$FemFUAge1_dose1) | is.na(input$FemFUAge1_total)) {
            return(NULL)
          } else if (input$FemFUAge1_dose1 > input$FemFUAge1_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f9_2plus_fwup <- renderUI({if (is.na(input$FemFUAge1_dose2) | is.na(input$FemFUAge1_total)) {
          return(NULL)
        } else if (input$FemFUAge1_dose2 > input$FemFUAge1_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_f9a_2plus_fwup <- renderUI({
          if (is.na(input$FemFUAge1_dose2) | is.na(input$FemFUAge1_dose1)) {
            return(NULL)
          } else if (input$FemFUAge1_dose2 >= input$FemFUAge1_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        
        output$MenFUAge1_dose1_rate <- renderText({
          validate(need(input$MenFUAge1_total, ""))
          if (is.na(input$MenFUAge1_dose1)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge1_dose1)) {
            rate <- input$MenFUAge1_dose1 / input$MenFUAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        output$MenFUAge1_dose2_rate <- renderText({
          validate(need(input$MenFUAge1_total, ""))
          if (is.na(input$MenFUAge1_dose2)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge1_dose2)) {
            rate <- input$MenFUAge1_dose2 / input$MenFUAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        output$error_m9_total_fwup <- renderUI({
          if (is.na(input$MenFUAge1_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_m9_gt1_fwup <- renderUI({
          if (is.na(input$MenFUAge1_dose1) | is.na(input$MenFUAge1_total)) {
            return(NULL)
          } else if (input$MenFUAge1_dose1 > input$MenFUAge1_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m9_2plus_fwup <- renderUI({if (is.na(input$MenFUAge1_dose2) | is.na(input$MenFUAge1_total)) {
          return(NULL)
        } else if (input$MenFUAge1_dose2 > input$MenFUAge1_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_m9a_2plus_fwup <- renderUI({
          if (is.na(input$MenFUAge1_dose2) | is.na(input$MenFUAge1_dose1)) {
            return(NULL)
          } else if (input$MenFUAge1_dose2 >= input$MenFUAge1_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        
      }
      
      # Ages 9-10, combined
      if ("9-10" %in% input$Q2FU &
          input$Q3FU == "No, we will have combined rate data") {
        # Create the 'male and females combined' box
        output$furates_9_10 <- renderUI({
          box(
            h3("Vaccination Rates for Male and Female Patients Combined, Ages 9-10"),
            title = "Vaccination Rates for Male and Female Patients Combined, Ages 9-10",
            collapsible = T,
            solidHeader = T,
            status = "success",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active medical patients ",
                em("ages 9-10 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "BothFUAge1_total",
              label = NULL,
              value = followupRates$BothFUAge1_total,
              min = 0,
              step = 1,
              width = '50%'
            ),
            uiOutput("error_9_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge1_dose1",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge1_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge1_dose1_rate")
            ),
            uiOutput("error_9_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(inputId = "BothFUAge1_dose2",
                           label = NULL,
                           min = 0,
                           value = followupRates$BothFUAge1_dose2,
                           step = 1,
                           width = '50%'
              ),
              textOutput("BothFUAge1_dose2_rate")
            ),
            uiOutput("error_9_2plus_fwup"),
            uiOutput("error_9a_2plus_fwup")
          )
        })
        output$BothFUAge1_dose1_rate <- renderText({
          validate(need(input$BothFUAge1_total, ""))
          if (is.na(input$BothFUAge1_dose1)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge1_dose1)) {
            rate <- input$BothFUAge1_dose1 / input$BothFUAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        output$BothFUAge1_dose2_rate <- renderText({
          validate(need(input$BothFUAge1_total, ""))
          if (is.na(input$BothFUAge1_dose2)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge1_dose2)) {
            rate <- input$BothFUAge1_dose2 / input$BothFUAge1_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group.")
            return(rate)
          }
        })
        
        output$error_9_total_fwup <- renderUI({
          if (is.na(input$BothFUAge1_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_9_gt1_fwup <- renderUI({
          if (is.na(input$BothFUAge1_dose1) | is.na(input$BothFUAge1_total)) {
            return(NULL)
          } else if (input$BothFUAge1_dose1 > input$BothFUAge1_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_9_2plus_fwup <- renderUI({if (is.na(input$BothFUAge1_dose2) | is.na(input$BothFUAge1_total)) {
          return(NULL)
        } else if (input$BothFUAge1_dose2 > input$BothFUAge1_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_9a_2plus_fwup <- renderUI({
          if (is.na(input$BothFUAge1_dose2) | is.na(input$BothFUAge1_dose1)) {
            return(NULL)
          } else if (input$BothFUAge1_dose2 >= input$BothFUAge1_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        
      }
      
    }) # end the 9-10 observation
    
    # Create the ages 11-12 boxes
    observeEvent(list(input$Q2FU, input$Q3FU), {
      validate(need(input$Q2FU, ""))
      validate(need(input$Q3FU, ""))
      
      if ("11-12" %in% input$Q2FU &
          input$Q3FU == "Yes, we'll report separate data for male and female patients") {
        
        # Girls ages 11-12
        output$furates_11_12 <- renderUI({
          verticalLayout(
            splitLayout(
          box(
            h3("Vaccination Rates for Female Patients, Ages 11-12"),
            title = "Vaccination Rates for Female Patients, Ages 11-12",
            collapsible = T,
            solidHeader = T,
            status = "primary",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("female ", style = "color:blue"),
                "medical patients ",
                em("ages 11-12 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "FemFUAge2_total",
              label = NULL,
              value = followupRates$FemFUAge2_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_f11_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge2_dose1",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge2_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge2_dose1_rate")
            ),
            uiOutput("error_f11_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge2_dose2",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge2_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge2_dose2_rate")
            ),
            uiOutput("error_f11_2plus_fwup"),
            uiOutput("error_f11a_2plus_fwup"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge2_mening",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge2_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge2_mening_rate")
            ),
            uiOutput("error_f11_mening_fwup"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge2_tdap",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge2_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge2_tdap_rate")
            ),
            uiOutput("error_f11_Tdap_fwup"),
          ),
          box( # boys aged 11-12
            h3("Males, ages 11-12"),
            title = "Males, ages 11-12",
            collapsible = T,
            solidHeader = T,
            status = "danger",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("male ", style = "color:blue"),
                "medical patients ",
                em("ages 11-12 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "MenFUAge2_total",
              label = NULL,
              value = followupRates$MenFUAge2_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_m11_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge2_dose1",
                label = NULL,
                min = 0,
                value = followupRates$MenFUAge2_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge2_dose1_rate")
            ),
            uiOutput("error_m11_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge2_dose2",
                label = NULL,
                min = 0,
                value = followupRates$MenFUAge2_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge2_dose2_rate")
            ),
            uiOutput("error_m11_2plus_fwup"),
            uiOutput("error_m11a_2plus_fwup"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge2_mening",
                label = NULL,
                min = 0,
                value = followupRates$MenFUAge2_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge2_mening_rate")
            ),
            uiOutput("error_m11_mening_fwup"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge2_tdap",
                label = NULL,
                min = 0,
                value = followupRates$MenFUAge2_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge2_tdap_rate")
            ),
            uiOutput("error_m11_Tdap_fwup"),
          )
            )) # end split and vertical layout
        })
        output$FemFUAge2_dose1_rate <- renderText({
          validate(need(input$FemFUAge2_total, ""))
          if (is.na(input$FemFUAge2_dose1)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge2_dose1)) {
            rate <- input$FemFUAge2_dose1 / input$FemFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemFUAge2_dose2_rate <- renderText({
          validate(need(input$FemFUAge2_total, ""))
          if (is.na(input$FemFUAge2_dose2)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge2_dose2)) {
            rate <- input$FemFUAge2_dose2 / input$FemFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemFUAge2_mening_rate <- renderText({
          validate(need(input$FemFUAge2_total, ""))
          if (is.na(input$FemFUAge2_mening)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge2_mening)) {
            rate <- input$FemFUAge2_mening / input$FemFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemFUAge2_tdap_rate <- renderText({
          validate(need(input$FemFUAge2_total, ""))
          if (is.na(input$FemFUAge2_tdap)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge2_tdap)) {
            rate <- input$FemFUAge2_tdap / input$FemFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        output$error_f11_total_fwup <- renderUI({
          if (is.na(input$FemFUAge2_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_f11_gt1_fwup <- renderUI({
          if (is.na(input$FemFUAge2_dose1) | is.na(input$FemFUAge2_total)) {
            return(NULL)
          } else if (input$FemFUAge2_dose1 > input$FemFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f11_2plus_fwup <- renderUI({if (is.na(input$FemFUAge2_dose2) | is.na(input$FemFUAge2_total)) {
          return(NULL)
        } else if (input$FemFUAge2_dose2 > input$FemFUAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_f11a_2plus_fwup <- renderUI({
          if (is.na(input$FemFUAge2_dose2) | is.na(input$FemFUAge2_dose1)) {
            return(NULL)
          } else if (input$FemFUAge2_dose2 >= input$FemFUAge2_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_f11_mening_fwup <- renderUI({
          if (is.na(input$FemFUAge2_mening) | is.na(input$FemFUAge2_total)) {
            return(NULL)
          } else if (input$FemFUAge2_mening > input$FemFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f11_Tdap_fwup <- renderUI({
          if (is.na(input$FemFUAge2_tdap) | is.na(input$FemFUAge2_total)) {
            return(NULL)
          } else if (input$FemFUAge2_tdap > input$FemFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
        output$MenFUAge2_dose1_rate <- renderText({
          validate(need(input$MenFUAge2_total, ""))
          if (is.na(input$MenFUAge2_dose1)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge2_dose1)) {
            rate <- input$MenFUAge2_dose1 / input$MenFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenFUAge2_dose2_rate <- renderText({
          validate(need(input$MenFUAge2_total, ""))
          if (is.na(input$MenFUAge2_dose2)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge2_dose2)) {
            rate <- input$MenFUAge2_dose2 / input$MenFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenFUAge2_mening_rate <- renderText({
          validate(need(input$MenFUAge2_total, ""))
          if (is.na(input$MenFUAge2_mening)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge2_mening)) {
            rate <- input$MenFUAge2_mening / input$MenFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenFUAge2_tdap_rate <- renderText({
          validate(need(input$MenFUAge2_total, ""))
          if (is.na(input$MenFUAge2_tdap)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge2_tdap)) {
            rate <- input$MenFUAge2_tdap / input$MenFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })

        output$error_m11_total_fwup <- renderUI({
          if (is.na(input$MenFUAge2_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_m11_gt1_fwup <- renderUI({
          if (is.na(input$MenFUAge2_dose1) | is.na(input$MenFUAge2_total)) {
            return(NULL)
          } else if (input$MenFUAge2_dose1 > input$MenFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m11_2plus_fwup <- renderUI({if (is.na(input$MenFUAge2_dose2) | is.na(input$MenFUAge2_total)) {
          return(NULL)
        } else if (input$MenFUAge2_dose2 > input$MenFUAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_m11a_2plus_fwup <- renderUI({
          if (is.na(input$MenFUAge2_dose2) | is.na(input$MenFUAge2_dose1)) {
            return(NULL)
          } else if (input$MenFUAge2_dose2 >= input$MenFUAge2_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_m11_mening_fwup <- renderUI({
          if (is.na(input$MenFUAge2_mening) | is.na(input$MenFUAge2_total)) {
            return(NULL)
          } else if (input$MenFUAge2_mening > input$MenFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m11_Tdap_fwup <- renderUI({
          if (is.na(input$MenFUAge2_tdap) | is.na(input$MenFUAge2_total)) {
            return(NULL)
          } else if (input$MenFUAge2_tdap > input$MenFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
      }
      # Ages 11-12 - boys and girls combined
      if ("11-12" %in% input$Q2FU &
          input$Q3FU == "No, we will have combined rate data") {
        output$furates_11_12 <- renderUI({
          box(
            h3("Vaccination Rates for Male and Female Patients Combined, Ages 11-12"),
            title = "Vaccination Rates for Male and Female Patients Combined, Ages 11-12",
            collapsible = T,
            solidHeader = T,
            status = "success",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active medical patients ",
                em("ages 11-12 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "BothFUAge2_total",
              label = NULL,
              value = followupRates$BothFUAge2_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_11_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge2_dose1",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge2_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge2_dose1_rate")
            ),
            uiOutput("error_11_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge2_dose2",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge2_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge2_dose2_rate")
            ),
            uiOutput("error_11_2plus_fwup"),
            uiOutput("error_11a_2plus_fwup"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge2_mening",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge2_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge2_mening_rate")
            ),
            uiOutput("error_11_mening_fwup"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge2_tdap",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge2_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge2_tdap_rate")
            ),
            uiOutput("error_11_Tdap_fwup"),
          )
        })
        output$BothFUAge2_dose1_rate <- renderText({
          validate(need(input$BothFUAge2_total, ""))
          if (is.na(input$BothFUAge2_dose1)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge2_dose1)) {
            rate <- input$BothFUAge2_dose1 / input$BothFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothFUAge2_dose2_rate <- renderText({
          validate(need(input$BothFUAge2_total, ""))
          if (is.na(input$BothFUAge2_dose2)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge2_dose2)) {
            rate <- input$BothFUAge2_dose2 / input$BothFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothFUAge2_mening_rate <- renderText({
          validate(need(input$BothFUAge2_total, ""))
          if (is.na(input$BothFUAge2_mening)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge2_mening)) {
            rate <- input$BothFUAge2_mening / input$BothFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothFUAge2_Tdap_rate <- renderText({
          validate(need(input$BothFUAge2_total, ""))
          if (is.na(input$BothFUAge2_tdap)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge2_tdap)) {
            rate <- input$BothFUAge2_tdap / input$BothFUAge2_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        output$error_11_total_fwup <- renderUI({
          if (is.na(input$BothFUAge2_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_11_gt1_fwup <- renderUI({
          if (is.na(input$BothFUAge2_dose1) | is.na(input$BothFUAge2_total)) {
            return(NULL)
          } else if (input$BothFUAge2_dose1 > input$BothFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_11_2plus_fwup <- renderUI({if (is.na(input$BothFUAge2_dose2) | is.na(input$BothFUAge2_total)) {
          return(NULL)
        } else if (input$BothFUAge2_dose2 > input$BothFUAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_11a_2plus_fwup <- renderUI({
          if (is.na(input$BothFUAge2_dose2) | is.na(input$BothFUAge2_dose1)) {
            return(NULL)
          } else if (input$BothFUAge2_dose2 >= input$BothFUAge2_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_11_mening_fwup <- renderUI({
          if (is.na(input$BothFUAge2_mening) | is.na(input$BothFUAge2_total)) {
            return(NULL)
          } else if (input$BothFUAge2_mening > input$BothFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_11_Tdap_fwup <- renderUI({
          if (is.na(input$BothFUAge2_tdap) | is.na(input$BothFUAge2_total)) {
            return(NULL)
          } else if (input$BothFUAge2_tdap > input$BothFUAge2_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
      }
    }) # end the 11-12 observation
    
    
    # Age 13 boxes
    observeEvent(list(input$Q2FU, input$Q3FU), {
      validate(need(input$Q2FU, ""))
      validate(need(input$Q3FU, ""))
      
      if ("13" %in% input$Q2FU &
          input$Q3FU == "Yes, we'll report separate data for male and female patients") {
        
        # Girls age 13
        output$furates_13 <- renderUI({
          verticalLayout(
            splitLayout(
          box(
            h3("Vaccination Rates for Female Patients, Age 13"),
            title = "Vaccination Rates for Female Patients, Age 13",
            collapsible = T,
            solidHeader = T,
            status = "primary",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("female ", style = "color:blue"),
                "medical patients ",
                em("age 13 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "FemFUAge3_total",
              label = NULL,
              value = followupRates$FemFUAge3_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_f13_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge3_dose1",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge3_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge3_dose1_rate")
            ),
            uiOutput("error_f13_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge3_dose2",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge3_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge3_dose2_rate")
            ),
            uiOutput("error_f13_2plus_fwup"),
            uiOutput("error_f13a_2plus_fwup"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge3_mening",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge3_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge3_mening_rate")
            ),
            uiOutput("error_f13_mening_fwup"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "FemFUAge3_tdap",
                label = NULL,
                min = 0,
                value = followupRates$FemFUAge3_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("FemFUAge3_tdap_rate")
            ),
            uiOutput("error_f13_Tdap_fwup"),
          ),
          box( # boys aged 13
            h3("Males, age 13"),
            title = "Males, age 13",
            collapsible = T,
            solidHeader = T,
            status = "danger",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active ",
                strong("male ", style = "color:blue"),
                "medical patients ",
                em("age 13 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "MenFUAge3_total",
              label = NULL,
              value = followupRates$MenFUAge3_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_m13_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge3_dose1",
                label = NULL,
                min = 0,
                value = followupRates$MenFUAge3_total,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge3_dose1_rate")
            ),
            uiOutput("error_m13_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge3_dose2",
                label = NULL,
                min = 0,
                value = followupRates$MenFUAge3_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge3_dose2_rate")
            ),
            uiOutput("error_m13_2plus_fwup"),
            uiOutput("error_m13a_2plus_fwup"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge3_mening",
                label = NULL,
                min = 0,
                value = followupRates$MenFUAge3_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge3_mening_rate")
            ),
            uiOutput("error_m13_mening_fwup"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "MenFUAge3_tdap",
                label = NULL,
                min = 0,
                value = followupRates$MenFUAge3_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("MenFUAge3_tdap_rate")
            ),
            uiOutput("error_m13_Tdap_fwup"),
          )
            )) # end split and vertical layout
        })
        output$FemFUAge3_dose1_rate <- renderText({
          validate(need(input$FemFUAge3_total, ""))
          if (is.na(input$FemFUAge3_dose1)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge3_dose1)) {
            rate <- input$FemFUAge3_dose1 / input$FemFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemFUAge3_dose2_rate <- renderText({
          validate(need(input$FemFUAge3_total, ""))
          if (is.na(input$FemFUAge3_dose2)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge3_dose2)) {
            rate <- input$FemFUAge3_dose2 / input$FemFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemFUAge3_mening_rate <- renderText({
          validate(need(input$FemFUAge3_total, ""))
          if (is.na(input$FemFUAge3_mening)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge3_mening)) {
            rate <- input$FemFUAge3_mening / input$FemFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$FemFUAge3_tdap_rate <- renderText({
          validate(need(input$FemFUAge3_total, ""))
          if (is.na(input$FemFUAge3_tdap)) {
            return(NULL)
          }
          if (!is.na(input$FemFUAge3_tdap)) {
            rate <- input$FemFUAge3_tdap / input$FemFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        output$error_f13_total_fwup <- renderUI({
          if (is.na(input$FemFUAge3_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_f13_gt1_fwup <- renderUI({
          if (is.na(input$FemFUAge3_dose1) | is.na(input$FemFUAge3_total)) {
            return(NULL)
          } else if (input$FemFUAge3_dose1 > input$FemFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f13_2plus_fwup <- renderUI({if (is.na(input$FemFUAge3_dose2) | is.na(input$FemFUAge3_total)) {
          return(NULL)
        } else if (input$FemFUAge3_dose2 > input$FemFUAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_f13a_2plus_fwup <- renderUI({
          if (is.na(input$FemFUAge3_dose2) | is.na(input$FemFUAge3_dose1)) {
            return(NULL)
          } else if (input$FemFUAge3_dose2 >= input$FemFUAge3_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_f13_mening_fwup <- renderUI({
          if (is.na(input$FemFUAge3_mening) | is.na(input$FemFUAge3_total)) {
            return(NULL)
          } else if (input$FemFUAge3_mening > input$FemFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_f13_Tdap_fwup <- renderUI({
          if (is.na(input$FemFUAge3_tdap) | is.na(input$FemFUAge3_total)) {
            return(NULL)
          } else if (input$FemFUAge3_tdap > input$FemFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
        output$MenFUAge3_dose1_rate <- renderText({
          validate(need(input$MenFUAge3_total, ""))
          if (is.na(input$MenFUAge3_dose1)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge3_dose1)) {
            rate <- input$MenFUAge3_dose1 / input$MenFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenFUAge3_dose2_rate <- renderText({
          validate(need(input$MenFUAge3_total, ""))
          if (is.na(input$MenFUAge3_dose2)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge3_dose2)) {
            rate <- input$MenFUAge3_dose2 / input$MenFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenFUAge3_mening_rate <- renderText({
          validate(need(input$MenFUAge3_total, ""))
          if (is.na(input$MenFUAge3_mening)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge3_mening)) {
            rate <- input$MenFUAge3_mening / input$MenFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$MenFUAge3_tdap_rate <- renderText({
          validate(need(input$MenFUAge3_total, ""))
          if (is.na(input$MenFUAge3_tdap)) {
            return(NULL)
          }
          if (!is.na(input$MenFUAge3_tdap)) {
            rate <- input$MenFUAge3_tdap / input$MenFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        output$error_m13_total_fwup <- renderUI({
          if (is.na(input$MenFUAge3_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_m13_gt1_fwup <- renderUI({
          if (is.na(input$MenFUAge3_dose1) | is.na(input$MenFUAge3_total)) {
            return(NULL)
          } else if (input$MenFUAge3_dose1 > input$MenFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m13_2plus_fwup <- renderUI({if (is.na(input$MenFUAge3_dose2) | is.na(input$MenFUAge3_total)) {
          return(NULL)
        } else if (input$MenFUAge3_dose2 > input$MenFUAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_m13a_2plus_fwup <- renderUI({
          if (is.na(input$MenFUAge3_dose2) | is.na(input$MenFUAge3_dose1)) {
            return(NULL)
          } else if (input$MenFUAge3_dose2 >= input$MenFUAge3_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_m13_mening_fwup <- renderUI({
          if (is.na(input$MenFUAge3_mening) | is.na(input$MenFUAge3_total)) {
            return(NULL)
          } else if (input$MenFUAge3_mening > input$MenFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_m13_Tdap_fwup <- renderUI({
          if (is.na(input$MenFUAge3_tdap) | is.na(input$MenFUAge3_total)) {
            return(NULL)
          } else if (input$MenFUAge3_tdap > input$MenFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
        
      }
      # Boys and girls combined - age 13
      if ("13" %in% input$Q2FU &
          input$Q3FU == "No, we will have combined rate data") {
        output$furates_13 <- renderUI({
          box(
            h3("Vaccination Rates for Male and Female Patients Combined, Age 13"),
            title = "Vaccination Rates for Male and Female Patients Combined, Age 13",
            collapsible = T,
            solidHeader = T,
            status = "success",
            collapsed = T,
            width = '100%',
            HTML("<br>"),
            h5(
              strong(
                "Total number of active medical patients ",
                em("age 13 ", style = "color:red"),
                "your clinic saw in 2021?"
              )
            ),
            numericInput(
              "BothFUAge3_total",
              label = NULL,
              value = followupRates$BothFUAge3_total,
              min = 0,
              step = 1,
              width = '100%'
            ),
            uiOutput("error_13_total_fwup"),
            h5(
              strong(
                "Number of active patients who received at least one (1) HPV dose"
              )
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge3_dose1",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge3_dose1,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge3_dose1_rate")
            ),
            uiOutput("error_13_gt1_fwup"),
            h5(
              strong("Number of active patients who received both (2) HPV doses")
            ),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge3_dose2",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge3_dose2,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge3_dose2_rate")
            ),
            uiOutput("error_13_2plus_fwup"),
            uiOutput("error_13a_2plus_fwup"),
            h5(strong("Meningococcal")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge3_mening",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge3_mening,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge3_mening_rate")
            ),
            uiOutput("error_13_mening_fwup"),
            h5(strong("Tdap")),
            splitLayout(
              cellWidths = c("30%", "70%"),
              numericInput(
                "BothFUAge3_tdap",
                label = NULL,
                min = 0,
                value = followupRates$BothFUAge3_tdap,
                step = 1,
                width = '50%'
              ),
              textOutput("BothFUAge3_tdap_rate")
            ),
            uiOutput("error_13_Tdap_fwup"),
          )
        })
        output$BothFUAge3_dose1_rate <- renderText({
          validate(need(input$BothFUAge3_total, ""))
          if (is.na(input$BothFUAge3_dose1)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge3_dose1)) {
            rate <- input$BothFUAge3_dose1 / input$BothFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothFUAge3_dose2_rate <- renderText({
          validate(need(input$BothFUAge3_total, ""))
          if (is.na(input$BothFUAge3_dose2)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge3_dose2)) {
            rate <- input$BothFUAge3_dose2 / input$BothFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothFUAge3_mening_rate <- renderText({
          validate(need(input$BothFUAge3_total, ""))
          if (is.na(input$BothFUAge3_mening)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge3_mening)) {
            rate <- input$BothFUAge3_mening / input$BothFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        output$BothFUAge3_tdap_rate <- renderText({
          validate(need(input$BothFUAge3_total, ""))
          if (is.na(input$BothFUAge3_tdap)) {
            return(NULL)
          }
          if (!is.na(input$BothFUAge3_tdap)) {
            rate <- input$BothFUAge3_tdap / input$BothFUAge3_total
            rate <- rate * 100
            rate <- format(round(rate, 1), nsmall = 1)
            rate <-
              paste0(rate, "% vaccination rate in this age group")
          }
        })
        
        output$error_13_total_fwup <- renderUI({
          if (is.na(input$BothFUAge3_total)) {
            helpText("This number is required", style = "color:red")
          } else {
            NULL
          }
        })
        output$error_13_gt1_fwup <- renderUI({
          if (is.na(input$BothFUAge3_dose1) | is.na(input$BothFUAge3_total)) {
            return(NULL)
          } else if (input$BothFUAge3_dose1 > input$BothFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_13_2plus_fwup <- renderUI({if (is.na(input$BothFUAge3_dose2) | is.na(input$BothFUAge3_total)) {
          return(NULL)
        } else if (input$BothFUAge3_dose2 > input$BothFUAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
        })
        output$error_13a_2plus_fwup <- renderUI({
          if (is.na(input$BothFUAge3_dose2) | is.na(input$BothFUAge3_dose1)) {
            return(NULL)
          } else if (input$BothFUAge3_dose2 >= input$BothFUAge3_dose1){
            helpText("This number can't be greater than the total number of patients who received the first dose",
                     style = "color:red")
          }
        })
        output$error_13_mening_fwup <- renderUI({
          if (is.na(input$BothFUAge3_mening) | is.na(input$BothFUAge3_total)) {
            return(NULL)
          } else if (input$BothFUAge3_mening > input$BothFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        output$error_13_Tdap_fwup <- renderUI({
          if (is.na(input$BothFUAge3_tdap) | is.na(input$BothFUAge3_total)) {
            return(NULL)
          } else if (input$BothFUAge3_tdap > input$BothFUAge3_total) {
            helpText("This number can't be greater than the number of patients seen",
                     style = "color:red")
          }
        })
        
      }
      
    }) # end the 13 observation

  
  output$static_table_fwup <- renderTable({
    q <-
      data.frame(rbind(
        c("AGES", "BORN", "EVER RECEIVED THE FOLLOWING VACCINES"),
        c("9-10", "2011-2012", "At least 1 dose of HPV; 2 doses of HPV"),
        c("11-12",  "2009-2010", "At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap"),
        c("13", "2008","At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap")
      ))
    
  }, striped = FALSE, align = "l", colnames = FALSE, rownames = FALSE, bordered =
    TRUE, spacing = "m", width = "70%")
  
  
  observeEvent(input$button_fwup_rates, {
    
    # Vaccination rates:
    Q1FU <- input$Q1FU
    Q1FU_other <-  ifelse(is.null(input$Q1FU_other), "", input$Q1FU_other)
    
    # Q2FU Groupbox text input
    Q2FU <-  ifelse(is.null(input$Q2FU), "", paste(input$Q2FU, collapse = ", "))
    answers <- c("9-10", "11-12",  "13",
                 "We can't report on these ages (other, please specify)?")
    Q2FU_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q2FU, fixed = T)) == 1, x, NA)
    })
    Q2FU_1 <- as.character(Q2FU_responses[1])
    Q2FU_2 <- as.character(Q2FU_responses[2])
    Q2FU_3 <- as.character(Q2FU_responses[3])
    Q2FU_4 <- as.character(Q2FU_responses[4])
    rm(Q2FU_responses, answers)
    
    
    Q2FU_other <- ifelse(is.null(input$Q2FU_other), "", input$Q2FU_other)
    Q3FU <- input$Q3FU
    Q4FU <- input$Q4FU
    
    
    
    # Rate variables
    
    # Girls - ages 9-10
    FemFUAge1_total <- ifelse(is.null(input$FemFUAge1_total), NA, input$FemFUAge1_total)
    FemFUAge1_dose1 <- ifelse(is.null(input$FemFUAge1_dose1), NA, input$FemFUAge1_dose1)
    FemFUAge1_dose2 <- ifelse(is.null(input$FemFUAge1_dose2), NA, input$FemFUAge1_dose2)
    # Boys - ages 9-10
    MenFUAge1_total <- ifelse(is.null(input$MenFUAge1_total), NA, input$MenFUAge1_total)
    MenFUAge1_dose1 <- ifelse(is.null(input$MenFUAge1_dose1), NA, input$MenFUAge1_dose1)
    MenFUAge1_dose2 <- ifelse(is.null(input$MenFUAge1_dose2), NA, input$MenFUAge1_dose2)
    # Both - ages 9-10
    BothFUAge1_total <- ifelse(is.null(input$BothFUAge1_total), NA, input$BothFUAge1_total)
    BothFUAge1_dose1 <- ifelse(is.null(input$BothFUAge1_dose1), NA, input$BothFUAge1_dose1)
    BothFUAge1_dose2 <- ifelse(is.null(input$BothFUAge1_dose2), NA, input$BothFUAge1_dose2)
    
    # Girls - ages 11-12
    FemFUAge2_total <- ifelse(is.null(input$FemFUAge2_total), NA, input$FemFUAge2_total)
    FemFUAge2_dose1 <- ifelse(is.null(input$FemFUAge2_dose1), NA, input$FemFUAge2_dose1)
    FemFUAge2_dose2 <- ifelse(is.null(input$FemFUAge2_dose2), NA, input$FemFUAge2_dose2)
    FemFUAge2_mening <- ifelse(is.null(input$FemFUAge2_mening), NA, input$FemFUAge2_mening)
    FemFUAge2_tdap <- ifelse(is.null(input$FemFUAge2_tdap), NA, input$FemFUAge2_tdap)
    
    # Boys - ages 11-12
    MenFUAge2_total <- ifelse(is.null(input$MenFUAge2_total), NA, input$MenFUAge2_total)
    MenFUAge2_dose1 <- ifelse(is.null(input$MenFUAge2_dose1), NA, input$MenFUAge2_dose1)
    MenFUAge2_dose2 <- ifelse(is.null(input$MenFUAge2_dose2), NA, input$MenFUAge2_dose2)
    MenFUAge2_mening <- ifelse(is.null(input$MenFUAge2_mening), NA, input$MenFUAge2_mening)
    MenFUAge2_tdap <- ifelse(is.null(input$MenFUAge2_tdap), NA, input$MenFUAge2_tdap)
    
    # Both - ages 11-12
    BothFUAge2_total <- ifelse(is.null(input$BothFUAge2_total), NA, input$BothFUAge2_total)
    BothFUAge2_dose1 <- ifelse(is.null(input$BothFUAge2_dose1), NA, input$BothFUAge2_dose1)
    BothFUAge2_dose2 <- ifelse(is.null(input$BothFUAge2_dose2), NA, input$BothFUAge2_dose2)
    BothFUAge2_mening <- ifelse(is.null(input$BothFUAge2_mening), NA, input$BothFUAge2_mening)
    BothFUAge2_tdap <- ifelse(is.null(input$BothFUAge2_tdap), NA, input$BothFUAge2_tdap)
    
    # Girls - ages 13
    FemFUAge3_total <-  ifelse(is.null(input$FemFUAge3_total), NA, input$FemFUAge3_total)
    FemFUAge3_dose1 <-  ifelse(is.null(input$FemFUAge3_dose1), NA, input$FemFUAge3_dose1)
    FemFUAge3_dose2 <- ifelse(is.null(input$FemFUAge3_dose2), NA, input$FemFUAge3_dose2)
    FemFUAge3_mening <- ifelse(is.null(input$FemFUAge3_mening), NA, input$FemFUAge3_mening)
    FemFUAge3_tdap <-  ifelse(is.null(input$FemFUAge3_tdap), NA, input$FemFUAge3_tdap)
    # Boys - ages 13
    MenFUAge3_total <- ifelse(is.null(input$MenFUAge3_total), NA, input$MenFUAge3_total)
    MenFUAge3_dose1 <- ifelse(is.null(input$MenFUAge3_dose1), NA, input$MenFUAge3_dose1)
    MenFUAge3_dose2 <- ifelse(is.null(input$MenFUAge3_dose2), NA, input$MenFUAge3_dose2)
    MenFUAge3_mening <- ifelse(is.null(input$MenFUAge3_mening), NA, input$MenFUAge3_mening)
    MenFUAge3_tdap <- ifelse(is.null(input$MenFUAge3_tdap), NA, input$MenFUAge3_tdap)
    
    # Both - ages 13
    BothFUAge3_total <- ifelse(is.null(input$BothFUAge3_total), NA, input$BothFUAge3_total)
    BothFUAge3_dose1 <- ifelse(is.null(input$BothFUAge3_dose1), NA, input$BothFUAge3_dose1)
    BothFUAge3_dose2 <- ifelse(is.null(input$BothFUAge3_dose2), NA, input$BothFUAge3_dose2)
    BothFUAge3_mening <- ifelse(is.null(input$BothFUAge3_mening), NA, input$BothFUAge3_mening)
    BothFUAge3_tdap <- ifelse(is.null(input$BothFUAge3_tdap), NA, input$BothFUAge3_tdap)
    
    # Create a data frame of rates, impute the BothFUAgexxx variables if needed
    rates <- data.frame(
      FemFUAge1_total, FemFUAge1_dose1, FemFUAge1_dose2,
      MenFUAge1_total, MenFUAge1_dose1, MenFUAge1_dose2,
      BothFUAge1_total, BothFUAge1_dose1,  BothFUAge1_dose2,
      FemFUAge2_total, FemFUAge2_dose1, FemFUAge2_dose2, FemFUAge2_mening,  FemFUAge2_tdap,
      MenFUAge2_total, MenFUAge2_dose1, MenFUAge2_dose2, MenFUAge2_mening, MenFUAge2_tdap,
      BothFUAge2_total, BothFUAge2_dose1, BothFUAge2_dose2, BothFUAge2_mening, BothFUAge2_tdap,
      FemFUAge3_total, FemFUAge3_dose1, FemFUAge3_dose2, FemFUAge3_mening, FemFUAge3_tdap,
      MenFUAge3_total, MenFUAge3_dose1,  MenFUAge3_dose2, MenFUAge3_mening, MenFUAge3_tdap,
      BothFUAge3_total, BothFUAge3_dose1,  BothFUAge3_dose2, BothFUAge3_mening, BothFUAge3_tdap)
    
    # make sure all are numeric
    rates[, names(rates)] <-
      lapply(rates[, names(rates)], as.numeric)
    
    rm(FemFUAge1_total, FemFUAge1_dose1, FemFUAge1_dose2,
       MenFUAge1_total, MenFUAge1_dose1, MenFUAge1_dose2,
       BothFUAge1_total, BothFUAge1_dose1,  BothFUAge1_dose2,
       FemFUAge2_total, FemFUAge2_dose1, FemFUAge2_dose2, FemFUAge2_mening,  FemFUAge2_tdap,
       MenFUAge2_total, MenFUAge2_dose1, MenFUAge2_dose2, MenFUAge2_mening, MenFUAge2_tdap,
       BothFUAge2_total, BothFUAge2_dose1, BothFUAge2_dose2, BothFUAge2_mening, BothFUAge2_tdap,
       FemFUAge3_total, FemFUAge3_dose1, FemFUAge3_dose2, FemFUAge3_mening, FemFUAge3_tdap,
       MenFUAge3_total, MenFUAge3_dose1,  MenFUAge3_dose2, MenFUAge3_mening, MenFUAge3_tdap,
       BothFUAge3_total, BothFUAge3_dose1,  BothFUAge3_dose2, BothFUAge3_mening, BothFUAge3_tdap)

    
    followupRates <- data.frame(Username = session$user, fwupratesDate = as.numeric(Sys.time()),
                                Q1FU, Q1FU_other, 
                                Q2FU, Q2FU_1, Q2FU_2, Q2FU_3, Q2FU_4, 
                                Q2FU_other, Q3FU, Q4FU, rates, FollowupRatesComments=FollowupRatesComments)
        updateDB(DataSrc, userFilename, "followupRates", followupRates)
    updateTabsetPanel(session, "tabbox_final", selected = "p2_additional")

  })
  
  
  # Add latest dates
    output$date_fwup_rates <- renderValueBox({
      if (is.na(followupRates$fwupratesDate)) {
        valueBox(tags$p("Data entry started", style="font-size: 75%"), 
tags$p(Sys.Date(), style="font-size: 150%;"),
                 icon=icon("horse"),
                 color="blue")   
      } else {
       valueBox(tags$p("Last Submitted", style="font-size: 75%"), 
                tags$p(as.POSIXct(followupRates$fwupratesDate, origin = "1970-01-01"), style="font-size: 150%;"), 
             icon=icon("horse"),
             color="blue")     
      }

  })
  
  
  
  
  # Additional information tab ----------------------------------------------
  
    
    
    
  additionalInfo <- getRecentData(DataSrc, userFilename, "additionalInfo")
  
    AdditionalInfoComments <- additionalInfo$AdditionalInfoComments
    
    foo7FU <- with(additionalInfo, c(Q7FU_1, Q7FU_2, Q7FU_3, Q7FU_4, Q7FU_5))
    foo7FU <- foo7FU[!is.na(foo7FU)]
    
    foo8FU <- with(additionalInfo, c(Q8FU_1, Q8FU_2, Q8FU_3))
    foo8FU <- foo8FU[!is.na(foo8FU)]
    
    foo9FU <- with(additionalInfo, c(Q9FU_1, Q9FU_2, Q9FU_3, Q9FU_4,
                                     Q9FU_5, Q9FU_6, Q9FU_7, Q9FU_8, Q9FU_9))
    foo9FU <- foo9FU[!is.na(foo9FU)]
    
    foo13FU <- with(additionalInfo, c(Q13FU_1, Q13FU_2, Q13FU_3, Q13FU_4,
                                      Q13FU_5, Q13FU_6, Q13FU_7, Q13FU_8))
    foo13FU <- foo13FU[!is.na(foo13FU)]
    
    foo14FU <- with(additionalInfo, c(Q14FU_1, Q14FU_2, Q14FU_3, Q14FU_4, Q14FU_5))
    foo14FU <- foo14FU[!is.na(foo14FU)]
    
    
    
    output$Q5FU <- renderUI({
      selectInput(
        inputId = "Q5FU",
        label = "5.  Did you use the same primary data source for all pulls?",
        choices = c("", "Yes", "No"),
        selected = additionalInfo$Q5FU)
    })
    
    
    output$Q5FU_text <- renderUI({
      validate(need(input$Q5FU, ""))
      if (input$Q5FU != "No")
        return(NULL)
      else{
        textInput("Q5FU_text",
                  label = "5a. Please describe what changed",
                  value = additionalInfo$Q5FU_text,
                  width = "100%")
      }
    })
    output$Q6FU <- renderUI({
      selectInput(
        inputId = "Q6FU",
        label = "6.  Did you meet your project goals?",
        choices = c("", "Yes", "Some, but not all",  "No"),
        selected = additionalInfo$Q6FU)
    })
    
    output$Q6FU_text <- renderUI({
      validate(need(input$Q6FU, ""))
      if (c("Yes", "") %in% input$Q6FU)
        return(NULL)
      else{
        textInput("Q6FU_text",
                  label = "6a. Tell us more about this",
                  value = additionalInfo$Q6FU_text,
                  width = "100%")
      }
    })
    
    # Q7FU - checkbox group
    output$Q7FU <- renderUI({
      checkboxGroupInput(
        inputId = "Q7FU",
        label = p(
          "7.  We conducted or participated in the following HPV vaccination training and education activities in 2021: ",
          em(" (check all that apply)")),
        choices = c(
          "Educated staff on HPV vaccination as cancer prevention",
          "Educated staff on strategies to improve HPV vaccination rates",
          "Identified HPV vaccination champions",
          "Trained providers to make an effective HPV vaccine recommendation",
          "Other (specify)"),
        selected =  foo7FU,
        width = '100%')
    })
    output$Q7FU_other <- renderUI({
      validate(need(input$Q7FU, ""))
      if ("Other (specify)" %in% input$Q7FU) {
        textInput(
          "Q7FU_other",
          label = "Please specify",
          value = additionalInfo$Q7FU_other,
          width = "100%"
        )
      }
    })
    
    # Q8FU - checkbox group
    output$Q8FU <- renderUI({
      checkboxGroupInput(
        inputId = "Q8FU",
        label = p("8.  Did you participate in any of the following in 2021 (select all that apply)?"),
        choices = c("ECHO",
                    "Learning collaborative",
                    "Other (specify)"),
        width = "100%",
        selected = foo8FU)
    })
    output$Q8FU_other <- renderUI({
      validate(need(input$Q8FU, ""))
      if ("Other (specify)" %in% input$Q8FU) {
        textInput(
          "Q8FU_other",
          label = "Please specify",
          value = additionalInfo$Q8FU,
          width = "100%"
        )
      }
    })
    
    # Q9FU - checkbox group
    output$Q9FU <- renderUI({
      checkboxGroupInput(
        inputId = "Q9FU",
        label = p(
          "9.  We implemented the following interventions to increase HPV vaccination in 2021: ",
          em("(check all that apply)")),
        choices = c(
          "Extended hours",
          "Modified EHR",
          "Offered in alternative settings like schools or mobile units",
          "Parent/patient education",
          "Patient reminders",
          "Provider assessment and feedback",
          "Provider prompts/reminders",
          "Standing orders",
          "Other (specify)"),
        width = '100%',
        selected =  foo9FU)
    })
    output$Q9FU_other <- renderUI({
      validate(need(input$Q9FU, ""))
      if ("Other (specify)" %in% input$Q9FU) {
        textInput(
          "Q9FU_other",
          label = "Please specify",
          value = additionalInfo$Q9FU_other,
          width = "100%"
        )
      }
    })
    
    output$Q10FU <- renderUI({
      numericInput(inputId = "Q10FU",
                   label = "10.  How many providers did you train?",
                   width = '100%',
                   min = 0,
                   step = 1,
                   value = additionalInfo$Q10FU)
    })
    output$Q11FU <- renderUI({              
      numericInput( inputId = "Q11FU",
                    label = "11.  What was the average number of training hours per provider?",
                    min = 0,
                    width = '100%',
                    value = additionalInfo$Q11FU)
    })
    output$Q12FU <- renderUI({
      numericInput(
        inputId = "Q12FU",
        label = "12  How many other staff did we educate?",
        min = 0,
        step = 1,
        width = '100%',
        value = additionalInfo$Q12FU)
    })
    
    
    
    # Q13FU - checkbox group
    output$Q13FU <- renderUI({
      checkboxGroupInput(inputId = "Q13FU",
                         label = p("13.  We used the following tools and resources: ",
                                   em("(check all that apply)")),
                         choices = c("ACS 'Steps' Guide",
                                     "ACS 'HPV 101' slides",
                                     "ACS Reminder Cards",
                                     "ACS and/or Merck EHR Guides",
                                     "ACS and/or CDC parent handout",
                                     "CDC 'You Are The Key' slides",
                                     "Other ACS Key Resources Roundtable Action Guides",
                                     "Other (specify)"),
                         width = '100%',
                         selected =  foo13FU)
    })
    output$Q13FU_other <- renderUI({
      validate(need(input$Q13FU, ""))
      if ("Other (specify)" %in%  input$Q13FU) {
        textInput(
          "Q13FU_other",
          label = "Please specify",
          value = additionalInfo$Q13FU_other,
          width = "100%"
        )
      }
    })
    
    
    # Q14FU checkbox group
    output$Q14FU <- renderUI({
      checkboxGroupInput(inputId = "Q14FU",
                         label = "14.  What roles did ACS play in this project?",
                         choices = c(
                           "Member of HPV QI project team",
                           "Provided input on strategy",
                           "Provided materials",
                           "Provided technical assistance",
                           "Other (specify)"),
                         width = '100%',
                         selected =  foo14FU)
    })
    output$Q14FU_other <- renderUI({
      validate(need(input$Q14FU, ""))
      if ("Other (specify)" %in%  input$Q14FU) {
        textInput(
          "Q14FU_other",
          label = "Please specify",
          value = additionalInfo$Q14FU_other,
          width = "100%"
        )
      }
    })
    
    output$Q15FU <- renderUI({
      textAreaInput(
        inputId = "Q15FU",
        label = "15. What were the successes and challenges of this project?",
        value = additionalInfo$Q15FU,
        width = '100%',
        rows = 3
      )
    })
    output$Q16FU <- renderUI({
      textAreaInput(
        inputId = "Q16FU",
        label = "16.  What is your plan to sustain the changes you made in this project?",
        value = additionalInfo$Q16FU,
        width = '100%',
        rows = 3)
    })
    output$Q17FU <- renderUI({
      textAreaInput(
        inputId = "Q17FU",
        label = "17.  Is there anything else to add about this project?",
        value = additionalInfo$Q17FU,
        width = '100%',
        rows = 3)
    })
    output$Q18FU <- renderUI({
      selectInput(
        inputId = "Q18FU",
        label = "18.  Will your system continue to work with ACS to increase our HPV vaccination rates?",
        choices = c("", "Yes", "No"),
        selected =  additionalInfo$Q18FU)
    })
   output$Q18FU_more <- renderUI({
     if (input$Q18FU =="No"){
      textInput(
        inputId = "Q18FU_more",
        label = "18a. Tell us more about why not",
        value= additionalInfo$Q18FU_more,
        width='100%')
     }
    })

  
  
  observeEvent(input$save_followup, {
    
    
    Q5FU <- ifelse(is.null(input$Q5FU), "", input$Q5FU)
    Q5FU_text <- ifelse(is.null(input$Q5FU_text), "", input$Q5FU_text)
    Q6FU <- ifelse(is.null(input$Q6FU), "", input$Q6FU)
    Q6FU_text <- ifelse(is.null(input$Q6FU_text), "", input$Q6FU_text)
    
    
    Q7FU <- ifelse(is.null(input$Q7FU), "", paste(input$Q7FU, collapse = ", "))
    answers <- c("Educated staff on HPV vaccination as cancer prevention",
                 "Educated staff on strategies to improve HPV vaccination rates",
                 "Identified HPV vaccination champions",
                 "Trained providers to make an effective HPV vaccine recommendation",
                 "Other (specify)")
    Q7FU_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q7FU, fixed = T)) == 1, x, NA)
    })
    Q7FU_1 <- as.character(Q7FU_responses[1])
    Q7FU_2 <- as.character(Q7FU_responses[2]) 
    Q7FU_3 <- as.character(Q7FU_responses[3])
    Q7FU_4 <- as.character(Q7FU_responses[4])
    Q7FU_5 <- as.character(Q7FU_responses[5])
    rm(answers, Q7FU_responses)
    Q7FU_other <- ifelse(is.null(input$Q7FU_other), "", input$Q7FU_other)
    
    
    Q8FU <- ifelse(is.null(input$Q8FU), "", paste(input$Q8FU, collapse = ", "))
    answers <- c("ECHO",
                 "Learning collaborative",
                 "Other (specify)")
    Q8FU_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q8FU, fixed = T)) == 1, x, NA)
    })
    Q8FU_1 <- as.character(Q8FU_responses[1])
    Q8FU_2 <- as.character(Q8FU_responses[2])
    Q8FU_3 <- as.character(Q8FU_responses[3])
    rm(answers, Q8FU_responses)
    Q8FU_other <- ifelse(is.null(input$Q8FU_other), "", input$Q8FU_other)
    
    
    Q9FU <- ifelse(is.null(input$Q9FU), "", paste(input$Q9FU, collapse = ", "))
    answers <-  c("Extended hours",
                  "Modified EHR",
                  "Offered in alternative settings like schools or mobile units",
                  "Parent/patient education",
                  "Patient reminders",
                  "Provider assessment and feedback",
                  "Provider prompts/reminders",
                  "Standing orders",
                  "Other (specify)")
    Q9FU_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q9FU, fixed = T)) == 1, x, NA)
    })
    Q9FU_1 <- as.character(Q9FU_responses[1])
    Q9FU_2 <- as.character(Q9FU_responses[2])
    Q9FU_3 <- as.character(Q9FU_responses[3])
    Q9FU_4 <- as.character(Q9FU_responses[4])
    Q9FU_5 <- as.character(Q9FU_responses[5])
    Q9FU_6 <- as.character(Q9FU_responses[6])
    Q9FU_7 <- as.character(Q9FU_responses[7])
    Q9FU_8 <- as.character(Q9FU_responses[8])
    Q9FU_9 <- as.character(Q9FU_responses[9])
    rm(Q9FU_responses, answers)
    Q9FU_other <-  ifelse(is.null(input$Q9FU_other), "", input$Q9FU_other)
    Q10FU <- as.numeric(input$Q10FU)
    Q11FU <- as.numeric(input$Q11FU)
    Q12FU <- as.numeric(input$Q12FU)
    
    Q13FU <- ifelse(is.null(input$Q13FU), "", paste(input$Q13FU, collapse = ", "))
    answers <- c("ACS 'Steps' Guide",
                 "ACS 'HPV 101' slides",
                 "ACS Reminder Cards",
                 "ACS and/or Merck EHR Guides",
                 "ACS and/or CDC parent handout",
                 "CDC 'You Are The Key' slides",
                 "Other ACS Key Resources Roundtable Action Guides",
                 "Other (specify)")
    Q13FU_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q13FU, fixed = T)) == 1, x, NA)
    })
    Q13FU_1 <- as.character(Q13FU_responses[1])
    Q13FU_2 <- as.character(Q13FU_responses[2])
    Q13FU_3 <- as.character(Q13FU_responses[3])
    Q13FU_4 <- as.character(Q13FU_responses[4])
    Q13FU_5 <- as.character(Q13FU_responses[5])
    Q13FU_6 <- as.character(Q13FU_responses[6])
    Q13FU_7 <- as.character(Q13FU_responses[7])
    Q13FU_8 <- as.character(Q13FU_responses[8])
    Q13FU_other <-  ifelse(is.null(input$Q13FU_other), "", input$Q13FU_other)
    
    Q14FU <- ifelse(is.null(input$Q14FU), "", paste(input$Q14FU, collapse = ", "))
    answers <- c("Member of HPV QI project team",
                 "Provided input on strategy",
                 "Provided materials",
                 "Provided technical assistance",
                 "Other (specify)")
    Q14FU_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q14FU, fixed = T)) == 1, x, NA)
    })
    Q14FU_1 <- as.character(Q14FU_responses[1])
    Q14FU_2 <- as.character(Q14FU_responses[2])
    Q14FU_3 <- as.character(Q14FU_responses[3])
    Q14FU_4 <- as.character(Q14FU_responses[4])
    Q14FU_5 <- as.character(Q14FU_responses[5])    
    rm(answers, Q14FU_responses)
    Q14FU_other <- ifelse(is.null(input$Q14FU_other), "", input$Q14FU_other)
    Q15FU <- input$Q15FU
    Q16FU <- input$Q16FU
    Q17FU <- input$Q17FU
    Q18FU <- ifelse(is.null(input$Q18FU), "", input$Q18FU)
    Q18FU_more <- ifelse(is.null(input$Q18FU_more), "", input$Q18FU_more)
    
    
    # foo7FU <- with(additionalInfo, c(Q7FU_1, Q7FU_2, Q7FU_3, Q7FU_4, Q7FU_5))
    # foo7FU <- foo7FU[!is.na(foo7FU)]
    # 
    # foo8FU <- with(additionalInfo, c(Q8FU_1, Q8FU_2, Q8FU_3))
    # foo8FU <- foo8FU[!is.na(foo8FU)]
    # 
    # foo9FU <- with(additionalInfo, c(Q9FU_1, Q9FU_2, Q9FU_3, Q9FU_4,
    #                                  Q9FU_5, Q9FU_6, Q9FU_7, Q9FU_8, Q9FU_9))
    # foo9FU <- foo9FU[!is.na(foo9FU)]
    # 
    # foo13FU <- with(additionalInfo, c(Q13FU_1, Q13FU_2, Q13FU_3, Q13FU_4,
    #                                   Q13FU_5, Q13FU_6, Q13FU_7, Q13FU_8))
    # foo13FU <- foo13FU[!is.na(foo13FU)]
    # 
    # foo14FU <- with(additionalInfo, c(Q14FU_1, Q14FU_2, Q14FU_3, Q14FU_4, Q14FU_5))
    # foo14FU <- foo14FU[!is.na(foo14FU)]
    
    
    additionalInfo <- data.frame(Username = session$user, additionalInfoDate = as.numeric(Sys.time()), Q5FU, Q5FU_text, Q6FU, Q6FU_text,
                                 Q7FU, Q7FU_1, Q7FU_2, Q7FU_3, Q7FU_4, Q7FU_5,Q7FU_other,
                                 Q8FU, Q8FU_1, Q8FU_2, Q8FU_3,  Q8FU_other,
                                 Q9FU, Q9FU_1, Q9FU_2, Q9FU_3, Q9FU_4,
                                 Q9FU_5, Q9FU_6, Q9FU_7, Q9FU_8, Q9FU_9, Q9FU_other,
                                 Q10FU, Q11FU, Q12FU, 
                                 Q13FU, Q13FU_1, Q13FU_2, Q13FU_3, Q13FU_4,
                                 Q13FU_5, Q13FU_6, Q13FU_7, Q13FU_8, Q13FU_other,
                                 Q14FU, Q14FU_1, Q14FU_2, Q14FU_3, Q14FU_4, Q14FU_5, Q14FU_other,
                                 Q15FU, Q16FU, Q17FU, Q18FU, Q18FU_more,
                                 AdditionalInfoComments=AdditionalInfoComments)

    
    
    rm(Q5FU, Q5FU_text, Q6FU, Q6FU_text,
       Q7FU, Q7FU_1, Q7FU_2, Q7FU_3, Q7FU_4, Q7FU_5,Q7FU_other,
       Q8FU, Q8FU_1, Q8FU_2, Q8FU_3, Q8FU_other,
       Q9FU, Q9FU_1, Q9FU_2, Q9FU_3, Q9FU_4,
       Q9FU_5, Q9FU_6, Q9FU_7, Q9FU_8, Q9FU_9, Q9FU_other,
       Q10FU, Q11FU, Q12FU, 
       Q13FU, Q13FU_1, Q13FU_2, Q13FU_3, Q13FU_4,
       Q13FU_5, Q13FU_6, Q13FU_7, Q13FU_8, Q13FU_other,
       Q14FU, Q14FU_1, Q14FU_2, Q14FU_3, Q14FU_4, Q14FU_other,
       Q15FU, Q16FU, Q17FU, Q18FU, foo7FU, foo9FU, foo13FU, foo14FU, foo8FU)
    
    updateDB(DataSrc, userFilename, "additionalInfo", additionalInfo)
    updateTabsetPanel(session, "tabbox_final", selected = "p2_submission")

    
  })
  
  # Latest update box
    output$date_fwup_additional <- renderValueBox({
      if (is.na(additionalInfo$additionalInfoDate)){
        valueBox(tags$p("Data entry started", style="font-size: 75%"), 
tags$p(Sys.Date(), style="font-size: 150%;"),
                 icon=icon("horse"),
                 color="blue")   
      } else {
    valueBox(tags$p("Last Submitted", style="font-size: 75%"), 
             tags$p(as.POSIXct(additionalInfo$additionalInfoDate, origin = "1970-01-01"), style="font-size: 150%;"), 
             icon=icon("horse"),
             color="blue")        
      }

  })
    

  # Submit the followup data ------------------------------------------------

  
  observeEvent(input$button_fwup_final, {
    
    
    # nullDB() creates an empty dataset that matches the
    nullDB()
    nullData <- dbConnect(SQLite(), "nullDB.DB")
    followupTable <- getFollowupData(loc = DataSrc, filename = userFilename)

    FUrates <- c("FemFUAge1_total","FemFUAge1_dose1", "FemFUAge1_dose2",
               "MenFUAge1_total", "MenFUAge1_dose1", "MenFUAge1_dose2",
               "BothFUAge1_total","BothFUAge1_dose1", "BothFUAge1_dose2",
               "FemFUAge2_total", "FemFUAge2_dose1", "FemFUAge2_dose2", "FemFUAge2_mening", "FemFUAge2_tdap",
               "MenFUAge2_total", "MenFUAge2_dose1", "MenFUAge2_dose2", "MenFUAge2_mening", "MenFUAge2_tdap",
               "BothFUAge2_total", "BothFUAge2_dose1", "BothFUAge2_dose2", "BothFUAge2_mening", "BothFUAge2_tdap",
               "FemFUAge3_total", "FemFUAge3_dose1", "FemFUAge3_dose2", "FemFUAge3_mening", "FemFUAge3_tdap",
               "MenFUAge3_total", "MenFUAge3_dose1", "MenFUAge3_dose2", "MenFUAge3_mening", "MenFUAge3_tdap",
               "BothFUAge3_total", "BothFUAge3_dose1", "BothFUAge3_dose2", "BothFUAge3_mening", "BothFUAge3_tdap")
    FURates <- apply(followupTable[,FUrates], 1, sum, na.rm=T)



    # 2.  Missing additional info
    additionalInfo <- dbReadTable(nullData, "additionalInfo")
    additionalInfo2 <- followupTable[,names(additionalInfo)]
    
    condition <- ifelse(FURates == 0, 1, ifelse(
      identical(additionalInfo,additionalInfo2), 2, 99))


if (condition == 1) {
  shinyalert(title = "Uh oh",
             text = "It looks like you didn't fill out the vaccination rate tables.  Please return to the 'Updated Rates'
             tab and enter your data into the tables",
             type = "error",
             showConfirmButton = F, showCancelButton = T)
} else if (condition == 2) {
  shinyalert(title="Uh oh",
             text="It looks like you did not fill out the data in the Additional Info tab.  Please return to this section and input your data",
             type="error", showConfirmButton = FALSE, showCancelButton = TRUE)
  
} else {
  shinyalert(title="Confirm submit", text="You are about to submit the follow-up information for your health system.
              Have you checked and confirmed you've answered every question and your answers
              are correct? If yes, hit continue. If not, hit cancel to review your answers again.",
             type="warning", showCancelButton=TRUE, showConfirmButton = TRUE, closeOnClickOutside=FALSE,
             closeOnEsc= FALSE, confirmButtonText = "Submit", cancelButtonText="Cancel (go back to review)",
             inputId="fwup_shinyalert")
  }
    
  })
  
  observe({
    req(input$fwup_shinyalert)
    
    followupTable <- getFollowupData(loc = DataSrc, filename = userFilename)
    n <- names(followupTable)
    n <- n[!n %in% c("Username", "FollowupSubmitDate")]
    followupTable$FollowupSubmitDate <- Sys.time()
    followupTable <- followupTable[,c("Username", "FollowupSubmitDate", n)]
    updateDB(loc = DataSrc, filename = userFilename, table = "followupTable", df = followupTable)
    session$reload()
  })
  
  # Baseline Dashboard ------------------------------------------------------
  
  df <- getBaselineData(loc=DataSrc, filename = userFilename)
  allData <- allStudyData(loc = DataSrc)
  
  # Add their q36f aim to the top of the dashboard
  output$remembergoal1 <- renderUI({
        h4(df$Q36f, align = "center")
  })
  
  
  
      # Figure 1 - all ages combined
      output$conversationCorner1 <- renderText({
        paste(
          "Look at the difference between the rates<br/>for HPV initiation and completion.",
          "<br/><br/>",
          "If initiation is high, what evidence-based<br/>interventions are best for increasing HPV<br/>completion?",
          "<br/><br/>",
          "If initiation is low, focus on improving that<br/>
          prior to completion.  Which evidence-based<br/>
          interventions specifically target HPV initiation?"
        )
      })
  
 
  
      output$baseVacRates1 <- renderPlot({
        figure1(df)
      })
      
      # Figure 2 - age 13+, all collected vaccines
      output$conversationCorner2 <- renderText({
        paste(
          "Look at the difference between the HPV<br/>
              rates and the other two vaccines among<br/>
              13 year olds.",
          "<br/><br/>",
          "What do these gaps tell us about missed<br/>",
          "opportunities?",
          "<br/><br/>",
          "How could a system improve HPV initiation",
          "<br/>",
          "for children who are already receiving",
          "<br/>",
          "other adolescent vaccines?"
        )
      })
      output$baseVacRates2 <- renderPlot({
        age2 <- figure2(df, "age2")
        age3 <- figure2(df, "age3")
        leg <- getLegend(age2)
        
        mytitle <- paste("Baseline HPV vaccination rates compared to other vaccines",
                          df[["HealthSystem"]],
                          sep ="\n")
        
        grid.arrange(age2 + labs(subtitle = "Ages 11-12", title = NULL) + theme(legend.position = "none") , 
                     age3 + labs(subtitle = "Age 13", title = NULL) + theme(legend.position = "bottom"),
                     top = mytitle,
                     nrow = 2,
                     heights = c(3,4)) 
      })
      
      
      # Figure 3 - HPV vaccination rates by age (1 dose vs 2 dose)
      output$conversationCorner3 <- renderText({
        paste(
          "Remember that targeting 9-12 year olds during",
          "<br/>",
          "the intervention year is how you improve rates",
          "<br/>",
          "among children age 13 when you sbmit followup",
          "<br/>",
          "data in 2022."
        )
      })
      
      output$baseVacRates3 <- renderPlot({
        figure3(df)
      })
      
      output$baseVacRates4 <- renderPlot({
        figure4(dat = df, myData = allData, region = input$selectRegion,
                system = input$selectSystemType)
      })

      
  # Baseline dashboard - tab2 - plot by site --------------------------------
 
     
      
      
      output$bySiteConversationBaseline <- renderText({
        paste(
          "Are there any superstar sites?",
          "<br/>",
          "<br/>",
          "What are the successful sites doing",
          "<br/>",
          "differently?  How can other sites learn",
          "<br/>",
          "from them?"
        )
      })
      

  
  
  
  
  # Followup dashboard - tab 1 ---------------------------------------
  
      df <- getAllData(loc=DataSrc, filename = userFilename)
      
      
      # Add their q36f aim to the top of the dashboard
      output$remembergoal2 <- renderUI({
        h4(df$Q36f, align = "center")
      })
      
      
      # Figure 1
      output$FUconversationCorner1 <- renderText({
        "Don't be surprised if rates have not increased much in the <br/>
          final updates.  Use this information to inform preparations <br/>
          for the summer vaccine season, a time when you can have the most impact <br/>
          on vaccination rates!"
      })
      
      
      output$fuTable1 <- DT::renderDataTable({
        fuDisplay1(df)
      },
      options = list(
        paging = FALSE,
        searching = FALSE,
        lengthChange = FALSE,
        pageLength = 15,
        ordering = FALSE
      ),
      rownames = FALSE)
      output$fuFigure1 <-
        renderPlot(fuDisplay1(getAllData(loc=DataSrc, filename = userFilename), 1))
      
      # Figure2
      output$fuTable2 <- DT::renderDataTable({
        fuDisplay2(df)
      },
      options = list(
        paging = FALSE,
        searching = FALSE,
        lengthChange = FALSE,
        pageLength = 15,
        ordering = FALSE
      ),
      rownames = FALSE)
      output$fuFigure2 <-
        renderPlot(
          fuDisplay2(df, 1)
        )

      
      output$FUconversationCorner3 <- renderText({
        "Were there increases in HPV vaccination rates? <br/><br/>
          Did you see more movement with initiation or completion? <br/><br/>
          Was the cross-over impact on mening or tdap vaccination rates? <br/><br/>"
      })
      output$fuFigure3 <-
        renderPlot(fuDisplay3(df))
      output$fuFigure4 <- 
        renderPlot(fuDisplay4(df))
        #renderPlot(fuDisplay3(getFollowupData(loc=DataSrc, filename = userFilename)))
     # output$fuFigure4 <-
        #renderPlot(fuDisplay4(getFollowupData(loc=DataSrc, filename = userFilename)))
      
      
      


 
  
  
  
   # --- ----- GENERATE THE BASELINE REPORT -------------#
  
output$baseline_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "Baselinereport.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "Baseline_report.Rmd")
        file.copy("Baseline_report.Rmd", tempReport, overwrite = TRUE)
        

        # Pull in all the relevant datasets
        storage_download(DataSrc, userFilename, overwrite = T)
        conn <- dbConnect(RSQLite::SQLite(), userFilename)
  
        getTable <- function(table) {
          df <- dbGetQuery(conn, paste("SELECT * FROM", table))
          dateVar <- names(df)[2]  # date is always the second column
          df <- df[order(df[[dateVar]], decreasing = T),]
          return(df[1,])
      }
      
      # You can then do this to pull and merge everything together
      fields <- c("demographics", "systems", "savedRates", "savedActivities")
      merged <- lapply(fields, function(x) {
        getTable(x)
      }) %>%
        Reduce(function(x,y) full_join(x,y,"Username"), .)

        # Set up parameters to pass to Rmd document
        params <- list(HealthSystem = merged$HealthSystem,
                        Q1_DBA= merged$Q1_DBA, 
                       Q2= merged$Q2, 
                       Q2_other=merged$Q2_other, 
                       Q3= merged$Q3,
                       Q4a= merged$Q4a, 
                       Q4b= merged$Q4b, 
                       Q4c= merged$Q4c, 
                       Q5= merged$Q5, 
                       Q5_Email= merged$Q5_Email,
                        Q6= merged$Q6, 
                       Q7= merged$Q7, 
                       Q8= merged$Q8, 
                       Q8_Email= merged$Q8_Email, 
                       Q9= merged$Q9,
                        Q10= merged$Q10, 
                       Q10_other= merged$Q10_other, 
                        Q11= merged$Q11, 
                        Q11_other= merged$Q11_other,
                      Q12= merged$Q12,  
                      Q12_other= merged$Q12_other, 
                      Q13= merged$Q13, 
                      Q13_amount= merged$Q13_amount,
                        Q13_source= merged$Q13_source,  
                      Q13_date= merged$Q13_date, 
                      Q13_length= merged$Q13_length,
                        Q14= merged$Q14, 
                      Q14_other= merged$Q14_other,
                      # Systems questions
                        Q15=merged$Q15,
                        Q15_other= merged$Q15_other,
                        Q15_EHRversion=merged$Q15_EHRversion,
                        Q16= merged$Q16,
                        Q17= merged$Q17,
                        Q18=merged$Q18,
                        Q19=merged$Q19,
                        Q20= merged$Q20,
                        Q20_notes= merged$Q20_notes,
                        Q20_orders= merged$Q20_orders,
                        Q20_other= merged$Q20_other,
                        Q21= merged$Q21,
                        Q21_text= merged$Q21_text,
                        Q22= merged$Q22,
                        Q22_notes= merged$Q22_notes,
                        Q23=merged$Q23,
                        Q23_notes= merged$Q23_notes,
                        Q24= merged$Q24,
                        Q24_years=merged$Q24_years,
                        Q25=merged$Q25,
                        Q26=merged$Q26,
                        Q26_other=merged$Q26_other,
                      
                      # Vaccination rates
                        Q27= merged$Q27,
                        Q27_other= merged$Q27_other,
                        Q28= merged$Q28,
                        Q28_other= merged$Q28_other,
                        Q29= merged$Q29,
                        Q30= merged$Q30,
                        FemAge1_total= merged$FemAge1_total,
                        FemAge1_dose1= merged$FemAge1_dose1,
                        FemAge1_dose2= merged$FemAge1_dose2,
                        MenAge1_total= merged$MenAge1_total,
                        MenAge1_dose1= merged$MenAge1_dose1,
                        MenAge1_dose2= merged$MenAge1_dose2,
                        BothAge1_total= merged$BothAge1_total,
                        BothAge1_dose1= merged$BothAge1_dose1,
                        BothAge1_dose2= merged$BothAge1_dose2,
                        FemAge2_total= merged$FemAge2_total,
                        FemAge2_dose1= merged$FemAge2_dose1,
                        FemAge2_dose2= merged$FemAge2_dose2,
                        FemAge2_mening= merged$FemAge2_mening,
                        FemAge2_tdap= merged$FemAge2_tdap,
                        MenAge2_total= merged$MenAge2_total,
                        MenAge2_dose1= merged$MenAge2_dose1,
                        MenAge2_dose2= merged$MenAge2_dose2,
                        MenAge2_mening= merged$MenAge2_mening,
                        MenAge2_tdap= merged$MenAge2_tdap,
                        BothAge2_total= merged$BothAge2_total,
                        BothAge2_dose1= merged$BothAge2_dose1,
                        BothAge2_dose2= merged$BothAge2_dose2,
                        BothAge2_mening= merged$BothAge2_mening,
                        BothAge2_tdap= merged$BothAge2_tdap,
                        FemAge3_total=merged$FemAge3_total,
                        FemAge3_dose1= merged$FemAge3_dose1,
                        FemAge3_dose2= merged$FemAge3_dose2,
                        FemAge3_mening= merged$FemAge3_mening,
                        FemAge3_tdap= merged$FemAge3_tdap,
                        MenAge3_total= merged$MenAge3_total,
                        MenAge3_dose1= merged$MenAge3_dose1,
                        MenAge3_dose2= merged$MenAge3_dose2,
                        MenAge3_mening=merged$MenAge3_mening,
                        MenAge3_tdap= merged$MenAge3_tdap,
                        BothAge3_total= merged$BothAge3_total,
                        BothAge3_dose1= merged$BothAge3_dose1,
                        BothAge3_dose2=merged$BothAge3_dose2,
                        BothAge3_mening=merged$BothAge3_mening,
                        BothAge3_tdap=merged$BothAge3_tdap,
                        Q31=merged$Q31,
                        Q31_other=merged$Q31_other,
                        Q32=merged$Q32,
                        Q32_details=merged$Q32_details,
                        Q32_other=merged$Q32_other,
                        Q33=merged$Q33,
                        Q33_other=merged$Q33_other,      
                        Q34=merged$Q34,
                      
                      # aCTIVITY PLAN
                        Q35= merged$Q35,
                        Q35_other= merged$Q35_other,
                        Q36a= merged$Q36a,
                        Q36b= merged$Q36b,
                        Q36c= merged$Q36c,
                        Q36d= merged$Q36d,
                        Q36e= merged$Q36e,
                        Q36f = merged$Q36f,
                        Q37= merged$Q37,
                        Q38= merged$Q38,
                        Q38_other= merged$Q38_other,
                        Q39= merged$Q39,
                        Q39_other= merged$Q39_other,
                        Q40= merged$Q40,
                        Q41= merged$Q41,
                        Q42= merged$Q42,
                        act1= merged$act1,
                        act2= merged$act2,
                        act3= merged$act3,
                        act4= merged$act4,
                        act5= merged$act5,
                        act6= merged$act6,
                        act7= merged$act7,
                        act8= merged$act8,
                        act9= merged$act9,
                        act10= merged$act10,
                        ppl1= merged$ppl1,
                        ppl2= merged$ppl2,
                        ppl3= merged$ppl3,
                        ppl4= merged$ppl4,
                        ppl5= merged$ppl5,
                        ppl6= merged$ppl6,
                        ppl7= merged$ppl7,
                        ppl8= merged$ppl8,
                        ppl9= merged$ppl9,
                        ppl10= merged$ppl10,
                        time1= merged$time1,
                        time2= merged$time2,
                        time3= merged$time3,
                        time4= merged$time4,
                        time5= merged$time5,
                        time6= merged$time6,
                        time7= merged$time7,
                        time8= merged$time8,
                        time9= merged$time9,
                        time10= merged$time10
                      ) # END PARAMS

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )  
  

   # --- ----- GENERATE THE FINAL REPORT -------------#
  
output$final_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "Finalreport.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "final_report.Rmd")
        file.copy("final_report.Rmd", tempReport, overwrite = TRUE)
        

        # Pull in all the relevant datasets
        storage_download(DataSrc, userFilename, overwrite = T)
        conn <- dbConnect(RSQLite::SQLite(), userFilename)
  
        getTable <- function(table) {
          df <- dbGetQuery(conn, paste("SELECT * FROM", table))
          dateVar <- names(df)[2]  # date is always the second column
          df <- df[order(df[[dateVar]], decreasing = T),]
          return(df[1,])
      }
      
      # You can then do this to pull and merge everything together
      fields <- c("demographics", "followupRates", "additionalInfo")
      merged <- lapply(fields, function(x) {
        getTable(x)
      }) %>%
        Reduce(function(x,y) full_join(x,y,"Username"), .)

        # Set up parameters to pass to Rmd document
        params <- list(HealthSystem = merged$HealthSystem,
                       Q1FU= merged$Q1FU,
                        Q1FU_other= merged$Q1FU_other,
                        Q2FU= merged$Q2FU, 
                        Q2FU_other= merged$Q2FU_other, 
                        Q3FU= merged$Q3FU,
                        Q4FU= merged$Q4FU,
                         
                        FemFUAge1_total= merged$FemFUAge1_total,
                        FemFUAge1_dose1= merged$FemFUAge1_dose1, 
                        FemFUAge1_dose2= merged$FemFUAge1_dose2,
                        MenFUAge1_total= merged$MenFUAge1_total,
                        MenFUAge1_dose1= merged$MenFUAge1_dose1, 
                        MenFUAge1_dose2= merged$MenFUAge1_dose2,
                        BothFUAge1_total= merged$BothFUAge1_total,
                        BothFUAge1_dose1= merged$BothFUAge1_dose1,
                        BothFUAge1_dose2= merged$BothFUAge1_dose2,
                            
                        FemFUAge2_total= merged$FemFUAge2_total,
                        FemFUAge2_dose1= merged$FemFUAge2_dose1,
                        FemFUAge2_dose2= merged$FemFUAge2_dose2,
                        FemFUAge2_mening= merged$FemFUAge2_mening,
                        FemFUAge2_tdap= merged$FemFUAge2_tdap,
                        MenFUAge2_total= merged$MenFUAge2_total,
                        MenFUAge2_dose1= merged$MenFUAge2_dose1,
                        MenFUAge2_dose2= merged$MenFUAge2_dose2,
                        MenFUAge2_mening= merged$MenFUAge2_mening,
                        MenFUAge2_tdap= merged$MenFUAge2_tdap,
                        BothFUAge2_total= merged$BothFUAge2_total,
                        BothFUAge2_dose1= merged$BothFUAge2_dose1,
                        BothFUAge2_dose2= merged$BothFUAge2_dose2,
                        BothFUAge2_mening= merged$BothFUAge2_mening,
                        BothFUAge2_tdap= merged$BothFUAge2_tdap,
                            
                        FemFUAge3_total= merged$FemFUAge3_total,
                        FemFUAge3_dose1= merged$FemFUAge3_dose1,
                        FemFUAge3_dose2= merged$FemFUAge3_dose2,
                        FemFUAge3_mening= merged$FemFUAge3_mening,
                        FemFUAge3_tdap= merged$FemFUAge3_tdap,
                        MenFUAge3_total= merged$MenFUAge3_total,
                        MenFUAge3_dose1= merged$MenFUAge3_dose1,
                        MenFUAge3_dose2= merged$MenFUAge3_dose2,
                        MenFUAge3_mening= merged$MenFUAge3_mening,
                        MenFUAge3_tdap= merged$MenFUAge3_tdap,
                        BothFUAge3_total= merged$BothFUAge3_total,
                        BothFUAge3_dose1= merged$BothFUAge3_dose1,
                        BothFUAge3_dose2= merged$BothFUAge3_dose2,
                        BothFUAge3_mening= merged$BothFUAge3_mening,
                        BothFUAge3_tdap= merged$BothFUAge3_tdap,
                        
                        Q5FU= merged$Q5FU,
                        Q5FU_text= merged$Q5FU_text,
                        Q6FU= merged$Q6FU,
                        Q6FU_text= merged$Q6FU_text,
                        Q7FU= merged$Q7FU,
                        Q7FU_other= merged$Q7FU_other,
                        Q8FU= merged$Q8FU, 
                        Q8FU_other= merged$Q8FU_other,
                        Q9FU= merged$Q9FU,
                        Q9FU_other= merged$Q9FU_other,
                        Q10FU= merged$Q10FU,
                        Q11FU= merged$Q11FU,
                        Q12FU= merged$Q12FU, 
                        Q13FU= merged$Q13FU,
                        Q13FU_other= merged$Q13FU_other,
                        Q14FU= merged$Q14FU,
                        Q14FU_other= merged$Q14FU_other,
                        Q15FU= merged$Q15FU,
                        Q16FU= merged$Q16FU,
                        Q17FU= merged$Q17FU,
                        Q18FU= merged$Q18FU,
                        Q18FU_more= merged$Q18FU_more 
                      ) # END PARAMS

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )  
  
  


  # Optional monthly data ---------------------------------------------------

myMonths <- c("January",
              "February",
              "March",
              "April",
              "May",
              "June",
              "July",
              "August",
              "September",
              "October",
              "November",
              "December")

storage_download(DataSrc, userFilename, overwrite = T)
myDB <- dbConnect(SQLite(), userFilename)
monthInput <- dbReadTable(myDB, "age1")

# grab the most recent goal
activities <- dbReadTable(myDB, "savedActivities")
activities <- activities[order(activities$activitiesDate),]
activities <- activities[nrow(activities),]
# Add their q36f aim to the top of the dashboard
output$remembergoal3 <- renderUI({
  h4(activities$Q36f, align = "center")
})
dbDisconnect(myDB)
rm(myDB)


output$start_month <- renderUI({
  selectInput(inputId = "start_month",
              label = "What month will you begin collecting data?",
              choices = c("January",
                          "February",
                          "March",
                          "April",
                          "May",
                          "June",
                          "July",
                          "August",
                          "September",
                          "October",
                          "November",
                          "December"),
              selected = monthInput$Month[1])
})




observeEvent(list(input$mu_ages, input$mu_sex, input$start_month), {
  validate(need(input$mu_ages, ""))
  validate(need(input$mu_sex, ""))
  validate(need(input$start_month, ""))
  
  storage_download(DataSrc, userFilename, overwrite = T)
  myDB <- dbConnect(SQLite(), userFilename)
  monthInput <- dbReadTable(myDB, "age1")
  
  # Set a title for the first figure
  age <- input$mu_ages
  sex <- input$mu_sex
  myTitle <<- paste("Monthly vaccination rates in", tolower(sex), "age", age)
  
  
  # When they update the start month, we can order all the months identically
  monthString <- monthInput$Month
  startIndex <- grep(input$start_month,monthString)
  newMonthString <- c(monthString[startIndex:12], monthString[1:(startIndex-1)])[1:12]
  
  myData <- list()
  myData$age1 <- dbReadTable(myDB, "age1")
  myData$age2 <- dbReadTable(myDB, "age2")
  myData$age3 <- dbReadTable(myDB, "age3")
  myData <- lapply(myData, function(x) {
    x$Month <- factor(x$Month,
                      levels = newMonthString,
                      labels = newMonthString)
    x[order(x$Month),]
  })
  
  # Rewrite the database with the new month order
  dbWriteTable(myDB, "age1", myData$age1, overwrite = T)
  dbWriteTable(myDB, "age2", myData$age2, overwrite = T)
  dbWriteTable(myDB, "age3", myData$age3, overwrite = T)
  
  # Update the database on Azure
  saveAzure(DataSrc, userFilename)
  dbDisconnect(myDB)
  rm(myDB)
  
  # this little bit will make the tables disappear when unselected
  observe({
    x <- input$mu_ages
    if (!"9-10" %in% x) {
      output$rates_9_10 <- renderUI(NULL)
    }
    if (!"11-12" %in% x) {
      output$rates_11_12 <- renderUI(NULL)
    }
    if (!"13" %in% x) {
      output$rates_13 <- renderUI(NULL)
    }
    
  })
  
  # Prepare the datasets
  girlsage1 <- dplyr::select(myData$age1, Month, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F)
  girlsage2 <- dplyr::select(myData$age2, Month, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F, Meningococcal = MeningococcalF, Tdap = TDapF)
  girlsage3 <- dplyr::select(myData$age3, Month, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F, Meningococcal = MeningococcalF, Tdap = TDapF)
  
  boysage1 <- dplyr::select(myData$age1, Month, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M)
  boysage2 <- dplyr::select(myData$age2, Month, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M, Meningococcal = MeningococcalM, Tdap = TDapM)
  boysage3 <- dplyr::select(myData$age3, Month, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M, Meningococcal = MeningococcalM, Tdap = TDapM)
  
  bothage1 <- dplyr::select(myData$age1, Month, Total, Dose1, Dose2)
  bothage2 <- dplyr::select(myData$age2, Month, Total, Dose1, Dose2, Meningococcal, Tdap = TDap)
  bothage3 <- dplyr::select(myData$age3, Month, Total, Dose1, Dose2, Meningococcal, Tdap = TDap)
  
  
  
  # Layout the UI elements
  if (sex == "Males and females combined" & age == "9-10") {
    output$rates_9_10 <- renderUI({
      wellPanel(
        h4("Males and Females Combined, Ages 9-10", align = "center"),
      fluidRow(
        column(6, 
                 uiOutput("both_9_10")
               ),
        column(6, align = "center",
               plotOutput("both_9_10_plots")
      )))
    })
  }
  if (sex == "Males and females combined" & age == "11-12") {
    output$rates_11_12 <- renderUI({
      wellPanel(
        h4("Males and Females Combined, Ages 11-12", align = "center"),
        fluidRow(
          column(6, 
                   uiOutput("both_11_12")
                 ),
          column(6, 
                 plotOutput("both_11_12_plots")
          )))
    })
  }
  if (sex == "Males and females combined" & age == "13") {
    output$rates_13 <- renderUI({
      wellPanel(
        h4("Males and Females Combined, Ages 13", align = "center"),
        fluidRow(
          column(6, 
                   uiOutput("both_13")
                 ),
          column(6, 
                 plotOutput("both_13_plots")
          )))
    })
  }
  
  if (sex == "Males and females seperately" & age == "9-10") {
    output$rates_9_10 <- renderUI({
      wellPanel(
      h4("Males and Females Seperately, Ages 9-10", align="center"),
      fluidRow(
        column(width = 6, 
                  verticalLayout(
                    h4("Females", align = "center"),
                    uiOutput("girls_9_10"),
                    br(),
                    h4("Males", align = "center"),
                    uiOutput("boys_9_10")
                    )),
        column(width = 6,
               verticalLayout(
                 br(),
                 br(),
               plotOutput("seperate_9_10_plots", height = "650px")))))
      
    }) 
  }
  if (sex == "Males and females seperately" & age == "11-12") {
    output$rates_11_12 <- renderUI({
      
      wellPanel(
        h4("Males and Females Seperately, Ages 11_12", align="center"),
        fluidRow(
          column(width = 6, 
                 verticalLayout(
                   h4("Females", align = "center"),
                   uiOutput("girls_11_12"),
                   br(),
                   h4("Males", align = "center"),
                   uiOutput("boys_11_12")
                 )),
          column(width = 6,
                 verticalLayout(
                   br(),br(),
                 plotOutput("seperate_11_12_plots", height = "650px")))))
    }) 
  }
  if (sex == "Males and females seperately" & age == "13") {
    output$rates_13 <- renderUI({
      wellPanel(
        h4("Males and Females Seperately, Age 13", align="center"),
        fluidRow(
          column(width = 6, 
                 verticalLayout(
                   h4("Females", align = "center"),
                   uiOutput("girls_13"),
                   br(),
                   h4("Males", align = "center"),
                   uiOutput("boys_13")
                 )),
          column(width = 6,
                 verticalLayout(
                   br(),br(),
                 plotOutput("seperate_13_plots", height = "650px")))))
    }) 
  }
  
  # Define the table UI
  output$both_9_10 <- renderUI({rHandsontableOutput("bothtable1", width = "100%")})
  output$both_11_12 <- renderUI({rHandsontableOutput("bothtable2", width = "100%")})
  output$both_13 <- renderUI({rHandsontableOutput("bothtable3", width = "100%")})
  
  output$girls_9_10 <- renderUI({rHandsontableOutput("girlstable1", width = "100%")})
  output$girls_11_12 <- renderUI({rHandsontableOutput("girlstable2", width = "100%")})
  output$girls_13 <- renderUI({rHandsontableOutput("girlstable3", width = "100%")})
  
  output$boys_9_10 <- renderUI({rHandsontableOutput("boystable1", width = "100%")})
  output$boys_11_12 <- renderUI({rHandsontableOutput("boystable2", width = "100%")})
  output$boys_13 <- renderUI({rHandsontableOutput("boystable3", width = "100%")})
  
  
  
  # Render the tables      
  output$bothtable1 <- renderRHandsontable({
    rhandsontable(bothage1, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(bothage1),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$bothtable2 <- renderRHandsontable({
    rhandsontable(bothage2, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(bothage2),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$bothtable3 <- renderRHandsontable({
    rhandsontable(bothage3, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(bothage3),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })  
  
  
  output$girlstable1 <- renderRHandsontable({
    rhandsontable(girlsage1, rowHeaders = NULL, width = "200%", stretchH = "all") %>%
      hot_col(names(girlsage1),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$girlstable2 <- renderRHandsontable({
    rhandsontable(girlsage2, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(girlsage2),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$girlstable3 <- renderRHandsontable({
    rhandsontable(girlsage3, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(girlsage3),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })     
  
  output$boystable1 <- renderRHandsontable({
    rhandsontable(boysage1, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(boysage1),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }")
  })      
  output$boystable2 <- renderRHandsontable({
    rhandsontable(boysage2, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(boysage2),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$boystable3 <- renderRHandsontable({
    rhandsontable(boysage3, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(boysage3),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })     
  

  # Figures
  if (sex == "Males and females combined") {
  output$both_9_10_plots <- renderPlot(monthlyFigure(hot_to_r(input$bothtable1)))
  output$both_11_12_plots <- renderPlot(monthlyFigure(hot_to_r(input$bothtable2)))
  output$both_13_plots <- renderPlot(monthlyFigure(hot_to_r(input$bothtable3)))
  }
  
  renderFigures <- function(girls, boys) {
    grid.arrange(arrangeGrob(
      girls + theme(legend.position = "none", title = element_text(size = 10)) + labs(title = "Females"),
      boys + theme(legend.position = "none", title = element_text(size = 10)) + labs(title = "Males"),
      nrow = 2, heights = c(10,10)),
      getLegend(girls),
      nrow=2, heights = c(20,1), 
      top = textGrob("Monthly vaccination rates", gp = gpar(fontsize = 16)))
  }
  output$seperate_9_10_plots <- renderPlot({
    girls <- monthlyFigure(hot_to_r(input$girlstable1))
    boys <- monthlyFigure(hot_to_r(input$boystable1))
    renderFigures(girls, boys)
  })
  
  output$seperate_11_12_plots <- renderPlot({
    girls <- monthlyFigure(hot_to_r(input$girlstable2))
    boys <- monthlyFigure(hot_to_r(input$boystable2))
    renderFigures(girls, boys)
  })
  
  output$seperate_13_plots <- renderPlot({
    girls <- monthlyFigure(hot_to_r(input$girlstable3))
    boys <- monthlyFigure(hot_to_r(input$boystable3))
    renderFigures(girls, boys)
  })
  
})

observeEvent(input$save_monthly, {
  
  
  # It is easier to reload the database (with the correctly ordered months)
  # And then update the list
  storage_download(DataSrc, userFilename, overwrite = T)
  myDB <- dbConnect(SQLite(), userFilename)
  myData <- list()
  myData$age1 <- dbReadTable(myDB, "age1")
  myData$age2 <- dbReadTable(myDB, "age2")
  myData$age3 <- dbReadTable(myDB, "age3")
  
  age <- input$mu_ages
  sex <- input$mu_sex
  
  
  
  
  if (sex == "Males and females combined" & age == "9-10") {
  both1 <- hot_to_r(input$bothtable1)
  myData$age1$Total <- both1$Total
  myData$age1$Dose1  <- both1$Dose1
  myData$age1$Dose2  <- both1$Dose2
  }
  
  if (sex == "Males and females combined" & age == "11-12") {
  both2 <- hot_to_r(input$bothtable2)
  myData$age2$Total <- both2$Total
  myData$age2$Dose1 <- both2$Dose1
  myData$age2$Dose2 <- both2$Dose2
  myData$age2$Meningococcal <- both2$Meningococcal
  myData$age2$TDap <- both2$Tdap
  }
  
  if (sex == "Males and females combined" & age == "13") {
  both3 <- hot_to_r(input$bothtable3)
  myData$age3$Total <- both3$Total
  myData$age3$Dose1 <- both3$Dose1
  myData$age3$Dose2 <- both3$Dose2
  myData$age3$Meningococcal <- both3$Meningococcal
  myData$age3$TDap <- both3$Tdap
  }
  
  
  # Age 9-10
  if (sex == "Males and females seperately" & age == "9-10") {
    girls1 <- hot_to_r(input$girlstable1)
    boys1 <- hot_to_r(input$boystable1)
    
    myData$age1$TotalM <- boys1$Total
    myData$age1$Dose1M <- boys1$Dose1
    myData$age1$Dose2M <- boys1$Dose2
    
    myData$age1$TotalF <- girls1$Total
    myData$age1$Dose1F <- girls1$Dose1
    myData$age1$Dose2F <- girls1$Dose2
    
    
  }
  if (sex == "Males and females seperately" & age == "11-12") {
    girls2 <- hot_to_r(input$girlstable2)
    boys2 <- hot_to_r(input$boystable2)
    
    
    myData$age2$TotalM <- boys2$Total
    myData$age2$Dose1M <- boys2$Dose1
    myData$age2$Dose2M <- boys2$Dose2
    myData$age2$MeningococcalM <- boys2$Meningococcal
    myData$age2$TDapM <- boys2$Tdap
    
    myData$age2$TotalF <- girls2$Total
    myData$age2$Dose1F <- girls2$Dose1
    myData$age2$Dose2F <- girls2$Dose2
    myData$age2$MeningococcalF <- girls2$Meningococcal
    myData$age2$TDapF <- girls2$Tdap
    
    
  }
  if (sex == "Males and females seperately" & age == "13") {
    girls3 <- hot_to_r(input$girlstable3)
    boys3 <- hot_to_r(input$boystable3)
    
    
    myData$age3$TotalM <- boys3$Total
    myData$age3$Dose1M <- boys3$Dose1
    myData$age3$Dose2M <- boys3$Dose2
    myData$age3$MeningococcalM <- boys3$Meningococcal
    myData$age3$TDapM <- boys3$Tdap
    
    myData$age3$TotalF <- girls3$Total
    myData$age3$Dose1F <- girls3$Dose1
    myData$age3$Dose2F <- girls3$Dose2
    myData$age3$MeningococcalF <- girls3$Meningococcal
    myData$age3$TDapF <- girls3$Tdap
    
  }
  
  # Write the database and send it back to Azure.
  dbWriteTable(myDB, "age1", myData$age1, overwrite = T)
  dbWriteTable(myDB, "age2", myData$age2, overwrite = T)
  dbWriteTable(myDB, "age3", myData$age3, overwrite = T)
  saveAzure(DataSrc, userFilename)
  dbDisconnect(myDB)
  rm(myDB)
  
  # It will refresh the allPlots when clicking save
  output$allPlots <- renderPlot(threeFigures(myData))
  
})

# This will generate the plots based on whatever data is loaded with the app
storage_download(DataSrc, userFilename, overwrite = T)
tempDB <- dbConnect(SQLite(), userFilename)
n <- c("age1","age2","age3")
dat <- lapply(n, function(x) {
  dbReadTable(tempDB, x)
})
names(dat) <- n
rm(n)
dbDisconnect(tempDB)
output$allPlots <- renderPlot(threeFigures(dat))

# Clinic level data -------------------------------------------------------

# Figure 1 - all ages combined
output$siteMessage <- renderText({
  paste(
    "Select an age group and entire you data into the table below.",
    "<br/><br/>",
    "Are there any superstar sites?",
    "<br/>",
    "What are the successful sites doing differently?<br/>
    How can other sites learn from them?")
})


observeEvent(list(input$site_ages, input$site_sex, input$num_sites), {
  validate(need(input$site_ages, ""))
  validate(need(input$site_sex, ""))
  validate(need(input$num_sites, ""))
  
  storage_download(DataSrc, userFilename, overwrite = T)
  myDB <- dbConnect(SQLite(), userFilename)
  myData <- list()
  myData$age1 <- dbReadTable(myDB, "siteAge1")
  myData$age2 <- dbReadTable(myDB, "siteAge2")
  myData$age3 <- dbReadTable(myDB, "siteAge3")
  dbDisconnect(myDB)
  
  # I will need to subset these data based on input$num_sites
  subsites <-function(ageDat) {
    row <- match(num_sites, ageDat$Site)
    return(ageDat[1:row,])
  }
  
  
  age <- input$site_ages
  sex <- input$site_sex
  num_sites <- input$num_sites
  
  # this little bit will make the tables disappear when unselected
  observe({
    x <- input$site_ages
    if (!"9-10" %in% x) {
      output$site_9_10 <- renderUI(NULL)
    }
    if (!"11-12" %in% x) {
      output$site_11_12 <- renderUI(NULL)
    }
    if (!"13" %in% x) {
      output$site_13 <- renderUI(NULL)
    }
  })
  
  
  # Prepare the datasets
  sitegirlsage1 <- dplyr::select(myData$age1, Site, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F) %>% subsites()
  sitegirlsage2 <- dplyr::select(myData$age2, Site, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F, Meningococcal = MeningococcalF, Tdap = TDapF) %>% subsites()
  sitegirlsage3 <- dplyr::select(myData$age3, Site, Total = TotalF, Dose1 = Dose1F, Dose2 = Dose2F, Meningococcal = MeningococcalF, Tdap = TDapF) %>% subsites()
  
  siteboysage1 <- dplyr::select(myData$age1, Site, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M) %>% subsites()
  siteboysage2 <- dplyr::select(myData$age2, Site, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M, Meningococcal = MeningococcalM, Tdap = TDapM) %>% subsites()
  siteboysage3 <- dplyr::select(myData$age3, Site, Total = TotalM, Dose1 = Dose1M, Dose2 = Dose2M, Meningococcal = MeningococcalM, Tdap = TDapM) %>% subsites()
  
  sitebothage1 <- dplyr::select(myData$age1, Site, Total, Dose1, Dose2) %>% subsites()
  sitebothage2 <- dplyr::select(myData$age2, Site, Total, Dose1, Dose2, Meningococcal, Tdap = TDap) %>% subsites()
  sitebothage3 <- dplyr::select(myData$age3, Site, Total, Dose1, Dose2, Meningococcal, Tdap = TDap) %>% subsites()
  
  
  # Layout the UI elements dynamically
  if (sex == "Males and females combined" & age == "9-10") {
    output$site_9_10 <- renderUI({
      wellPanel(
        h4("Males and Females Combined, Ages 9-10", align = "center"),
        fluidRow(
          column(6, 
                 uiOutput("siteboth_9_10")
          ),
          column(6, align = "center",
                 plotOutput("siteboth_9_10_plots")
          )))
    })
  }
  if (sex == "Males and females combined" & age == "11-12") {
    output$site_11_12 <- renderUI({
      wellPanel(
        h4("Males and Females Combined, Ages 11-12", align = "center"),
        fluidRow(
          column(6, 
                 uiOutput("siteboth_11_12")
          ),
          column(6, 
                 plotOutput("siteboth_11_12_plots")
          )))
    })
  } 
  if (sex == "Males and females combined" & age == "13") {
    output$site_13 <- renderUI({
      wellPanel(
        h4("Males and Females Combined, Ages 13", align = "center"),
        fluidRow(
          column(6, 
                 uiOutput("siteboth_13")
          ),
          column(6, 
                 plotOutput("siteboth_13_plots")
          )))
    })
  }
  
  if (sex == "Males and females seperately" & age == "9-10") {
    output$site_9_10 <- renderUI({
      wellPanel(
        h4("Males and Females Seperately, Ages 9-10", align="center"),
        fluidRow(
          column(width = 6, 
                 verticalLayout(
                   h4("Females", align = "center"),
                   uiOutput("sitegirls_9_10"),
                   br(),
                   h4("Males", align = "center"),
                   uiOutput("siteboys_9_10")
                 )),
          column(width = 6,
                 verticalLayout(
                   br(),
                   br(),
                   plotOutput("siteseperate_9_10_plots", height = "650px")))))
      
    }) 
  }
  if (sex == "Males and females seperately" & age == "11-12") {
    output$site_11_12 <- renderUI({
      
      wellPanel(
        h4("Males and Females Seperately, Ages 11-12", align="center"),
        fluidRow(
          column(width = 6, 
                 verticalLayout(
                   h4("Females", align = "center"),
                   uiOutput("sitegirls_11_12"),
                   br(),
                   h4("Males", align = "center"),
                   uiOutput("siteboys_11_12")
                 )),
          column(width = 6,
                 verticalLayout(
                   br(),br(),
                   plotOutput("siteseperate_11_12_plots", height = "650px")))))
    }) 
  }
  if (sex == "Males and females seperately" & age == "13") {
    output$site_13 <- renderUI({
      wellPanel(
        h4("Males and Females Seperately, Age 13", align="center"),
        fluidRow(
          column(width = 6, 
                 verticalLayout(
                   h4("Females", align = "center"),
                   uiOutput("sitegirls_13"),
                   br(),
                   h4("Males", align = "center"),
                   uiOutput("siteboys_13")
                 )),
          column(width = 6,
                 verticalLayout(
                   br(),br(),
                   plotOutput("siteseperate_13_plots", height = "650px")))))
    }) 
  }
  
  
  # Define the table UI
  output$siteboth_9_10 <- renderUI({rHandsontableOutput("sitebothtable1", width = "100%")})
  output$siteboth_11_12 <- renderUI({rHandsontableOutput("sitebothtable2", width = "100%")})
  output$siteboth_13 <- renderUI({rHandsontableOutput("sitebothtable3", width = "100%")})
  
  output$sitegirls_9_10 <- renderUI({rHandsontableOutput("sitegirlstable1", width = "100%")})
  output$sitegirls_11_12 <- renderUI({rHandsontableOutput("sitegirlstable2", width = "100%")})
  output$sitegirls_13 <- renderUI({rHandsontableOutput("sitegirlstable3", width = "100%")})
  
  output$siteboys_9_10 <- renderUI({rHandsontableOutput("siteboystable1", width = "100%")})
  output$siteboys_11_12 <- renderUI({rHandsontableOutput("siteboystable2", width = "100%")})
  output$siteboys_13 <- renderUI({rHandsontableOutput("siteboystable3", width = "100%")})
  
  
  # Render the tables
  output$sitebothtable1 <- renderRHandsontable({
    rhandsontable(sitebothage1, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(sitebothage1),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })   
  output$sitebothtable2 <- renderRHandsontable({
    rhandsontable(sitebothage2, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(sitebothage2),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$sitebothtable3 <- renderRHandsontable({
    rhandsontable(sitebothage3, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(sitebothage3),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })  
  
  
  output$sitegirlstable1 <- renderRHandsontable({
    rhandsontable(sitegirlsage1, rowHeaders = NULL, width = "200%", stretchH = "all") %>%
      hot_col(names(sitegirlsage1),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$sitegirlstable2 <- renderRHandsontable({
    rhandsontable(sitegirlsage2, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(sitegirlsage2),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$sitegirlstable3 <- renderRHandsontable({
    rhandsontable(sitegirlsage3, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(sitegirlsage3),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })     
  
  output$siteboystable1 <- renderRHandsontable({
    rhandsontable(siteboysage1, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(siteboysage1),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }")
  })      
  output$siteboystable2 <- renderRHandsontable({
    rhandsontable(siteboysage2, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(siteboysage2),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })      
  output$siteboystable3 <- renderRHandsontable({
    rhandsontable(siteboysage3, rowHeaders = NULL, width = "200%", stretchH = "all")%>%
      hot_col(names(siteboysage3),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })     
  
  # Render the figures
  # Figures
  if (sex == "Males and females combined") {
    output$siteboth_9_10_plots <- renderPlot(siteFigure(hot_to_r(input$sitebothtable1)))
    output$siteboth_11_12_plots <- renderPlot(siteFigure(hot_to_r(input$sitebothtable2)))
    output$siteboth_13_plots <- renderPlot(siteFigure(hot_to_r(input$sitebothtable3)))
  }
  
  
  renderFigures <- function(girls, boys) {
    grid.arrange(arrangeGrob(
      girls + theme(legend.position = "none", title = element_text(size = 10)) + labs(title = "Females"),
      boys + theme(legend.position = "none", title = element_text(size = 10)) + labs(title = "Males"),
      nrow = 2, heights = c(10,10)),
      getLegend(girls),
      nrow=2, heights = c(20,1), 
      top = textGrob("Vaccination rates by site or provider", gp = gpar(fontsize = 16)))
  }
  output$siteseperate_9_10_plots <- renderPlot({
    girls <- siteFigure(hot_to_r(input$sitegirlstable1))
    boys <- siteFigure(hot_to_r(input$siteboystable1))
    renderFigures(girls, boys)
  })
  output$siteseperate_11_12_plots <- renderPlot({
    girls <- siteFigure(hot_to_r(input$sitegirlstable2))
    boys <- siteFigure(hot_to_r(input$siteboystable2))
    renderFigures(girls, boys)
  })
  output$siteseperate_13_plots <- renderPlot({
    girls <- siteFigure(hot_to_r(input$sitegirlstable3))
    boys <- siteFigure(hot_to_r(input$siteboystable3))
    renderFigures(girls, boys)
  })
  
  
  newData <- lapply(myData, subsites)

  
  
})




observeEvent(input$save_site, {
  
  storage_download(DataSrc, userFilename, overwrite = T)
  myDB <- dbConnect(SQLite(), userFilename)
  myData <- list()
  myData$age1 <- dbReadTable(myDB, "siteAge1")
  myData$age2 <- dbReadTable(myDB, "siteAge2")
  myData$age3 <- dbReadTable(myDB, "siteAge3")
  
  
  
  age <- input$site_ages
  sex <- input$site_sex
  n_rows <- match(input$num_sites, myData$age1$Site)
  
  
  
  
  if (sex == "Males and females combined" & age == "9-10") {
    both1 <- hot_to_r(input$sitebothtable1)
    myData$age1$Total[1:nrow(both1)] <- both1$Total
    myData$age1$Dose1[1:nrow(both1)]   <- both1$Dose1
    myData$age1$Dose2[1:nrow(both1)]   <- both1$Dose2
  }
  if (sex == "Males and females combined" & age == "11-12") {
    both2 <- hot_to_r(input$sitebothtable2)
    myData$age2$Total[1:nrow(both2)]  <- both2$Total
    myData$age2$Dose1[1:nrow(both2)]  <- both2$Dose1
    myData$age2$Dose2[1:nrow(both2)]  <- both2$Dose2
    myData$age2$Meningococcal[1:nrow(both2)]  <- both2$Meningococcal
    myData$age2$TDap[1:nrow(both2)]  <- both2$Tdap
  }
  if (sex == "Males and females combined" & age == "13") {
    both3 <- hot_to_r(input$sitebothtable3)
    myData$age3$Total[1:nrow(both3)]  <- both3$Total
    myData$age3$Dose1[1:nrow(both3)] <- both3$Dose1
    myData$age3$Dose2[1:nrow(both3)] <- both3$Dose2
    myData$age3$Meningococcal[1:nrow(both3)] <- both3$Meningococcal
    myData$age3$TDap[1:nrow(both3)] <- both3$Tdap
  }
  
  
  # Age 9-10
  if (sex == "Males and females seperately" & age == "9-10") {
    girls1 <- hot_to_r(input$sitegirlstable1)
    boys1 <- hot_to_r(input$siteboystable1)
    
    myData$age1$TotalM[1:nrow(boys1)]<- boys1$Total
    myData$age1$Dose1M[1:nrow(boys1)] <- boys1$Dose1
    myData$age1$Dose2M[1:nrow(boys1)] <- boys1$Dose2
    
    myData$age1$TotalF[1:nrow(girls1)] <- girls1$Total
    myData$age1$Dose1F[1:nrow(girls1)] <- girls1$Dose1
    myData$age1$Dose2F[1:nrow(girls1)] <- girls1$Dose2
    
    
  }
  if (sex == "Males and females seperately" & age == "11-12") {
    girls2 <- hot_to_r(input$sitegirlstable2)
    boys2 <- hot_to_r(input$siteboystable2)
    
    
    myData$age2$TotalM[1:nrow(boys2)] <- boys2$Total
    myData$age2$Dose1M[1:nrow(boys2)] <- boys2$Dose1
    myData$age2$Dose2M[1:nrow(boys2)] <- boys2$Dose2
    myData$age2$MeningococcalM[1:nrow(boys2)] <- boys2$Meningococcal
    myData$age2$TDapM[1:nrow(boys2)] <- boys2$Tdap
    
    myData$age2$TotalF[1:nrow(girls2)] <- girls2$Total
    myData$age2$Dose1F[1:nrow(girls2)] <- girls2$Dose1
    myData$age2$Dose2F[1:nrow(girls2)] <- girls2$Dose2
    myData$age2$MeningococcalF[1:nrow(girls2)] <- girls2$Meningococcal
    myData$age2$TDapF[1:nrow(girls2)] <- girls2$Tdap
    
    
  }
  if (sex == "Males and females seperately" & age == "13") {
    girls3 <- hot_to_r(input$sitegirlstable3)
    boys3 <- hot_to_r(input$siteboystable3)
    
    
    myData$age3$TotalM[1:nrow(boys3)] <- boys3$Total
    myData$age3$Dose1M[1:nrow(boys3)]  <- boys3$Dose1
    myData$age3$Dose2M[1:nrow(boys3)]  <- boys3$Dose2
    myData$age3$MeningococcalM[1:nrow(boys3)]  <- boys3$Meningococcal
    myData$age3$TDapM[1:nrow(boys3)]  <- boys3$Tdap
    
    myData$age3$TotalF[1:nrow(girls3)]  <- girls3$Total
    myData$age3$Dose1F[1:nrow(girls3)] <- girls3$Dose1
    myData$age3$Dose2F[1:nrow(girls3)] <- girls3$Dose2
    myData$age3$MeningococcalF[1:nrow(girls3)] <- girls3$Meningococcal
    myData$age3$TDapF[1:nrow(girls3)] <- girls3$Tdap
    
  }
  
  # Write the database and send it back to Azure.
  
  
  
  
  dbWriteTable(myDB, "siteAge1", myData$age1, overwrite = T)
  dbWriteTable(myDB, "siteAge2", myData$age2, overwrite = T)
  dbWriteTable(myDB, "siteAge3", myData$age3, overwrite = T)
  saveAzure(DataSrc, userFilename)
  dbDisconnect(myDB)
  rm(myDB)
  
  
})







} # end the server


shinyApp(ui=ui, server=server)


