#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(rhandsontable)

shinyApp(

ui = dashboardPage( title="Optional Data",
     dashboardHeader(title=NULL),
    
    dashboardSidebar(
      sidebarMenu(id="tabs",
        menuItem("Home", tabName = "home", icon = icon("home"))
      )
    ),
    dashboardBody(
      
      tags$head(tags$style(HTML('
                .box {margin: 0px;}'
                ))), # this removes the spacing between boxes that are stacked atop each other
      tags$hr(style="border-color: purple;"),
      
      tabItems(
        tabItem("home",
                
            tabBox(width = NULL,
               tabPanel(title = "Hypothetical data entry",
                        HTML("<br>"),
                        
                h4(strong("Step 1: "), "Enter the ages included in data. Are these just 13 year olds, or are you looking at a larger age range? 
                         If you are using 2020 SASI definitions, the age range would be 9-13."),
                checkboxGroupInput(inputId = "mu_ages",
                                   label = "What ages are you reporting?",
                                   choices = c("9-10","11-12", "13"),
                                   selected = NULL),
                h4(strong("Step 2: "), "What is the time period of the data? For example, is this baseline data looking at the rates for the calendar year of 2020? 
                         We recommend using a 12 month period, as vaccination rates fluctuate through the year."),
                numericInput(inputId = "mu_months", label="Number of months examined", value=12, min=0, max=24, step=1, width='100%'),
                
                h4(strong("Step 3: "), "How many sites do you have?"),
                numericInput(inputId="num_sites", label="Number of Sites", value=1, min=1, max=36),
                
                h4(strong("Step 4: "), "Would you like to enter the data separately by sex, or combined?"),
                selectInput(inputId = "sites_sex",
                                                label = "Will you enter data by sex (males and females separately) or combined?",
                                                choices = c("Males and Females Separately",
                                                            "Combined"),
                                                selected = "Males and Females Separately"),
                
                
                h4(strong("Step 5: "), "Enter data into the table below"),
                  tags$ul(
                                   tags$li(strong("Be sure to use the same age group for all three vaccines, so that the active patient population is consistent.")),
                                   tags$li("Each row is a new site or provider. The entire table is intended to look at data for a single period in time, for example, at baseline or follow-up."),
                                   tags$li("Type the site or provider names into the first column"),
                                   tags$li("Fill in the white cells with patient counts. If not looking at mening or tdap, leave the cells for these vaccines blank."),
                                   tags$li("Blue cells auto-calculate vaccination rates. Light gray cells auto-calculate the number of patients for males and females combined. If you cannot report sex separately, delete the formulas and enter data.")
                                 ),
                HTML("<br>"),
                HTML("<br>"),
                
                rHandsontableOutput("sites_data", width = "100%"),

                
                
               ), # end tab panel 1
            tabPanel(title="View hypothetical results",
                     
                     
                     
                     
            ) # end tab panel of visualization page       
            ) # end tab box
            
                
                
                ) # end tab home
      ) # end tab items
      
      
      
    ) # end dashboard body
),

server = function(input, output, session) {
  
  
    output$sites_data <- renderRHandsontable({
      
      a<- input$num_sites
      
      if (input$sites_sex == "Combined") {
      df <- data.frame(Sites = 1:a,
                Total = NA,
                  Dose1 = NA,
                  Dose2 = NA,
                  Mening = NA,
                  Tdap = NA)
      }
      if (input$sites_sex == "Males and Females Separately") {
        f <- data.frame(Sites = 1:a,
                Total = NA,
                  Dose1 = NA,
                  Dose2 = NA,
                  Mening = NA,
                  Tdap = NA)

        f2 <- data.frame(cbind(f,f[2:6]))
        df <- rbind(c("Sites", rep("Females", 5), rep("Males", 5)), f2)
      }
      
      
    rhandsontable(df, rowHeaders = NULL) %>%
      hot_col(names(df),  
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") 
  })
  
  output$myplot <- renderPlot(hot_to_r(input$sites_data))
  

  
    } # end server
) # end shiny app