library(shiny)
library(DT)
library(png)
library(grid)
library(shinythemes)
library(lubridate)
library(shinymanager)
#This Forecasting System is developed by
#     Author: Alex R Cook
#Affiliation: Saw Swee Hock School of Public Health
#             National University of Singapore
#Created Date: 13 May 2020
# Modified: 17 June 2020

# inactivity <- "function idleTimer() {
# var t = setTimeout(logout, 120000);
# window.onmousemove = resetTimer; // catches mouse movements
# window.onmousedown = resetTimer; // catches mouse movements
# window.onclick = resetTimer;     // catches mouse clicks
# window.onscroll = resetTimer;    // catches scrolling
# window.onkeypress = resetTimer;  //catches keyboard actions
# 
# function logout() {
# window.close();  //close the window
# }
# 
# function resetTimer() {
# clearTimeout(t);
# t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
# }
# }
# idleTimer();"
# 
# credentials <- data.frame(
#   user = c("alex", "moh", ""),
#   password = c("alex", "moh", ""),
#   stringsAsFactors = FALSE
# )

ui = shinyUI(navbarPage("COVID-19 in Singapore",theme = shinytheme("flatly"),
# ui = secure_app(head_auth = tags$script(inactivity), theme = 'flatly',
#                 navbarPage("COVID-19 in Singapore",theme = shinytheme("flatly"),
# Step 1. MCMC Running------------------------------------------------------
                        tabPanel(
                          "Step 0 : DATA UPDATE",
                          sidebarPanel(
                            fileInput("file1","Choose daily data."),
                            fileInput("file2","Choose google mobility dataset.")
                          ),
                          mainPanel(
                            h3("Please update the daily data here."),
                            h4("If you want to skip the MCMC running step, 
                                you could go to Step 2 directly, the previous MCMC results will be used.")
                          )
                        ),
# Step 1. MCMC Running------------------------------------------------------
                      tabPanel(
                        "Step 1 : MCMC RUN",
                        sidebarPanel(
                          numericInput('NEWs',"Number of Foreign Workers",value = 323000),
                          numericInput("MCMCITS", "Number of MCMC iteration", value = 10000),
                          actionButton("runMCMC", label="Run")
                         ),
                        mainPanel(
                          h3("This step runs MCMC, which could be skipped if needed."),
                          textOutput(outputId="message_mcmc")
                         )
                        ),
###########################################################################  

# Step 2. Projector analysis-------------------------------------------------
                      tabPanel(
                        "Step 2 : PROJECTOR",
                        sidebarPanel(
                          numericInput('NEWs',"Number of Foreign Workers",value = 323000),
                          numericInput('projectionwindow',"Projection Window (days)", value = 60),
                          actionButton("analyse", label="Analyse"),
                        ),
                        mainPanel(
                          h3("Short term projections of COVID-19 in Singapore"),
                          textOutput("message_proj"),
                          br()
                        )
                      ),
###########################################################################    

# Step 3. Plotter ---------------------------------------------------------
                      tabPanel(
                        "Step 3 : PLOT",
                        sidebarPanel(
                          ## Download setting, including date range. place to export.
                          # selectInput("scenarioid","Scenario:", choices = c(
                          #   'Scenario 1: Modest future growth' = 1,
                          #   'Scenario 2: Dorm spillover' = 2,
                          #   'Scenario 3: Elevated future growth' = 3,
                          #   'Scenario 4: Return of importation' = 4,
                          #   'Scenario 5: Elevated transmission and spillover' = 5,
                          #   'Scenario 6: Confluence of problems' = 6
                          #   )
                          #   ),
                          radioButtons("group", label = "Plot Type:",
                                       choices = c("1. New ICU cases and Deaths" = "group1",
                                                   # "1. Newly ICU cases, Newly Death, Reporduction Number" = "group1",
                                                   "2. New cases (by type)" = "group2",
                                                   "3. Total cases (by type)" = "group3",
                                                   "4. Total cases, log scale (by type)" = "group4"
                                                   # "Newly Imported Cases" = 'ci', "Newly Local Cases" = 'cc', "Newly Dormitory Cases" = 'cd',
                                       )
                          ),
                          br(),
                          actionButton("plot", label="Plot"),
                          br(),
                          br(),
                          selectInput("downfile","Download file:", choices = NULL),
                          downloadButton(
                            outputId = "downloadData",
                            label = "Export",
                            icon = icon("download"),
                            style = "color: black; margin-left: 3px; margin-bottom: 5px;"
                          )

                          ),
                        mainPanel(
                          h3("Short term projections of COVID-19 in Singapore"),
                          plotOutput("plot"),
                          br(),br(), br(),br(),br(), br(),br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(),br(),
                          textOutput("message_plot"),
                          h3('Important points to note:'),
                          tags$ul(
                            tags$li("This is a research project. The projections here are not validated. No warranty is expressed or implied. Do not book your holidays based on these."), 
                            tags$li("These are ",tags$em("projections")," not ",tags$em("forecasts"),". ",tags$strong("If")," the assumptions underlying them are met, then the projections may come true. However, there remains substantial uncertainty about the local epidemiology, in particular with regards to the outbreaks among foreign workers. 
              It would therefore be rash to construe these are forecasts of what we think will happen."), 
                            tags$li("Data are obtained from the", tags$a(href="https://www.moh.gov.sg/covid-19/situation-report","MOH COVID-19 situation report"), "We try to update them daily."), 
                            tags$li("Three scenarios are considered for the foreign worker epidemics. The base case assumes that the reproduction rate is reduced by 40% due to control, and that 60% of infections are detected. The worst case assumes no reduction in the reproduction 
      number and that 80% of infections are detected. The best case assumes that the reproduction number falls by 70%, and that 40% of cases are detected. All three scenarios assume a maximum testing capacity which truncates the cases at less than 1000 per day, 
      and thus the scenarios cannot be differentiated statistically."), 
                            tags$li("The model of imported cases assumes no rebound in case counts from overseas. This may not hold after borders start opening up."),
                            tags$li("The number of ‘local’ cases (not foreign workers, and not imported) depends on recent cases in all three groups, and are based on pre- and peri-circuit break."),
                            tags$li("The number of beds in use for intensive care depend on recent infections in all three groups. The risk of being admitted to the ICU differs by group but is assumed constant with time. Thus if the age profile of cases were to change substantially, 
              the projections could be badly wrong."),
                            tags$li("Deaths are assumed to depend on the number of people in ICU.")
                            
                          ),
                          p("The model and web application were developed by Lawrence Chew, Esther Choo, Alex R Cook and Yinxiaohe Sun.")
                        )
                        )
###########################################################################    

))
