
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(readr)
library(janitor)
library(readxl)

# Load any global datasets if needed
depression_rates <- read_csv("data/depression_prevelance.csv")
HEDCO <- read_excel("data/Depression_HEDCO.xlsx", sheet = 2)

# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
fluidPage(
  titlePanel("School-Based Depression Prevention Programs"),
  
  # Reset Button Row
  fluidRow(
    column(10),
    column(2, 
           actionButton("reset_filters", "Reset Filters", 
                        style = "margin-bottom: 10px; padding: 8px 15px; 
                                 background-color: #004F6E; color: white; 
                                 border: none; font-weight: bold;"))), 

    column(3,
           selectInput("study_year_group", "Research Publication Year:",
                       choices = c("Before 2000", "2001-2010", "2011-2015", "After 2015"),
                       selected = c("Before 2000", "2001-2010", "2011-2015", "After 2015"), multiple = TRUE)),
  
  column(3,
         selectInput("inter_admin", "Intervention Facilitator:",
                     choices = c("All", "External Facilitator", "School Staff", "Teacher", "Online Program", "School Nurse",
                                 "Research Staff", "Mental Health Professional", "Mindfulness Trainer","Not Reported"),
                     selected = "All")
                      ),
  column(3,
         selectInput("sim_length", "Intervention Length:",
                     choices = c("All", "4 Weeks or Less", "4-10 Weeks", "10-12 Weeks",  "12-20 Weeks", "More than 20 Weeks", "Not Reported"),
                     selected = "All")),
  
  column(3,
         selectInput("grade_group", "Grade Level:",
                     choices = c("All", "Grades K-5", "Grades 6-8", "Grades 9-12", "Not Reported"),
                     selected = "All")),
  

    fluidRow(
      column(10, leafletOutput("map", width = "100%", height = "575px")),
      
      column(2,
             # Dynamic Circle
             uiOutput("depression_circle"),
             
             uiOutput("depression_text"),
             
             tags$img(src = "dark_blue.png", 
                      style = "display: block; margin: 0 auto; width: 100%; max-width: 200px;"),
             
             uiOutput("school_text")
      )
    ),
    
    fluidRow(
      column(12, uiOutput("study_list")),
      tags$p( style ="padding: 20px;",
        "*Depression prevalence data comes from the ",
        tags$a(href = "https://www.healthdata.org/", target = "_blank", "Institute for Health Metrics and Evaluations"),
        "."
      )# Full-width study list below
    )
  )
  
