library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(readr)
library(janitor)
library(readxl)
library(rsconnect)

depression_rates <- read_csv("data/depression_prevelance.csv")
HEDCO <- read_excel("data/Depression_HEDCO.xlsx", sheet = 2)

world_data <- readRDS("data/world_data.rds")

HEDCO$grade_group <- as.character(HEDCO$grade_group)

library(tidyr)
library(dplyr)

number_data <- depression_rates %>%
  filter(metric_name == "Number") %>%
  select(location_name, sex_name, age_name, number = val, upper, lower) %>%
  rename(upper_number = upper,
         lower_number = lower,
         country = location_name,
         gender = sex_name,
         age = age_name)

percent_data <- depression_rates %>%
  filter(metric_name == "Percent") %>%
  select(location_name, sex_name, age_name, percent = val, upper, lower)  %>%
  rename(upper_per = upper,
         lower_per = lower,
         country = location_name,
         gender = sex_name,
         age = age_name)

percent_data$whole_percent <- round(percent_data$percent*100, 2)

combined <- inner_join(number_data, percent_data, by = c("country", "gender", "age"))

combined <- combined %>% mutate(EstimatedPopulation = number / percent)

combined_summary <- combined %>%
  group_by(country, gender) %>%
  summarise(Total_Cases = sum(number, na.rm = TRUE),
            Total_EstimatedPop = sum(EstimatedPopulation, na.rm = TRUE)) %>%
  mutate(Combined_Percent = Total_Cases / Total_EstimatedPop) %>%
  ungroup() %>%
  clean_names()

combined_summary$whole_percent <- round(combined_summary$combined_percent*100, 2)

HEDCO <- HEDCO %>%
  mutate(
    study_year_group = case_when(
      study_publication_year < 2000 ~ "Before 2000",
      study_publication_year >= 2001 & study_publication_year <= 2010 ~ "2001-2010",
      study_publication_year >= 2011 & study_publication_year <= 2015 ~ "2011-2015",
      study_publication_year > 2015 ~ "After 2015",
      TRUE ~ "Unknown"  # Fallback for missing or invalid years
    )
  )

world <- world_data %>% filter(name != "Antarctica")

depress <- combined_summary %>%
  filter(gender == "Both")


function(input, output, session) {

  
  selected_country <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    
    filtered_countries <- HEDCO %>%
      filter(
        # School Type filter
        (length(input$inter_admin) == 0 || "All" %in% input$inter_admin) |
          (sapply(HEDCO$inter_admin, function(x) any(input$inter_admin %in% unlist(strsplit(x, ", ")) ))),
        
        (length(input$grade_group) == 0 || "All" %in% input$grade_group) |
          (sapply(HEDCO$grade_group, function(x) any(input$grade_group %in% unlist(strsplit(x, ", ")) ))),
        
        # Study Year filter
        (length(input$study_year_group) == 0 || "All" %in% input$study_year_group) | (study_year_group %in% input$study_year_group),
        
        # Grade Level filter 
        (length(input$sim_length) == 0 ||"All" %in% input$sim_length) | (sim_length %in% input$sim_length)
        )%>%
      pull(country)
    
    world_filtered <- world %>%
      mutate(data_available = ifelse(name %in% filtered_countries, "Yes", "No"))
    
    # Colors
    pal <- colorFactor(c("#D8DCDA", "#007030"), domain = c("Yes", "No"))
    
    leaflet(world_filtered,
            options = leafletOptions(scrollWheelZoom = FALSE)) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(data_available),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 1,
        highlight = highlightOptions(weight = 2, color = "#FEE11A", bringToFront = TRUE),
        label = ~name,
        layerId = ~name  # Store country name as ID for clicks
      ) %>%
      addLegend(pal = pal, position = "bottomleft", values = ~data_available, title = "Data Availabile for Selected Filters")
  })
  
  observeEvent(input$map_shape_click, {
    selected_country(input$map_shape_click$id)
  })
  
  output$depression_circle <- renderUI({
    country <- selected_country()
    
    # Default to a global rate if no country is selected
    rate <- if (!is.null(country) && country %in% depress$country) {
      depress$whole_percent[depress$country == country]
    } else {
      1.26  # Global average depression rate
    }
    
    tags$div(
      style = "display: flex; justify-content: center; align-items: center; height: 200px;",
      tags$div(
        style = "width: 150px; height: 150px; border-radius: 50%; background-color: #FEE11A;
                 display: flex; justify-content: center; align-items: center; color: white; 
                 font-size: 24px; font-weight: bold; text-align: center;",
        paste0(rate, "%*")
      )
    )
  })
  
  output$depression_text <- renderUI({
    country <- selected_country()
    text <- if (!is.null(country)) {
      paste("Percentage of people between 5 and 19 years old experiencing depression in", country)
    } else {
      "Percentage of people between 5 and 19 years old experiencing depression worldwide."
    }
    
    tags$p(text, style = "text-align: center; font-size: 16px")
  })
  
  output$school_text <- renderUI({
    country <- selected_country()  # Get selected country
    
    filtered_schools <- HEDCO %>%
      filter(
        
        # Study Year filter
        (length(input$study_year_group) == 0 || "All" %in% input$study_year_group) | (study_year_group %in% input$study_year_group),
        
        # Intervention Admin filter (NEW)
        (length(input$inter_admin) == 0 || "All" %in% input$inter_admin) |
          (sapply(HEDCO$inter_admin, function(x) any(input$inter_admin %in% unlist(strsplit(x, ", ")) ))),
        
        # Intervention length
        (length(input$sim_length) == 0 || "All" %in% input$sim_length) | (sim_length %in% input$sim_length),
        
        # Grade grouping filters
    (length(input$grade_group) == 0 || "All" %in% input$grade_group) |
      (sapply(HEDCO$grade_group, function(x) any(input$grade_group %in% unlist(strsplit(x, ", ")) )))
      )
    
    # If no country is selected, show total count across all countries
    if (is.null(country)) {
      total_schools <- sum(filtered_schools$number_schools[filtered_schools$number_schools != -999], na.rm = TRUE)
      
      return(tags$p(
        paste("Total number of schools in dataset with selected filters:", total_schools),
        style = "text-align: center; font-size: 16px"
      ))
    }
    
    country_schools <- filtered_schools %>%
      filter(country == !!country)
    
    # ignoring -999 values
    total_schools <- sum(country_schools$number_schools[country_schools$number_schools != -999], na.rm = TRUE)

#when no schools display        
    if (total_schools == 0) {
      return(tags$p(
        paste("There are no schools in", country, "from this dataset, or no data is available for your filter selection."),
        style = "text-align: center; font-size: 16px"
      ))
    }
    
#displaying # of schools
    tags$p(
      paste("Total number of schools in", country, "from this dataset:", total_schools),
      style = "text-align: center; font-size: 16px"
    )
  })
  
  output$study_list <- renderUI({  
    country <- selected_country()  # Get country
    
    if (is.null(country)) {
      return(tags$p("Select a country to see study details.", style = "text-align: left; font-size: 16px;"))
    }
    
    # Apply filters to dataset for the country
    filtered_studies <- HEDCO %>%
      filter(
        country == !!country,  
        (length(input$study_year_group) == 0 || "All" %in% input$study_year_group) | (study_year_group %in% input$study_year_group),
        (length(input$inter_admin) == 0 || "All" %in% input$inter_admin) |
          (sapply(HEDCO$inter_admin, function(x) any(input$inter_admin %in% unlist(strsplit(x, ", ")) ))),
        (length(input$sim_length) == 0 || "All" %in% input$sim_length) | (sim_length %in% input$sim_length),
        (length(input$grade_group) == 0 || "All" %in% input$grade_group) |
          (sapply(HEDCO$grade_group, function(x) any(input$grade_group %in% unlist(strsplit(x, ", ")) )))
      ) %>%
      pull(study_author_year)
    
    if (length(filtered_studies) == 0) {
      return(tags$p(paste("No study data available for", country, "with the selected filters.")))
    }
    
    filtered_studies <- sort(filtered_studies)
    
    # study list with clickable buttons
    tags$div(
      tags$h4(paste("Studies from your selection in", country), style = "margin-bottom: 10px;"),
      style = "display: flex; flex-direction: column; gap: 10px;",
      lapply(seq_along(filtered_studies), function(i) {
        study_id <- paste0("study_", i)  # Unique ID for each study
        tags$div(class = "study-card",
                 actionButton(study_id, filtered_studies[i], class = "study-btn",
                              style = sprintf(
                                "padding: 10px; border-radius: 5px; font-size: 16px; font-weight: bold;
                               color: black; width: 100%%; text-align: center;
                               background-color: %s;",
                                ifelse(i %% 2 == 0, "#007030", "#FEE11A")
                              ))
        )
      })
    )
  })

  observe({
    req(selected_country())  
    
    removeModal() 
    filtered_studies <- HEDCO %>%
      filter(
        country == selected_country(),  
        (length(input$study_year_group) == 0 || "All" %in% input$study_year_group) | (study_year_group %in% input$study_year_group),
        (length(input$inter_admin) == 0 || "All" %in% input$inter_admin) |
          (sapply(HEDCO$inter_admin, function(x) any(input$inter_admin %in% unlist(strsplit(x, ", ")) ))),
        (length(input$sim_length) == 0 || "All" %in% input$sim_length) | (sim_length %in% input$sim_length),
        (length(input$grade_group) == 0 || "All" %in% input$grade_group) |
          (sapply(HEDCO$grade_group, function(x) any(input$grade_group %in% unlist(strsplit(x, ", ")) )))
      )
    
    session$sendCustomMessage(type = "resetStudyClicks", message = NULL)
    
    # Dynamically generate button IDs for only the filtered studies
    lapply(seq_along(filtered_studies$study_author_year), function(i) {
      study_id <- paste0("study_", i)
      
      observeEvent(input[[study_id]], {
        study_details <- filtered_studies[i, ] 

        study_link <- if (!is.na(study_details$article_link) && study_details$article_link != "") {
          tags$a(href = study_details$article_link, target = "_blank", rel = "noopener noreferrer", study_details$study_author_year)
        } else {
          study_details$study_author_year
        }
        
        intervention_name <- study_details$Intervention  
        
        if (!is.na(study_details$inter_link) && study_details$inter_link != "" && study_details$inter_link != "Not Available") {
          intervention_display <- tags$p(
            tags$b("Intervention: "), 
            tags$a(href = study_details$inter_link, target = "_blank", rel = "noopener noreferrer", intervention_name)  # Clickable link
          )
        } else {
          intervention_display <- tags$p(
            tags$b("Intervention: "), intervention_name,
            tags$span("(Link to intervention unavailable)") 
          )
        }
        
  effect_size <- as.numeric(study_details$effect_size)
  variance <- as.numeric(study_details$variance)           
        
        showModal(modalDialog(
          title = div(tags$p(style = "font-weight: bold; font-size: 18px; margin-bottom: 2px;",
            study_link),  # Study name as a hyperlink
            tags$p(
              style = "font-size: 14px; color: #104735; margin-top: 0px;",
              study_details$research  # Research article title in smaller gray text
            )),
          div( style = "display: flex; justify-content: space-between; align-items: center; gap: 10px;",
         
          div(
            style = "flex: 2;",     
          intervention_display,
          tags$p(tags$b("Intervention Description:"), study_details$description),
          tags$p(tags$b("Intervention Cost:"), study_details$inter_cost),
          tags$p(tags$b("Who Administers the Intervention:"), study_details$inter_admin),
          tags$p(tags$b("Intervention Length:"), study_details$inter_length),
          tags$p(tags$b("Grade Level:"), study_details$grade_group)
          
          ),
          div(style = "flex: 1; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 10px;",
              tags$div(
                style = "display: flex; flex-direction: column; align-items: center; text-align: center",
                tags$img(src = "body.png", width = "50px"),
                tags$p(
                  paste(study_details$percent_female,"female student participants.")  # Dynamic percentage
                )
              ),
              
              tags$div(
                style = "display: flex; align-items: center; justify-content: center; 
                 width: 80px; height: 80px; border-radius: 50%; 
                 background-color: #007030; color: white; 
                 font-size: 16px; text-align: center; font-weight: bold; 
                 flex-shrink: 0;",
                if (!is.na(effect_size)) {
                  paste(round(effect_size, 3))
                } else {
                  "Not Available"
                }
              ),
              # Effect Size Text
              tags$p(
                style = "font-size: 12px; text-align: center",
                
                # Convert effect size and variance to numeric
                if (!is.na(effect_size) & !is.na(variance)) {
                  # Compute Standard Error (SE) and 95% CI
                  SE <- sqrt(variance)
                  CI_lower <- round(effect_size - 1.96 * SE, 3)
                  CI_upper <- round(effect_size + 1.96 * SE, 3)
                  
                  HTML(paste("Standardized mean difference (SMD*) =", round(effect_size, 3),
                             "<br>",
                             "(95% CI [", CI_lower, ", ", CI_upper,"])"))
                } else {
                  "Standardized Mean Difference (SMD*) is not available."
                }
              ))
          ),
          
          
          tags$hr(),
          
          # Intervention Effect Summary
          tags$p(
            style = "font-size: 14px; text-align: center;",
            tags$b("Intervention effect:"), 
            
            if (!is.na(study_details$interpretation) & study_details$interpretation != "") {
              paste(
                "On average, students in the depression prevention program had",
                study_details$interpretation,
                "depression symptoms compared to students in control groups."
              )
            } else {
              "We do not have the SMD to report intervention effect on depression symptoms." # If interpretation is missing, this will return an empty string
            },
            
            tags$br(),
            tags$br(),
            "*SMD is a way to measure how big or small the difference is between two groups in a study. 
  In this data, a negative SMD means the participants who received the intervention had fewer depression symptoms."
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        observeEvent(input$modal_close, {
          updateActionButton(session, study_id, label = study_details$study_author_year)  # Reset button state
        })
      }, ignoreNULL = TRUE, ignoreInit = TRUE)  # Prevent old clicks from triggering
      })
    })
  
  
  
  # 7) Reset Filters Button
  default_study_year <- c("Before 2000", "2001-2010", "2011-2015", "After 2015")          
  default_inter_admin <- "All"  # Set default intervention admin
  default_sim_length <- "All"
  default_grade <- "All"
  
  # Reset Filters Button
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)  # Ensures a valid click
    selected_country(NULL) 
    selected_country(input$map_shape_click$id)  # Update country selection
  })
  
  observeEvent(input$reset_filters, {
    selected_country(NULL)  # Reset country selection properly
    
    updateCheckboxGroupInput(session, "study_year_group", selected = default_study_year)
    updateCheckboxGroupInput(session, "inter_admin", selected = default_inter_admin)
    updateCheckboxGroupInput(session, "sim_length", selected = default_sim_length)
    updateCheckboxGroupInput(session, "grade_group", selected = default_grade)

    })

  
  
  session$allowReconnect(TRUE)
  
  
}

