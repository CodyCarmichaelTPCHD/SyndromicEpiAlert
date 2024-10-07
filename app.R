library(shiny)
library(shinydashboard)
library(shinythemes)
library(rmarkdown)

# Define the dashboard header with a title
header <- dashboardHeader(title = "ESSENCE Extractor", titleWidth = 250)

# Define the dashboard sidebar, you can add additional menu items as needed
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Form", tabName = "form", icon = icon("wpforms")),
    menuItem("Description", tabName = "description", icon = icon("info-circle"))
    # Add additional menu items here if necessary
  )
)

# Define the dashboard body with the form elements
body <- dashboardBody(
    # Apply a Bootstrap theme for styling
  
  # Use tabItems to manage multiple pages or sections
  tabItems(
    tabItem(tabName = "form",
            fluidRow(
              box(
                title = "Data Input",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                textInput("username", "Username"),
                passwordInput("password", "Password"),
                textInput("folderPath", "Folder Path", value = "C:/Users/ccarmichael/Documents/Test_Shiny_UIPage/"),
                checkboxGroupInput("syndromes", "Syndromes", choices = c( "alcohol v1",
                                                                          "all drug v2",
                                                                          "anxiety disorders v1",
                                                                          "assault firearm injury v1",
                                                                          "attention-deficit hyperactivity disorders v1",
                                                                          "benzodiazepine overdose v1",
                                                                          "benzodiazepine overdose v2 parsed",
                                                                          "bipolar disorders v1",
                                                                          "cocaine overdose v1",
                                                                          "depressive disorders v1",
                                                                          "disruptive behavioral and impulse-control v1",
                                                                          "eating disorders v1",
                                                                          "fentanyl overdose v1",
                                                                          "fentanyl overdose v2 parsed",
                                                                          "firearm injury v1",
                                                                          "firearm injury v2",
                                                                          "heroin overdose v4",
                                                                          "intentional firearm injury v1",
                                                                          "intimate partner violence v2",
                                                                          "mental health v1",
                                                                          "obsessive-compulsive disorders v1",
                                                                          "opioid overdose v3",
                                                                          "persons experiencing homelessness dd v1",
                                                                          "schizophrenia spectrum disorders v1",
                                                                          "seizure or epilepsy v1",
                                                                          "sexual violence v3",
                                                                          "stimulants v3",
                                                                          "suicidal ideation v1",
                                                                          "suicide attempt v2",
                                                                          "suspected child abuse and neglect v1",
                                                                          "synthetic cannabinoids v1",
                                                                          "tic disorders v1",
                                                                          "trauma and stressor-related disorders v1",
                                                                          "unintentional firearm injury v1",
                                                                          "sdc disaster related mental health v1",
                                                                          "sdc suicide related v1",
                                                                          "sexual violence v2")),
                checkboxInput("comboExamination", "Combo-Examination"),
                uiOutput("comboDropdowns"),
                selectInput("useHistoricalData", "Use Historical Data?", choices = c("False", "True")),
                uiOutput("historicalDataOptions"),
                dateInput("startDate", "Start Date", value = (Sys.Date()-30), format = "mm/dd/yyyy"),
                dateInput("endDate", "End Date", value = (Sys.Date()-14), format = "mm/dd/yyyy"),
                selectInput("climateData", "Climate Data:", choices = c("No", "Yes"), selected = "No"),
                uiOutput("pm25Options"),
                checkboxInput("ageGenderGraphs", "Age and Gender Graphs", value = FALSE),
                checkboxInput("ageOnlyGraphs", "Age Only Graphs", value = FALSE),
                checkboxInput("genderOnlyGraphs", "Gender Only Graphs", value = FALSE),
                selectInput("ageGroup", "Age Group Selection?", choices = c("PED", "Standard", "PIT")),
                checkboxInput("raceEthnicityGraphs", "Race/Ethnicity Graphs", value = FALSE),
                checkboxInput("maps", "Maps -use only if PC has good Memory/CPU", value = FALSE),
                conditionalPanel(
                  condition = "input.maps == true",  # This condition checks if 'maps' is checked
                  checkboxInput("spatialAnalysis", "Spatial Analysis -use only if PC has good Memory/CPU", value = FALSE)
                ),
                checkboxInput("hospitalGroupings", "Hospital Groupings", value = FALSE),
                checkboxInput("timeOfDayAnalysis", "Time of Day Analysis", value = FALSE),
                checkboxInput("timeSeriesCountGraphs", "Time Series Count Graphs", value = FALSE),
                checkboxInput("timeSeriesPer10kVisits", "Per 10,000 Visits Graphs", value = FALSE),
                checkboxInput("nlpAnalysis", "Natural Language Processing Analysis -use only if PC has good Memory/CPU", value = FALSE),
                actionButton("submit", "Submit", class = "btn-primary btn-lg"),
                textOutput("saveStatus")
              )
            )
            ),
    tabItem(tabName = "description",
            fluidRow(
              box(
                title = "About the Application",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                p("This Shiny application facilitates the extraction and analysis of data relevant to the ESSENCE project."),
                p("Please navigate to the 'Input Form' tab to provide data and configure the parameters for analysis."),
                p("Ensure that the provided information is accurate to avoid any errors during data processing."),
                p("For support or further instructions, please contact Cody Carmichael, CCarmichael@TPCHD.org.")
              )
            ))
    # Additional tabs or content can be added here
  )
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session) {
  
  output$comboDropdowns <- renderUI({
    if(input$comboExamination) {
      tagList(
        selectInput("syndrome1", "Syndrome 1:",
                    
                    
                    
                    choices = c( "alcohol v1",
                                 "all drug v2",
                                 "anxiety disorders v1",
                                 "assault firearm injury v1",
                                 "attention-deficit hyperactivity disorders v1",
                                 "benzodiazepine overdose v1",
                                 "benzodiazepine overdose v2 parsed",
                                 "bipolar disorders v1",
                                 "cocaine overdose v1",
                                 "depressive disorders v1",
                                 "disruptive behavioral and impulse-control v1",
                                 "eating disorders v1",
                                 "fentanyl overdose v1",
                                 "fentanyl overdose v2 parsed",
                                 "firearm injury v1",
                                 "firearm injury v2",
                                 "heroin overdose v4",
                                 "intentional firearm injury v1",
                                 "intimate partner violence v2",
                                 "mental health v1",
                                 "obsessive-compulsive disorders v1",
                                 "opioid overdose v3",
                                 "persons experiencing homelessness dd v1",
                                 "schizophrenia spectrum disorders v1",
                                 "seizure or epilepsy v1",
                                 "sexual violence v3",
                                 "stimulants v3",
                                 "suicidal ideation v1",
                                 "suicide attempt v2",
                                 "suspected child abuse and neglect v1",
                                 "synthetic cannabinoids v1",
                                 "tic disorders v1",
                                 "trauma and stressor-related disorders v1",
                                 "unintentional firearm injury v1",
                                 "sdc disaster related mental health v1",
                                 "sdc suicide related v1",
                                 "sexual violence v2")
                    
                    
                    
                    
                    ),
        selectInput("syndrome2", "Syndrome 2:",
                    
                    
                    
                    
                    choices = c( "alcohol v1",
                                 "all drug v2",
                                 "anxiety disorders v1",
                                 "assault firearm injury v1",
                                 "attention-deficit hyperactivity disorders v1",
                                 "benzodiazepine overdose v1",
                                 "benzodiazepine overdose v2 parsed",
                                 "bipolar disorders v1",
                                 "cocaine overdose v1",
                                 "depressive disorders v1",
                                 "disruptive behavioral and impulse-control v1",
                                 "eating disorders v1",
                                 "fentanyl overdose v1",
                                 "fentanyl overdose v2 parsed",
                                 "firearm injury v1",
                                 "firearm injury v2",
                                 "heroin overdose v4",
                                 "intentional firearm injury v1",
                                 "intimate partner violence v2",
                                 "mental health v1",
                                 "obsessive-compulsive disorders v1",
                                 "opioid overdose v3",
                                 "persons experiencing homelessness dd v1",
                                 "schizophrenia spectrum disorders v1",
                                 "seizure or epilepsy v1",
                                 "sexual violence v3",
                                 "stimulants v3",
                                 "suicidal ideation v1",
                                 "suicide attempt v2",
                                 "suspected child abuse and neglect v1",
                                 "synthetic cannabinoids v1",
                                 "tic disorders v1",
                                 "trauma and stressor-related disorders v1",
                                 "unintentional firearm injury v1",
                                 "sdc disaster related mental health v1",
                                 "sdc suicide related v1",
                                 "sexual violence v2")
                    
                    
                    
                    ),
        textInput("pairedSyndromeName", "Paired Syndrome Name:", value = "")
      )
    }
  })
  
  output$historicalDataOptions <- renderUI({
    if(input$useHistoricalData == "True") {
      tagList(
        selectInput("lookbackYears", "Years of Lookback:",
                    choices = 1:3),

      )
    }
  })
  
  output$pm25Options <- renderUI({
    if(input$climateData == "Yes") {
      selectInput("pm25Measures", "PM 2.5 Measures?", choices = c("No", "Yes"))
    }
  })
  
  observeEvent(input$submit, {
    # Create the data frame with the input values
    result <- data.frame(
      Username = input$username,
      Password = input$password,  # Be cautious with password handling!
      Syndromes = paste(input$syndromes, collapse = ";"),
      ComboExamination = input$comboExamination,
      Syndrome1 = if(input$comboExamination) input$syndrome1 else NA,
      Syndrome2 = if(input$comboExamination) input$syndrome2 else NA,
      PairedSyndromeName = if(input$comboExamination) input$pairedSyndromeName else NA,
      UseHistoricalData = input$useHistoricalData,
      LookbackYears = if(input$useHistoricalData == "True") input$lookbackYears else NA,
      StartDate = input$startDate,
      EndDate = input$endDate,
      ClimateData = input$climateData,
      PM25Measures = if(input$climateData == "Yes") input$pm25Measures else "No",
      AgeGenderGraphs = input$ageGenderGraphs,
      AgeOnlyGraphs = input$ageOnlyGraphs,
      GenderOnlyGraphs = input$genderOnlyGraphs,
      AgeGroup = input$ageGroup,
      RaceEthnicityGraphs = input$raceEthnicityGraphs,
      Maps = input$maps,
      SpatialAnalysis = input$spatialAnalysis,
      HospitalGroupings = input$hospitalGroupings,
      TimeOfDayAnalysis = input$timeOfDayAnalysis,
      TimeSeriesCountGraphs = input$timeSeriesCountGraphs,
      TimeSeriesPer10kVisits = input$timeSeriesPer10kVisits,
      NLPAnalysis = input$nlpAnalysis,
      stringsAsFactors = FALSE
    )
    
    # Check if the result data frame is empty
    if (nrow(result) == 0 || all(sapply(result, function(x) all(is.na(x))))) {
      output$saveStatus <- renderText("No data to save. Please check input values.")
      return()
    }
    
    # Define the filename and full path
    fileName <- "UserInputs.csv"
    fullPath <- file.path(input$folderPath, fileName)
    
    
    ### Move this to a script to iterate down.
    
    # Attempt to write the CSV and execute further actions
    tryCatch({
      # Write the CSV file
      write.csv(result, fullPath, row.names = FALSE, quote = TRUE)
      
      # Notify user of CSV save
      output$saveStatus <- renderText("Data saved successfully to CSV!")
      
      # Run the R script
      source("STEP_3_SOURCER_CONTROL.R")
      
      # Update status
      output$saveStatus <- renderText("R script executed and R Markdown rendered successfully.")
      
    }, error = function(e) {
      # Catch any error and display the message to the user
      output$saveStatus <- renderText(paste("An error occurred:", e$message))
    })
  })
  
}

shinyApp(ui, server)
