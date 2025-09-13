#' data_upload UI Function
#'
#' @description A shiny Module for data upload functionality.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Data Upload", status = "primary", solidHeader = TRUE, width = 12,
        fileInput(ns("file"), "Choose CSV File",
                  accept = c(".csv")),
        checkboxInput(ns("use_sample"), "Use Sample Data", value = TRUE),
        hr(),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary"),
        br(), br(),
        verbatimTextOutput(ns("upload_status"))
      )
    ),
    fluidRow(
      box(
        title = "Data Preview", status = "info", solidHeader = TRUE, width = 12,
        DT::dataTableOutput(ns("data_preview"))
      )
    )
  )
}

#' data_upload Server Functions
#'
#' @param id Module ID
#' @param analyzer sustainability_analyzer R6 object
#'
#' @noRd
mod_data_upload_server <- function(id, analyzer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    values <- reactiveValues(
      data = NULL,
      upload_message = "No data loaded yet."
    )

    # Sample data
    sample_data <- data.frame(
      id = 1:11,
      site = c("VDRK", "AKBB", "GFBN", "GECE", "GFBN", "GECE",
               "AKBB", "AKBB", "AKBB", "GFBN", "VDRK"),
      date = c("03/08/2025", "05/09/2025", "01/09/2025", "20/08/2025",
               "03/08/2025", "08/08/2025", "01/09/2025", "08/08/2025",
               "18/08/2025", "06/08/2025", "27/08/2025"),
      type = c("Water", "Electricity", "Waste", "Gas", "Gas", "Gas",
               "Waste", "Gas", "Water", "Fuel", "Water"),
      value = c(8156, 96086, 12805, 46952, 86287, 69730,
                90153, 37488, 75316, 12869, 15104),
      carbon_emission_kgco2e = c(28, 79, 62, 75, 74, 1, 100, 58, 1, 84, 84)
    )

    # Load data when button is clicked
    observeEvent(input$load_data, {
      if (input$use_sample) {
        # Use sample data
        if (analyzer$load_data(sample_data)) {
          values$data <- sample_data
          values$upload_message <- "Sample data loaded successfully!"
        } else {
          values$upload_message <- "Error loading sample data."
        }
      } else {
        # Use uploaded file
        req(input$file)

        tryCatch({
          df <- readr::read_csv(input$file$datapath, show_col_types = FALSE)

          if (analyzer$load_data(df)) {
            values$data <- df
            values$upload_message <- paste("File uploaded successfully!", nrow(df), "rows loaded.")
          } else {
            values$upload_message <- "Error: Invalid data format. Please check required columns."
          }
        }, error = function(e) {
          values$upload_message <- paste("Error reading file:", e$message)
        })
      }
    })

    # Upload status output
    output$upload_status <- renderText({
      values$upload_message
    })

    # Data preview table
    output$data_preview <- DT::renderDataTable({
      req(values$data)
      DT::datatable(values$data,
                    options = list(scrollX = TRUE, pageLength = 10))
    })

    # Return reactive data for other modules
    return(reactive({ values$data }))
  })
}
