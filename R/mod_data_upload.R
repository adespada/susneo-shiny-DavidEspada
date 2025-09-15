#' data_upload UI Function
#'
#' @description A shiny Module for data upload functionality with modern dark theme.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Data Source Selection Section
    div(class = "dashboard-section",
        div(class = "section-title",
            icon("question-circle", class = "section-icon"),
            "Data Source Selection"
        ),
        fluidRow(
          column(12,
                 div(class = "upload-container",
                     div(style = "text-align: center; margin-bottom: 30px;",
                         h4("How would you like to proceed with your data?",
                            style = "color: #ffffff; margin-bottom: 25px; font-weight: 500;"),
                         fluidRow(
                           column(6,
                                  div(class = "upload-card", style = "cursor: pointer;",
                                      onclick = paste0("Shiny.setInputValue('", ns("data_choice"), "', 'sample', {priority: 'event'});"),
                                      div(class = "upload-card-header",
                                          icon("database", class = "upload-icon"),
                                          h4("Use Sample Data", class = "upload-title")
                                      ),
                                      div(class = "upload-card-body",
                                          p("Start with pre-configured sample data for testing and exploration",
                                            class = "upload-description")
                                      )
                                  )
                           ),
                           column(6,
                                  div(class = "upload-card", style = "cursor: pointer;",
                                      onclick = paste0("Shiny.setInputValue('", ns("data_choice"), "', 'custom', {priority: 'event'});"),
                                      div(class = "upload-card-header",
                                          icon("file-upload", class = "upload-icon"),
                                          h4("Upload Custom Data", class = "upload-title")
                                      ),
                                      div(class = "upload-card-body",
                                          p("Upload your own CSV file to analyze your data",
                                            class = "upload-description")
                                      )
                                  )
                           )
                         )
                     )
                 )
          )
        )
    ),

    # Custom Data Upload Section (conditionally shown)
    conditionalPanel(
      condition = paste0("input['", ns("data_choice"), "'] == 'custom'"),
      div(class = "dashboard-section",
          div(class = "section-title",
              icon("cloud-upload-alt", class = "section-icon"),
              "Data Upload"
          ),
          fluidRow(
            column(12,
                   div(class = "upload-container",
                       div(class = "upload-card",
                           div(class = "upload-card-header",
                               icon("file-csv", class = "upload-icon"),
                               h4("Upload CSV File", class = "upload-title")
                           ),
                           div(class = "upload-card-body",
                               div(class = "file-input-wrapper",
                                   fileInput(ns("file"),
                                             label = NULL,
                                             accept = c(".csv"),
                                             buttonLabel = "Browse Files",
                                             placeholder = "No file selected",
                                             width = "100%")
                               ),
                               p("Select a CSV file from your computer to upload",
                                 class = "upload-description")
                           )
                       ),
                       div(class = "upload-actions",
                           actionButton(ns("load_data"),
                                        "Load Data",
                                        class = "btn-load-data",
                                        icon = icon("play"))
                       )
                   )
            )
          )
      )
    ),

    # Sample Data Section (conditionally shown)
    conditionalPanel(
      condition = paste0("input['", ns("data_choice"), "'] == 'sample'"),
      div(class = "dashboard-section",
          div(class = "section-title",
              icon("database", class = "section-icon"),
              "Sample Data"
          ),
          fluidRow(
            column(12,
                   div(class = "upload-container",
                       div(class = "upload-card",
                           div(class = "upload-card-header",
                               icon("database", class = "upload-icon"),
                               h4("Sample Data", class = "upload-title")
                           ),
                           div(class = "upload-card-body",
                               div(class = "checkbox-container",
                                   checkboxInput(ns("use_sample"),
                                                 "Use Sample Data",
                                                 value = TRUE)
                               ),
                               p("Load pre-configured sample data for testing",
                                 class = "upload-description")
                           )
                       ),
                       div(class = "upload-actions",
                           actionButton(ns("load_data"),
                                        "Load Data",
                                        class = "btn-load-data",
                                        icon = icon("play"))
                       )
                   )
            )
          )
      )
    ),

    # Status Message Section (always visible when choice is made)
    conditionalPanel(
      condition = paste0("input['", ns("data_choice"), "'] != ''"),
      div(class = "dashboard-section",
          div(class = "upload-status-container",
              verbatimTextOutput(ns("upload_status"))
          )
      )
    ),

    # Reset Choice Section
    conditionalPanel(
      condition = paste0("input['", ns("data_choice"), "'] != ''"),
      div(style = "text-align: center; margin: 20px 0;",
          actionButton(ns("reset_choice"),
                       "Change Data Source",
                       class = "btn btn-outline-secondary",
                       icon = icon("arrow-left"),
                       style = "background-color: transparent; border: 2px solid #4ecdc4; color: #4ecdc4; padding: 8px 16px; border-radius: 20px;")
      )
    ),

    # Data Preview Section
    div(class = "dashboard-section",
        div(class = "section-title",
            icon("table", class = "section-icon"),
            "Data Preview"
        ),
        div(class = "preview-container",
            DT::dataTableOutput(ns("data_preview"))
        )
    )
  )
}

#' data_upload Server Functions
#'
#' @importFrom golem app_sys
#' @param id Module ID
#' @param analyzer class_sustainability_analyzer R6 object
#'
#' @noRd
mod_data_upload_server <- function(id, analyzer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initiate data_choice
    observe({
      if (is.null(input$data_choice)) {
        session$sendInputMessage("data_choice", list(value = ""))
      }
    })

    # Handle data choice and automatically set checkbox
    observeEvent(input$data_choice, {
      if (input$data_choice == "sample") {
        updateCheckboxInput(session, "use_sample", value = TRUE)
      } else if (input$data_choice == "custom") {
        updateCheckboxInput(session, "use_sample", value = FALSE)
      }
    })

    # Reactive values
    values <- reactiveValues(
      data = NULL,
      upload_message = "No data loaded yet."
    )

    # Check
    observeEvent(input$load_data, {
      if (input$use_sample) {
        # Cargar datos de muestra
        tryCatch({
          # Leer el archivo
          file_path <- file.path("data", "SAMPLE ASSIGNMENT DATA.csv")
          sample_data <- readr::read_csv(file_path, show_col_types = FALSE)

          # Renombrar columna problemática
          names(sample_data)[names(sample_data) == "carbon emission in kgco2e"] <- "carbon_emission_kgco2e"

          # Cargar en el analyzer
          if (analyzer$load_data(sample_data)) {
            values$data <- sample_data
            values$upload_message <- paste("Sample data loaded successfully!", nrow(sample_data), "rows loaded.")
          } else {
            values$upload_message <- "Error: Invalid data format. Please check required columns."
          }

        }, error = function(e) {
          values$upload_message <- paste("Error loading sample data:", e$message)
        })

      } else {
        # Usar archivo subido
        req(input$file)
        tryCatch({
          df <- readr::read_csv(input$file$datapath, show_col_types = FALSE)

          # Renombrar columna si existe
          if ("carbon emission in kgco2e" %in% names(df)) {
            names(df)[names(df) == "carbon emission in kgco2e"] <- "carbon_emission_kgco2e"
          }

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

    # Handle data choice and automatically set checkbox
    observeEvent(input$data_choice, {
      if (input$data_choice == "sample") {
        updateCheckboxInput(session, "use_sample", value = TRUE)
      } else if (input$data_choice == "custom") {
        updateCheckboxInput(session, "use_sample", value = FALSE)
      }
    })

    # Reset choice functionality
    observeEvent(input$reset_choice, {
      # Reset the data choice
      session$sendInputMessage("data_choice", list(value = ""))

      # Reset other inputs
      updateCheckboxInput(session, "use_sample", value = TRUE)

      # Reset file input (requires custom JavaScript - see below)
      session$sendCustomMessage(
        type = 'resetFileInput',
        message = ns('file')
      )

      # Clear data and messages
      values$data <- NULL
      values$upload_message <- "No data loaded yet."
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
