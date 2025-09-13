#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Initialize sustainability analyzer
  analyzer <- sustainability_analyzer$new()

  # Data upload module
  uploaded_data <- mod_data_upload_server("data_upload_1", analyzer)

  # Dashboard module
  mod_dashboard_server("dashboard_1", uploaded_data, analyzer)
}
