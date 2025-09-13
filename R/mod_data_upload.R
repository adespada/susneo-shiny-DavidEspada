#' data_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' data_upload Server Functions
#'
#' @noRd 
mod_data_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_data_upload_ui("data_upload_1")
    
## To be copied in the server
# mod_data_upload_server("data_upload_1")
