#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic starts here
    dashboardPage(
      dashboardHeader(title = "SUSNEO Sustainability Dashboard"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
          menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "upload",
                  mod_data_upload_ui("data_upload_1")
          ),
          tabItem(tabName = "dashboard",
                  mod_dashboard_ui("dashboard_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SUSNEO Sustainability Dashboard"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),

    # JavaScript for file input reset functionality
    tags$script(HTML("
      Shiny.addCustomMessageHandler('resetFileInput', function(inputId) {
        var input = document.getElementById(inputId);
        if (input) {
          input.value = '';
          // Trigger change event to notify Shiny
          var event = new Event('change', { bubbles: true });
          input.dispatchEvent(event);
        }
      });
    "))
  )
}
