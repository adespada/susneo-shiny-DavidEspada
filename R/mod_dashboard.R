#' dashboard UI Function
#'
#' @description A shiny Module for dashboard visualization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Filters
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = TRUE, width = 12,
        fluidRow(
          column(6,
                 dateRangeInput(ns("date_range"), "Date Range:",
                                start = Sys.Date() - 30,
                                end = Sys.Date())
          ),
          column(6,
                 selectInput(ns("sites"), "Select Sites:",
                             choices = NULL,
                             multiple = TRUE)
          )
        )
      )
    ),

    # KPI Boxes
    fluidRow(
      valueBoxOutput(ns("total_consumption"), width = 3),
      valueBoxOutput(ns("total_emissions"), width = 3),
      valueBoxOutput(ns("avg_consumption"), width = 3),
      valueBoxOutput(ns("unique_sites"), width = 3)
    ),

    # Charts
    fluidRow(
      box(
        title = "Energy Consumption Over Time", status = "primary",
        solidHeader = TRUE, width = 6,
        plotly::plotlyOutput(ns("time_series_plot"))
      ),
      box(
        title = "Consumption by Site", status = "success",
        solidHeader = TRUE, width = 6,
        plotly::plotlyOutput(ns("site_comparison_plot"))
      )
    ),

    # Data Table
    fluidRow(
      box(
        title = "Summary Table", status = "info", solidHeader = TRUE, width = 12,
        DT::dataTableOutput(ns("summary_table"))
      )
    )
  )
}

#' dashboard Server Functions
#'
#' @param id Module ID
#' @param data_reactive Reactive data from upload module
#' @param analyzer sustainability_analyzer R6 object
#'
#' @noRd
mod_dashboard_server <- function(id, data_reactive, analyzer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update site choices when data changes
    observe({
      req(data_reactive())
      sites <- analyzer$get_sites()
      updateSelectInput(session, "sites",
                        choices = sites,
                        selected = sites)
    })

    # Update date range when data changes
    observe({
      req(data_reactive())
      date_range <- analyzer$get_date_range()
      updateDateRangeInput(session, "date_range",
                           start = date_range$min,
                           end = date_range$max)
    })

    # Reactive filtered data
    filtered_data <- reactive({
      req(data_reactive())
      analyzer$filter_data(
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        selected_sites = input$sites
      )
    })

    # Calculate KPIs
    kpis <- reactive({
      analyzer$calculate_kpis(filtered_data())
    })

    # KPI Value Boxes
    output$total_consumption <- renderValueBox({
      valueBox(
        value = scales::comma(round(kpis()$total_consumption)),
        subtitle = "Total Consumption",
        icon = icon("bolt"),
        color = "blue"
      )
    })

    output$total_emissions <- renderValueBox({
      valueBox(
        value = scales::comma(round(kpis()$total_emissions)),
        subtitle = "Total Emissions (kg CO2e)",
        icon = icon("cloud"),
        color = "red"
      )
    })

    output$avg_consumption <- renderValueBox({
      valueBox(
        value = scales::comma(round(kpis()$avg_consumption)),
        subtitle = "Average Consumption",
        icon = icon("chart-line"),
        color = "green"
      )
    })

    output$unique_sites <- renderValueBox({
      valueBox(
        value = kpis()$unique_sites,
        subtitle = "Active Sites",
        icon = icon("building"),
        color = "yellow"
      )
    })

    # Time Series Plot
    output$time_series_plot <- plotly::renderPlotly({
      req(filtered_data())

      p <- filtered_data() %>%
        dplyr::group_by(date, site) %>%
        dplyr::summarise(total_value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = total_value, color = site)) +
        ggplot2::geom_line(size = 1.2) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(x = "Date", y = "Energy Consumption",
                      title = "Energy Consumption Trends") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = ggplot2::element_blank())

      plotly::ggplotly(p)
    })

    # Site Comparison Plot
    output$site_comparison_plot <- plotly::renderPlotly({
      req(filtered_data())

      p <- filtered_data() %>%
        dplyr::group_by(site) %>%
        dplyr::summarise(
          total_consumption = sum(value, na.rm = TRUE),
          total_emissions = sum(carbon_emission_kgco2e, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        ggplot2::ggplot(ggplot2::aes(x = reorder(site, total_consumption), y = total_consumption)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "Site", y = "Total Consumption",
                      title = "Total Consumption by Site") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # Summary Table
    output$summary_table <- DT::renderDataTable({
      req(filtered_data())

      summary_data <- filtered_data() %>%
        dplyr::group_by(site, type) %>%
        dplyr::summarise(
          total_consumption = sum(value, na.rm = TRUE),
          total_emissions = sum(carbon_emission_kgco2e, na.rm = TRUE),
          avg_consumption = mean(value, na.rm = TRUE),
          records_count = n(),
          .groups = 'drop'
        ) %>%
        dplyr::mutate(
          total_consumption = round(total_consumption, 2),
          total_emissions = round(total_emissions, 2),
          avg_consumption = round(avg_consumption, 2)
        )

      DT::datatable(
        summary_data,
        options = list(scrollX = TRUE, pageLength = 10),
        colnames = c("Site", "Type", "Total Consumption", "Total Emissions (kg CO2e)",
                     "Avg Consumption", "Records")
      )
    })
  })
}
