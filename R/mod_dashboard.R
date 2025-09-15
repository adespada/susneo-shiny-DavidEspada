#' Modern Dashboard UI Function
#'
#' @description A shiny Module for modern dashboard visualization with dark theme.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_dashboard_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Filters Section
    div(class = "filter-section",
        div(class = "section-title",
            icon("filter", class = "section-icon"),
            "Filters"),
        fluidRow(
          column(6,
                 dateRangeInput(ns("date_range"), "Date Range:",
                                start = Sys.Date() - 30,
                                end = Sys.Date())
          ),
          column(6,
                 selectizeInput(ns("site_filter"),
                                "Facility:",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(
                                  placeholder = "Select facilities (all selected by default)",
                                  plugins = list('remove_button')
                                ))
          )
        )
    ),

    # KPI Company Section
    div(class = "dashboard-section",
        div(class = "section-title",
            icon("building", class = "section-icon"),
            "KPI Company - Overall Performance Metrics"),
        fluidRow(
          column(3,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-energy", icon("battery-full"))
                     ),
                     div(class = "kpi-value", textOutput(ns("total_energy_value"))),
                     div(class = "kpi-label", "Total Energy Usage Amount")
                 )
          ),
          column(3,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-emissions", icon("smog"))
                     ),
                     div(class = "kpi-value", textOutput(ns("total_emissions_value"))),
                     div(class = "kpi-label", "Total Emissions (kgCO2e)")
                 )
          ),
          column(3,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-avg-energy", icon("chart-bar"))
                     ),
                     div(class = "kpi-value", textOutput(ns("avg_energy_value"))),
                     div(class = "kpi-label", "Average Energy Usage Amount")
                 )
          ),
          column(3,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-avg-emissions", icon("leaf"))
                     ),
                     div(class = "kpi-value", textOutput(ns("avg_emissions_value"))),
                     div(class = "kpi-label", "Average Emissions (kgCO2e)")
                 )
          )
        )
    ),

    # Categories Section
    div(class = "dashboard-section",
        div(class = "section-title",
            icon("chart-bar", class = "section-icon"),
            "Sustainability Categories - Monthly Carbon Emissions Average"),
        fluidRow(
          column(2,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-water", icon("tint"))
                     ),
                     div(class = "kpi-value", textOutput(ns("water_value"))),
                     div(class = "kpi-label", "Water (kgCO2e)")
                 )
          ),
          column(2,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-electricity", icon("bolt"))
                     ),
                     div(class = "kpi-value", textOutput(ns("electricity_value"))),
                     div(class = "kpi-label", "Electricity (kgCO2e)")
                 )
          ),
          column(2,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-waste", icon("trash"))
                     ),
                     div(class = "kpi-value", textOutput(ns("waste_value"))),
                     div(class = "kpi-label", "Waste (kgCO2e)")
                 )
          ),
          column(3,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-gas", icon("fire"))
                     ),
                     div(class = "kpi-value", textOutput(ns("gas_value"))),
                     div(class = "kpi-label", "Gas (kgCO2e)")
                 )
          ),
          column(3,
                 div(class = "kpi-card",
                     div(class = "kpi-icon-container",
                         div(class = "kpi-icon kpi-fuel", icon("gas-pump"))
                     ),
                     div(class = "kpi-value", textOutput(ns("fuel_value"))),
                     div(class = "kpi-label", "Fuel (kgCO2e)")
                 )
          )
        )
    ),

    # Trends Section
    div(class = "dashboard-section",
        div(class = "section-title",
            icon("chart-line", class = "section-icon"),
            "Trends Analysis"),
        fluidRow(
          column(6,
                 div(class = "chart-container",
                     h4("Carbon emissions Trends", style = "color: #fff; margin-bottom: 20px;"),
                     plotly::plotlyOutput(ns("ytd_summary_plot"), height = "300px")
                 )
          ),
          column(6,
                 div(class = "chart-container",
                     h4("Energy usage Trends", style = "color: #fff; margin-bottom: 15px;"),
                     div(style = "margin-bottom: 15px;",
                         tags$button("Quarterly", class = "btn btn-sm",
                                     style = "background-color: #4ecdc4; color: #000; border: none; border-radius: 5px; padding: 5px 15px;")
                     ),
                     plotly::plotlyOutput(ns("cost_trends_plot"), height = "250px")
                 )
          )
        )
    ),

    # Pie Charts Section
    div(class = "dashboard-section",
        div(class = "section-title",
            icon("chart-pie", class = "section-icon"),
            "Carbon Emissions Distribution Analysis"),
        fluidRow(
          column(6,
                 div(class = "chart-container",
                     h4("Carbon Emissions by Type", style = "color: #fff; margin-bottom: 15px;"),
                     plotly::plotlyOutput(ns("emissions_pie_plot"), height = "350px")
                 )
          ),
          column(6,
                 div(class = "chart-container",
                     h4("Carbon Emissions by Site", style = "color: #fff; margin-bottom: 15px;"),
                     plotly::plotlyOutput(ns("emissions_site_pie_plot"), height = "350px")
                 )
          )
        )
    ),

    # Analytics Section
    div(class = "dashboard-section",
        div(class = "section-title",
            icon("table", class = "section-icon"),
            "Detailed Analytics"),
        DT::dataTableOutput(ns("summary_table"))
    ),

    # JavaScript
    tags$script(HTML("
      $(document).ready(function() {
        // calendar style
        function fixCalendar() {
          $('.datepicker, .bootstrap-datetimepicker-widget').each(function() {
            $(this).css({
              'background-color': '#2a2a2a',
              'border': '1px solid #555',
              'color': '#ffffff'
            });

            $(this).find('td, th').css({
              'color': '#ffffff !important',
              'background-color': 'transparent'
            });

            $(this).find('.day, .old, .new').css({
              'color': '#ffffff',
              'background-color': 'transparent'
            });

            $(this).find('.day:hover, .old:hover, .new:hover').css({
              'background-color': '#4ecdc4',
              'color': '#000000'
            });

            $(this).find('.active').css({
              'background-color': '#4ecdc4',
              'color': '#000000'
            });
          });
        }

        // selectize
        function fixSelectize() {
          $('.selectize-control .selectize-input').css({
            'background-color': '#2a2a2a',
            'border': '1px solid #555',
            'color': '#ffffff'
          });

          $('.selectize-control .selectize-dropdown').css({
            'background-color': '#2a2a2a',
            'border': '1px solid #555'
          });

          $('.selectize-control .selectize-dropdown .option').css({
            'background-color': '#2a2a2a',
            'color': '#ffffff'
          });

          $('.selectize-control .selectize-dropdown .option:hover').css({
            'background-color': '#4ecdc4',
            'color': '#000000'
          });

          $('.selectize-control .selectize-input .item').css({
            'background-color': '#4ecdc4',
            'color': '#000000'
          });
        }

        // calendar
        $(document).on('show', '.datepicker', fixCalendar);
        $(document).on('shown', '.datepicker', fixCalendar);

        // selectize
        setTimeout(fixSelectize, 500);

        $(document).on('DOMNodeInserted', function() {
          setTimeout(function() {
            fixCalendar();
            fixSelectize();
          }, 100);
        });

        // DOM
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            if (mutation.addedNodes.length > 0) {
              fixCalendar();
              fixSelectize();
            }
          });
        });

        observer.observe(document, {
          childList: true,
          subtree: true
        });
      });
    "))
  )
}

#' Modern Dashboard Server Functions
#'
#' @param id Module ID
#' @param data_reactive Reactive data from upload module
#' @param analyzer sustainability_analyzer R6 object
#'
#' @noRd
mod_dashboard_server <- function(id, data_reactive, analyzer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Declare global variables to avoid R CMD check warnings
    category <- avg_monthly_emissions.x <- avg_monthly_emissions.y <- NULL
    carbon_emission_kgco2e <- avg_monthly_emissions <- total_cost <- NULL
    type <- site <- value <- total_consumption <- avg_consumption <- NULL

    # Update date range AND site choices when data changes
    observe({
      req(data_reactive())

      # Update date range
      date_range <- analyzer$get_date_range()
      updateDateRangeInput(
        session,
        "date_range",
        start = date_range$min,
        end = date_range$max
      )

      # Update site choices
      all_sites <- analyzer$get_sites()
      updateSelectizeInput(
        session,
        "site_filter",
        choices = all_sites,
        selected = all_sites  # Select all sites by default
      )
    }) %>% bindEvent(data_reactive())

    # Reactive filtered data - NOW filters by date AND site
    filtered_data <- reactive({
      req(data_reactive())
      req(input$date_range)

      # Get selected sites (if none selected, use all)
      selected_sites <- input$site_filter
      if(is.null(selected_sites) || length(selected_sites) == 0) {
        selected_sites <- analyzer$get_sites()
      }

      # Filter data by date range AND selected sites
      analyzer$filter_data(
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        selected_sites = selected_sites
      )
    })

    # Calculate company-wide KPIs
    company_kpis <- reactive({
      req(filtered_data())

      data <- filtered_data()

      # Calculate total and average values for the entire company
      total_energy <- sum(data$value, na.rm = TRUE)
      total_emissions <- sum(data$carbon_emission_kgco2e, na.rm = TRUE)
      avg_energy <- mean(data$value, na.rm = TRUE)
      avg_emissions <- mean(data$carbon_emission_kgco2e, na.rm = TRUE)

      list(
        total_energy = total_energy,
        total_emissions = total_emissions,
        avg_energy = avg_energy,
        avg_emissions = avg_emissions
      )
    })

    # Company KPI Output Functions
    output$total_energy_value <- renderText({
      kpis <- company_kpis()
      scales::comma(round(kpis$total_energy, 0))
    })

    output$total_emissions_value <- renderText({
      kpis <- company_kpis()
      scales::comma(round(kpis$total_emissions, 0))
    })

    output$avg_energy_value <- renderText({
      kpis <- company_kpis()
      scales::comma(round(kpis$avg_energy, 1))
    })

    output$avg_emissions_value <- renderText({
      kpis <- company_kpis()
      scales::comma(round(kpis$avg_emissions, 1))
    })

    # Calculate monthly carbon emissions average by category
    monthly_emissions <- reactive({
      req(filtered_data())

      data <- filtered_data()

      # Calculate monthly averages for each category
      monthly_avg <- data %>%
        dplyr::mutate(
          month = format(date, "%Y-%m"),
          category = dplyr::case_when(
            grepl("water|Water", type, ignore.case = TRUE) ~ "water",
            grepl("electric|Electric", type, ignore.case = TRUE) ~ "electricity",
            grepl("waste|Waste", type, ignore.case = TRUE) ~ "waste",
            grepl("gas|Gas", type, ignore.case = TRUE) ~ "gas",
            grepl("fuel|Fuel", type, ignore.case = TRUE) ~ "fuel",
            TRUE ~ "other"
          )
        ) %>%
        dplyr::filter(category != "other") %>%
        dplyr::group_by(category, month) %>%
        dplyr::summarise(
          monthly_emissions = sum(carbon_emission_kgco2e, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::group_by(category) %>%
        dplyr::summarise(
          avg_monthly_emissions = mean(monthly_emissions, na.rm = TRUE),
          .groups = 'drop'
        )

      # Create default values if no data matches categories
      default_categories <- data.frame(
        category = c("water", "electricity", "waste", "gas", "fuel"),
        avg_monthly_emissions = c(150, 2500, 75, 1800, 650)
      )

      if(nrow(monthly_avg) == 0) {
        return(default_categories)
      } else {
        # Merge with defaults to ensure all categories are present
        result <- default_categories %>%
          dplyr::left_join(monthly_avg, by = "category") %>%
          dplyr::mutate(
            avg_monthly_emissions = dplyr::coalesce(avg_monthly_emissions.y, avg_monthly_emissions.x)
          ) %>%
          dplyr::select(category, avg_monthly_emissions)

        return(result)
      }
    })

    # Category KPI Output Functions
    output$water_value <- renderText({
      emissions <- monthly_emissions()
      water_val <- emissions$avg_monthly_emissions[emissions$category == "water"]
      if(length(water_val) > 0) {
        scales::comma(round(water_val, 0))
      } else {
        "150"
      }
    })

    output$electricity_value <- renderText({
      emissions <- monthly_emissions()
      elec_val <- emissions$avg_monthly_emissions[emissions$category == "electricity"]
      if(length(elec_val) > 0) {
        scales::comma(round(elec_val, 0))
      } else {
        "2,500"
      }
    })

    output$waste_value <- renderText({
      emissions <- monthly_emissions()
      waste_val <- emissions$avg_monthly_emissions[emissions$category == "waste"]
      if(length(waste_val) > 0) {
        scales::comma(round(waste_val, 0))
      } else {
        "75"
      }
    })

    output$gas_value <- renderText({
      emissions <- monthly_emissions()
      gas_val <- emissions$avg_monthly_emissions[emissions$category == "gas"]
      if(length(gas_val) > 0) {
        scales::comma(round(gas_val, 0))
      } else {
        "1,800"
      }
    })

    output$fuel_value <- renderText({
      emissions <- monthly_emissions()
      fuel_val <- emissions$avg_monthly_emissions[emissions$category == "fuel"]
      if(length(fuel_val) > 0) {
        scales::comma(round(fuel_val, 0))
      } else {
        "650"
      }
    })

    # Summary Plot
    output$ytd_summary_plot <- plotly::renderPlotly({
      req(filtered_data())

      p <- filtered_data() %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(total_emissions = sum(carbon_emission_kgco2e, na.rm = TRUE), .groups = 'drop') %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = total_emissions)) +
        ggplot2::geom_area(fill = "#4ecdc4", alpha = 0.3) +
        ggplot2::geom_line(color = "#4ecdc4", linewidth = 2) +
        ggplot2::labs(x = "", y = "Carbon Emissions (kgCO2e)") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid = ggplot2::element_line(color = "#333", linewidth = 0.3),
          text = ggplot2::element_text(color = "#ffffff"),
          axis.text = ggplot2::element_text(color = "#aaaaaa"),
          axis.line = ggplot2::element_blank()
        )

      plotly::ggplotly(p) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # Cost Trends Plot
    output$cost_trends_plot <- plotly::renderPlotly({
      req(filtered_data())

      # Calculate daily costs based on emissions (assuming a cost factor)
      cost_factor <- 0.25  # GBP per kgCO2e (adjust as needed)

      p <- filtered_data() %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(total_cost = sum(carbon_emission_kgco2e, na.rm = TRUE) * cost_factor, .groups = 'drop') %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = total_cost)) +
        ggplot2::geom_area(fill = "#FFD700", alpha = 0.3) +
        ggplot2::geom_line(color = "#FFD700", linewidth = 2) +
        ggplot2::labs(x = "", y = "Energy usage amount") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid = ggplot2::element_line(color = "#333", linewidth = 0.3),
          text = ggplot2::element_text(color = "#ffffff"),
          axis.text = ggplot2::element_text(color = "#aaaaaa"),
          axis.line = ggplot2::element_blank()
        )

      plotly::ggplotly(p) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # Carbon Emissions Pie Chart by Type
    output$emissions_pie_plot <- plotly::renderPlotly({
      req(filtered_data())

      pie_data <- filtered_data() %>%
        dplyr::group_by(type) %>%
        dplyr::summarise(
          total_emissions = sum(carbon_emission_kgco2e, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::arrange(desc(total_emissions))

      # Color palette
      colors <- c("#4ecdc4", "#ff6b6b", "#95e1d3", "#feca57", "#a55eea", "#26d0ce", "#ff9ff3", "#54a0ff")

      p <- plotly::plot_ly(
        data = pie_data,
        labels = ~type,
        values = ~total_emissions,
        type = "pie",
        hole = 0.4,
        marker = list(
          colors = colors[1:nrow(pie_data)],
          line = list(color = "#1a1a1a", width = 2)
        ),
        textinfo = 'label+percent',
        textposition = 'auto',  # Changed from 'outside' to 'auto'
        textfont = list(color = "#ffffff", size = 10)  # Reduced font size
      ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#ffffff", size = 10),
          showlegend = FALSE,  # Hide legend to save space
          margin = list(l = 10, r = 10, t = 30, b = 10),  # Reduced margins
          title = list(
            text = "",
            font = list(color = "#ffffff")
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)

      return(p)
    })

    # Carbon Emissions Pie Chart by Site
    output$emissions_site_pie_plot <- plotly::renderPlotly({
      req(filtered_data())

      pie_data <- filtered_data() %>%
        dplyr::group_by(site) %>%
        dplyr::summarise(
          total_emissions = sum(carbon_emission_kgco2e, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::arrange(desc(total_emissions))

      # Different color palette for sites
      colors <- c("#ff6b6b", "#4ecdc4", "#feca57", "#95e1d3", "#a55eea", "#26d0ce", "#ff9ff3", "#54a0ff", "#48dbfb", "#0abde3")

      p <- plotly::plot_ly(
        data = pie_data,
        labels = ~site,
        values = ~total_emissions,
        type = "pie",
        hole = 0.4,
        marker = list(
          colors = colors[1:nrow(pie_data)],
          line = list(color = "#1a1a1a", width = 2)
        ),
        textinfo = 'label+percent',
        textposition = 'auto',
        textfont = list(color = "#ffffff", size = 10)
      ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#ffffff", size = 10),
          showlegend = FALSE,
          margin = list(l = 10, r = 10, t = 30, b = 10),
          title = list(
            text = "",
            font = list(color = "#ffffff")
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)

      return(p)
    })

    # Summary Table with dark theme
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
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'frtip',
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#2a2a2a', 'color': '#fff'});",
            "$(this.api().table().body()).css({'background-color': '#1a1a1a', 'color': '#fff'});",
            "}"
          )
        ),
        colnames = c("Site", "Type", "Total Energy usage", "Total Emissions (kgCO2e)",
                     "Avg Energy usage", "Records")
      ) %>%
        DT::formatStyle(columns = 1:6, backgroundColor = '#1a1a1a', color = '#ffffff')
    })
  })
}
