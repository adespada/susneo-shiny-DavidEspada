#' sustainability_analyzer
#'
#' @description A class generator function
#'
#' @noRd
sustainability_analyzer <- R6::R6Class(
  "SustainabilityAnalyzer",

  public = list(
    #' @field data Raw data storage
    data = NULL,

    #' @description Initialize the analyzer
    initialize = function() {
      self$data <- NULL
    },

    #' @description Load and validate data
    #' @param df Data frame with sustainability data
    load_data = function(df) {
      if (self$validate_data(df)) {
        self$data <- df %>%
          mutate(
            date = as.Date(date, format = "%d/%m/%Y"),
            value = as.numeric(value),
            carbon_emission_kgco2e = as.numeric(carbon_emission_kgco2e)
          )
        return(TRUE)
      }
      return(FALSE)
    },

    #' @description Validate data structure
    #' @param df Data frame to validate
    validate_data = function(df) {
      required_cols <- c("id", "site", "date", "type", "value", "carbon_emission_kgco2e")

      if (!all(required_cols %in% colnames(df))) {
        return(FALSE)
      }

      if (nrow(df) == 0) {
        return(FALSE)
      }

      return(TRUE)
    },

    #' @description Filter data by date range and sites
    #' @param start_date Start date for filtering
    #' @param end_date End date for filtering
    #' @param selected_sites Vector of selected sites
    filter_data = function(start_date = NULL, end_date = NULL, selected_sites = NULL) {
      if (is.null(self$data)) return(NULL)

      filtered_data <- self$data

      if (!is.null(start_date) && !is.null(end_date)) {
        filtered_data <- filtered_data %>%
          filter(date >= start_date, date <= end_date)
      }

      if (!is.null(selected_sites) && length(selected_sites) > 0) {
        filtered_data <- filtered_data %>%
          filter(site %in% selected_sites)
      }

      return(filtered_data)
    },

    #' @description Calculate KPIs
    #' @param filtered_data Filtered dataset
    calculate_kpis = function(filtered_data = NULL) {
      if (is.null(filtered_data)) {
        filtered_data <- self$data
      }

      if (is.null(filtered_data) || nrow(filtered_data) == 0) {
        return(list(
          total_consumption = 0,
          total_emissions = 0,
          avg_consumption = 0,
          unique_sites = 0
        ))
      }

      list(
        total_consumption = sum(filtered_data$value, na.rm = TRUE),
        total_emissions = sum(filtered_data$carbon_emission_kgco2e, na.rm = TRUE),
        avg_consumption = mean(filtered_data$value, na.rm = TRUE),
        unique_sites = length(unique(filtered_data$site))
      )
    },

    #' @description Get unique sites
    get_sites = function() {
      if (is.null(self$data)) return(character(0))
      unique(self$data$site)
    },

    #' @description Get date range
    get_date_range = function() {
      if (is.null(self$data)) return(list(min = Sys.Date(), max = Sys.Date()))
      list(
        min = min(self$data$date, na.rm = TRUE),
        max = max(self$data$date, na.rm = TRUE)
      )
    }
  )
)
