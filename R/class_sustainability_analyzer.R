#' sustainability_analyzer
#'
#' @description A class generator function
#'
#' @import R6
#' @import dplyr
#' @import lubridate
#' @export
sustainability_analyzer <- R6::R6Class(
  classname = 'sustainability_analyzer',
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
            date = lubridate::dmy(date),
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
    #' @description Calculate KPIs including company-wide metrics
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
          avg_emissions = 0,
          unique_sites = 0,
          total_energy = 0,
          avg_energy = 0
        ))
      }

      # Ensure numeric columns
      if (!"value" %in% names(filtered_data) || !"carbon_emission_kgco2e" %in% names(filtered_data)) {
        return(list(
          total_consumption = 0,
          total_emissions = 0,
          avg_consumption = 0,
          avg_emissions = 0,
          unique_sites = 0,
          total_energy = 0,
          avg_energy = 0
        ))
      }

      # Calculate all KPIs with robust error handling
      total_consumption <- tryCatch({
        sum(as.numeric(filtered_data$value), na.rm = TRUE)
      }, error = function(e) 0)

      total_emissions <- tryCatch({
        sum(as.numeric(filtered_data$carbon_emission_kgco2e), na.rm = TRUE)
      }, error = function(e) 0)

      avg_consumption <- tryCatch({
        mean(as.numeric(filtered_data$value), na.rm = TRUE)
      }, error = function(e) 0)

      avg_emissions <- tryCatch({
        mean(as.numeric(filtered_data$carbon_emission_kgco2e), na.rm = TRUE)
      }, error = function(e) 0)

      unique_sites <- tryCatch({
        length(unique(filtered_data$site))
      }, error = function(e) 0)

      # Handle NaN and Inf values
      if (is.nan(avg_consumption) || is.infinite(avg_consumption)) avg_consumption <- 0
      if (is.nan(avg_emissions) || is.infinite(avg_emissions)) avg_emissions <- 0

      list(
        total_consumption = total_consumption,
        total_emissions = total_emissions,
        avg_consumption = avg_consumption,
        avg_emissions = avg_emissions,
        unique_sites = unique_sites,
        total_energy = total_consumption,
        avg_energy = avg_consumption
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
