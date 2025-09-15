# SusNeo Shiny - Sustainability Analytics Dashboard

A modern, dark-themed Shiny application for sustainability data analysis and carbon emissions tracking. Built with R6 classes and modular architecture using the `{golem}` framework.

## Overview

SusNeo Shiny provides an intuitive dashboard for analyzing sustainability metrics across different facilities, tracking carbon emissions, energy consumption, and environmental impact indicators. The application features interactive visualizations, KPI monitoring, and comprehensive data filtering capabilities.

## Features

### Dashboard Analytics
- **Company-wide KPIs**: Track total and average energy usage and carbon emissions
- **Category Breakdown**: Monitor sustainability metrics by type (Water, Electricity, Waste, Gas, Fuel)
- **Interactive Trends**: Visual analysis of carbon emissions and energy usage over time
- **Distribution Charts**: Pie charts showing emissions by type and facility location
- **Detailed Analytics**: Comprehensive data table with filtering and search capabilities

### Data Management
- **Flexible Data Input**: Upload custom CSV files or use pre-loaded sample data
- **Data Validation**: Automatic validation of required columns and data formats
- **Real-time Filtering**: Dynamic filtering by date range and facility selection
- **Data Preview**: Interactive table preview with pagination and sorting

### User Experience
- **Modern Dark Theme**: Professional dark UI optimized for data visualization
- **Responsive Design**: Works seamlessly across desktop and mobile devices
- **Interactive Controls**: Custom-styled date pickers, dropdowns, and selection tools
- **Real-time Updates**: Automatic chart and metric updates based on filter selections

## Installation

### Prerequisites
- R (>= 4.0.0)
- RStudio (recommended)

### Install Dependencies
```r
# Install required packages
install.packages(c(
  "shiny", "golem", "R6", "dplyr", "ggplot2", 
  "plotly", "DT", "readr", "lubridate", "scales", 
  "htmlwidgets"
))
```

### Install Development Version
```r
# Install from GitHub (when available)
# devtools::install_github("yourusername/susneo-shiny")

# Or clone and install locally
devtools::install_local("path/to/susneo-shiny")
```

## Usage

### Launch Application
```r
# Load and run the application
pkgload::load_all()
susneoshiny::run_app()
```

### Data Requirements
Your CSV data should contain these required columns:
- `id`: Unique identifier for each record
- `site`: Facility or location name
- `date`: Date in DD/MM/YYYY format
- `type`: Type of sustainability metric (e.g., "Electricity", "Water", "Gas")
- `value`: Numeric value of consumption/usage
- `carbon_emission_kgco2e`: Carbon emissions in kgCO2e

### Sample Data Format
```csv
id,site,date,type,value,carbon_emission_kgco2e
1,London Office,01/01/2024,Electricity,1250,425.50
2,Manchester Plant,01/01/2024,Gas,850,198.75
3,Birmingham Warehouse,01/01/2024,Water,450,12.30
```

## Application Structure

### Core Components

#### R6 Class: `sustainability_analyzer`
The backbone of data processing and analysis:
```r
analyzer <- sustainability_analyzer$new()
analyzer$load_data(your_data)
filtered_data <- analyzer$filter_data(start_date, end_date, selected_sites)
kpis <- analyzer$calculate_kpis(filtered_data)
```

#### Modules
- **`mod_data_upload`**: Handles data import and validation
- **`mod_dashboard`**: Main analytics dashboard with visualizations

### Key Functions
- `load_data()`: Load and validate sustainability data
- `filter_data()`: Apply date and facility filters
- `calculate_kpis()`: Compute key performance indicators
- `get_sites()`: Retrieve available facility locations
- `get_date_range()`: Get data date boundaries

## Development

### Project Structure
```
susneo-shiny/
├── R/
│   ├── app_config.R          # Application configuration
│   ├── app_ui.R              # Main UI definition
│   ├── app_server.R          # Main server logic
│   ├── mod_data_upload.R     # Data upload module
│   ├── mod_dashboard.R       # Dashboard module
│   ├── class_sustainability_analyzer.R      # R6 sustainability analyzer class
│   └── run_app.R             # App launcher
├── inst/
│   ├── app/www/              # Static assets (CSS, JS)
│   ├── extdata/             # Static assets (CSS, JS)
│   └── golem-config.yml      # Golem configuration
├── data/
│   └── SAMPLE_ASSIGNMENT_DATA.csv  # Sample dataset
├── tests/
└── DESCRIPTION
└── app.R
```

### Key Technologies
- **Framework**: Shiny with Golem structure
- **Styling**: Custom CSS with dark theme
- **Charts**: Plotly for interactive visualizations
- **Tables**: DT for data display
- **Data Processing**: dplyr, R6 classes

### Custom Styling
The application uses a modern dark theme with:
- Dark backgrounds (#1a1a1a, #2a2a2a)
- Cyan accent color (#4ecdc4)
- Responsive card layouts
- Custom styled form controls

## Deployment

### Local Development
```r
# Run in development mode
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = FALSE)
susneoshiny::run_app()
```


## Configuration

### Environment Variables
- `GOLEM_CONFIG_ACTIVE`: Set deployment environment (default, production, dev)
- `R_CONFIG_ACTIVE`: R configuration environment

### Customization
Modify `inst/golem-config.yml` for:
- Application settings
- Database connections
- External API configurations

## Troubleshooting

### Common Issues

**Data Upload Errors**
- Ensure CSV contains all required columns
- Check date format (DD/MM/YYYY)
- Verify numeric columns contain valid numbers

**Visualization Issues**
- Clear browser cache if charts don't load
- Ensure plotly package is installed
- Check browser console for JavaScript errors

**Performance Issues**
- Large datasets (>10,000 rows) may cause slower rendering
- Consider data aggregation for better performance
- Use date filtering to reduce data scope

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines
- Use R6 classes for data processing logic
- Follow golem module patterns
- Write comprehensive tests
- Maintain consistent code style
- Document new functions with roxygen2


## Acknowledgments

- Built with the [Golem](https://thinkr-open.github.io/golem/) framework
- Uses [Plotly](https://plotly.com/r/) for interactive visualizations
- Inspired by modern dashboard design principles

## Support

For support and questions:
- Open an issue on GitHub
- Contact the development team
- Review the documentation and examples

---

**Version**: 0.0.0.9000  
**Last Updated**: September 2025  
**Built with**: R, Shiny, Golem, R6, Plotly
