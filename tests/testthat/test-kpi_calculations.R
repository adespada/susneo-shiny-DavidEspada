test_that("KPI calculations work correctly", {
  # Create test analyzer and load data
  analyzer <- sustainability_analyzer$new()
  test_data <- data.frame(
    id = 1:4,
    site = c("A", "A", "B", "B"),
    date = c("01/01/2024", "02/01/2024", "01/01/2024", "02/01/2024"),
    type = c("Gas", "Electricity", "Gas", "Electricity"),
    value = c(100, 200, 150, 250),
    carbon_emission_kgco2e = c(10, 20, 15, 25)
  )
  analyzer$load_data(test_data)

  # Test KPI calculations
  kpis <- analyzer$calculate_kpis()

  # Original KPIs
  expect_equal(kpis$total_consumption, 700)
  expect_equal(kpis$total_emissions, 70)
  expect_equal(kpis$avg_consumption, 175)
  expect_equal(kpis$unique_sites, 2)

  # New company-wide KPIs
  expect_equal(kpis$total_energy, 700)  # Should equal total_consumption
  expect_equal(kpis$avg_energy, 175)    # Should equal avg_consumption
  expect_equal(kpis$avg_emissions, 17.5) # Average of carbon emissions: (10+20+15+25)/4 = 17.5

  # Test with empty data
  empty_kpis <- analyzer$calculate_kpis(data.frame())
  expect_equal(empty_kpis$total_consumption, 0)
  expect_equal(empty_kpis$total_emissions, 0)
  expect_equal(empty_kpis$avg_consumption, 0)
  expect_equal(empty_kpis$avg_emissions, 0)
  expect_equal(empty_kpis$unique_sites, 0)
  expect_equal(empty_kpis$total_energy, 0)
  expect_equal(empty_kpis$avg_energy, 0)

  # Test with NULL data
  null_kpis <- analyzer$calculate_kpis(NULL)
  expect_equal(null_kpis$total_consumption, 0)
  expect_equal(null_kpis$total_emissions, 0)
  expect_equal(null_kpis$avg_consumption, 0)
  expect_equal(null_kpis$avg_emissions, 0)
  expect_equal(null_kpis$unique_sites, 0)
  expect_equal(null_kpis$total_energy, 0)
  expect_equal(null_kpis$avg_energy, 0)

  # Test robustness with problematic data
  problematic_data <- data.frame(
    id = 1:3,
    site = c("A", "B", "C"),
    date = c("01/01/2024", "02/01/2024", "03/01/2024"),
    type = c("Gas", "Electricity", "Water"),
    value = c(100, NA, "invalid"),  # Mix of valid, NA, and invalid values
    carbon_emission_kgco2e = c(10, 20, NA)
  )
  analyzer$load_data(test_data)  # Reset to valid data first

  # Test with filtered data (subset)
  filtered_data <- test_data[test_data$site == "A", ]
  filtered_kpis <- analyzer$calculate_kpis(filtered_data)
  expect_equal(filtered_kpis$total_consumption, 300)  # 100 + 200
  expect_equal(filtered_kpis$total_emissions, 30)     # 10 + 20
  expect_equal(filtered_kpis$avg_consumption, 150)    # (100 + 200) / 2
  expect_equal(filtered_kpis$avg_emissions, 15)       # (10 + 20) / 2
  expect_equal(filtered_kpis$unique_sites, 1)         # Only site "A"
  expect_equal(filtered_kpis$total_energy, 300)
  expect_equal(filtered_kpis$avg_energy, 150)
})
