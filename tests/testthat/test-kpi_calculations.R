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

  expect_equal(kpis$total_consumption, 700)
  expect_equal(kpis$total_emissions, 70)
  expect_equal(kpis$avg_consumption, 175)
  expect_equal(kpis$unique_sites, 2)

  # Test with empty data
  empty_kpis <- analyzer$calculate_kpis(data.frame())
  expect_equal(empty_kpis$total_consumption, 0)
  expect_equal(empty_kpis$total_emissions, 0)
  expect_equal(empty_kpis$avg_consumption, 0)
  expect_equal(empty_kpis$unique_sites, 0)
})
