test_that("Data validation works correctly", {
  # Create test analyzer
  analyzer <- sustainability_analyzer$new()

  # Test valid data
  valid_data <- data.frame(
    id = 1:3,
    site = c("A", "B", "C"),
    date = c("01/01/2024", "02/01/2024", "03/01/2024"),
    type = c("Gas", "Electricity", "Water"),
    value = c(100, 200, 300),
    carbon_emission_kgco2e = c(10, 20, 30)
  )

  expect_true(analyzer$validate_data(valid_data))
  expect_true(analyzer$load_data(valid_data))

  # Test invalid data - missing columns
  invalid_data <- data.frame(
    id = 1:3,
    site = c("A", "B", "C")
  )

  expect_false(analyzer$validate_data(invalid_data))

  # Test empty data
  empty_data <- data.frame()
  expect_false(analyzer$validate_data(empty_data))
})
