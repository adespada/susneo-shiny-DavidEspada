test_that("Helper functions work correctly", {
  # Create test analyzer and load data
  analyzer <- sustainability_analyzer$new()

  test_data <- data.frame(
    id = 1:4,
    site = c("Site1", "Site2", "Site1", "Site3"),
    date = c("01/01/2024", "02/01/2024", "03/01/2024", "04/01/2024"),
    type = c("Gas", "Electricity", "Gas", "Water"),
    value = c(100, 200, 150, 250),
    carbon_emission_kgco2e = c(10, 20, 15, 25)
  )

  analyzer$load_data(test_data)

  # Test get_sites function
  sites <- analyzer$get_sites()
  expect_length(sites, 3)
  expect_true(all(c("Site1", "Site2", "Site3") %in% sites))

  # Test get_date_range function
  date_range <- analyzer$get_date_range()
  expect_true("min" %in% names(date_range))
  expect_true("max" %in% names(date_range))

  # Test filter_data function
  filtered <- analyzer$filter_data(selected_sites = c("Site1", "Site2"))
  expect_equal(nrow(filtered), 3)
  expect_true(all(filtered$site %in% c("Site1", "Site2")))
})
