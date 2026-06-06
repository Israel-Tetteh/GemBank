library(testthat)
library(GemBank)

test_that("Database initialization and germplasm registration works", {
  # Use a temporary file for testing
  test_db <- tempfile(fileext = ".sqlite")
  init_db(test_db)
  con <- DBI::dbConnect(RSQLite::SQLite(), test_db)
  on.exit(DBI::dbDisconnect(con))
  
  # Test registration
  expect_true(add_germplasm(con, "TEST-001", "Pedigree A", "Sorghum"))
  
  # Test uniqueness constraint
  expect_error(add_germplasm(con, "TEST-001", "Pedigree B", "Sorghum"))
  
  # Verify data
  res <- get_inventory_status(con)
  expect_equal(nrow(res), 1)
  expect_equal(res$accession_name, "TEST-001")
})

test_that("Inventory transactions work correctly", {
  test_db <- tempfile(fileext = ".sqlite")
  init_db(test_db)
  con <- DBI::dbConnect(RSQLite::SQLite(), test_db)
  on.exit(DBI::dbDisconnect(con))
  
  add_germplasm(con, "SEED-X", "Cross 1", "Sorghum")
  
  # Test deposit
  add_inventory_deposit(con, "SEED-X", 1000, "Freezer 1", "Tester", "Initial Stock")
  status <- get_inventory_status(con)
  expect_equal(status$quantity, 1000)
  
  # Test withdrawal
  withdraw_seed(con, "SEED-X", 200, "Tester", "Trial use")
  status <- get_inventory_status(con)
  expect_equal(status$quantity, 800)
  
  # Test overdraft prevention
  expect_error(withdraw_seed(con, "SEED-X", 1000, "Tester", "Too much"), "Insufficient stock")
})

test_that("Trial and Observation linkage works", {
  test_db <- tempfile(fileext = ".sqlite")
  init_db(test_db)
  con <- DBI::dbConnect(RSQLite::SQLite(), test_db)
  on.exit(DBI::dbDisconnect(con))
  
  add_germplasm(con, "ACC-1", "P1", "S")
  add_trial(con, "Trial-2026", "Loc A", "2026-01-01")
  add_trait(con, "Yield", "kg")
  
  # Test plot creation
  expect_silent(add_plot(con, "Trial-2026", "ACC-1", 101, 1))
  
  # Test observation recording
  expect_silent(add_observation(con, "Trial-2026", 101, "Yield", 55.5, "Tester"))
  
  # Verify wide format retrieval
  wide_df <- get_trial_data_wide(con, "Trial-2026")
  expect_equal(nrow(wide_df), 1)
  expect_equal(wide_df$Yield, 55.5)
})
