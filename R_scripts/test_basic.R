library(testthat)

test_that("basic workflow works", {
  df <- fetch_temperature_data(22.3, 70.8, "2023-01-01", "2023-01-10")
  df_clean <- clean_climate_data(df)
  df_anomaly <- detect_anomalies(df_clean)
  
  expect_true(nrow(df_anomaly) > 0)
})