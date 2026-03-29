clean_climate_data <- function(df) {
  library(dplyr)
  library(lubridate)
  
  # Ensure rainfall column exists
  if (!"rainfall" %in% colnames(df)) {
    df$rainfall <- 0
  }
  
  df_clean <- df %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(temp_max) & !is.na(temp_min)) %>%
    mutate(
      temp_avg = (temp_max + temp_min) / 2,
      rainfall = ifelse(is.na(rainfall), 0, rainfall)
    )
  
  return(df_clean)
}