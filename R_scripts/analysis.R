detect_anomalies <- function(df) {
  library(dplyr)
  
  mean_temp <- mean(df$temp_avg, na.rm = TRUE)
  sd_temp <- sd(df$temp_avg, na.rm = TRUE)
  
  df <- df %>%
    mutate(
      temp_anomaly = abs(temp_avg - mean_temp) > 2 * sd_temp,
      heavy_rain = rainfall > quantile(rainfall, 0.95, na.rm = TRUE)
    )
  
  return(df)
}