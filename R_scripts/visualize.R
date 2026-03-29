plot_climate_trend <- function(df) {
  library(ggplot2)
  
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = temp_avg, color = "Temperature")) +
    
    # 🌧️ Rainfall bars
    geom_bar(aes(y = rainfall), stat = "identity", alpha = 0.3, fill = "blue") +
    
    # 🔴 Temperature anomalies (ADD THIS HERE)
    geom_point(
      data = subset(df, temp_anomaly == TRUE),
      aes(y = temp_avg),
      color = "red",
      size = 2
    ) +
    
    labs(
      title = "Climate Trend (Temperature & Rainfall)",
      x = "Date",
      y = "Value",
      color = "Legend"
    ) +
    theme_minimal()
}