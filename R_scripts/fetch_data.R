fetch_climate_data <- function(latitude, longitude, start_date, end_date) {
  library(httr)
  library(jsonlite)
  
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", latitude,
    "&longitude=", longitude,
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum",
    "&timezone=auto"
  )
  
  response <- GET(url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Check if precipitation_sum exists
  if (is.null(data$daily$precipitation_sum)) {
    data$daily$precipitation_sum <- rep(0, length(data$daily$time))
  }
  
  df <- data.frame(
    date = data$daily$time,
    temp_max = data$daily$temperature_2m_max,
    temp_min = data$daily$temperature_2m_min,
    rainfall = data$daily$precipitation_sum
  )
  
  return(df)
}