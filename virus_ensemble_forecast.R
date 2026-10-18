library(zeallot)
library(tidyverse)
library(forecast)

# "COVID-19 Hospital Data (ARCHIVED)" dataset
url <- "https://data.chhs.ca.gov/dataset/2df3e19e-9ee4-42a6-a087-9761f82033f6/resource/47af979d-8685-4981-bced-96a6b79d3ed5/download/covid19hospitalbycounty.csv"

data_ingestion <- function(url) {
  # Pull raw data directly from url
  raw_df <- read_csv(url, show_col_types = FALSE)
  
  # Clean the raw data
  clean_df <- raw_df |>
    filter(county == "Sacramento") |>
    mutate(Date = as.Date(todays_date, format = "%m/%d/%Y"),
           Confirmed_Covid = hospitalized_covid_confirmed_patients) |>
    select(Date, Confirmed_Covid) |>
    arrange(Date)
  
  return(clean_df)
}
raw <- data_ingestion(url)

convert_to_time_series <- function(dataframe) {
  time_series <- ts(dataframe$Confirmed_Covid, frequency = 7) # frequency = 7 for daily data
  return(time_series)
}

# Making the ensemble forecast
median_forecast <- function(time_series, days_in_advance) {
  message("Fitting ARIMA.")
  fit_arima <- auto.arima(time_series)
  message("Fitting NNETAR.")
  fit_nnetar <- nnetar(time_series)
  message("Making forecasts.")
  arima_forecast <- forecast(fit_arima, h=days_in_advance)$mean
  nnetar_forecast <- forecast(fit_nnetar, h=days_in_advance)$mean
  
  ensemble <- apply(cbind(as.numeric(arima_forecast), as.numeric(nnetar_forecast)), 1, median)
  message("Forecasting completed.")
  return(list(ensemble, arima_forecast, nnetar_forecast))
}

# Running the full pipeline
c(my_forecast, arima_forecast, nnetar_forecast) %<-% median_forecast(convert_to_time_series(raw), 30)
print(my_forecast)

# Plotting
ensemble_plots <- function(ensemble, arima_forecast, nnetar_forecast, clean_df) {
  # Extract historical data (e.g., last 90 days for visual clarity)
  history <- tail(clean_df, 90)
  
  # Generate future date sequence matching the forecast horizon
  forecast_horizon <- length(ensemble)
  last_date <- max(history$Date)
  future_dates <- seq.Date(last_date + 1, by = "day", length.out = forecast_horizon)
  
  all_values <- c(as.numeric(arima_forecast), as.numeric(nnetar_forecast), as.numeric(ensemble))
  ymin <- min(all_values, na.rm = TRUE) * 0.95
  ymax <- max(all_values, na.rm = TRUE) * 2.2
  
  # Create base plot using historical dates on the X-axis
  plot(
    history$Date, history$Confirmed_Covid, 
    type = "l", 
    col = "black", 
    lwd = 1.5,
    xlim = c(min(history$Date), max(future_dates)),
    ylim = c(ymin, ymax),
    xaxt = "n", # Suppress default axis so we can format dates nicely
    xlab = "Date", 
    ylab = "Confirmed Hospitalized Covid Patients",
    main = "Greater Sierra-Sacramento: Ensemble Forecast"
  )
  axis.Date(1, at = seq(min(history$Date), max(future_dates), by = "2 weeks"), format = "%b %d, %Y")
  
  lines(future_dates, arima_forecast, col = "black", lty = 2, lwd = 1.5)
  lines(future_dates, nnetar_forecast, col = "red", lty = 2, lwd = 1.5)
  lines(future_dates, ensemble, col = "blue", lwd = 2)
  legend("topleft", legend=c("ARIMA", "NNETAR", "Median Ensemble"), 
         col=c("black", "red", "blue"), lty=c(1,2,1), lwd=c(1,1,2))
}
ensemble_plots(my_forecast, arima_forecast, nnetar_forecast, raw)
# ARIMA provides a smooth baseline while NNETAR is able to capture more dynamic, non-linear movements 
# The median allows us to capture a midpoint between them, avoiding the extremities of both models
