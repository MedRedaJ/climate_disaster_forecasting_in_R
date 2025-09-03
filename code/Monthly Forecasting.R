# Libraries
library(fpp3)
library(zoo)
library(readxl)

# Load Data
US_Disaster_Data_R <- read_excel("C:\\EOR\\uOttawa courses\\Business Forecasting Analytics\\US Disaster Data.xlsx", sheet = "Cleaned Data")

############################################ Data Preprocess ############################################







# Prepare data with disaster count and CPI-adjusted cost
us_disaster <- US_Disaster_Data_R %>% 
  select(Name, Disaster, `End Date`, `CPI-Adjusted Cost (Millions)`)

us_disaster <- us_disaster %>%
  mutate(count = 1)

us_disaster_agg <- us_disaster %>%
  group_by(`End Date`) %>%
  summarize(adj_cost = sum(`CPI-Adjusted Cost (Millions)`), 
            disaster_count = sum(count))

us_disaster_agg <- us_disaster_agg %>%
  mutate(year = year(`End Date`))

us_disaster_agg <- us_disaster_agg %>%
  group_by(year) %>%
  summarize(yr_adj_cost = sum(adj_cost), 
            yr_disaster_count = sum(disaster_count))

us_disaster_ts <- us_disaster_agg %>% 
  as_tsibble(index = year)

us_disaster_ts <- us_disaster_ts %>%
  tsibble::fill_gaps() %>%
  mutate(yr_adj_cost = if_else(is.na(yr_adj_cost), 0, yr_adj_cost),
         yr_disaster_count = if_else(is.na(yr_disaster_count), 0, yr_disaster_count))

# Training-testing split
# Training data: 1980-2009
training_data <- us_disaster_ts %>%
  filter(year >= 1980 & year <= 2009)

# Testing data: 2010-2023 (explicitly excludes 2024+)
testing_data <- us_disaster_ts %>%
  filter(year >= 2010 & year <= 2023)


autoplot(us_disaster_ts, yr_adj_cost)  #no autocorrelation, no arima, no ets, auxiliary information(i dont have these)

acf(us_disaster_ts) # not a lot of autocorrelation

us_disaster_ts %>% gg_tsdisplay(yr_adj_cost, plot_type="partial")

us_disaster_ts %>% gg_tsdisplay(yr_disaster_count, plot_type="partial")

autoplot(us_disaster_ts, yr_disaster_count)



############################################ Model Fitting and Evaluation ############################################

### ARIMA ###
# Fit ARIMA on training data
arima_model <- training_data %>%
  model(ARIMA(yr_disaster_count))

# Summary of the fitted ARIMA model
report(arima_model)

# Diagnostic plots
gg_tsresiduals(arima_model)

# Forecast for the testing period
arima_forecast <- arima_model %>%
  forecast(new_data = testing_data)

# Plot forecast with historical data
autoplot(arima_forecast, us_disaster_ts) +
  labs(title = "Forecast of Disaster Counts (ARIMA)",
       y = "Number of Disasters",
       x = "Year")

### TSLM ###
# Fit a TSLM model on training data
tslm_model <- training_data %>%
  model(TSLM(yr_disaster_count ~ trend()))

# Forecast for the testing period
tslm_forecast <- tslm_model %>%
  forecast(new_data = testing_data)

# Plot TSLM forecast
autoplot(us_disaster_ts, yr_disaster_count) +
  autolayer(tslm_forecast, level = c(80, 95), color = "red") +
  labs(title = "TSLM Forecast of Disaster Counts",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal()

### ETS ###
# Fit ETS on training data
ets_model <- training_data %>%
  model(ETS(yr_disaster_count))

# Forecast for the testing period
ets_forecast <- ets_model %>%
  forecast(new_data = testing_data)

# Plot ETS forecast
autoplot(us_disaster_ts, yr_disaster_count) +
  autolayer(ets_forecast, level = c(80, 95), color = "blue") +
  labs(title = "ETS Forecast of Disaster Counts",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal()

### Comparing All Models ###
# Fit multiple models on training data
models <- training_data %>%
  model(
    ARIMA = ARIMA(yr_disaster_count),
    TSLM = TSLM(yr_disaster_count ~ trend()),
    Naive = NAIVE(yr_disaster_count),
    Mean = MEAN(yr_disaster_count),
    ETS = ETS(yr_disaster_count)
  )

# Generate forecasts for testing period
all_forecasts <- models %>%
  forecast(new_data = testing_data)

# Plot historical data, fitted values, and forecasts for all models
autoplot(us_disaster_ts, yr_disaster_count) +
  autolayer(all_forecasts, aes(color = .model), level = c(80, 95)) +
  labs(
    title = "Forecast Comparison Across Models",
    x = "Year",
    y = "Number of Disasters",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Accuracy evaluation for all models on testing data
model_accuracy <- all_forecasts %>%
  accuracy(testing_data)

# Print accuracy table
print(model_accuracy)





# Plot ARIMA forecast
autoplot(training_data, yr_disaster_count) +
  autolayer(all_forecasts %>% filter(.model == "ARIMA"), aes(y = .mean), color = "red") +
  labs(title = "ARIMA Model Forecast",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal()

# Plot TSLM forecast
autoplot(training_data, yr_disaster_count) +
  autolayer(all_forecasts %>% filter(.model == "TSLM"), aes(y = .mean), color = "blue") +
  labs(title = "TSLM Model Forecast",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal()

# Plot Naive forecast
autoplot(training_data, yr_disaster_count) +
  autolayer(all_forecasts %>% filter(.model == "Naive"), aes(y = .mean), color = "green") +
  labs(title = "Naive Model Forecast",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal()

# Plot Mean forecast
autoplot(training_data, yr_disaster_count) +
  autolayer(all_forecasts %>% filter(.model == "Mean"), aes(y = .mean), color = "purple") +
  labs(title = "Mean Model Forecast",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal()

# Plot ETS forecast
autoplot(training_data, yr_disaster_count) +
  autolayer(all_forecasts %>% filter(.model == "ETS"), aes(y = .mean), color = "orange") +
  labs(title = "ETS Model Forecast",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal()






###ARIMA###

disaster_arima <- training_data %>%
  model(ARIMA(yr_disaster_count))

# Summary of the fitted ARIMA model
report(disaster_arima)

# Diagnostic plots
gg_tsresiduals(disaster_arima)

# Forecast for the next 10 years
disaster_forecast <- disaster_arima %>%
  forecast(h = 10)

# View forecast
print(disaster_forecast)


# Plot forecast
autoplot(disaster_forecast, us_disaster_ts) +
  labs(title = "Forecast of Disaster Counts",
       y = "Number of Disasters",
       x = "Year")



# Add fitted values for the historical period
fitted_data <- augment(disaster_arima)

# Plot the fitted values and forecast together
autoplot(us_disaster_ts %>%
           filter(year >= 1980 & year <= 2009), yr_disaster_count) +
  autolayer(fitted_data, .fitted, color = "blue", linetype = "dashed", size = 1, alpha = 0.8) +
  autolayer(disaster_forecast, yr_disaster_count, color = "red") +
  labs(title = "Disaster Count: Fitted and Forecast",
       y = "Number of Disasters",
       x = "Year") +
  theme_minimal()



###TSLM###

# Assume `us_disaster_ts` is the prepared tsibble with columns `year` and `yr_disaster_count`.

# Fit a TSLM model with a trend and seasonality (if applicable)
disaster_tslm <- training_data %>%
  model(tslm_model = TSLM(yr_disaster_count ~ trend()))

# Generate forecasts for the next 10 years
disaster_forecast <- disaster_tslm %>%
  forecast(h = 10)


# Diagnostic plots
gg_tsresiduals(disaster_tslm)

# Plot historical data, fitted values, and forecast
disaster_tslm %>%
  augment() %>% # Extract fitted values
  ggplot(aes(x = year)) +
  geom_line(aes(y = yr_disaster_count), color = "black", size = 1) + # Actual historical data
  geom_line(aes(y = .fitted), color = "blue", size = 1, linetype = "dashed") + # Fitted values
  autolayer(disaster_forecast, level = c(80, 95), color = "red") + # Forecast with confidence intervals
  labs(title = "TSLM Forecast of Disaster Counts",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Legend"))


###ETS MODEL###
# Fit the ETS model
ets_model <- training_data %>%
  model(ETS = ETS(yr_disaster_count))

# Generate forecast for the ETS model
ets_forecast <- ets_model %>%
  forecast(h = 10)

# Extract fitted values for the ETS model
ets_fitted <- ets_model %>%
  augment()

#plot residuals
gg_tsresiduals(ets_model)


# Plot the historical data, fitted values, and forecast
ggplot() +
  # Historical data
  geom_line(data = us_disaster_ts %>%
              filter(year >= 1980 & year <= 2009), aes(x = year, y = yr_disaster_count), color = "black", size = 1) +
  # Fitted values
  geom_line(data = ets_fitted, aes(x = year, y = .fitted), color = "blue", linetype = "dashed", size = 1) +
  # Forecast
  autolayer(ets_forecast, aes(x = year, y = .mean), color = "red", size = 1) +
  labs(
    title = "Disaster Count: Fitted and Forecast (ETS Model)",
    y = "Number of Disasters",
    x = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

