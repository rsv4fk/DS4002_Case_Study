# Import necessary packages
library(forecast)
library(tidyverse)

# Import and format data
data <- read.csv("C:\\Users\\ravza\\Downloads\\data.csv")
data <- as.data.frame(data[-c(1, 2, 3, 4),])
datarows <- 1850:2022
rownames(data) <- datarows

# Convert temperature anomalies to numeric
data$Anomaly <- 0
for(x in 1:173){
  data[x,2] <- as.numeric(data[x,1])
}

# Add a column representing the true temperature value
data$Temp <- 12.9  # Use a base value of 12.9 degrees C based on the 1901-2000 long-term average
for(x in 1:173){
  data[x,3] <- data[x,2]+12.9 
}

# Add a column for the year
data$Year <- 1850:2022

# METHOD 1: PREDICT USING A REGRESSION
# Create a data frame with prediction years 2020, 2021, 2022
prediction_data <- data.frame(Year = c(2020, 2021, 2022))

# Fit the linear model with Year predicting Temp
mod1 <- lm(Temp~Year, data = data)
summary(mod1) # The "Year" variable is a significant predictor of temperature with a p-val < 2e-16. R-sq value = 0.6176, so 61.76% of the variation in Temp is explained by Year. This means our model is somewhat accurate in predicting Temp

# Predict the future values for 2020, 2021, and 2022
predict(mod1, newdata = prediction_data)

# Method 1 returns 2020, 2021, and 2022 values of 13.41571, 13.42131, and 13.42691 
# The true values for those years are 13.95, 13.85, and 13.65


# METHOD 2: PREDICT USING THE ARIMA  MODEL

# Create the dataset to be used for training; this dataset excludes the 2020, 2021, and 2022 values
training_data <- data[-c(171:173),]

# Forecast model for 2020-2022 using the arima model
fit <- auto.arima(as.data.frame(training_data[,3]))
forecastedValues <- forecast(fit, 3)
forecastedValues

# Plot forecasted values for 2020, 2021, and 2022
plot(forecastedValues)

# Method 2 returns 2020, 2021, and 2022 values of 13.82643, 13.83257, and 13.83871
# The true values for those years are 13.95, 13.85, and 13.65
# These values are captured in the 95% confidence intervals produced by the arima model
# Confidence interval for 2020: 13.55450 14.09837
# Confidence interval for 2021: 13.54722 14.11792
# Confidence interval for 2022: 13.54055 14.13687

# Thus, Method 2 is more accurate, so we will use the arima model to predict the temperature values for 2023-2033

# Forecast model for 2023-2033 using the arima model
fit2 <- auto.arima(as.data.frame(data[,3]))
forecastedValues2 <- forecast(fit2, 10)

# Plot forecasted values for 2023-2033
plot(forecastedValues2)



