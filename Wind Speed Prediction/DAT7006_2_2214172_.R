
# title: "Data Science Assessment 2"
# author: "Tolulope Abe"
# ID: "2214172"
# Session: "Monday 10am"
# Instructor: "Dr Pradeep"
# Date: "2023-05-15"
# output: pdf_document

# Get and set the working directory into R environment
getwd()
setwd <- ("C:\\Users\\Tolulope Abe\\Desktop\\Data Science_Assignment 2_Tolulope Abe\\Wind Speed Prediction")

# Install and load the necessary libraries
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("imputeTS")
install.packages("DescTools")
install.packages("ggpubr")
install.packages("randomForest")
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(imputeTS)
library(DescTools)
library(ggpubr)


# Read the data
weather_data <- read.csv("WRFdata_May2018.csv")
str(weather_data)
# Remove duplicate row
weather_data <- unique(weather_data)

# Remove any row with NA in Latitude and Longitude
weather_data <- weather_data[complete.cases(weather_data[,1:2]), ]

# Separate the headers
header <- weather_data[1,]

# Select 305 observations as sample size for the exploratory analysis
weather_sample <- weather_data[3743:4047,]

# combine the header variable and weather_sample dataframe
weather_sample <- rbind(header, weather_sample)

start <- 3
step <- 10
# Initialize a new dataframe - w_sample
w_sample <- data.frame(TSK=double(),
                       PSFC=double(),
                       U10=double(),
                       V10=double(),
                       Q2=double(),
                       RAINC=double(),
                       RAINNC=double(),
                       SNOW=double(),
                       TSLB=double(),
                       SMOIS=double(),
                       datetime=character(),
                       stringsAsFactors=FALSE)

# Initialize a new dataframe - w_location
w_location <- data.frame(lat=character(),
                         long=character(),
                         stringsAsFactors=FALSE)
# Initializes three variables to specify a range of rows to extract from the
# weather_sample dataframe.
start_row <- 1
stop_row <- nrow(weather_sample)-1
row_range <- stop_row-start_row

# Loop over columns in weather_sample data frame, skipping the first two
for (i in seq(from=start, to=ncol(weather_sample), by=step)) {
  # Get date from column header
  date <- colnames(weather_sample)[i]
  
  # Subset weather_sample data frame to columns for this date
  subset <- weather_sample[2:nrow(weather_sample),i:(i+step-1)]
  
  # Add date column
  subset$date <- date
  
  # Rename columns
  colnames(subset) <- c("TSK", "PSFC", "U10" ,"V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "datetime")
  
  # Add subset to w_sample data frame
  w_sample <- rbind(w_sample, subset)
  
  # Append subset of rows from weather_sample containing only the latitude and longitude information to the w_location dataframe
  w_location[start_row:stop_row,1] <- weather_sample[2:nrow(weather_sample),1]
  w_location[start_row:stop_row,2] <- weather_sample[2:nrow(weather_sample),2]
  start_row <- stop_row+1
  stop_row <- start_row + row_range
}

# Bind location and w_sample
w_sample <- cbind(w_location, w_sample)

# Print w_sample data frame
print(w_sample)
# Examine the data structure and summary statistics
# View the dataset
head(w_sample, n=10)
tail(w_sample, n=10)
# Check structure of the w_sample
str(w_sample)

# Check summary statistics of the output
summary(w_sample)

# Convert to numeric
for (col in c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS")) {
  w_sample[[col]] <- as.numeric(w_sample[[col]])
}

# Check structure again
str(w_sample)

# Data cleaning and preprocessing
# Missing values or NAs
# Check for any missing values
colSums(is.na(w_sample))
sum(is.na(w_sample))

# Sort the output dataframe by latitude and longitude.
w_sample <- arrange(w_sample, lat, long)

# Interpolate missing values using periods before and after
# Loop through each unique combination of lat and long
for (g1 in unique(w_sample$lat)) {
  for (g2 in unique(w_sample$long[w_sample$lat == g1])) {
    # Subset data frame to current combination
    sub_w_sample <- w_sample %>%
      filter(lat == g1 & long == g2)
    # Interpolate missing values in the column
    for (col in c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS")) {
      sub_w_sample[[col]] <- na_interpolation(sub_w_sample[[col]], option = "stine", maxgap = 3)
      # Replace original values with interpolated values in the original data frame
      w_sample[w_sample$lat == g1 & w_sample$long == g2, col] <- sub_w_sample[[col]]
    }
  }
}

# Re-check for any missing values
colSums(is.na(w_sample))
sum(is.na(w_sample))

library(lubridate)
# Convert datetime column datetime format
w_sample$datetime <- dmy_hm(gsub("^X", "", w_sample$datetime))
colSums(is.na(w_sample))
# Find maximum datetime without NA values
max_datetime <- max(w_sample$datetime[!is.na(w_sample$datetime)])
colSums(is.na(w_sample))
# Fill NA values with maximum datetime plus 3 hours
w_sample$datetime[is.na(w_sample$datetime)] <- max_datetime + hours(3)

colSums(is.na(w_sample))
str(w_sample)


#Outlier detection using zscore method
# Extract the columns for outlier detection
outlier_cols <- c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS")
w_sample_df <- w_sample[, outlier_cols]
z_scores <- scale(w_sample_df)
threshold <- 3
outliers <- which(abs(z_scores) > threshold, arr.ind = TRUE)


# outliers detection using IQR
# Get only numerical columns
w_sample_num <- w_sample %>% select_if(is.numeric)

# Calculate IQR, Q1, Q3, lower and upper bounds for each column separately
IQR_value <- apply(w_sample_num, 2, IQR)
Q1 <- apply(w_sample_num, 2, quantile, 0.25)
Q3 <- apply(w_sample_num, 2, quantile, 0.75)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Find outliers for each column separately
IQR_outliers <- lapply(seq_along(w_sample_num), function(i) {
  outliers_IQR <- w_sample_num[[i]][w_sample_num[[i]] < lower_bound[i] | w_sample_num[[i]] > upper_bound[i]]
  if (length(outliers_IQR) > 0) {
    return(outliers_IQR)
  }
})



#Handle the outlier using winsorization method

# Winsorize the data
probs <- c(0.05, 0.95)
z_scores_winsorized <- apply(w_sample_df, 2, function(x) DescTools::Winsorize(x, probs = probs))
excluded_cols <- colnames(w_sample[, -which(names(w_sample) %in% outlier_cols)])
wsample_winsorized <- cbind(z_scores_winsorized, w_sample[, excluded_cols])
wsample_winsorized <- wsample_winsorized %>%
  select(lat, long, datetime, TSK, PSFC, U10, V10, Q2, RAINC, RAINNC, SNOW, TSLB, SMOIS)
print(wsample_winsorized)

# Initial Exploratory time series visualisations
head(wsample_winsorized, n=10)
tail(wsample_winsorized, n=10)
str(wsample_winsorized)
summary(wsample_winsorized)


# Feature engineering
wsample_new <- wsample_winsorized %>% 
  mutate(wind_speed = sqrt(U10^2 + V10^2))
sum(is.na(wsample_new$wind_speed))

wsample_new <- wsample_new %>%
  select(lat, long, datetime, TSK, PSFC, U10, V10, Q2, RAINC, RAINNC, SNOW, TSLB, SMOIS, wind_speed)
print(wsample_new)


#Data analysis on wsample_new dataset to find maximum values and locations"

# highest value recorded for each variable
max_var <- wsample_new %>% 
  summarise(across(c(TSK, PSFC, Q2, RAINC, RAINNC, SNOW, TSLB, SMOIS, wind_speed), ~ .[which.max(.)]))
max_var

# Find the row index of the maximum value for each variable
max_row <- sapply(wsample_new[,c("TSK", "PSFC", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "wind_speed")], which.max)

# Extract the latitude and longitude values for each maximum
max_location <- wsample_new[max_row, c("lat", "long")]

# Print the result
cat("Location with highest values:\n")
max_location

# Group the data frame by latitude and longitude, calculate the mean for each variable within each group, and add the results as columns
avg_data <- wsample_new %>% 
  group_by(lat, long) %>% 
  summarize(across(c("TSK", "PSFC", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "wind_speed"), mean))

# Find the row index of the maximum average value for each variable within each group
max_avg_row <- sapply(avg_data[,c("TSK", "PSFC", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "wind_speed")], which.max)
max_avg_row

# Extract the latitude and longitude values for each maximum
max_avg_location <- avg_data[max_avg_row, c("lat", "long")]

# Print the result
cat("Location with highest average values:\n")
print(max_avg_location)

#Selection of dataset for preprocessing
df3 <- wsample_new[wsample_new[,1] == "51.529" & wsample_new[,2] == "0.022",]
write.csv(df3, "lcawd.csv")

# Preprocess dataset
LC_data <- df3 %>%
  select(datetime, wind_speed)
print(LC_data)

# Summary statistics
summary(LC_data$wind_speed)



# Time series visualisation
# Time series plot
ggplot(LC_data, aes(x = datetime, y = wind_speed)) +
  geom_line(color = "blue") +
  labs(title = "Wind_Speed Concentration Over Time",
       x = "Date and Time",
       y = "Wind_Speed Concentration") +
  theme_minimal()

# scatter plot
ggplot(LC_data, aes(x = datetime, y = wind_speed)) +
  geom_point() +
  labs(title = "Wind_Speed Concentration) Over Time",
       x = "Date and Time",
       y = "Wind_Speed Concentration") +
  theme_minimal()

#time series with smoothen
ggplot(LC_data, aes(x = datetime, y = wind_speed)) + 
  geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, color = "orange") +
  labs(title = "Wind_Speed Concentration Over Time",
       x = "Date and Time",
       y = "Wind_Speed") +
  theme_minimal()

## rolling average
LC_data$wind_speed_category <- cut(LC_data$wind_speed, breaks 
                                   = c(-Inf,280 , 290, Inf), labels 
                                   = c("Low", "Medium", "High"))

ggplot(LC_data, aes(x = datetime, y = wind_speed, color = wind_speed_category)) +
  geom_line() +
  labs(title = "Wind_Speed Concentration Over Time",
       x = "Date and Time",
       y = "Wind Speed") +
  theme_minimal() +
  scale_color_manual(values = c("Low" = "green", "Medium" = "orange", "High" = "red"))


# Create a variable to represent time
LC_data <- LC_data %>%
  mutate(time = as.numeric(difftime(datetime, min(datetime), units = "hours"))) %>%
  select(-wind_speed_category)
print(LC_data)


# Training and Testing Datasets
# Split the data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(LC_data), 0.8 * nrow(LC_data))
train_data <- LC_data[train_indices, ]
test_data <- LC_data[-train_indices, ]
str(train_data)
str(test_data)


# Modelling
# Linear regression model
# Fit the model on the training set
train_model <- lm(wind_speed ~ time, data = train_data)
summary(train_model)

# Predict Wind Speed values for the test set
predictions <- predict(train_model, newdata = test_data)

# Calculate the mean absolute error (MAE) for Linear Regression
mae_LM <- mean(abs(test_data$wind_speed - predictions))
cat("MAE:", mae_LM)

#Plot actual vs predicted value
p1 <- ggplot() +
  geom_point(data = test_data, aes(x = wind_speed, y = predictions), color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  labs(title = "Actual vs. Predicted Wind Speed Concentration",
       x = "Actual Wind Speed Concentration",
       y = "Predicted Wind Speed Concentration") +
  theme_minimal()
p1


#Time Series Univariate Data Analytics in R using Support Vector Regression
library(e1071)

# Build the SVR model using RBF, Poly, and Linear Kernels
# Fit a SVR RBF model on the training set
svr_radial_model <- svm(wind_speed ~ time, data = train_data, kernel = "radial")
# Display the SVR model summary
summary(svr_radial_model)

# Predict Wind Speed values for the test set using the SVR model with RBF kernel
svr_radial_predictions <- predict(svr_radial_model, newdata = test_data)

# Calculate the mean absolute error (MAE) for the SVR-RBF model
mae_svr_radial <- mean(abs(test_data$wind_speed - svr_radial_predictions))

cat("MAE SVR RBF:", mae_svr_radial)

# Fit an SVR model on the training set Poly
svr_poly_model <- svm(wind_speed ~ time, data = train_data, kernel = "poly")
# Display the SVR model summary
summary(svr_poly_model)

# Predict Wind Speed values for the test set using the SVR Poly model
svr_poly_predictions <- predict(svr_poly_model, newdata = test_data)

# Calculate the mean absolute error (MAE) for the SVR Poly model
mae_svr_poly <- mean(abs(test_data$wind_speed - svr_poly_predictions))

cat("MAE SVR Poly:", mae_svr_poly)

# Fit an SVR model on the training set Linear
svr_linear_model <- svm(wind_speed ~ time, data = train_data, kernel = "linear")
# Display the SVR model summary
summary(svr_linear_model)

# Predict Wind Speed values for the test set using the SVR Linear model
svr_linear_predictions <- predict(svr_linear_model, newdata = test_data)

# Calculate the mean absolute error (MAE) for the SVR Poly model
mae_svr_linear <- mean(abs(test_data$wind_speed - svr_linear_predictions))

cat("MAE SVR Linear:", mae_svr_linear)

# Compare the MAE values of the SVR models
cat("\nMAE SVR RBF:", mae_svr_radial)
cat("\nMAE SVR Poly:", mae_svr_poly)
cat("\nMAE SVR Linear:", mae_svr_linear)

# Plot the actual vs. predicted values for the SVR Kernels - RBF, Poly and Linear models
p2 <- ggplot() +
  geom_point(data = test_data, aes(x = wind_speed, y = svr_radial_predictions), color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  labs(title = "SVR (RBF Kernel): Actual vs. Predicted Wind Speed",
       x = "Actual Wind Speed",
       y = "Predicted Wind Speed") +
  theme_minimal()
p2

p3 <- ggplot() +
  geom_point(data = test_data, aes(x = wind_speed, y = svr_poly_predictions), color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  labs(title = "SVR (Polynomial Kernel): Actual vs. Predicted Wind Speed",
       x = "Actual Wind Speed",
       y = "Predicted Wind Speed") +
  theme_minimal()
p3

p4 <- ggplot() +
  geom_point(data = test_data, aes(x = wind_speed, y = svr_linear_predictions), color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  labs(title = "SVR (Linear Kernel): Actual vs. Predicted Wind Speed",
       x = "Actual Wind Speed",
       y = "Predicted Wind Speed") +
  theme_minimal()
p4

library(gridExtra)
grid.arrange(p2, p3, p4, ncol = 3)


## compare the different SVR MODELS(RBF,POLY,LINEAR)
# Set color palette
my_colors <- c("Linear" = "#0072B2", "Poly" = "#009E73", "RBF" = "#D55E00")

# Create a data frame for MAE of SVR models
mae_svr_df <- data.frame(
  Model = c("Linear", "Poly", "RBF"),
  MAE_svr = c(mae_svr_linear, mae_svr_poly, mae_svr_radial)
)

# Create a bar plot with error bars to compare the MAE of SVR models
ggplot(mae_svr_df, aes(x = Model, y = MAE_svr, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  labs(title = "Mean Absolute Error for SVR Models",
       x = "SVR Models",
       y = "Mean Absolute Error") +
  scale_fill_manual(values = my_colors) +
  theme_bw(base_size = 8) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, max(mae_svr_df$MAE_svr) * 1.1)) +
  
  # Add a label for each bar with the corresponding mean absolute error value
  annotate("text", x = c(1, 2, 3), y = mae_svr_df$MAE_svr + 0.1,
           label = round(mae_svr_df$MAE_svr, 2),
           color = "black", size = 2.5, fontface = "bold")


# Building the random forest model with ntrees (100, 200, 500)
install.packages("randomForest")
library(randomForest)


# Fit a Random Forest model (ntree=100) on the training set
rf_100_model <- randomForest(wind_speed ~ time, data = train_data, ntree = 100)
# Display the Random Forest model summary
summary(rf_100_model)

#Model evaluation comparison
# Predict Wind Speed values for the test set using the Random Forest model
rf_100_predictions <- predict(rf_100_model, newdata = test_data)
# Calculate the mean absolute error RF_100
mae_rf_100 <- mean(abs(test_data$wind_speed - rf_100_predictions))
cat("MAE for Random Forest_100:", mae_rf_100)

# Fit a Random Forest model (ntree=200) on the training set
rf_200_model <- randomForest(wind_speed ~ time, data = train_data, ntree = 200)
# Display the Random Forest model summary
summary(rf_200_model)

#Model evaluation comparison
# Predict Wind Speed values for the test set using the Random Forest model
rf_200_predictions <- predict(rf_200_model, newdata = test_data)
# Calculate the mean absolute error for RF_200
mae_rf_200 <- mean(abs(test_data$wind_speed - rf_200_predictions))
cat("MAE for Random Forest_200:", mae_rf_200)

# Fit a Random Forest model (ntree=500) on the training set
rf_500_model <- randomForest(wind_speed ~ time, data = train_data, ntree = 500)
# Display the Random Forest model summary
summary(rf_500_model)

#Model evaluation comparison
# Predict Wind Speed values for the test set using the Random Forest model
rf_500_predictions <- predict(rf_500_model, newdata = test_data)
# Calculate the mean absolute error for RF_500
mae_rf_500 <- mean(abs(test_data$wind_speed - rf_500_predictions))
cat("MAE for Random Forest_500:", mae_rf_500)

# Compare the RMSE values of RFmodels for - 100, 200, and 500 trees
cat("\nMAE for RF_100:", mae_rf_100)
cat("\nMAE for RF_200:", mae_rf_200)
cat("\nMAE for RF_500:", mae_rf_500)

# Plot the actual vs. predicted values for the RFmodels - 100, 200, and 500 trees
p5 <- ggplot() +
  geom_point(data = test_data, aes(x = wind_speed, y = rf_100_predictions), color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  labs(title = "RF (100): Actual vs. Predicted Wind Speed",
       x = "Actual Wind Speed",
       y = "Predicted Wind Speed") +
  theme_minimal()



p6 <- ggplot() +
  geom_point(data = test_data, aes(x = wind_speed, y = rf_200_predictions), color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  labs(title = "RF (200): Actual vs. Predicted Wind Speed",
       x = "Actual Wind Speed",
       y = "Predicted Wind Speed") +
  theme_minimal()


p7 <- ggplot() +
  geom_point(data = test_data, aes(x = wind_speed, y = rf_500_predictions), color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  labs(title = "RF (500): Actual vs. Predicted Wind Speed",
       x = "Actual Wind Speed",
       y = "Predicted Wind Speed") +
  theme_minimal()

library(gridExtra)
grid.arrange(p5, p6, p7, ncol = 3)


## compare the different MAE of the RF MODELS(ntree=100, 200, 500) and visualise
# Load required packages
library(ggplot2)
library(scales)

# Create a data frame with model results
mae_rf_df <- data.frame(
  Model = c("ntree=100", "ntree=200", "ntree=500"),
  mae_rf = c(mae_rf_100, mae_rf_200, mae_rf_500)
)

# Create the plot using ggplot2
ggplot(mae_rf_df, aes(x = Model, y = mae_rf, fill = Model)) +
  # Add bars
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  # Add x and y axis labels and plot title
  labs(title = "Mean Absolute Error for RF Models",
       x = "Number of Trees",
       y = "Mean Absolute Error") +
  # Customize plot appearance using themes
  theme_bw(base_size = 8) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, max(mae_rf_df$mae_rf) * 1.1)) +
  # Add custom color scale
  scale_fill_manual(values = c("ntree=100" = "#004488",
                               "ntree=200" = "#DDAA33",
                               "ntree=500" = "#BB5566")) +
  
  # Add a label for each bar with the corresponding mean absolute error value
  annotate("text", x = c(1, 2, 3), y = mae_rf_df$mae_rf + 0.05,
           label = round(mae_rf_df$mae_rf, 2),
           color = "black", size = 2.5, fontface = "bold")

#Time Series Univariate Data Analytics using Auto Regression Integrated Moving Average
# Build the ARIMA model
library(data.table)
library(dplyr)
library(forecast)

LC_data_arima <- copy(LC_data)
# Create separate columns for year, month, day,and hour
LC_data_arima$Year <- as.numeric(format(LC_data_arima$datetime,"%Y"))
LC_data_arima$Month <- as.numeric(format(LC_data_arima$datetime,"%m "))
LC_data_arima$Day <- as.numeric(format (LC_data_arima$datetime,"%d"))
LC_data_arima$Hour <- as.numeric(format(LC_data_arima$datetime,"%H"))
print (LC_data_arima)

# Remove unnecessary columns
LC_data_arima <- LC_data_arima %>% select(-datetime, -time)
print(LC_data_arima)
# Splitting into training and testing for ARIMA Model

n_hours <- nrow(LC_data_arima)
# Convert the dataset to a time series object
time_points <- seq(from = as.POSIXct("2018-05-01 00:00:00"), to = as.POSIXct("2018-05-31 23:59:59"), by = "3 hours")
LC_ts <- ts(LC_data_arima$wind_speed, start = start(time_points), end = end(time_points), frequency = 8)
LC_ts

library(tseries)
adf.test(LC_ts)

train_end_idx <- which(LC_data_arima$Year == 2018 &
                         LC_data_arima$Month == 5 &
                         LC_data_arima$Day == 25 &
                         LC_data_arima$Hour == 15)[1]

# Split the dataset into training and testing sets
train_ts_data <- LC_ts[1:train_end_idx]
test_ts_data <- LC_ts[(train_end_idx+1):n_hours]
str(train_ts_data)
str(test_ts_data)

# Fit the ARIMA model
arima_model <- auto.arima(train_ts_data, seasonal = TRUE, stepwise = TRUE)
arima_model
#Forecast using the ARIMA model
arima_forecast <- forecast (arima_model, h = length (test_ts_data))
print(arima_forecast)

checkresiduals(arima_model)

# Calculate the Mean Absolute Error (MAE)
mae_arima <- mean(abs(test_ts_data - arima_forecast$mean))

# Display the performance metrics
cat("MAE for ARIMA Model:", mae_arima, "\n")

# Plot the actual vs. predicted values for ARIMA model
arima_df <- data.frame(test_ts_data, arima_forecast$mean, row.names = 1:length(test_ts_data))
p8 <- ggplot() +
  geom_point(data = arima_df, aes(x = test_ts_data, y = arima_forecast.mean), color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  labs(title = "Arima Model: Actual vs. Predicted Wind Speed",
       x = "Actual Wind Speed",
       y = "Predicted Wind Speed") +
  theme_minimal()
p8

# Plot the visuals for comparism

# Plot the actual vs. predicted values for the ARIMA, Linear Regression, SVR_RBF, and RF_100 models
grid.arrange(p1, p2, nrow = 2)
grid.arrange(p5, p8, nrow = 2)

# Compare the MAE values of the ARIMA, Linear Regression, SVR_radial, and RF_100 models
cat("\nMAE for Linear Regression:", mae_LM)
cat("\nMAE for SVR_RBF:", mae_svr_radial)
cat("\nMAE for RF(ntree=100):", mae_rf_100)
cat("\nMAE for ARIMA:", mae_arima)

# Visualisation of the comparison
# Create a dataframe with the MAE values for ARIMA, Linear Regression, SVR_RBF, and Random Forest (ntree=100)
mae_comparison_df <- data.frame(
  Model = c("ARIMA", "Linear Regression", "SVR_RBF", "Random Forest (ntree=100)"),
  MAE = c(mae_arima, mae_LM, mae_svr_radial, mae_rf_100)
)

# Bar chart to visualize the comparison

ggplot(mae_comparison_df, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", size = 0.25) +
  geom_segment(aes(x = as.numeric(factor(Model)), y = MAE, xend = as.numeric(factor(Model))+0.9, yend = lead(MAE, 1, order_by = as.numeric(factor(Model)), default = max(MAE)+1)),
               color = "black", size = 1) +
  labs(title = "Mean Absolute Error for Selected Models",
       x = "Model",
       y = "Mean Absolute Error",
       size = 12,
       color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("ARIMA" = "#69b3a2", "Linear Regression" = "#404080", "SVR_RBF" = "#8b0000", "Random Forest (ntree=100)" = "#ff8c00")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#f0f0f0"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        panel.grid.major.x = element_line(color = "#e0e0e0"))

