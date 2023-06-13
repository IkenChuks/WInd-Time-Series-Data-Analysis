################################################################################
### EXPLORATORY DATA ANALYSIS OF THE ####
### WEATHER RESEARCH AND FORECASTING MODEL DATA ###

# Imported the .csv file
df <- read.csv("WRFdata_May2018.csv")
View(df) 
class(df)
nrow(df)
ncol(df)

# Imported janitor, dplyr and tidyverse libraries to clean data frame and
# correct column names
library(janitor)
library(dplyr)
library(tidyverse)

anomaly_loc <- which(df == "[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]", arr.ind = TRUE)
anomaly_loc # Printed the row and column indexes where the brackets were found

colnames(df)[6]
df <- df %>% 
  mutate(X.4 = replace(X.4, match("[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]", X.4), NA))
View(df)

# Noticed the wrong column names issue
colnames(df)
nrow(df)
df_1 <- df %>% row_to_names(row_number = 1)

colnames(df_1)
#Number of rows confirmed wrong column names have been deleted
nrow(df_1)
View(df_1)


###############################################################################
### EDA WITH FIRST 300 ROWS ###
# In view of the fact that missing XLAT and XLONG figures could not simply 
# be averaged or randomly imputed, over 300 rows were selected as those
# with either a missing XLAT or XLONG figure would eventually be removed.


df_2 <- df_1[1:350,] %>%
  drop_na(XLAT, XLONG)

nrow(df_2)

eda300_df <- as.data.frame(df_2[1:300, ])
class(eda300_df)
View(eda300_df)

# In order to choose the technique for filling in the missing values of 
# eda300_df, the latitude and longitude had to be arranged in order. This  
# could only be done with the appropriate variable data type.
str(eda300_df)

eda300_df <- as.data.frame(sapply(eda300_df, as.numeric))
# changed values from character type to numeric data type

class(eda300_df)
str(eda300_df)

eda300_df <- eda300_df[order(eda300_df$XLAT, eda300_df$XLONG), ]
rownames(eda300_df) <- NULL
class(eda300_df)
View(eda300_df)

# It was observed that there were essentially 10 unique columns (column names
# with the exceptions of XLAT and XLONG - the reference variables) repeated
# 248 times for different time intervals

ncol(eda300_df)

# Since XLAT and XLONG were the reference variables, they were not repeated. The 
# next 10 columns were however repeated. Calculations were done to determine the 
# number of repetitions

rep_unique_var <- (ncol(eda300_df)-2)/10
rep_unique_var

# The 2 reference and 10 unique variables were explored to proceed on how to
# treat the missing variables for each field
edavar12_df <- eda300_df[,1:12]
edavar12_df
class(edavar12_df)
View(edavar12_df)

# After examination, interpolation was chosen as the technique to be adopted
# for filling the NA's in the EDA 300 rows

# Missing values filled with linear interpolation
install.packages("zoo") #zoo package installed and imported
library(zoo) 

eda300filled_df <- na.approx(eda300_df, rule = 2, na.rm = FALSE, maxgap = Inf)
sum(is.na(eda300filled_df)) 

View(eda300filled_df)



#######################################################################
#######################################################################





#######################################################################
### LOCATION COORDIATES AND CHOSEN TIME SERIES VARIABLE ANALYSIS ###

# A location in the UK was selected to determine its suitability for
# for a wind power station or turbine set up.

# Hence coordinates XLAT = 53.394 and XLONG = -3.768 were selected which
# are coordinates for a location near Colwyn Bay, North Wales.

rowindex <- which(apply(df_1, 1,
                        function(x) x["XLAT"] == "53.394" &
                          x["XLONG"] == "-3.768"))
rowindex

colwyn_data <- df_1[2798,] 
View(colwyn_data)

colnames(colwyn_data) <- gsub("\"", "", colnames(colwyn_data))
colwyn_data
length(colnames(colwyn_data))

ucolname_ind <- which(colnames(colwyn_data) == "U10")
ucolname_ind
length(ucolname_ind)

vcolname_ind <- which(colnames(colwyn_data) == "V10")
length(vcolname_ind)

# Found the indexes of missing column names
missing_cols_idx <- which(is.na(colnames(colwyn_data)))
missing_cols_idx
length(missing_cols_idx)

sum(grepl("5$", missing_cols_idx))
sum(grepl("6$", missing_cols_idx))

# Found the indexes of missing column names ending with "5"
missing_cols_idx_5 <- missing_cols_idx[grepl("5$", missing_cols_idx)]
missing_cols_idx_6 <- missing_cols_idx[grepl("6$", missing_cols_idx)]
missing_cols_idx_5
missing_cols_idx_6

colnames(colwyn_data)[missing_cols_idx_5] <- "U10"
colnames(colwyn_data)[missing_cols_idx_6] <- "V10"
colnames(colwyn_data)

length(which(is.na(colnames(colwyn_data))))

length(colnames(colwyn_data))
View(colwyn_data)

# Searched for the U10 and V10 values
my_values_u <- colwyn_data[1, grep("U10", names(colwyn_data))]
my_values_u
length(my_values_u)
typeof(my_values_u)

output_listu <- list()  # created an empty list to store the outputs

for (i in my_values_u) {
  output_listu <- c(output_listu, list(i))
}
output_listu <- unlist(output_listu)
output_listu
typeof(output_listu)

my_values_v <- colwyn_data[1, grep("V10", names(colwyn_data))]
my_values_v
length(my_values_v)
output_listv <- list()  # created an empty list to store the outputs

for (i in my_values_v) {
  output_listv <- c(output_listv, list(i))
}
output_listv <- unlist(output_listv)
output_listv
typeof(output_listv)


vel_df <- data.frame(output_listu, output_listv) # formed the velocity df
vel_df

vel_df$U10_values <- as.numeric(vel_df$output_listu)
vel_df$output_listu <- NULL
vel_df$V10_values <- as.numeric(vel_df$output_listv)
vel_df$output_listv <- NULL
vel_df

# Used interpolation to fill NA's
vel_df <- as.data.frame(na.approx(vel_df, rule = 3, 
                                  na.rm = FALSE, maxgap = Inf))
vel_df
sum(is.na(vel_df))  
class(vel_df)

# Resultant velocity calculated 
vel_df$resultant_vel <- sqrt(vel_df$U10_values^2 + vel_df$V10_values^2)
vel_df$resultant_vel
vel_df

resultvel <- vel_df[, -1:-2]
resultvel

# Created the sequence of dates and times with a frequency of 3 hours
datetimes <- seq(from=as.POSIXct("2018-05-01 00:00"), 
            to=as.POSIXct("2018-05-31 21:00"), 
            by="3 hours")
datetimes
typeof(datetimes)
class(datetimes)

rvel_df <- data.frame(datetimes, resultvel)
rvel_df
# Plotted the resultant velocity against time
library(ggplot2)
ggplot(rvel_df, aes(x = datetimes, y = resultvel)) + geom_line() + 
  labs(title = "Colwyn Bay May 2018 Wind Velocity Graph",
       x = "DateTime", y = "Resultant Velocity")

# Creating a time series, libraries are imported
install.packages("forecast")
library(forecast)

# Used zoo() to create the time series
vel_ts <- zoo(rvel_df$resultvel, 
              order.by = rvel_df$datetimes)
vel_ts
head(vel_ts)
length(vel_ts)

# Aggregated the time series
ag.vel_ts <- aggregate(vel_ts, as.Date, mean)
ag.vel_ts
daily <- index(ag.vel_ts)
daily
length(ag.vel_ts)

# Checked for outliers in the ts
ag.vel_ts <- tsclean(ag.vel_ts)
ag.vel_ts
summary(ag.vel_ts)

# Plotted the time series
ggplot(ag.vel_ts, aes(x = daily, y = ag.vel_ts)) + geom_line() + 
  labs(title = "Aggregated Colwyn Wind Velocity",
       x = "Date", y = "Mean Resultant Velocity")

dev.off()

# Other time series analysis

# Displayed the degree of correlation at different lags
tsdisplay(vel_ts, lag.max = 50) 

library(tseries)
adf.test(vel_ts)
# Failed to reject the null hypothesis since p-value > 0.05 hence, time series 
# time series is non-stationary

vel_arima <- auto.arima(vel_ts, trace=T, stepwise = F, approximation = F)
vel_arima
# Setting trace as True in order to trace the model selection process,
# an ARIMA(1,0,1) with non-zero mean was arrived at, AR term of 0.8219,  
# MA term of 0.1114 and differencing of 0.

arima_rmse <- accuracy(vel_arima)[2]
arima_rmse


#########################################################################



#########################################################################
# Machine Learning

# Loaded the required packages
library(lubridate)
library(ggplot2)
library(zoo)
install.packages("caret")
library(caret)
library(future)

vel_ts
rvel_df <- fortify(vel_ts) #Fortified vel_ts to a data frame
rvel_df
sum(is.na(rvel_df))
colnames(rvel_df) <- c("datetime","wind_velocity")
class(rvel_df$datetime)

# The data frame was split into training and testing sets
set.seed(321)
trainIndex <- createDataPartition(rvel_df$wind_velocity, p = 0.8, list = FALSE)
train <- rvel_df[trainIndex, ]
nrow(train)
test <- rvel_df[-trainIndex, ]
nrow(test)

# Linear regression model
install.packages("readr")
library(readr)

# Specified the time series cross-validation method
lm_tc <- trainControl(method = "timeslice", initialWindow = 24,
                   horizon = 1, fixedWindow = TRUE)

# specified the model and its parameters
lm_model <- train(wind_velocity ~ datetime, data = train,
               method = "lm", trControl = lm_tc, preProc = NULL)

# Used the trained model to make predictions on the test set
lm_predictions <- predict(lm_model, newdata = test)
lm_predictions
length(lm_predictions)

# Calculated the mean squared error and root mean squared error
lm_mse <- mean((test$wind_velocity - lm_predictions)^2)
lm_rmse <- sqrt(lm_mse)
lm_rmse


# Support Vector Machine Regression SVMR model
install.packages("e1071")
library(e1071)

# Specify the time series cross-validation method
svm_tc <- trainControl(method = "timeslice", initialWindow = 24,
                      horizon = 1, fixedWindow = TRUE)

# Specify the model and its parameters
svm_model <- train(wind_velocity ~ datetime, data = train,
               method = "svmRadial", trControl = svm_tc,
               preProc = NULL, tuneGrid = expand.grid(sigma = 10.33358, C = 2))

svm_model

# Use the trained model to make predictions on the test set
svm_predictions <- predict(svm_model, newdata = test)
svm_predictions

# Calculate the mean squared error and root mean squared error
svm_mse <- mean((svm_predictions - test$wind_velocity)^2)
svm_mse
svm_rmse <- sqrt(svm_mse)
svm_rmse


# Random Forest model
install.packages("randomForest")
library(randomForest)

# specify the time series cross-validation method
rf_tc <- trainControl(method = "timeslice", initialWindow = 24,
                   horizon = 1, fixedWindow = TRUE)

# specify the model and its parameters
set.seed(152)
rf_model <- train(wind_velocity ~ datetime, data = train,
               method = "rf", trControl = rf_tc, preProc = NULL, tuneLength = 10)
rf_model

# use the trained model to make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test)
rf_predictions

# calculate the error metrics
rf_mse <- mean((rf_predictions - test$wind_velocity)^2)
rf_mse 
rf_rmse <- sqrt(rf_mse)
rf_rmse


# Create a table of model performance metrics
models <- c("ARIMA", "Linear Reg", "SVMR", "Random Forest")
total_rmse <- c(arima_rmse, lm_rmse, svm_rmse, rf_rmse)
results <- data.frame(Model = models, RMSE = total_rmse)

# Print the table of model performance metrics
print(results)

# Based on the RSME scores, the most accurate model - random forest - would be
# plotted first showing its predicted vs. actual values

# Create data frame of actual and predicted values with time stamps
install.packages("ggpubr")
library(ggpubr)

# Random forest plot
rfres <- data.frame(
  time = test$datetime,
  actual = test$wind_velocity,
  predicted = rf_predictions
)
# Plot actual vs. predicted wind velocity against time
ggplot(rfres, aes(x = time)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(x = "Time", y = "Wind Velocity", 
       title = "Actual vs. Predicted Wind Velocity(Random Forest)")



# SVMR plot
svmres <- data.frame(
  time = test$datetime,
  actual = test$wind_velocity,
  predicted = svm_predictions
)
# Plot actual vs. predicted wind velocity against time
ggplot(svmres, aes(x = time)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "green")) +
  labs(x = "Time", y = "Wind Velocity", 
       title = "Actual vs. Predicted Wind Velocity(SVMR)")



# Linear regression plot
lmres <- data.frame(
  time = test$datetime,
  actual = test$wind_velocity,
  predicted = lm_predictions
)
# Plot actual vs. predicted wind velocity against time
ggplot(lmres, aes(x = time)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "orange")) +
  facet
  labs(x = "Time", y = "Wind Velocity", 
       title = "Actual vs. Predicted Wind Velocity(Linear Regression)")




###############################################################################
# Correlation and Testing

# To find the relationship between surface temperature and resultant wind
# velocity, the Pearson correlation was used

# But first, the surface temperature for Colwyn Bay for the period was 
# filtered out with the steps:
colwyn_data <- df_1[2798,]
colwyn_data %>% View()

tsk_ind <- which(colnames(colwyn_data) == "TSK") #Surface temperature TSK index
tsk_ind
length(tsk_ind)

# Since the expected result of 248 indexes not applicable, the 
# indexes of missing column names was found
missing_cols_idx <- which(is.na(colnames(colwyn_data)))
missing_cols_idx
length(missing_cols_idx)

sum(grepl("3$", missing_cols_idx)) #Chose indexes that end with 3

# Found the indexes of missing column names ending with "3"
missing_cols_idx_3 <- missing_cols_idx[grepl("3$", missing_cols_idx)]
missing_cols_idx_3

colnames(colwyn_data)[missing_cols_idx_3] <- "TSK"
colnames(colwyn_data)

length(which(is.na(colnames(colwyn_data))))

# Searched for the TSK values
tsk_values <- colwyn_data[1, grep("TSK", names(colwyn_data))]
tsk_values
length(tsk_values) #Number of TSK values now correctly identified as 248
typeof(tsk_values)

output_tsk <- list()  # created an empty list to store the outputs

for (i in tsk_values) {
  output_tsk <- c(output_tsk, list(i))
}
output_tsk <- unlist(as.numeric(output_tsk))
output_tsk
typeof(output_tsk)

# Linear interpolation was used for missing values
output_tsk <- na.approx(output_tsk, rule = 2, na.rm = FALSE, maxgap = Inf)
output_tsk

# Invoked the resultant velocity data frame rvel_df and added the 
# surface temperature variable to it
rvel_df$surftemp <- output_tsk
rvel_df
rvel_df %>% View()

# Used min-max scaling on velocity and temperature variables
mm1_vel <- (rvel_df$wind_velocity - min(rvel_df$wind_velocity))
mm2_vel <- (max(rvel_df$wind_velocity)-min(rvel_df$wind_velocity))
mm_resultvel<- mm1_vel/mm2_vel
head(mm_resultvel)

mm1_tsk <- (rvel_df$surftemp - min(rvel_df$surftemp))
mm2_tsk <- (max(rvel_df$surftemp)-min(rvel_df$surftemp))
mm_surftemp<- mm1_tsk/mm2_tsk
head(mm_surftemp)

rvel_df$mm_resultvel <- mm_resultvel
rvel_df$mm_surftemp <- mm_surftemp
head(rvel_df) #The scaled variables added to the data frame

# Spearman's rank correlation was used for wind velocity and surface temperature
# variables correlation test
rvel_df %>%
  select(mm_resultvel, mm_surftemp) %>%
  cor(method='spearman')

rvel_df %>%
  cor.test(~mm_resultvel + mm_surftemp, data = ., method='spearman')



###############################################################################
##### Discussion Calculations
# Using random forest prediction and comparing for the required wind speeds of
# 3m/s to 6m/s

cat(rf_predictions)
summary(rf_predictions)
sd(rf_predictions)
var(rf_predictions)



# Using summary() and describe() for EDA of the unique variables along the column

install.packages("psych") #imported psych package in order to use describe()
library(psych)
edavar12_df
edavar12filled_df <- eda300filled_df[,1:12]
edavar12filled_df
class(edavar12filled_df)

summary(edavar12filled_df[, 3:12])
describe(edavar12filled_df[, 3:12])




# Plotting the scaled velocity vs surface temperature for correlation testing

ggplot(rvel_df, aes(x = mm_resultvel, y = mm_surftemp)) +
  geom_point(color = "lightgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(x = "Resultant Velocity(Scaled)", y = "Surface Temperature(Scaled)") +
  ggtitle(paste("Velocity - Temperature Correlation:", round(cor(rvel_df$mm_resultvel,
                                          rvel_df$mm_surftemp, method = "spearman"), 5)))



