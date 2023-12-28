library(forecast)
library(zoo)
library(tseries)
library(Metrics)

# Reading the data
data <- read.csv(file.choose(), header=T)
head(data)
summary(data)

# Checking if the values of the Sales column are in numeric format
class(data$Sales[0])

# Reading the data in the time series format
sales <- ts(data$Sales, start=c(2012, 1), frequency=4)
sales
plot(sales)

adf.test(sales)


# Q1 Code
boxplot(data$Sales ~ data$Year,
        main = "Boxplot of sales across the year", 
        xlab = "Year", ylab = "Sales in millions",
        outline = TRUE, # Show outliers
        border = "black",
)
grid()


# Q2 Code
boxplot(data$Sales ~ data$Quarter,
        xlab = "Quarter", ylab = "Sales in millions",
        main = "Boxplot of sales across the quarter",
        outline = TRUE, # Show outliers
        border = "black",
)
grid()


# Q3 Code
acf(data$Sales)

results <- numeric(length(data$Sales) - 1) 

# Loop through each lag and perform the Ljung-Box test
for (i in 1:length(data$Sales) - 1) {
  results[i] <- Box.test(data$Sales, type = "Ljung-Box", lag = i)$p.value
}

lags_ljung <- which(results < 0.05)
print(lags_ljung)


# Loop through each lag and perform the Box-Pierce test
for (i in 1: length(data$Sales) - 1) {
  results[i] <- Box.test(data$Sales, type="Box-Pierce", lag=i)$p.value
}

lags_pierce <- which(results < 0.05)
print(lags_pierce)


# Q4 Code
train <- data[1:(nrow(data)-4),]
test <- tail(data, 4)


# Q5 Code

# Additive model fitting
train_add <- data.frame(train)
train_add_sales <- ts(train_add$Sales, start=c(2012, 1), frequency=4)
train_add_decompose <- decompose(train_add_sales)
train_add$SI <- rep(train_add_decompose$figure, length.out = nrow(train_add))

train_add$deseasonalized_data <- train_add$Sales - train_add$SI
time <- 1:length(train_add$deseasonalized_data)

additive_model <- lm(train_add$deseasonalized_data ~ time)
summary(additive_model)
train_add$desonalized_predicted <- predict(additive_model)
train_add$predicted_actual <- train_add$desonalized_predicted + train_add$SI
train_add$res <- train_add$Sales - train_add$predicted_actual


# Multiplicative model fitting
train$ma4q <- rollmean(x=train$Sales, k=4, fill=NA)
shift_by <- 1
train$ma4q <- c(rep(NA, shift_by), train$ma4q[1:(nrow(train) - shift_by)])
train
train$centered_ma <- rollmean(x=train$ma4q, k=2, fill=NA)
train$specific_season <- train$Sales / train$centered_ma
train$SI <- NA

train$SI[train$Quarter == 'Winter'] <- round(mean(train$specific_season[train$Quarter == 'Winter'], na.rm=TRUE), 2)
train$SI[train$Quarter == 'Spring'] <- round(mean(train$specific_season[train$Quarter == 'Spring'], na.rm=TRUE), 2)
train$SI[train$Quarter == 'Summer'] <- round(mean(train$specific_season[train$Quarter == 'Summer'], na.rm=TRUE), 2)
train$SI[train$Quarter == 'Fall'] <- round(mean(train$specific_season[train$Quarter == 'Fall'], na.rm=TRUE), 2)

train$deseasonalized_data <- train$Sales / train$SI
time <- 1:length(train$deseasonalized_data)

multiplicative_model <- lm(train$deseasonalized_data ~ time)
summary(multiplicative_model)
train$desonalized_predicted <- predict(multiplicative_model)
train$predicted_actual <- train$desonalized_predicted * train$SI
train$res <- train$Sales - train$predicted_actual 


# Q6 Code

# White noise checking for additive model
acf(train_add$res)

results <- numeric(length(train_add$res) - 1) 

# Loop through each lag and perform the Ljung-Box test
for (i in 1:length(train_add$res) - 1) {
  results[i] <- Box.test(train_add$res, type = "Ljung-Box", lag = i)$p.value
}

lags_ljung <- which(results < 0.05)
print(lags_ljung)


# Loop through each lag and perform the Box-Pierce test
for (i in 1: length(train_add$res) - 1) {
  results[i] <- Box.test(train_add$res, type="Box-Pierce", lag=i)$p.value
}

lags_pierce <- which(results < 0.05)
print(lags_pierce)


# White noise checking for multiplicative model
acf(train$res)

results <- numeric(length(train$res) - 1) 

# Loop through each lag and perform the Ljung-Box test
for (i in 1:length(train$res) - 1) {
  results[i] <- Box.test(train$res, type = "Ljung-Box", lag = i)$p.value
}

lags_ljung <- which(results < 0.05)
print(lags_ljung)


# Loop through each lag and perform the Box-Pierce test
for (i in 1: length(train$res) - 1) {
  results[i] <- Box.test(train$res, type="Box-Pierce", lag=i)$p.value
}

lags_pierce <- which(results < 0.05)
print(lags_pierce)


# Q7 Code

# accessing the row indices for taking them as time values for inference
row_indices <- as.numeric(rownames(test))
print(row_indices)

# RMSE for additive model
additive_pred <- 8.27 + 0.077 * row_indices + train_add_decompose$figure
additive_rmse <- rmse(additive_pred, test$Sales)

# RMSE for multiplicative model
multiplicative_SI <- train$SI[1:4]
multiplicative_pred <- (8.18 + 0.08 * row_indices) * multiplicative_SI 
multiplicative_rmse <- rmse(multiplicative_pred, test$Sales)






