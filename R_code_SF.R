install.packages("tidyverse")
install.packages("forecast")
install.packages("caret")

library(tidyverse)
library(forecast)
library(zoo)
library(lubridate)
library(caret)
setwd(getwd())



###1. Auto Arima Model

#Import your data and format your dates

data <- read.csv("publicTransport_part.csv")

data$datetime <- paste(data$DATE, data$TIME)
data$datetime <- as.POSIXct(data$datetime, format = "%d-%b-%y %H:%M")
data <- subset(data, select = -c(1,2))

#Create your Time Series and differentiate your data
data.ts <- ts(data$DEMAND, frequency = 96)
data.ts <- diff(diff(data.ts, lag = 96), lag = 1)

#Set up your training data for the ARIMA model
train_data <- subset(data, datetime >= "2005-03-01 06:30:00" & datetime <= "2005-03-19 22:00:00")

train.ts <- ts(train_data$DEMAND, frequency = 96)
train.ts <- diff(diff(train.ts, lag = 96), lag = 1)

#Calculate the best lambda value
lambda = BoxCox.lambda(train.ts)

train.ts_boxcox <- BoxCox(train.ts, lambda)

train.ARIMA <- auto.arima(train.ts_boxcox, seasonal = TRUE)

summary(train.ARIMA)
train.ARIMA.pred <- forecast(train.ts_boxcox, h = 32)
accuracy(train.ARIMA.pred, data.ts)

checkresiduals(train.ARIMA.pred)


train.ARIMA.pred <- forecast(train.ARIMA, h = 480)

plot(train.ts, col = "blue", xlab = "Time", ylab = "DEMAND", bty = "l")
lines(train.ARIMA.pred$fitted, col = "red", lwd = 2)
lines(train.ARIMA.pred$mean, col = "green", lwd = 2)



#forecasting by one day

lambda = BoxCox.lambda(data.ts)

data.ts_boxcox <- BoxCox(data.ts, lambda)
data.ARIMA <- auto.arima(data.ts_boxcox)

summary(data.ARIMA)

data.ARIMA.pred <- forecast(data.ARIMA, h = 192)

plot(data.ARIMA.pred, col= "green", ylim = c(0, 100), xlab = "Days", ylab = "Arrivals")
lines(data.ts, col = "red")
lines(data.ARIMA.pred$fitted, col = "blue")



####2. Regression Model

#https://www.met.ie/climate/available-data/historical-data
#CASEMENT 1944 STATION


#Import data and parse dates
library(timeDate)
data <- read.csv("publicTransport_part.csv")

data$datetime <- paste(data$DATE, data$TIME)
data$datetime <- as.POSIXct(data$datetime, format = "%d-%b-%y %H:%M")

diff_data <- diff(data$DEMAND)


###Import Weather Forecast and merge

hly3723$datetime <- as.POSIXct(hly3723$date, format = "%d/%m/%Y %H:%M")

data <- merge(data, hly3723, by = "datetime")



#Select predictors
data$DOW <- wday(data$datetime, label = TRUE)
data$Hour <- hour(data$datetime)
#data$Minute <- minute(data$datetime)


# Create dummy variables.

DOW.dummies <- model.matrix(~ 0 + DOW, data = data)
datetime.dummies <- model.matrix(~ 0 + datetime, data = data)
HOUR.dummies <- model.matrix(~ 0 + Hour, data = data)
#MINUTE.dummies <- model.matrix(~ 0 + Minute, data = data)
Rain.dummies <- model.matrix(~ 0 + rain, data = data)
Temp.dummies <- model.matrix(~ 0 + temp, data = data)

# Change the names of the dummy variables.
colnames(DOW.dummies) <- gsub("DOW", "", colnames(DOW.dummies))
colnames(datetime.dummies) <- gsub("DT", "", colnames(datetime.dummies))
colnames(HOUR.dummies) <- gsub("Hour", "", colnames(HOUR.dummies))
#colnames(MINUTE.dummies) <- gsub("Minute", "", colnames(MINUTE.dummies))
colnames(Rain.dummies) <- gsub("Rain", "", colnames(Rain.dummies))
colnames(Temp.dummies) <- gsub("Temp", "", colnames(Temp.dummies))

# Set up training and validation sets.
x <- as.data.frame(cbind(DOW.dummies[, -7], Temp.dummies[,-16], Rain.dummies[, -16], datetime.dummies[, -16], HOUR.dummies[, -16]))
y <- data$DEMAND



nTotal <- length(y)
nValid <- 60
nTrain <- nTotal - nValid
xTrain <- x[1:nTrain, ]
yTrain <- y[1:nTrain]
xValid <- x[(nTrain + 1):nTotal, ]
yValid <- y[(nTrain + 1):nTotal]


#Create ts and run regression
yTrain.ts <- ts(yTrain) #create ts object
(formula <- as.formula(paste("yTrain.ts", paste(c("trend", colnames(xTrain)), collapse = "+"), sep = "~"))) #create a formula object to pass to tslm
data.tslm <- tslm(formula, data = xTrain, lambda = 1)
options(scipen = 999, digits = 6)
summary(data.tslm)

library(ggplot2)

ggplot(data, aes(x=datetime, y = DEMAND)) +
  geom_line() +
  labs(title = "Demand over time", x = "Date", y = "Demand")

data.tslm.pred <-  forecast(data.tslm, newdata = xValid)
accuracy(data.tslm.pred, yValid)

checkresiduals(data.tslm.pred)


colors <- c("blue", "green")
names <- c("Predicted", "Fitted")
ci <- predict(data.tslm.pred, interval = "confidence")



y.ts<- ts(y)
times.ts <- time(y.ts)
plot(data.tslm.pred, ylim = c(0, 200), xlab = "Time", ylab = "Passengers Arriving", main = "LM Model of Bus Demand")
lines(window(y.ts, col = colors[1], start = times.ts[nValid + 1]))
lines(data.tslm.pred$fitted, col = colors[2])
legend("topright", legend = names, col = colors, lty = 1)
matlines(ci, col=colors[2], lty = 2)


#####Forecasting Data 

Forecast$datetime <- as.POSIXct(Forecast$date, format = "%d/%m/%Y %H:%M")
Forecast$DOW <- wday(Forecast$datetime, label = TRUE)
Forecast$Hour <- hour(Forecast$datetime)


Forecast <- Forecast[, -1]

DOWF.dummies <- model.matrix(~ 0 + DOW, data = Forecast)
datetimeF.dummies <- model.matrix(~ 0 + datetime, data = Forecast)
HOURF.dummies <- model.matrix(~ 0 + Hour, data = Forecast)
#MINUTE.dummies <- model.matrix(~ 0 + Minute, data = data)
RainF.dummies <- model.matrix(~ 0 + rain, data = Forecast)
TempF.dummies <- model.matrix(~ 0 + temp, data = Forecast)

# Change the names of the dummy variables.
colnames(DOWF.dummies) <- gsub("DOW", "", colnames(DOW.dummies))
colnames(datetimeF.dummies) <- gsub("DT", "", colnames(datetime.dummies))
colnames(HOURF.dummies) <- gsub("Hour", "", colnames(HOUR.dummies))
#colnames(MINUTE.dummies) <- gsub("Minute", "", colnames(MINUTE.dummies))
colnames(RainF.dummies) <- gsub("Rain", "", colnames(Rain.dummies))
colnames(TempF.dummies) <- gsub("Temp", "", colnames(Temp.dummies))

# Set up training and validation sets.
x <- as.data.frame(cbind(DOWF.dummies[, -7], TempF.dummies[,-16], RainF.dummies[, -16], datetimeF.dummies[, -16], HOURF.dummies[, -16]))



data.tslm.pred2 <-  forecast(data.tslm, newdata = x)



plot(data.tslm.pred2, ylim = c(0, 200), xlab = "Time", ylab = "Passengers Arriving", main = "Forecast for the 22nd - 15th")
lines(window(y.ts, col = colors[1]))
lines(data.tslm.pred2$fitted, col = colors[2])
lines(data.tslm.pred2$fitted$mean, col = colors[2])
legend("topright", legend = names, col = colors, lty = 1)
matlines(ci, col=colors[1], lty = 2)

summary(data.tslm.pred2)

plot(data.tslm.pred2, col ="red", ylim = c(0, 200), xlab = "Days", ylab = "Number of Passengers Arriving")
lines(window(y.ts))
lines(data.tslm.pred2$fitted, col = "blue")

