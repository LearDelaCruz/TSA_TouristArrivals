#Libraries
library(forecast)
library(tseries)
library(Kendall)
library(seastests)
library(Metrics)
library(readxl)
library(ggplot2)

#Reading the data set
visitors <- read_excel("C:/Users/d/Downloads/For TSA Paper/Visitors to the Philippines from January 2008 to February 2024.xlsx")

visitors_ts <- ts(visitors, start = c(2008, 1), frequency = 12)
options(scipen = 2)
plot(visitors_ts, pch = 12, 
     ylab = "Tourist Arrivals", panel.first = grid ())

#Test for Stationarity
adf.test(visitors_ts)

#Test for Trend
MannKendall(visitors_ts)

#Test for Seasonality
kw(visitors_ts, freq = 4)

#ACF and PACF
acf(visitors_ts, 12, main = "ACF for Tourist Arrivals", 
    panel.first = grid (), xaxt = "n")
axis(1, at = 0:12/12, labels=0:12)
pacf(visitors_ts, 12, main = "PACF for Tourist Arrivals", 
    panel.first = grid (), xaxt = "n")
axis(1, at = 0:12/12, labels=0:12)

#Seasonal Differencing
diff_visitors <- diff(visitors_ts, lag = 12)
plot(diff_visitors, pch = 16, panel.first = grid ())
adf.test(diff_visitors)

acf(diff_visitors, 12, main = "ACF for Tourist Arrivals", 
    panel.first = grid (), xaxt = "n")
axis(1, at = 0:12/12, labels=0:12)
pacf(diff_visitors, 12, main = "PACF for Tourist Arrivals", 
     panel.first = grid (), xaxt = "n")
axis(1, at = 0:12/12, labels=0:12)

#Non-seasonal Differencing 
diff2_visitors <- diff(diff_visitors, differences = 1)

plot(diff2_visitors, pch = 16, panel.first = grid ())

adf.test(diff2_visitors)

acf(diff2_visitors, 12, main = "ACF for Tourist Arrivals", 
    panel.first = grid (), xaxt = "n")
axis(1, at = 0:12/12, labels=0:12)
pacf(diff2_visitors, 12, main = "PACF for Tourist Arrivals", 
     panel.first = grid (), xaxt = "n")
axis(1, at = 0:12/12, labels=0:12)


#Model Comparison
auto_model <- auto.arima(visitors_ts, d=1, max.p=1, max.q=1, stationary = FALSE, 
                         seasonal = TRUE, stepwise = FALSE, approximation =  FALSE)
summary(auto_model)

model1_sarima <- arima(visitors_ts, order = c(1, 1, 1),
                        seas = list(order = c(1, 1, 1), 12))
summary(model1_sarima)

model2_sarima <- arima(visitors_ts, order = c(0, 1, 1),
                       seas = list(order = c(1, 1, 1), 12))
summary(model2_sarima)

model3_sarima <- arima(visitors_ts, order = c(0, 1, 0),
                       seas = list(order = c(1, 1, 1), 12))
summary(model3_sarima)

model4_sarima <- arima(visitors_ts, order = c(1, 1, 0),
                       seas = list(order = c(1, 1, 1), 12))
summary(model4_sarima)

#Model Comparison in terms of MAPE
fitted1 <- fitted(auto_model)
MAPE1 <- mape(fitted1,visitors_ts)
MAPE1
fitted2 <- fitted(model1_sarima)
MAPE2 <- mape(fitted2,visitors_ts)
MAPE2
fitted3 <- fitted(model2_sarima)
MAPE3 <- mape(fitted3,visitors_ts)
MAPE3
fitted4 <- fitted(model3_sarima)
MAPE4 <- mape(fitted4,visitors_ts)
MAPE4
fitted5 <- fitted(model4_sarima)
MAPE5 <- mape(fitted5,visitors_ts)
MAPE5

#Test for White Noise
Box.test(model1_sarima$residuals, lag = 1, type = "Ljung-Box")

#Check for Normality of Residuals
checkresiduals(model1_sarima)
jarque.bera.test(model1_sarima$residuals)

#Forecasting 
fitted <- fitted(model1_sarima)
fitted
forecast.IPD <- forecast(model1_sarima,h=60) 
autoplot(visitors_ts, series="Actual") + autolayer(forecast.IPD, series="Forecast") + autolayer(fitted, series="Fitted") + ylab("Number of Visitors") + xlab("Month")
forecast.IPD
IPDMAPE <- mape(fitted,visitors_ts)
IPDMAPE

forecast_values <- forecast.IPD$mean
forecast_values
forecast <- read_excel("C:/Users/d/Downloads/Forecast.xlsx")
forecast_ts <- ts(forecast, start = c(2024, 3), frequency = 12)
autoplot(forecast_ts)+ ylab("Estimated Number of Visitors") + xlab("Month")

#Test for Heteroscedasticity
library(nortsTest)
arch.test(model1_sarima$residuals, arch = c("box"))

