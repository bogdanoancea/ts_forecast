library(AnalyzeTS)
library(forecast)

# Read data
construction <- read.csv("construction.csv", header=TRUE)

# Descriptive statistics
write.csv2(Descriptives(construction), file = 'construction_desc.csv', sep = ',')


# Read data
industry <- read.csv("industry.csv", header=TRUE)

# Descriptive statistics
write.csv2(Descriptives(industry), file = 'industry_desc.csv', sep = ',')

# Read data
retail <- read.csv("retail.csv", header=TRUE)

# Descriptive statistics
write.csv2(Descriptives(retail), file = 'retail_desc.csv', sep = ',')




# Building training and testing sets
construction_fit <- construction[1:46,]
industry_fit <- industry[1:46,]
retail_fit <- retail[1:46,]


construction_test <- construction[47:51,]
industry_test <- industry[47:51,]
retail_test <- retail[47:51,]

# Exponential smoothing
acc_ets_construction <- data.frame()
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  etsfit_construction <- ets(tc)
  test <- forecast(tc, model = etsfit_construction, use.initial.values=TRUE, h = 5)
  acc_ets_construction <- rbind(acc_ets_construction, av.res(as.data.frame(construction_test[,i+1]), as.data.frame(test$mean)))
}


acc_ets_industry <- data.frame()
for(i in 1:29) {
  ti <- ts(industry[,i+1], frequency = 12, start = c(2017,1))
  etsfit_industry <- ets(ti)
  test <- forecast(ti, model = etsfit_industry, use.initial.values=TRUE, h = 5)
  acc_ets_industry <- rbind(acc_ets_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(test$mean)))
}

acc_ets_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail[,i+1], frequency = 12, start = c(2017,1))
  etsfit_retail <- ets(tr)
  test <- forecast(tr, model = etsfit_retail, use.initial.values=TRUE, h = 5)
  acc_ets_retail <- rbind(acc_ets_retail, av.res(as.data.frame(retail_test[,i+1]), as.data.frame(test$mean)))
}

# ARIMA
acc_ARIMA_construction <- data.frame()
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_construction <- auto.arima(tc)
  test <- forecast(tc, model = ARIMAfit_construction, use.initial.values=TRUE, h=5)
  acc_ARIMA_construction <- rbind(acc_ARIMA_construction, av.res(as.data.frame(construction_test[,i+1]), as.data.frame(test$mean)))
}

acc_ARIMA_industry <- data.frame()
for(i in 1:29) {
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_industry <- auto.arima(ti)
  test <- forecast(ti, model = ARIMAfit_industry, use.initial.values=TRUE, h=5)
  acc_ARIMA_industry <- rbind(acc_ARIMA_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(test$mean)))
}

acc_ARIMA_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_retail <- auto.arima(tr)
  test <- forecast(tr, model = ARIMAfit_retail, use.initial.values=TRUE, h=5)
  acc_ARIMA_retail <- rbind(acc_ARIMA_retail, av.res(as.data.frame(retail_test[,i+1]), as.data.frame(test$mean)))
}
