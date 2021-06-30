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


forecast_horizon <- 5

# Building training and testing sets
nrows <- nrow(construction)
construction_fit <- construction[1:(nrows - forecast_horizon),]
industry_fit <- industry[1:(nrows - forecast_horizon),]
retail_fit <- retail[1:(nrows - forecast_horizon),]


construction_test <- construction[(nrows - forecast_horizon + 1 ):nrows,]
industry_test <- industry[(nrows - forecast_horizon + 1 ):nrows,]
retail_test <- retail[(nrows - forecast_horizon + 1 ):nrows,]

# Exponential smoothing
acc_ets_construction <- data.frame()
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  etsfit_construction <- ets(tc)
  test <- forecast(tc, model = etsfit_construction, use.initial.values=TRUE, h = forecast_horizon)
  acc_ets_construction <- rbind(acc_ets_construction, av.res(as.data.frame(construction_test[,i+1]), as.data.frame(test$mean)))
}


acc_ets_industry <- data.frame()
for(i in 1:29) {
  ti <- ts(industry[,i+1], frequency = 12, start = c(2017,1))
  etsfit_industry <- ets(ti)
  test <- forecast(ti, model = etsfit_industry, use.initial.values=TRUE, h = forecast_horizon)
  acc_ets_industry <- rbind(acc_ets_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(test$mean)))
}

acc_ets_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail[,i+1], frequency = 12, start = c(2017,1))
  etsfit_retail <- ets(tr)
  test <- forecast(tr, model = etsfit_retail, use.initial.values=TRUE, h = forecast_horizon)
  acc_ets_retail <- rbind(acc_ets_retail, av.res(as.data.frame(retail_test[,i+1]), as.data.frame(test$mean)))
}

# ARIMA
acc_ARIMA_construction <- data.frame()
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_construction <- auto.arima(tc)
  test <- forecast(tc, model = ARIMAfit_construction, use.initial.values=TRUE, h=forecast_horizon)
  acc_ARIMA_construction <- rbind(acc_ARIMA_construction, av.res(as.data.frame(construction_test[,i+1]), as.data.frame(test$mean)))
}

acc_ARIMA_industry <- data.frame()
for(i in 1:29) {
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_industry <- auto.arima(ti)
  test <- forecast(ti, model = ARIMAfit_industry, use.initial.values=TRUE, h=forecast_horizon)
  acc_ARIMA_industry <- rbind(acc_ARIMA_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(test$mean)))
}

acc_ARIMA_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_retail <- auto.arima(tr)
  test <- forecast(tr, model = ARIMAfit_retail, use.initial.values=TRUE, h=forecast_horizon)
  acc_ARIMA_retail <- rbind(acc_ARIMA_retail, av.res(as.data.frame(retail_test[,i+1]), as.data.frame(test$mean)))
}


## Fuzzy TS forecast
n <- round(1 + 3.3 * log10(46))
# Abasov- Mamedova
acc_AM_construction <- data.frame()
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(tc, n = n, type="Abbasov-Mamedova", show.complete = FALSE)
  Cseq <- seq(from=0.01, to=10, by = 0.01)
  minn = vector(mode = 'numeric', length = length(Cseq))
  k = 1
  for(x in Cseq) {
    pred <- fuzzy.ts2(tc, n = n , C = x, type = "Abbasov-Mamedova", forecast = forecast_horizon)
    minn[k] <- av.res(as.data.frame(construction_test[,i+1]), as.data.frame(pred$forecast))[6]
    k = k+1
  }
  index <- which.min(minn)
  pred <- fuzzy.ts2(tc, n = n ,C = Cseq[index], type = "Abbasov-Mamedova", forecast = forecast_horizon)
  acc_AM_construction <- rbind(acc_AM_construction, av.res(as.data.frame(construction_test[,i+1]), as.data.frame(pred$forecast)))
}


acc_AM_industry <- data.frame()
for(i in 1:29) {
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(ti,n = n, type="Abbasov-Mamedova", show.complete = FALSE)
  Cseq <- seq(from=0.01, to=10, by = 0.01)
  minn = vector(mode = 'numeric', length = length(Cseq))
  k = 1
  for(x in Cseq) {
    pred <- fuzzy.ts2(ti, n = n , C = x, type = "Abbasov-Mamedova", forecast = forecast_horizon)
    minn[k] <- av.res(as.data.frame(industry_test[,i+1]), as.data.frame(pred$forecast))[6]
    k = k+1
  }
  index <- which.min(minn)
  pred <- fuzzy.ts2(ti, n = n , C = Cseq[index], type = "Abbasov-Mamedova", forecast = forecast_horizon)
  acc_AM_industry <- rbind(acc_AM_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(pred$forecast)))
}


acc_AM_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(tr,n = n, type="Abbasov-Mamedova", show.complete = FALSE)
  Cseq <- seq(from=0.01, to=10, by = 0.01)
  minn = vector(mode = 'numeric', length = length(Cseq))
  k = 1
  for(x in Cseq) {
    pred <- fuzzy.ts2(tr, n = n , C = x, type = "Abbasov-Mamedova",forecast = forecast_horizon)
    minn[k] <- av.res(as.data.frame(retail_test[,i+1]), as.data.frame(pred$forecast))[6]
    k = k+1
  }
  index <- which.min(minn)
  pred <- fuzzy.ts2(tr, n = n , C = Cseq[index], type = "Abbasov-Mamedova",forecast = forecast_horizon)
  acc_AM_retail <- rbind(acc_AM_retail, av.res(as.data.frame(retail_test[,i+1]), as.data.frame(pred$forecast)))
}

# NFTS
acc_NFTS_construction <- data.frame()
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(tc, n = n, type="NFTS", show.complete = FALSE)
  Cseq <- seq(from=0.01, to=10, by = 0.01)
  minn = vector(mode = 'numeric', length = length(Cseq))
  k = 1
  for(x in Cseq) {
    pred <- fuzzy.ts2(tc, n = n , C = x, type = "NFTS",forecast = forecast_horizon)
    minn[k] <- av.res(as.data.frame(construction_test[,i+1]), as.data.frame(pred$forecast))[6]
    k = k+1
  }
  index <- which.min(minn)
  pred <- fuzzy.ts2(tc, n = n ,C = Cseq[index], type = "NFTS",forecast = forecast_horizon)
  acc_NFTS_construction <- rbind(acc_NFTS_construction, av.res(as.data.frame(construction_test[,i+1]), as.data.frame(pred$forecast)))
}


acc_NFTS_industry <- data.frame()
for(i in 1:29) {
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(ti,n = n, type="NFTS", show.complete = FALSE)
  Cseq <- seq(from=0.01, to=10, by = 0.01)
  minn = vector(mode = 'numeric', length = length(Cseq))
  k = 1
  for(x in Cseq) {
    pred <- fuzzy.ts2(ti, n = n , C = x, type = "NFTS",forecast = forecast_horizon)
    minn[k] <- av.res(as.data.frame(industry_test[,i+1]), as.data.frame(pred$forecast))[6]
    k = k+1
  }
  index <- which.min(minn)
  pred <- fuzzy.ts2(ti, n = n , C = Cseq[index], type = "NFTS", forecast = forecast_horizon)
  acc_NFTS_industry <- rbind(acc_NFTS_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(pred$forecast)))
}


acc_NFTS_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(tr,n = n, type="NFTS", show.complete = FALSE)
  Cseq <- seq(from=0.01, to=10, by = 0.01)
  minn = vector(mode = 'numeric', length = length(Cseq))
  k = 1
  for(x in Cseq) {
    pred <- fuzzy.ts2(tr, n = n , C = x, type = "NFTS", forecast = forecast_horizon)
    minn[k] <- av.res(as.data.frame(retail_test[,i+1]), as.data.frame(pred$forecast))[6]
    k = k+1
  }
  index <- which.min(minn)
  pred <- fuzzy.ts2(tr, n = n , C = Cseq[index], type = "NFTS", forecast = forecast_horizon)
  acc_NFTS_retail <- rbind(acc_NFTS_retail, av.res(as.data.frame(retail_test[,i+1]), as.data.frame(pred$forecast)))
}

# Save results
write.csv2(acc_AM_construction, file = paste0(forecast_horizon,'acc_AM_construction.csv'))
write.csv2(acc_AM_industry, file = paste0(forecast_horizon,'acc_AM_industry.csv'))
write.csv2(acc_AM_retail, file = paste0(forecast_horizon, 'acc_AM_retail.csv'))

write.csv2(acc_NFTS_construction, file = paste0(forecast_horizon, 'acc_NFTS_construction.csv'))
write.csv2(acc_NFTS_industry, file = paste0(forecast_horizon,'acc_NFTS_industry.csv'))
write.csv2(acc_NFTS_retail, file = paste0(forecast_horizon, 'acc_NFTS_retail.csv'))


write.csv2(acc_ARIMA_construction, file = paste0(forecast_horizon, 'acc_ARIMA_construction.csv'))
write.csv2(acc_ARIMA_industry, file = paste0(forecast_horizon,'acc_ARIMA_industry.csv'))
write.csv2(acc_ARIMA_retail, file = paste0(forecast_horizon, 'acc_ARIMA_retail.csv'))

write.csv2(acc_ets_construction, file = paste0(forecast_horizon, 'acc_ets_construction.csv'))
write.csv2(acc_ets_industry, file = paste0(forecast_horizon, 'acc_ets_industry.csv'))
write.csv2(acc_ets_retail, file = paste0(forecast_horizon, 'acc_ets_retail.csv'))


#ME
N_constrAMgtETS_ME = sum(acc_AM_construction[,1] < acc_ets_construction[,1])
N_constrAMgtARIMA_ME = sum(acc_AM_construction[,1] < acc_ARIMA_construction[,1])
N_constrNFTSgtETS_ME = sum(acc_NFTS_construction[,1] < acc_ets_construction[,1])
N_constrNFTSgtARIMA_ME = sum(acc_NFTS_construction[1,] < acc_ARIMA_construction[,1])

N_indAMgtETS_ME = sum(acc_AM_industry[1,] < acc_ets_industry[,1])
N_indAMgtARIMA_ME = sum(acc_AM_industry[1,] < acc_ARIMA_industry[,1])
N_indNFTSgtETS_ME = sum(acc_NFTS_industry[1,] < acc_ets_industry[,1])
N_indNFTSgtARIMA_ME = sum(acc_NFTS_industry[,1] < acc_ARIMA_industry[,1])

N_retailAMgtETS_ME = sum(acc_AM_retail[1,] < acc_ets_retail[,1])
N_retailAMgtARIMA_ME = sum(acc_AM_retail[1,] < acc_ARIMA_retail[,1])
N_retailNFTSgtETS_ME = sum(acc_NFTS_retail[,1] < acc_ets_retail[,1])
N_retailNFTSgtARIMA_ME = sum(acc_NFTS_retail[1,] < acc_ARIMA_retail[,1])



# MAE
N_constrAMgtETS_MAE = sum(acc_AM_construction[,2] < acc_ets_construction[,2])
N_constrAMgtARIMA_MAE = sum(acc_AM_construction[,2] < acc_ARIMA_construction[,2])
N_constrNFTSgtETS_MAE = sum(acc_NFTS_construction[,2] < acc_ets_construction[,2])
N_constrNFTSgtARIMA_MAE = sum(acc_NFTS_construction[,2] < acc_ARIMA_construction[,2])

N_indAMgtETS_MAE = sum(acc_AM_industry[,2] < acc_ets_industry[,2])
N_indAMgtARIMA_MAE = sum(acc_AM_industry[,2] < acc_ARIMA_industry[,2])
N_indNFTSgtETS_MAE = sum(acc_NFTS_industry[,2] < acc_ets_industry[,2])
N_indNFTSgtARIMA_MAE = sum(acc_NFTS_industry[ ,2]< acc_ARIMA_industry[,2])

N_retailAMgtETS_MAE = sum(acc_AM_retail[,2] < acc_ets_retail[,2])
N_retailAMgtARIMA_MAE = sum(acc_AM_retail[,2] < acc_ARIMA_retail[,2])
N_retailNFTSgtETS_MAE = sum(acc_NFTS_retail[,2] < acc_ets_retail[,2])
N_retailNFTSgtARIMA_MAE = sum(acc_NFTS_retail[,2] < acc_ARIMA_retail[,2])

#MPE
N_constrAMgtETS_MPE = sum(acc_AM_construction[,3] < acc_ets_construction[,3])
N_constrAMgtARIMA_MPE = sum(acc_AM_construction[,3] < acc_ARIMA_construction[,3])
N_constrNFTSgtETS_MPE = sum(acc_NFTS_construction[,3] < acc_ets_construction[,3])
N_constrNFTSgtARIMA_MPE = sum(acc_NFTS_construction[,3] < acc_ARIMA_construction[,3])

N_indAMgtETS_MPE = sum(acc_AM_industry[,3] < acc_ets_industry[,3])
N_indAMgtARIMA_MPE = sum(acc_AM_industry[,3] < acc_ARIMA_industry[,3])
N_indNFTSgtETS_MPE = sum(acc_NFTS_industry[,3] < acc_ets_industry[,3])
N_indNFTSgtARIMA_MPE = sum(acc_NFTS_industry[,3] < acc_ARIMA_industry[,3])

N_retailAMgtETS_MPE = sum(acc_AM_retail[,3] < acc_ets_retail[,3])
N_retailAMgtARIMA_MPE = sum(acc_AM_retail[,3] < acc_ARIMA_retail[,3])
N_retailNFTSgtETS_MPE = sum(acc_NFTS_retail[,3] < acc_ets_retail[,3])
N_retailNFTSgtARIMA_MPE = sum(acc_NFTS_retail[,3] < acc_ARIMA_retail[,3])

#MAPE
N_constrAMgtETS_MAPE = sum(acc_AM_construction[,4] < acc_ets_construction[,4])
N_constrAMgtARIMA_MAPE = sum(acc_AM_construction[,4] < acc_ARIMA_construction[,4])
N_constrNFTSgtETS_MAPE = sum(acc_NFTS_construction[,4] < acc_ets_construction[,4])
N_constrNFTSgtARIMA_MAPE = sum(acc_NFTS_construction[,4] < acc_ARIMA_construction[,4])

N_indAMgtETS_MAPE = sum(acc_AM_industry[,4] < acc_ets_industry[,4])
N_indAMgtARIMA_MAPE = sum(acc_AM_industry[,4] < acc_ARIMA_industry[,4])
N_indNFTSgtETS_MAPE = sum(acc_NFTS_industry[,4] < acc_ets_industry[,4])
N_indNFTSgtARIMA_MAPE = sum(acc_NFTS_industry[,4] < acc_ARIMA_industry[,4])

N_retailAMgtETS_MAPE = sum(acc_AM_retail[,4] < acc_ets_retail[,4])
N_retailAMgtARIMA_MAPE = sum(acc_AM_retail[,4] < acc_ARIMA_retail[,4])
N_retailNFTSgtETS_MAPE = sum(acc_NFTS_retail[,4] < acc_ets_retail[,4])
N_retailNFTSgtARIMA_MAPE = sum(acc_NFTS_retail[,4] < acc_ARIMA_retail[,4])

#MSE
N_constrAMgtETS_MSE = sum(acc_AM_construction[,5] < acc_ets_construction[,5])
N_constrAMgtARIMA_MSE = sum(acc_AM_construction[,5] < acc_ARIMA_construction[,5])
N_constrNFTSgtETS_MSE = sum(acc_NFTS_construction[,5] < acc_ets_construction[,5])
N_constrNFTSgtARIMA_MSE = sum(acc_NFTS_construction[,5] < acc_ARIMA_construction[,5])

N_indAMgtETS_MSE = sum(acc_AM_industry[,5] < acc_ets_industry[,5])
N_indAMgtARIMA_MSE = sum(acc_AM_industry[,5] < acc_ARIMA_industry[,5])
N_indNFTSgtETS_MSE = sum(acc_NFTS_industry[,5] < acc_ets_industry[,5])
N_indNFTSgtARIMA_MSE = sum(acc_NFTS_industry[,5] < acc_ARIMA_industry[,5])

N_retailAMgtETS_MSE = sum(acc_AM_retail[,5] < acc_ets_retail[,5])
N_retailAMgtARIMA_MSE = sum(acc_AM_retail[,5] < acc_ARIMA_retail[,5])
N_retailNFTSgtETS_MSE = sum(acc_NFTS_retail[,5]< acc_ets_retail[,5])
N_retailNFTSgtARIMA_MSE = sum(acc_NFTS_retail[,5] < acc_ARIMA_retail[,5])

#RMSE
N_constrAMgtETS_RMSE = sum(acc_AM_construction[,6] < acc_ets_construction[,6])
N_constrAMgtARIMA_RMSE = sum(acc_AM_construction[,6] < acc_ARIMA_construction[,6])
N_constrNFTSgtETS_RMSE = sum(acc_NFTS_construction[,6] < acc_ets_construction[,6])
N_constrNFTSgtARIMA_RMSE = sum(acc_NFTS_construction[,6] < acc_ARIMA_construction[,6])

N_indAMgtETS_RMSE = sum(acc_AM_industry[,6] < acc_ets_industry[,6])
N_indAMgtARIMA_RMSE = sum(acc_AM_industry[,6] < acc_ARIMA_industry[,6])
N_indNFTSgtETS_RMSE = sum(acc_NFTS_industry[,6] < acc_ets_industry[,6])
N_indNFTSgtARIMA_RMSE = sum(acc_NFTS_industry[,6] < acc_ARIMA_industry[,6])

N_retailAMgtETS_RMSE = sum(acc_AM_retail[,6] < acc_ets_retail[,6])
N_retailAMgtARIMA_RMSE = sum(acc_AM_retail[,6] < acc_ARIMA_retail[,6])
N_retailNFTSgtETS_RMSE = sum(acc_NFTS_retail[,6] < acc_ets_retail[,6])
N_retailNFTSgtARIMA_RMSE = sum(acc_NFTS_retail[,6] < acc_ARIMA_retail[,6])



