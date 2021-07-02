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


forecast_horizon <- 6

# Building training and testing sets
nrows <- nrow(construction) - 13
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
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  etsfit_industry <- ets(ti)
  test <- forecast(ti, model = etsfit_industry, use.initial.values=TRUE, h = forecast_horizon)
  acc_ets_industry <- rbind(acc_ets_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(test$mean)))
}

acc_ets_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
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
Cseq <- seq(from=0.01, to=10, by = 0.01)
wseq <- seq(from = 2, to = 10)
lc <- length(Cseq)
lw <- length(wseq)
minn = vector(mode = 'numeric', length = lc * lw)
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(tc, n = n, type="Abbasov-Mamedova", show.complete = FALSE)
  k = 1
  for(x in Cseq) {
  	for ( y in wseq) {
  		pred <- fuzzy.ts2(tc, n = n , C = x, w = y, type = "Abbasov-Mamedova", forecast = forecast_horizon)
  		minn[k] <- av.res(as.data.frame(construction_fit[,i+1]), as.data.frame(pred$interpolate))[6]
  		k = k+1
  	}
  }
  index <- which.min(minn)
  iC <- floor((index-1)/lw) + 1
  iw <- index - (index / lw) * lw + 1
  pred <- fuzzy.ts2(tc, n = n , C = Cseq[iC], w = wseq[iw], type = "Abbasov-Mamedova", forecast = forecast_horizon)  
  acc_AM_construction <- rbind(acc_AM_construction, av.res(as.data.frame(construction_test[,i+1]), as.data.frame(pred$forecast)))
  cat(paste0('construction AM i =  ', i, '\n'))
}


acc_AM_industry <- data.frame()
for(i in 1:29) {
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(ti,n = n, type="Abbasov-Mamedova", show.complete = FALSE)
  k = 1
  for(x in Cseq) {
  	for ( y in wseq) {
  		pred <- fuzzy.ts2(ti, n = n , C = x, w = y, type = "Abbasov-Mamedova", forecast = forecast_horizon)
  		minn[k] <- av.res(as.data.frame(industry_fit[,i+1]), as.data.frame(pred$interpolate))[6]
  		k = k+1
  	}
  }
  index <- which.min(minn)
  iC <- floor((index-1)/lw) + 1
  iw <- index - (index / lw) * lw + 1
  pred <- fuzzy.ts2(ti, n = n , C = Cseq[iC], w = wseq[iw], type = "Abbasov-Mamedova", forecast = forecast_horizon)  
  acc_AM_industry <- rbind(acc_AM_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(pred$forecast)))
  cat(paste0('industry AM i =  ', i, '\n'))
}


acc_AM_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(tr,n = n, type="Abbasov-Mamedova", show.complete = FALSE)
  k = 1
  for(x in Cseq) {
  	for ( y in wseq) {
  		pred <- fuzzy.ts2(tr, n = n , C = x, w = y, type = "Abbasov-Mamedova", forecast = forecast_horizon)
  		minn[k] <- av.res(as.data.frame(retail_fit[,i+1]), as.data.frame(pred$interpolate))[6]
  		k = k+1
  	}
  }
  index <- which.min(minn)
  iC <- floor((index-1)/lw) + 1
  iw <- index - (index / lw) * lw + 1
  pred <- fuzzy.ts2(tr, n = n , C = Cseq[iC], w = wseq[iw], type = "Abbasov-Mamedova", forecast = forecast_horizon)  
  acc_AM_retail <- rbind(acc_AM_retail, av.res(as.data.frame(retail_test[,i+1]), as.data.frame(pred$forecast)))
  cat(paste0('retail AM i =  ', i, '\n'))
}

# NFTS
acc_NFTS_construction <- data.frame()
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(tc, n = n, type="NFTS", show.complete = FALSE)
  k = 1
  for(x in Cseq) {
  	for ( y in wseq) {
  		pred <- fuzzy.ts2(tc, n = n , C = x, w = y, type = "NFTS", forecast = forecast_horizon)
  		minn[k] <- av.res(as.data.frame(construction_fit[,i+1]), as.data.frame(pred$interpolate))[6]
  		k = k+1
  	}
  }
  index <- which.min(minn)
  iC <- floor((index-1)/lw) + 1
  iw <- index - (index / lw) * lw + 1
  pred <- fuzzy.ts2(tc, n = n , C = Cseq[iC], w = wseq[iw], type = "NFTS", forecast = forecast_horizon)  
  acc_NFTS_construction <- rbind(acc_NFTS_construction, av.res(as.data.frame(construction_test[,i+1]), as.data.frame(pred$forecast)))
  cat(paste0('construction NFTS i =  ', i, '\n'))
}


acc_NFTS_industry <- data.frame()
for(i in 1:29) {
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(ti,n = n, type="NFTS", show.complete = FALSE)
  k = 1
  for(x in Cseq) {
  	for ( y in wseq) {
  		pred <- fuzzy.ts2(ti, n = n , C = x, w = y, type = "NFTS", forecast = forecast_horizon)
  		minn[k] <- av.res(as.data.frame(industry_fit[,i+1]), as.data.frame(pred$interpolate))[6]
  		k = k+1
  	}
  }
  index <- which.min(minn)
  iC <- floor((index-1)/lw) + 1
  iw <- index - (index / lw) * lw + 1
  pred <- fuzzy.ts2(ti, n = n , C = Cseq[iC], w = wseq[iw], type = "NFTS", forecast = forecast_horizon)
  acc_NFTS_industry <- rbind(acc_NFTS_industry, av.res(as.data.frame(industry_test[,i+1]), as.data.frame(pred$forecast)))
  cat(paste0('industry NFTS i =  ', i, '\n'))
}


acc_NFTS_retail <- data.frame()
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
  #C1 <- DOC(tr,n = n, type="NFTS", show.complete = FALSE)
  k = 1
  for(x in Cseq) {
  	for ( y in wseq) {
    	pred <- fuzzy.ts2(tr, n = n , C = x, w = y, type = "NFTS", forecast = forecast_horizon)
    	minn[k] <- av.res(as.data.frame(retail_fit[,i+1]), as.data.frame(pred$interpolate))[6]
    	k = k+1
  	}
  }
  index <- which.min(minn)
  iC <- floor((index-1)/lw) + 1
  iw <- index - (index / lw) * lw + 1
  pred <- fuzzy.ts2(tr, n = n , C = Cseq[iC], w = wseq[iw], type = "NFTS", forecast = forecast_horizon)
  acc_NFTS_retail <- rbind(acc_NFTS_retail, av.res(as.data.frame(retail_test[,i+1]), as.data.frame(pred$forecast)))
  cat(paste0('retail NFTS i =  ', i, '\n'))
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

# Build result tables

constr_table <- data.frame(row.names = seq(1,20))
constr_table <- cbind(constr_table, acc_AM_construction$MAE)
constr_table <- cbind(constr_table, acc_AM_construction$MAPE)
constr_table <- cbind(constr_table, acc_AM_construction$RMSE)

constr_table <- cbind(constr_table, acc_NFTS_construction$MAE)
constr_table <- cbind(constr_table, acc_NFTS_construction$MAPE)
constr_table <- cbind(constr_table, acc_NFTS_construction$RMSE)

constr_table <- cbind(constr_table, acc_ets_construction$MAE)
constr_table <- cbind(constr_table, acc_ets_construction$MAPE)
constr_table <- cbind(constr_table, acc_ets_construction$RMSE)

constr_table <- cbind(constr_table, acc_ARIMA_construction$MAE)
constr_table <- cbind(constr_table, acc_ARIMA_construction$MAPE)
constr_table <- cbind(constr_table, acc_ARIMA_construction$RMSE)

industry_table <- data.frame(row.names = seq(1,29))
industry_table <- cbind(industry_table, acc_AM_industry$MAE)
industry_table <- cbind(industry_table, acc_AM_industry$MAPE)
industry_table <- cbind(industry_table, acc_AM_industry$RMSE)

industry_table <- cbind(industry_table, acc_NFTS_industry$MAE)
industry_table <- cbind(industry_table, acc_NFTS_industry$MAPE)
industry_table <- cbind(industry_table, acc_NFTS_industry$RMSE)

industry_table <- cbind(industry_table, acc_ets_industry$MAE)
industry_table <- cbind(industry_table, acc_ets_industry$MAPE)
industry_table <- cbind(industry_table, acc_ets_industry$RMSE)

industry_table <- cbind(industry_table, acc_ARIMA_industry$MAE)
industry_table <- cbind(industry_table, acc_ARIMA_industry$MAPE)
industry_table <- cbind(industry_table, acc_ARIMA_industry$RMSE)

retail_table <- data.frame(row.names = seq(1:28))
retail_table <- cbind(retail_table, acc_AM_retail$MAE)
retail_table <- cbind(retail_table, acc_AM_retail$MAPE)
retail_table <- cbind(retail_table, acc_AM_retail$RMSE)

retail_table <- cbind(retail_table, acc_NFTS_retail$MAE)
retail_table <- cbind(retail_table, acc_NFTS_retail$MAPE)
retail_table <- cbind(retail_table, acc_NFTS_retail$RMSE)

retail_table <- cbind(retail_table, acc_ets_retail$MAE)
retail_table <- cbind(retail_table, acc_ets_retail$MAPE)
retail_table <- cbind(retail_table, acc_ets_retail$RMSE)

retail_table <- cbind(retail_table, acc_ARIMA_retail$MAE)
retail_table <- cbind(retail_table, acc_ARIMA_retail$MAPE)
retail_table <- cbind(retail_table, acc_ARIMA_retail$RMSE)


N_constr <- data.frame(row.names = seq(1:1))
cN_MAE_AM_MAE_ETS <- sum(constr_table$`acc_AM_construction$MAE`<=constr_table$`acc_ets_construction$MAE`)
cN_MAPE_AM_MAPE_ETS <- sum(constr_table$`acc_AM_construction$MAPE`<=constr_table$`acc_ets_construction$MAPE`)
cN_RMSE_AM_RMSE_ETS <- sum(constr_table$`acc_AM_construction$RMSE`<=constr_table$`acc_ets_construction$RMSE`)

cN_MAE_AM_MAE_ARIMA <- sum(constr_table$`acc_AM_construction$MAE`<=constr_table$`acc_ARIMA_construction$MAE`)
cN_MAPE_AM_MAPE_ARIMA <- sum(constr_table$`acc_AM_construction$MAPE`<=constr_table$`acc_ARIMA_construction$MAPE`)
cN_RMSE_AM_RMSE_ARIMA <- sum(constr_table$`acc_AM_construction$RMSE`<=constr_table$`acc_ARIMA_construction$RMSE`)

cN_MAE_NFTS_MAE_ETS <- sum(constr_table$`acc_NFTS_construction$MAE`<=constr_table$`acc_ets_construction$MAE`)
cN_MAPE_NFTS_MAPE_ETS <- sum(constr_table$`acc_NFTS_construction$MAPE`<=constr_table$`acc_ets_construction$MAPE`)
cN_RMSE_NFTS_RMSE_ETS <- sum(constr_table$`acc_NFTS_construction$RMSE`<=constr_table$`acc_ets_construction$RMSE`)

cN_MAE_NFTS_MAE_ARIMA <- sum(constr_table$`acc_NFTS_construction$MAE`<=constr_table$`acc_ARIMA_construction$MAE`)
cN_MAPE_NFTS_MAPE_ARIMA <- sum(constr_table$`acc_NFTS_construction$MAPE`<=constr_table$`acc_ARIMA_construction$MAPE`)
cN_RMSE_NFTS_RMSE_ARIMA <- sum(constr_table$`acc_NFTS_construction$RMSE`<=constr_table$`acc_ARIMA_construction$RMSE`)

N_industry <- data.frame(row.names = seq(1:1))
iN_MAE_AM_MAE_ETS <- sum(industry_table$`acc_AM_industry$MAE`<= industry_table$`acc_ets_industry$MAE`)
iN_MAPE_AM_MAPE_ETS <- sum(industry_table$`acc_AM_industry$MAPE`<=industry_table$`acc_ets_industry$MAPE`)
iN_RMSE_AM_RMSE_ETS <- sum(industry_table$`acc_AM_industry$RMSE`<=industry_table$`acc_ets_industry$RMSE`)

iN_MAE_AM_MAE_ARIMA <- sum(industry_table$`acc_AM_industry$MAE`<=industry_table$`acc_ARIMA_industry$MAE`)
iN_MAPE_AM_MAPE_ARIMA <- sum(industry_table$`acc_AM_industry$MAPE`<=industry_table$`acc_ARIMA_industry$MAPE`)
iN_RMSE_AM_RMSE_ARIMA <- sum(industry_table$`acc_AM_industry$RMSE`<=industry_table$`acc_ARIMA_industry$RMSE`)

iN_MAE_NFTS_MAE_ETS <- sum(industry_table$`acc_NFTS_industry$MAE`<=industry_table$`acc_ets_industry$MAE`)
iN_MAPE_NFTS_MAPE_ETS <- sum(industry_table$`acc_NFTS_industry$MAPE`<=industry_table$`acc_ets_industry$MAPE`)
iN_RMSE_NFTS_RMSE_ETS <- sum(industry_table$`acc_NFTS_industry$RMSE`<=industry_table$`acc_ets_industry$RMSE`)

iN_MAE_NFTS_MAE_ARIMA <- sum(industry_table$`acc_NFTS_industry$MAE`<=industry_table$`acc_ARIMA_industry$MAE`)
iN_MAPE_NFTS_MAPE_ARIMA <- sum(industry_table$`acc_NFTS_industry$MAPE`<=industry_table$`acc_ARIMA_industry$MAPE`)
iN_RMSE_NFTS_RMSE_ARIMA <- sum(industry_table$`acc_NFTS_industry$RMSE`<=industry_table$`acc_ARIMA_industry$RMSE`)



N_retail <- data.frame(row.names = seq(1:1))
rN_MAE_AM_MAE_ETS <- sum(retail_table$`acc_AM_retail$MAE`<= retail_table$`acc_ets_retail$MAE`)
rN_MAPE_AM_MAPE_ETS <- sum(retail_table$`acc_AM_retail$MAPE`<=retail_table$`acc_ets_retail$MAPE`)
rN_RMSE_AM_RMSE_ETS <- sum(retail_table$`acc_AM_retail$RMSE`<=retail_table$`acc_ets_retail$RMSE`)

rN_MAE_AM_MAE_ARIMA <- sum(retail_table$`acc_AM_retail$MAE`<=retail_table$`acc_ARIMA_retail$MAE`)
rN_MAPE_AM_MAPE_ARIMA <- sum(retail_table$`acc_AM_retail$MAPE`<=retail_table$`acc_ARIMA_retail$MAPE`)
rN_RMSE_AM_RMSE_ARIMA <- sum(retail_table$`acc_AM_retail$RMSE`<=retail_table$`acc_ARIMA_retail$RMSE`)

rN_MAE_NFTS_MAE_ETS <- sum(retail_table$`acc_NFTS_retail$MAE`<=retail_table$`acc_ets_retail$MAE`)
rN_MAPE_NFTS_MAPE_ETS <- sum(retail_table$`acc_NFTS_retail$MAPE`<=retail_table$`acc_ets_retail$MAPE`)
rN_RMSE_NFTS_RMSE_ETS <- sum(retail_table$`acc_NFTS_retail$RMSE`<=retail_table$`acc_ets_retail$RMSE`)

rN_MAE_NFTS_MAE_ARIMA <- sum(retail_table$`acc_NFTS_retail$MAE`<=retail_table$`acc_ARIMA_retail$MAE`)
rN_MAPE_NFTS_MAPE_ARIMA <- sum(retail_table$`acc_NFTS_retail$MAPE`<=retail_table$`acc_ARIMA_retail$MAPE`)
rN_RMSE_NFTS_RMSE_ARIMA <- sum(retail_table$`acc_NFTS_retail$RMSE`<=retail_table$`acc_ARIMA_retail$RMSE`)

