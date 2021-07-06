library(AnalyzeTS)
library(forecast)
library(Metrics)

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



forecast_horizon <- 2

# Building training and testing sets
nrows <- nrow(construction) -13 
construction_fit <- construction[1:(nrows - forecast_horizon),]
industry_fit <- industry[1:(nrows - forecast_horizon),]
retail_fit <- retail[1:(nrows - forecast_horizon),]


construction_test <- construction[(nrows - forecast_horizon + 1 ):nrows,]
industry_test <- industry[(nrows - forecast_horizon + 1 ):nrows,]
retail_test <- retail[(nrows - forecast_horizon + 1 ):nrows,]

desc_constr <- Descriptives(construction[1:nrows,])
desc_ind <- Descriptives(industry[1:nrows,])
desc_retail <- Descriptives(retail[1:nrows,])
write.csv(desc_constr, file = 'constr_desc.csv', sep = ',')
write.csv(desc_ind, file = 'ind_desc.csv', sep = ',')
write.csv(desc_retail, file = 'retail_desc.csv', sep = ',')




# simple Exponential smoothing
acc_ses_construction <- data.frame(0,0,0,0,0,0)
colnames(acc_ses_construction)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  etsfit_construction <- ses(tc)
  test <- forecast(tc, model = etsfit_construction, use.initial.values=TRUE, h = forecast_horizon)
  err <- c(forecast::accuracy(construction_test[,i+1], test$mean)[1:5],Metrics::mase(construction_test[,i+1], test$mean))
  acc_ses_construction <- rbind(acc_ses_construction, err)
}
acc_ses_construction <-acc_ses_construction[-1,]

acc_ses_industry <- data.frame(0,0,0,0,0,0)
colnames(acc_ses_industry)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:29) {
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  etsfit_industry <- ses(ti)
  test <- forecast(ti, model = etsfit_industry, use.initial.values=TRUE, h = forecast_horizon)
  err <- c(forecast::accuracy(industry_test[,i+1], test$mean)[1:5], Metrics::mase(industry_test[,i+1], test$mean))
  acc_ses_industry <- rbind(acc_ses_industry, err)
}
acc_ses_industry <- acc_ses_industry[-1,]

acc_ses_retail <- data.frame(0,0,0,0,0,0)
colnames(acc_ses_retail)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
  etsfit_retail <- ses(tr)
  test <- forecast(tr, model = etsfit_retail, use.initial.values=TRUE, h = forecast_horizon)
  err <- c(forecast::accuracy(retail_test[,i+1], test$mean)[1:5], Metrics::mase(retail_test[,i+1], test$mean))
  acc_ses_retail <- rbind(acc_ses_retail, err)
}
acc_ses_retail <- acc_ses_retail[-1,]

# Holt 
acc_holt_construction <- data.frame(0,0,0,0,0,0)
colnames(acc_holt_construction)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:20) {
	tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
	etsfit_construction <- holt(tc)
	test <- forecast(tc, model = etsfit_construction, use.initial.values=TRUE, h = forecast_horizon)
	err <- c(forecast::accuracy(construction_test[,i+1], test$mean)[1:5], Metrics::mase(construction_test[,i+1], test$mean))
	acc_holt_construction <- rbind(acc_holt_construction, err)
}
acc_holt_construction <- acc_holt_construction[-1,]

acc_holt_industry <- data.frame(0,0,0,0,0,0)
colnames(acc_holt_industry)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:29) {
	ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
	etsfit_industry <- holt(ti)
	test <- forecast(ti, model = etsfit_industry, use.initial.values=TRUE, h = forecast_horizon)
	err <- c(forecast::accuracy(industry_test[,i+1], test$mean)[1:5], Metrics::mase(industry_test[,i+1], test$mean))
	acc_holt_industry <- rbind(acc_holt_industry, err)
}
acc_holt_industry <- acc_holt_industry[-1,]

acc_holt_retail <- data.frame(0,0,0,0,0,0)
colnames(acc_holt_retail)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:28) {
	tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
	etsfit_retail <- holt(tr)
	test <- forecast(tr, model = etsfit_retail, use.initial.values=TRUE, h = forecast_horizon)
	err <- c(forecast::accuracy(retail_test[,i+1], test$mean)[1:5], Metrics::mase(retail_test[,i+1], test$mean))
	acc_holt_retail <- rbind(acc_holt_retail, err)
}
acc_holt_retail <- acc_holt_retail[-1,]


# ARIMA
acc_ARIMA_construction <- data.frame(0,0,0,0,0,0)
colnames(acc_ARIMA_construction)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:20) {
  tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_construction <- auto.arima(tc)
  test <- forecast(tc, model = ARIMAfit_construction, use.initial.values=TRUE, h=forecast_horizon)
  err<-c(forecast::accuracy(construction_test[,i+1], test$mean)[1:5], Metrics::mase(construction_test[,i+1], test$mean))
  acc_ARIMA_construction <- rbind(acc_ARIMA_construction, err)
}
acc_ARIMA_construction <-acc_ARIMA_construction[-1,]

acc_ARIMA_industry <- data.frame(0,0,0,0,0,0)
colnames(acc_ARIMA_industry)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:29) {
  ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_industry <- auto.arima(ti)
  test <- forecast(ti, model = ARIMAfit_industry, use.initial.values=TRUE, h=forecast_horizon)
  err<-c(forecast::accuracy(industry_test[,i+1], test$mean)[1:5], Metrics::mase(industry_test[,i+1], test$mean))
  acc_ARIMA_industry <- rbind(acc_ARIMA_industry, err)
}
acc_ARIMA_industry <-acc_ARIMA_industry[-1,]


acc_ARIMA_retail <- data.frame(0,0,0,0,0,0)
colnames(acc_ARIMA_retail)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:28) {
  tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
  ARIMAfit_retail <- auto.arima(tr)
  test <- forecast(tr, model = ARIMAfit_retail, use.initial.values=TRUE, h=forecast_horizon)
  err<-c(forecast::accuracy(retail_test[,i+1],test$mean)[1:5], Metrics::mase(retail_test[,i+1], test$mean))
  acc_ARIMA_retail <- rbind(acc_ARIMA_retail, err)
}
acc_ARIMA_retail <-  acc_ARIMA_retail[-1,]



#################################Simplify#######################################
## Fuzzy TS forecast
n <- round(1 + 3.3 * log10(46)) 
# Abasov- Mamedova
acc_AM_construction <- data.frame(0,0,0,0,0,0)
colnames(acc_AM_construction)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:20) {
	tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
	C1 <- DOC(tc, n = n, type="Abbasov-Mamedova", show.complete = FALSE)
	pred <- fuzzy.ts2(tc, n = n , C = as.numeric(C1[1]), type = "Abbasov-Mamedova", forecast = forecast_horizon)  
	err<-c(forecast::accuracy(construction_test[,i+1], pred$forecast)[1:5],Metrics::mase(construction_test[,i+1], pred$forecast))
	acc_AM_construction <- rbind(acc_AM_construction, err)
	cat(paste0('construction AM i =  ', i, '\n'))
}
acc_AM_construction <- acc_AM_construction[-1,]

acc_AM_industry <- data.frame(0,0,0,0,0,0)
colnames(acc_AM_industry)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:29) {
	ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
	C1 <- DOC(ti,n = n, type="Abbasov-Mamedova", show.complete = FALSE)
	pred <- fuzzy.ts2(ti, n = n , C = as.numeric(C1[1]), type = "Abbasov-Mamedova", forecast = forecast_horizon)  
	err<-c(forecast::accuracy(industry_test[,i+1], pred$forecast)[1:5], Metrics::mase(industry_test[,i+1], pred$forecast))
	acc_AM_industry <- rbind(acc_AM_industry, err)
	cat(paste0('industry AM i =  ', i, '\n'))
}
acc_AM_industry <- acc_AM_industry[-1,]

acc_AM_retail <- data.frame(0,0,0,0,0,0)
colnames(acc_AM_retail)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:28) {
	tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
	C1 <- DOC(tr,n = n, type="Abbasov-Mamedova", show.complete = FALSE)
	pred <- fuzzy.ts2(tr, n = n , C = as.numeric(C1[1]), type = "Abbasov-Mamedova", forecast = forecast_horizon)  
	err<-c(forecast::accuracy(retail_test[,i+1], pred$forecast)[1:5],Metrics::mase(retail_test[,i+1], pred$forecast))
	acc_AM_retail <- rbind(acc_AM_retail, err)
	cat(paste0('retail AM i =  ', i, '\n'))
}
acc_AM_retail <- acc_AM_retail[-1,]

# NFTS
acc_NFTS_construction <- data.frame(0,0,0,0,0,0)
colnames(acc_NFTS_construction)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:20) {
	tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
	C1 <- DOC(tc, n = n, type="NFTS", show.complete = FALSE)
	pred <- fuzzy.ts2(tc, n = n , C = as.numeric(C1[1]), type = "NFTS", forecast = forecast_horizon)  
	err<-c(forecast::accuracy(construction_test[,i+1], pred$forecast)[1:5], Metrics::mase(construction_test[,i+1], pred$forecast))
	acc_NFTS_construction <- rbind(acc_NFTS_construction, err)
	cat(paste0('construction NFTS i =  ', i, '\n'))
}
acc_NFTS_construction <- acc_NFTS_construction[-1,]

acc_NFTS_industry <- data.frame(0,0,0,0,0,0)
colnames(acc_NFTS_industry)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:29) {
	ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
	C1 <- DOC(ti,n = n, type="NFTS", show.complete = FALSE)
	pred <- fuzzy.ts2(ti, n = n , C = as.numeric(C1[1]), type = "NFTS", forecast = forecast_horizon)
	err<-c(forecast::accuracy(industry_test[,i+1], pred$forecast)[1:5], Metrics::mase(industry_test[,i+1], pred$forecast))
	acc_NFTS_industry <- rbind(acc_NFTS_industry, err)
	cat(paste0('industry NFTS i =  ', i, '\n'))
}
acc_NFTS_industry <- acc_NFTS_industry[-1,]

acc_NFTS_retail <- data.frame(0,0,0,0,0,0)
colnames(acc_NFTS_retail)<-c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE')
for(i in 1:28) {
	tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
	C1 <- DOC(tr,n = n, type="NFTS", show.complete = FALSE)
	pred <- fuzzy.ts2(tr, n = n , C = as.numeric(C1[1]), type = "NFTS", forecast = forecast_horizon)
	err<-c(forecast::accuracy(retail_test[,i+1], pred$forecast)[1:5], Metrics::mase(retail_test[,i+1], pred$forecast))
	acc_NFTS_retail <- rbind(acc_NFTS_retail, err)
	cat(paste0('retail NFTS i =  ', i, '\n'))
}
acc_NFTS_retail <- acc_NFTS_retail[-1,]

##################################################################################
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

write.csv2(acc_ses_construction, file = paste0(forecast_horizon, 'acc_ses_construction.csv'))
write.csv2(acc_ses_industry, file = paste0(forecast_horizon, 'acc_ses_industry.csv'))
write.csv2(acc_ses_retail, file = paste0(forecast_horizon, 'acc_ses_retail.csv'))

write.csv2(acc_holt_construction, file = paste0(forecast_horizon, 'acc_holt_construction.csv'))
write.csv2(acc_holt_industry, file = paste0(forecast_horizon, 'acc_holt_industry.csv'))
write.csv2(acc_holt_retail, file = paste0(forecast_horizon, 'acc_holt_retail.csv'))


# Build result tables

constr_table <- data.frame(row.names = seq(1,20))
constr_table <- cbind(constr_table, acc_AM_construction$MAE)
constr_table <- cbind(constr_table, acc_AM_construction$MAPE)
constr_table <- cbind(constr_table, acc_AM_construction$RMSE)
constr_table <- cbind(constr_table, acc_AM_construction$MASE)

constr_table <- cbind(constr_table, acc_NFTS_construction$MAE)
constr_table <- cbind(constr_table, acc_NFTS_construction$MAPE)
constr_table <- cbind(constr_table, acc_NFTS_construction$RMSE)
constr_table <- cbind(constr_table, acc_NFTS_construction$MASE)

constr_table <- cbind(constr_table, acc_ses_construction$MAE)
constr_table <- cbind(constr_table, acc_ses_construction$MAPE)
constr_table <- cbind(constr_table, acc_ses_construction$RMSE)
constr_table <- cbind(constr_table, acc_ses_construction$MASE)

constr_table <- cbind(constr_table, acc_holt_construction$MAE)
constr_table <- cbind(constr_table, acc_holt_construction$MAPE)
constr_table <- cbind(constr_table, acc_holt_construction$RMSE)
constr_table <- cbind(constr_table, acc_holt_construction$MASE)

constr_table <- cbind(constr_table, acc_ARIMA_construction$MAE)
constr_table <- cbind(constr_table, acc_ARIMA_construction$MAPE)
constr_table <- cbind(constr_table, acc_ARIMA_construction$RMSE)
constr_table <- cbind(constr_table, acc_ARIMA_construction$MASE)


industry_table <- data.frame(row.names = seq(1,29))
industry_table <- cbind(industry_table, acc_AM_industry$MAE)
industry_table <- cbind(industry_table, acc_AM_industry$MAPE)
industry_table <- cbind(industry_table, acc_AM_industry$RMSE)
industry_table <- cbind(industry_table, acc_AM_industry$MASE)

industry_table <- cbind(industry_table, acc_NFTS_industry$MAE)
industry_table <- cbind(industry_table, acc_NFTS_industry$MAPE)
industry_table <- cbind(industry_table, acc_NFTS_industry$RMSE)
industry_table <- cbind(industry_table, acc_NFTS_industry$MASE)

industry_table <- cbind(industry_table, acc_ses_industry$MAE)
industry_table <- cbind(industry_table, acc_ses_industry$MAPE)
industry_table <- cbind(industry_table, acc_ses_industry$RMSE)
industry_table <- cbind(industry_table, acc_ses_industry$MASE)

industry_table <- cbind(industry_table, acc_holt_industry$MAE)
industry_table <- cbind(industry_table, acc_holt_industry$MAPE)
industry_table <- cbind(industry_table, acc_holt_industry$RMSE)
industry_table <- cbind(industry_table, acc_holt_industry$MASE)

industry_table <- cbind(industry_table, acc_ARIMA_industry$MAE)
industry_table <- cbind(industry_table, acc_ARIMA_industry$MAPE)
industry_table <- cbind(industry_table, acc_ARIMA_industry$RMSE)
industry_table <- cbind(industry_table, acc_ARIMA_industry$MASE)

retail_table <- data.frame(row.names = seq(1:28))
retail_table <- cbind(retail_table, acc_AM_retail$MAE)
retail_table <- cbind(retail_table, acc_AM_retail$MAPE)
retail_table <- cbind(retail_table, acc_AM_retail$RMSE)
retail_table <- cbind(retail_table, acc_AM_retail$MASE)

retail_table <- cbind(retail_table, acc_NFTS_retail$MAE)
retail_table <- cbind(retail_table, acc_NFTS_retail$MAPE)
retail_table <- cbind(retail_table, acc_NFTS_retail$RMSE)
retail_table <- cbind(retail_table, acc_NFTS_retail$MASE)

retail_table <- cbind(retail_table, acc_ses_retail$MAE)
retail_table <- cbind(retail_table, acc_ses_retail$MAPE)
retail_table <- cbind(retail_table, acc_ses_retail$RMSE)
retail_table <- cbind(retail_table, acc_ses_retail$MASE)

retail_table <- cbind(retail_table, acc_holt_retail$MAE)
retail_table <- cbind(retail_table, acc_holt_retail$MAPE)
retail_table <- cbind(retail_table, acc_holt_retail$RMSE)
retail_table <- cbind(retail_table, acc_holt_retail$MASE)

retail_table <- cbind(retail_table, acc_ARIMA_retail$MAE)
retail_table <- cbind(retail_table, acc_ARIMA_retail$MAPE)
retail_table <- cbind(retail_table, acc_ARIMA_retail$RMSE)
retail_table <- cbind(retail_table, acc_ARIMA_retail$MASE)


N_constr <- data.frame(row.names = seq(1:1))
cN_MAE_AM_MAE_SES <- sum(constr_table$`acc_AM_construction$MAE`<=constr_table$`acc_ses_construction$MAE`)
cN_MAPE_AM_MAPE_SES <- sum(constr_table$`acc_AM_construction$MAPE`<=constr_table$`acc_ses_construction$MAPE`)
cN_RMSE_AM_RMSE_SES <- sum(constr_table$`acc_AM_construction$RMSE`<=constr_table$`acc_ses_construction$RMSE`)
cN_MASE_AM_MASE_SES <- sum(constr_table$`acc_AM_construction$MASE`<=constr_table$`acc_ses_construction$MASE`)

cN_MAE_AM_MAE_HOLT <- sum(constr_table$`acc_AM_construction$MAE`<=constr_table$`acc_holt_construction$MAE`)
cN_MAPE_AM_MAPE_HOLT <- sum(constr_table$`acc_AM_construction$MAPE`<=constr_table$`acc_holt_construction$MAPE`)
cN_RMSE_AM_RMSE_HOLT <- sum(constr_table$`acc_AM_construction$RMSE`<=constr_table$`acc_holt_construction$RMSE`)
cN_MASE_AM_MASE_HOLT <- sum(constr_table$`acc_AM_construction$MASE`<=constr_table$`acc_holt_construction$MASE`)

cN_MAE_AM_MAE_ARIMA <- sum(constr_table$`acc_AM_construction$MAE`<=constr_table$`acc_ARIMA_construction$MAE`)
cN_MAPE_AM_MAPE_ARIMA <- sum(constr_table$`acc_AM_construction$MAPE`<=constr_table$`acc_ARIMA_construction$MAPE`)
cN_RMSE_AM_RMSE_ARIMA <- sum(constr_table$`acc_AM_construction$RMSE`<=constr_table$`acc_ARIMA_construction$RMSE`)
cN_MASE_AM_MASE_ARIMA <- sum(constr_table$`acc_AM_construction$MASE`<=constr_table$`acc_ARIMA_construction$MASE`)

cN_MAE_NFTS_MAE_SES <- sum(constr_table$`acc_NFTS_construction$MAE`<=constr_table$`acc_ses_construction$MAE`)
cN_MAPE_NFTS_MAPE_SES <- sum(constr_table$`acc_NFTS_construction$MAPE`<=constr_table$`acc_ses_construction$MAPE`)
cN_RMSE_NFTS_RMSE_SES <- sum(constr_table$`acc_NFTS_construction$RMSE`<=constr_table$`acc_ses_construction$RMSE`)
cN_MASE_NFTS_MASE_SES <- sum(constr_table$`acc_NFTS_construction$MASE`<=constr_table$`acc_ses_construction$MASE`)

cN_MAE_NFTS_MAE_HOLT <- sum(constr_table$`acc_NFTS_construction$MAE`<=constr_table$`acc_holt_construction$MAE`)
cN_MAPE_NFTS_MAPE_HOLT <- sum(constr_table$`acc_NFTS_construction$MAPE`<=constr_table$`acc_holt_construction$MAPE`)
cN_RMSE_NFTS_RMSE_HOLT <- sum(constr_table$`acc_NFTS_construction$RMSE`<=constr_table$`acc_holt_construction$RMSE`)
cN_MASE_NFTS_MASE_HOLT <- sum(constr_table$`acc_NFTS_construction$MASE`<=constr_table$`acc_holt_construction$MASE`)

cN_MAE_NFTS_MAE_ARIMA <- sum(constr_table$`acc_NFTS_construction$MAE`<=constr_table$`acc_ARIMA_construction$MAE`)
cN_MAPE_NFTS_MAPE_ARIMA <- sum(constr_table$`acc_NFTS_construction$MAPE`<=constr_table$`acc_ARIMA_construction$MAPE`)
cN_RMSE_NFTS_RMSE_ARIMA <- sum(constr_table$`acc_NFTS_construction$RMSE`<=constr_table$`acc_ARIMA_construction$RMSE`)
cN_MASE_NFTS_MASE_ARIMA <- sum(constr_table$`acc_NFTS_construction$MASE`<=constr_table$`acc_ARIMA_construction$MASE`)


N_industry <- data.frame(row.names = seq(1:1))
iN_MAE_AM_MAE_SES <- sum(industry_table$`acc_AM_industry$MAE`<=industry_table$`acc_ses_industry$MAE`)
iN_MAPE_AM_MAPE_SES <- sum(industry_table$`acc_AM_industry$MAPE`<=industry_table$`acc_ses_industry$MAPE`)
iN_RMSE_AM_RMSE_SES <- sum(industry_table$`acc_AM_industry$RMSE`<=industry_table$`acc_ses_industry$RMSE`)
iN_MASE_AM_MASE_SES <- sum(industry_table$`acc_AM_industry$MASE`<=industry_table$`acc_ses_industry$MASE`)

iN_MAE_AM_MAE_HOLT <- sum(industry_table$`acc_AM_industry$MAE`<=industry_table$`acc_holt_industry$MAE`)
iN_MAPE_AM_MAPE_HOLT <- sum(industry_table$`acc_AM_industry$MAPE`<=industry_table$`acc_holt_industry$MAPE`)
iN_RMSE_AM_RMSE_HOLT <- sum(industry_table$`acc_AM_industry$RMSE`<=industry_table$`acc_holt_industry$RMSE`)
iN_MASE_AM_MASE_HOLT <- sum(industry_table$`acc_AM_industry$MASE`<=industry_table$`acc_holt_industry$MASE`)

iN_MAE_AM_MAE_ARIMA <- sum(industry_table$`acc_AM_industry$MAE`<=industry_table$`acc_ARIMA_industry$MAE`)
iN_MAPE_AM_MAPE_ARIMA <- sum(industry_table$`acc_AM_industry$MAPE`<=industry_table$`acc_ARIMA_industry$MAPE`)
iN_RMSE_AM_RMSE_ARIMA <- sum(industry_table$`acc_AM_industry$RMSE`<=industry_table$`acc_ARIMA_industry$RMSE`)
iN_MASE_AM_MASE_ARIMA <- sum(industry_table$`acc_AM_industry$MASE`<=industry_table$`acc_ARIMA_industry$MASE`)

iN_MAE_NFTS_MAE_SES <- sum(industry_table$`acc_NFTS_industry$MAE`<=industry_table$`acc_ses_industry$MAE`)
iN_MAPE_NFTS_MAPE_SES <- sum(industry_table$`acc_NFTS_industry$MAPE`<=industry_table$`acc_ses_industry$MAPE`)
iN_RMSE_NFTS_RMSE_SES <- sum(industry_table$`acc_NFTS_industry$RMSE`<=industry_table$`acc_ses_industry$RMSE`)
iN_MASE_NFTS_MASE_SES <- sum(industry_table$`acc_NFTS_industry$MASE`<=industry_table$`acc_ses_industry$MASE`)

iN_MAE_NFTS_MAE_HOLT <- sum(industry_table$`acc_NFTS_industry$MAE`<=industry_table$`acc_holt_industry$MAE`)
iN_MAPE_NFTS_MAPE_HOLT <- sum(industry_table$`acc_NFTS_industry$MAPE`<=industry_table$`acc_holt_industry$MAPE`)
iN_RMSE_NFTS_RMSE_HOLT <- sum(industry_table$`acc_NFTS_industry$RMSE`<=industry_table$`acc_holt_industry$RMSE`)
iN_MASE_NFTS_MASE_HOLT <- sum(industry_table$`acc_NFTS_industry$MASE`<=industry_table$`acc_holt_industry$MASE`)

iN_MAE_NFTS_MAE_ARIMA <- sum(industry_table$`acc_NFTS_industry$MAE`<=industry_table$`acc_ARIMA_industry$MAE`)
iN_MAPE_NFTS_MAPE_ARIMA <- sum(industry_table$`acc_NFTS_industry$MAPE`<=industry_table$`acc_ARIMA_industry$MAPE`)
iN_RMSE_NFTS_RMSE_ARIMA <- sum(industry_table$`acc_NFTS_industry$RMSE`<=industry_table$`acc_ARIMA_industry$RMSE`)
iN_MASE_NFTS_MASE_ARIMA <- sum(industry_table$`acc_NFTS_industry$MASE`<=industry_table$`acc_ARIMA_industry$MASE`)


N_retail <- data.frame(row.names = seq(1:1))
rN_MAE_AM_MAE_SES <- sum(retail_table$`acc_AM_retail$MAE`<=retail_table$`acc_ses_retail$MAE`)
rN_MAPE_AM_MAPE_SES <- sum(retail_table$`acc_AM_retail$MAPE`<= retail_table$`acc_ses_retail$MAPE`)
rN_RMSE_AM_RMSE_SES <- sum(retail_table$`acc_AM_retail$RMSE`<= retail_table$`acc_ses_retail$RMSE`)
rN_MASE_AM_MASE_SES <- sum(retail_table$`acc_AM_retail$MASE`<= retail_table$`acc_ses_retail$MASE`)

rN_MAE_AM_MAE_HOLT <- sum(retail_table$`acc_AM_retail$MAE`<= retail_table$`acc_holt_retail$MAE`)
rN_MAPE_AM_MAPE_HOLT <- sum(retail_table$`acc_AM_retail$MAPE`<=retail_table$`acc_holt_retail$MAPE`)
rN_RMSE_AM_RMSE_HOLT <- sum(retail_table$`acc_AM_retail$RMSE`<=retail_table$`acc_holt_retail$RMSE`)
rN_MASE_AM_MASE_HOLT <- sum(retail_table$`acc_AM_retail$MASE`<=retail_table$`acc_holt_retail$MASE`)

rN_MAE_AM_MAE_ARIMA <- sum(retail_table$`acc_AM_retail$MAE`<=retail_table$`acc_ARIMA_retail$MAE`)
rN_MAPE_AM_MAPE_ARIMA <- sum(retail_table$`acc_AM_retail$MAPE`<=retail_table$`acc_ARIMA_retail$MAPE`)
rN_RMSE_AM_RMSE_ARIMA <- sum(retail_table$`acc_AM_retail$RMSE`<=retail_table$`acc_ARIMA_retail$RMSE`)
rN_MASE_AM_MASE_ARIMA <- sum(retail_table$`acc_AM_retail$MASE`<=retail_table$`acc_ARIMA_retail$MASE`)

rN_MAE_NFTS_MAE_SES <- sum(retail_table$`acc_NFTS_retail$MAE`<=retail_table$`acc_ses_retail$MAE`)
rN_MAPE_NFTS_MAPE_SES <- sum(retail_table$`acc_NFTS_retail$MAPE`<=retail_table$`acc_ses_retail$MAPE`)
rN_RMSE_NFTS_RMSE_SES <- sum(retail_table$`acc_NFTS_retail$RMSE`<=retail_table$`acc_ses_retail$RMSE`)
rN_MASE_NFTS_MASE_SES <- sum(retail_table$`acc_NFTS_retail$MASE`<=retail_table$`acc_ses_retail$MASE`)

rN_MAE_NFTS_MAE_HOLT <- sum(retail_table$`acc_NFTS_retail$MAE`<=retail_table$`acc_holt_retail$MAE`)
rN_MAPE_NFTS_MAPE_HOLT <- sum(retail_table$`acc_NFTS_retail$MAPE`<=retail_table$`acc_holt_retail$MAPE`)
rN_RMSE_NFTS_RMSE_HOLT <- sum(retail_table$`acc_NFTS_retail$RMSE`<=retail_table$`acc_holt_retail$RMSE`)
rN_MASE_NFTS_MASE_HOLT <- sum(retail_table$`acc_NFTS_retail$MASE`<=retail_table$`acc_holt_retail$MASE`)

rN_MAE_NFTS_MAE_ARIMA <- sum(retail_table$`acc_NFTS_retail$MAE`<=retail_table$`acc_ARIMA_retail$MAE`)
rN_MAPE_NFTS_MAPE_ARIMA <- sum(retail_table$`acc_NFTS_retail$MAPE`<=retail_table$`acc_ARIMA_retail$MAPE`)
rN_RMSE_NFTS_RMSE_ARIMA <- sum(retail_table$`acc_NFTS_retail$RMSE`<=retail_table$`acc_ARIMA_retail$RMSE`)
rN_MASE_NFTS_MASE_ARIMA <- sum(retail_table$`acc_NFTS_retail$MASE`<=retail_table$`acc_ARIMA_retail$MASE`)



####Order of integration####

oi_construction <- c()
for(i in 1:20) {
	tc <- ts(construction_fit[,i+1], frequency = 12, start = c(2017,1))
	print(isSeasonal(tc))
	print(names(construction_fit)[i+1])
	adf <- adf.test(tc)	
	k = 0
	while(adf$p.value > 0.1) {
		tc <- diff(tc)
		k <- k + 1
		adf <- adf.test(tc)
	}
	print(k)
	oi_construction <- c(oi_construction, k)
}

print(oi_construction)

oi_industry <- c()
for(i in 1:29) {
	ti <- ts(industry_fit[,i+1], frequency = 12, start = c(2017,1))
	print(isSeasonal(ti))
	adf <- adf.test(ti)	
	k = 0
	while(adf$p.value > 0.1) {
		ti <- diff(ti)
		k <- k + 1
		adf <- adf.test(ti)
	}
	oi_industry <- c(oi_industry, k)
}

print(oi_industry)

oi_retail <- c()
for(i in 1:28) {
	tr <- ts(retail_fit[,i+1], frequency = 12, start = c(2017,1))
	print(isSeasonal(tr))
	adf <- adf.test(tr)	
	k = 0
	while(adf$p.value > 0.1) {
		tr <- diff(tr)
		k <- k + 1
		adf <- adf.test(tr)
	}
	oi_retail <- c(oi_retail, k)
}
print(oi_retail)
