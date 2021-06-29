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
etsfit_construction <- ets(construction_fit)
etsfit_industry <- ets(industry_fit)
etsfit_retail <- ets(retail_fit)