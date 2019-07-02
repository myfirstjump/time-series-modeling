
### 1.

## Download the data from URL
data_url <- 'https://od.cdc.gov.tw/eic/NHI_Influenza_like_illness.csv'
destfile <- 'C:/Users/CJSCOPE/Documents/git-repository/time-series-modeling/clinic_stat.csv'
destfile <- 'C:/Users/edwardchen/Documents/git-workspaces/time-series-modeling/clinic_stat.csv'
download.file(data_url, destfile = destfile, mode='wb')

## Cause its quite large, import data.table to read it faster.
library(data.table)
data <- fread(destfile, sep=',', header=T, blank.lines.skip=T, encoding = "UTF-8")
class(data) #[1] "data.table" "data.frame"

### 2.

## Reduced data
table(data$就診類別)
remain_idx <- which(data$就診類別 == '門診')
data <- data[remain_idx,]
## Aggregation
sapply(data, class)
agg_data <- data[, .(flu=sum(類流感健保就診人次), total=sum(健保就診總人次)), by = .(年, 週)]
agg_data$percentage <- with(agg_data, flu/total)
agg_data$time <- with(agg_data, paste0(年,'-',週))
plot(agg_data$flu, type = 'l')

### 3.

## Suspect for autocorrelation
## Do Durbin-Watson test
## add Date column
date_col <- as.Date(paste0(agg_data$年, agg_data$週, '星期一'), "%Y%U%a")
fill_na_date_col <- function(date_col) {
  na_idx <- which(is.na(date_col))
  for (i in na_idx) {
    date_col[i] <- date_col[i-1] + 3
  }
  return(date_col)
}
date_col <- fill_na_date_col(date_col)
agg_data$date_col <- date_col
data_train <- agg_data[date_col < '2018-01-01',]

model <- lm(flu ~ date_col, data=data_train)
library(lmtest)
dwtest(model)
# p-value < 2.2e-16 -> Reject Ho: No autocorrelation
# -> There is autocorrelation between variables -> Try to use time-series modeling method.

## Growth rate
# plot(diff(log(agg_data$flu), differences = 1), type='l')
data_train$growth <- c(0, diff(data_train$flu, differences = 1))

## Stationary
library(tseries)
adf.test(data_train$flu)
# p-value = 0.01 -> Reject Ho: Data not stationary
# -> Data is stationary.

plot(data_train$date_col, data_train$growth, type = 'l')
library(TSA)
period <- periodogram(data_train$growth)
period_df <- data.frame(freq=period$freq, spec=period$spec)
top_period <- 1/ (head(period_df[order(-period_df$spec),], 5)$freq)

## Seasonal and trend decomposition
ts_data <- ts(data_train[, .(date_col, growth)], frequency = 52)
decompose_mul <- decompose(ts_data[,2], type="multi")
plot(decompose_mul)
decompose_add <- decompose(ts_data[,2], type="addi")
plot(decompose_add)

stlts <- stl(ts_data[,2], s.window = "periodic")
seasonal_ <- stlts$time.series[, "seasonal"]
trend_ <- stlts$time.series[, "trend"]
comp_df <- data.frame(stlts$time.series[, c('seasonal', 'trend', 'remainder')])
comp_df$total <- rowSums(comp_df)
plot(seasonal_, type='l')

# seasonal pattern looks regularly, use naive model
library(forecast)
f_season <- forecast(decompose_mul$seasonal, method='naive', h = 52)
# -> point estimation = f_season$mean
season_predict <- f_season$mean

# Autoregression for trend data
# choose lagged weeks
vec <- c()
for (p in c(1:100)) {
  model_ar <- ar.ols(trend_, order = p)
  aic <- log(sum(model_ar$resid^2, na.rm=T)/model_ar$n.used) + 2*(p+1)/model_ar$n.used
  if(do.call(sum, model_ar[2]) <　1) {
    stationary <- TRUE
  } else {
    stationary <- FALSE
  }
  if (stationary == TRUE) {
    vec <- c(vec, as.integer(p), aic)
  }
}
info <- data.frame(matrix(vec, nrow=100, ncol=2, byrow=TRUE))
colnames(info) <- c('Lagged', 'AIC')
# -> choose lagged time = 98 weeks -> consider AR(p=98) for trend data
which.min(info$AIC)
model_ar <- ar.ols(trend_, order=98)
trend_pred <- predict(model_ar, n.ahead=52)$pred

growth_predict <- season_predict + trend_pred
last_data_point <- data_train[nrow(data_train),]$flu
flu_predict <- cumsum(growth_predict) + last_data_point

### 4.

dat <- agg_data[(date_col > '2018-01-01') & (date_col < '2019-01-01'),]
dat$flu_pred <- flu_predict

par(mfcol=c(1,2))
plot(dat$flu, type='l')
plot(dat$flu_pred, type='l')

dat$pred_percentage <- with(dat, flu_pred/total)
mae <- sum(abs(dat$percentage - dat$pred_percentage))


