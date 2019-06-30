
### 1.

## Download the data from URL
data_url <- 'https://od.cdc.gov.tw/eic/NHI_Influenza_like_illness.csv'
destfile <- 'C:/Users/CJSCOPE/Documents/git-repository/time-series-modeling/clinic_stat.csv'
# destfile <- 'C:/Users/edwardchen/Documents/git-workspaces/time-series-modeling/clinic_stat.csv'
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
model <- lm(flu ~ date_col, data=agg_data)
library(lmtest)
dwtest(model)
# p-value < 2.2e-16 -> Reject Ho: No autocorrelation
# -> There is autocorrelation between variables -> Try to use time-series modeling method.

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
plot(agg_data$date_col, agg_data$flu, type = 'l')
data_train <- agg_data[date_col < '2018-01-01',]
