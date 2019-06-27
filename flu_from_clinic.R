
### 1.

## Download the data from URL
data_url <- 'https://od.cdc.gov.tw/eic/NHI_Influenza_like_illness.csv'
destfile <- 'C:/Users/CJSCOPE/Documents/git-repository/acer_interview/clinic_stat.csv'

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

### 3.
## https://medium.com/r-%E8%AA%9E%E8%A8%80%E8%87%AA%E5%AD%B8%E7%B3%BB%E5%88%97/r%E8%AA%9E%E8%A8%80%E8%87%AA%E5%AD%B8%E6%97%A5%E8%A8%98-10-%E6%99%82%E9%96%93%E5%BA%8F%E5%88%97%E9%A0%90%E6%B8%AC%E6%96%B9%E6%B3%95-5aef00a9c997
plot(agg_data$年, agg_data$flu)
