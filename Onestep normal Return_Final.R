# ======== Part 0 Markdowns ============
setwd("/Users/yangh/Desktop/HKUST/FINA4414/Project") # please set working directory by yourself
# PACKAGES NEEDED
library("zoo")
library("backtest")

# ======== Part 1 INPUT DATA ============
dat <- read.csv("prices.csv",
                sep = ",",
                header = TRUE,
                stringsAsFactors = FALSE)
dat <- dat[,-1]

# creat zoo objects
DateIndex <- as.Date(dat$Date)
zoo.date <- zoo(dat[,-1], order.by = DateIndex)

# ======= Part 2 GENERATE RETURN DATA ========

zoo.month <- zoo( dat[,-1], order.by = as.yearmon(DateIndex) )
monthly.return <- aggregate(dat$Close, by = list(dat$Ticker,index(zoo.month)), 
                            FUN = function(df){sum(diff(df), na.rm = T)/df[[1]]})
monthly.return <- data.frame(Month = monthly.return[,2], Ticker = monthly.return[,1],
                             Return = monthly.return[,3], stringsAsFactors = F)


# ======== Part 3 DATA CLEAN =================
# 1. At least 24 months' data
# ticker <- unique(dat$Ticker)
# trading.day <- aggregate(zoo.date$Close, by = list(zoo.date$Ticker),
#                          FUN = function(x){sum(is.na(x))} ) 
# trading.day <- as.data.frame(trading.day)
# trading.day$Ticker <- row.names(trading.day)
# rm.ticker <- trading.day[which(trading.day$trading.day > 1687),2]
# for(element in rm.ticker){
#   zoo.date <- zoo.date[ -which(zoo.date$Ticker == element),]
# }
# at last there are 2489 stocks in zoo.date

# 2. Exclude long tail data (for further development)
#
#

# ======== Part 4 GENERATE BACKTEST OBJECT ==========
# transform into "backtest" data form and select time period
monthly.bt <- data.frame("Month" = monthly.return$Month,
                         "Ticker" = monthly.return$Ticker,
                         stringsAsFactors = FALSE)
st <- which(monthly.bt$Month == "Jan 2007")[1]
ed <- which(monthly.bt$Month == "Nov 2013")[1] - 1
monthly.bt <- monthly.bt[seq(from = st, to = ed, by = 1),]
rownames(monthly.bt) <- NULL

# create column names
ret.names <- vector( length = 8)
for(i in c(3,6,9,12)){
  ret.names[i/3] <- paste("ret",i,0, sep = ".")
  ret.names[i/3 + 4] <- paste("ret", 0, i, sep = ".")
}

x <- split(monthly.return[,3], monthly.return$Month, drop = FALSE)
df <- as.data.frame(x)
df <- data.frame(Ticker = unique(monthly.return$Ticker), 
                 df,
                 stringsAsFactors = F)
monthly.ret <- df

# store past return 3M, 6M, 9M, 12M
aVector <- vector(mode = "numeric", length = dim(monthly.bt)[1])
for(element in ret.names[1:4]){
  k <- which(ret.names == element)*3
  for(i in 1:82){
    for(j in 1:dim(monthly.ret)[1]){
      range <- seq(from = i + 13 - k, to = i + 12, by = 1)
      # when comes to the last loop, this = i*j, or 82 month*2465 stocks = 202130 rows
      idx <- (i-1)*dim(monthly.ret)[1] + j
      aVector[idx] <- apply(monthly.ret[j,range], MARGIN = 1,
                            FUN = function(x){(cumprod(x+1)[k]- 1)/k} )
      # notice: i*j = number of rows in monthly.bt
    }
  }
  monthly.bt[[element]] <- aVector
}

# store special past return: 1M, 2W, 1W, etc.
aVector <- vector(mode = "numeric", length = dim(monthly.bt)[1])
k <- 1
for(i in 1:82){
  for(j in 1:dim(monthly.ret)[1]){
    range <-  i + 12
    # when comes to the last loop, this = i*j, or 82 month*2465 stocks = 202130 rows
    idx <- (i-1)*dim(monthly.ret)[1] + j
    aVector[idx] <- monthly.ret[j,range]
    # notice: i*j = number of rows in monthly.bt
  }
}
monthly.bt$ret.1.0 <- aVector


# store future return, 3M,6M,9M,12M
aVector <- vector(mode = "numeric", length = dim(monthly.bt)[1])
for(element in ret.names[5:8]){
  k <- which(ret.names[5:8] == element)*3  # k = 3,6,9,12
  for(i in 1:82){
    for(j in 1:dim(monthly.ret)[1]){
      range <- seq(from = i + 12, to = i + k + 11, by = 1) # range of range = k
      idx <- (i-1)*dim(monthly.ret)[1] + j 
      aVector[idx] <- apply(monthly.ret[j,range], MARGIN = 1,
                            FUN = function(x){(cumprod(x+1)[k]- 1)/k} )
      # notice: i*j = number of rows in monthly.bt
    }
  }
  monthly.bt[[element]] <- aVector
}

# Save as csv
write.csv(monthly.bt, "/Users/yangh/Desktop/HKUST/FINA4414/Project/monthlyBTOneStep.csv")