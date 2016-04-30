# packages needed

# input data
setwd("/Users/yangh/Desktop/HKUST/FINA4414/Project")
dat <- read.csv("prices.csv",
                sep = ",",
                header = TRUE,
                stringsAsFactors = FALSE)
dat <- dat[,-1]
