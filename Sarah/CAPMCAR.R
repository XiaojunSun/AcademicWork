
## start
setwd("D:/RSpace/WISE/Sarah")
rm(list = ls())
load("D:/RSpace/WISE/Sarah/dat.RData")
# save.image("F:/datnew.RData")
library(plyr)
library(dplyr)
library(rlist)

## preparation
SHAidx <- filter(market, Indexcd==2) # Shanghai A stock
wnd_n <- 10     # 1/2 event window
wnd_e <- 180    # estimation window
stock_s <- select(.data=stock, Stkcd, Trddt, Clsprc)

source('D:/RSpace/WISE/Sarah/abnrt_func.R')


output <- abnrt(dataset=change_new, type="CAPM")
# dataset = change_new or Non_Standard ......
# type = "CAPM" or "ThreeFactor"

# output <- abnrt(dataset=Non_Standard, type="ThreeFactor")


str(output)

## test
abnrt <- list.map(output, abnrt)

num_null <- sum(list.mapv(abnrt, is.null(.)))
message(paste("There are", num_null, "stocks are excluded due to insufficient observations or unsuccessful date match."))

abnrt <- list.clean(abnrt, is.null)
abn_df <- as.data.frame(abnrt)

AR <- apply(abn_df, MARGIN = 1, FUN = mean)*100
CAR <- cumsum(AR)

AR_var <- apply(abn_df, MARGIN = 1, FUN = var)

AR_t <- apply(abn_df, MARGIN = 1, FUN = function(dat){t.test(dat)$statistic})
AR_p <- apply(abn_df, MARGIN = 1, FUN = function(dat){t.test(dat)$p.value})
CAR_z <- CAR/sqrt(cumsum(AR_var))
CAR_p <- 2*(1-pnorm(abs(CAR_z$AR),0,1))

result <- data.frame(AR, CAR, AR_var, AR_t, AR_p, CAR_z, CAR_p)

head(result)
