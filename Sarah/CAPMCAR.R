
## start
setwd("F:/")
rm(list = ls())
load("F:/dat.RData")
# save.image("F:/datnew.RData")
library(plyr)
library(dplyr)
library(rlist)

## preparation
SHAidx <- filter(market, Indexcd==2) # Shanghai A stock
wnd_n <- 10     # 1/2 event window
wnd_e <- 180    # estimation window
stock_s <- select(.data=stock, Stkcd, Trddt, Clsprc)

source('F:/abnrt_func.R')
str(output_capm)

## test
abnrt <- list.map(output_capm, abnrt)
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
