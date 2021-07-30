
library(data.table)

run_simulation <- function(data_dir){
  
  # sample sizes
  N <- 2e4
  J <- 2e2
  
  # construct firm data
  firmdata <- data.table(firm_ID = 1:J)
  firmdata[, state := sample(c("MA"), J, replace = TRUE)]
  firmdata[, foreign_tax := sample(c(0,1), J, replace = TRUE)]
  firmdata[, cz := .GRP, state] 
  firmdata[, naics := sample(c("111","211","236","311","331","481","511","517","531","611","711","811"), J, replace = TRUE)]
  
  # draw initial firm and final firm for each worker
  data_init <- data.table(worker_ID = 1:N)
  data_init[, firm_ID := sample(1:J,N,replace=T)]
  data_final <- data.table(worker_ID = 1:N)
  data_final[, firm_ID := sample(1:J,N,replace=T)]
  
  # form panel
  data1 <- data.table()
  for(yy in 2001:2004){
    data1 <- rbind(data1, copy(data_init)[, years := yy])
  }
  data2 <- data.table()
  for(yy in 2005:2008){
    data2 <- rbind(data2, copy(data_final)[, years := yy])
  }
  firmworkerdata <- rbind(data1,data2)
  data_init <- NULL; data_final <- NULL; data1 <- NULL; data2 <- NULL
  
  # switch some to stayers
  stayer_share <- 0.6
  firmworkerdata[, stayer := FALSE]
  firmworkerdata[worker_ID < 0.5*N, stayer := TRUE]
  firmworkerdata[, init_firm_ID := firm_ID[years==2008], worker_ID]
  firmworkerdata[stayer==TRUE, firm_ID := init_firm_ID]
  firmworkerdata[, stayer := NULL]
  firmworkerdata[, init_firm_ID := NULL]
  
  # construct key variables
  firmworkerdata[, gender := sample(c("M","F"),1), worker_ID]
  firmworkerdata[, yob := sample(1960:1980,1), worker_ID]
  firmworkerdata[, age := years - yob]
  firmworkerdata <- merge(firmworkerdata, firmdata, by='firm_ID')
  # firmworkerdata <- rbind(copy(firmworkerdata)[years %in% 2008:2014][, years := (years - 7)], firmworkerdata)
  
  # construct firm and market shocks
  firmdata <- unique(firmworkerdata[,.(firm_ID,years)])[order(firm_ID,years)]
  firmdata[, fe := rnorm(1)*0.1, firm_ID]
  firmdata[, theta := 1+rnorm(1)*0.02, firm_ID]
  firmdata[, va_shock := rnorm(nrow(firmdata))*0.1]
  firmdata[, va_randomwalk := cumsum(va_shock), list(firm_ID)]
  marketdata <- unique(firmworkerdata[,.(cz,naics,years)])[order(cz,naics,years)]
  marketdata[, va_shock_market := rnorm(nrow(marketdata))*0.1]
  marketdata[, va_randomwalk_market := cumsum(va_shock_market), list(cz,naics)]
  firmworkerdata <- merge(firmworkerdata,firmdata,by=c('firm_ID','years'))
  firmworkerdata <- merge(firmworkerdata,marketdata,by=c('cz','naics','years'))
  firmworkerdata <- firmworkerdata[order(worker_ID,years)]
  firmdata <- NULL; marketdata <- NULL
  
  # construct real variables
  firmworkerdata[, we := rnorm(1)*0.5, worker_ID]
  firmworkerdata[, wages_shock := rnorm(nrow(firmworkerdata))*0.01]
  firmworkerdata[, wages := 10 + we*theta + va_randomwalk*0.2 + va_randomwalk_market*0.4 + wages_shock + log(age)*.01]
  firmworkerdata[, wagebill_total := log(sum(exp(wages))), list(firm_ID,years)]
  firmworkerdata[, firmsize_total := log(.N), list(firm_ID,years)]
  va_intercept <- firmworkerdata[, mean(wagebill_total)]
  firmworkerdata[, va := (va_intercept + 1.2) + (va_randomwalk + va_randomwalk_market + fe)]
  
  # finish
  firmworkerdata <- firmworkerdata[,list(worker_ID,firm_ID,years,naics,cz,county=copy(cz),state,age,gender,wages,va,grossinc=copy(wages),netinc=1+copy(wages)*0.9,wagebill_total,firmsize_total,foreign_tax,ebitd=0.8*copy(va),vanet=0.9*copy(va)+rnorm(nrow(firmworkerdata))*0.1,operprofits=0.5*copy(va)+rnorm(nrow(firmworkerdata))*0.1)]; gc()
  firmworkerdata[, makeNA := sample(c(0,1),nrow(firmworkerdata),replace=T,prob=c(0.99,.01))] # this was just for testing out NA handling, not really needed
  firmworkerdata[makeNA==TRUE, `:=`(grossinc=NA,netinc=NA,ebitd=NA,vanet=NA,operprofits=NA)]
  firmworkerdata <- rbind(firmworkerdata, copy(firmworkerdata[years==2001])[, years := 2000][, firm_ID := 0]) # add year 2000 at a different firm, ensures 2001 will be treated as start of tenure
  firmworkerdata <- firmworkerdata[order(worker_ID,years)]
  saveRDS(firmworkerdata, file=sprintf("%sfull_sample.rds", data_dir), compress=TRUE)
  firmworkerdata <- NULL; gc()

}

run_simulation_auction <- function(data_dir){
  dd <- setDT(readRDS(file=sprintf("%sfull_sample.rds", data_dir)))
  dd <- unique(dd[,.(firm_ID, year=years, bid_active=TRUE, log_wagebill_fte=wagebill_total, log_EBITD=ebitd, log_workers_fte=firmsize_total)])
  dd[, log_meanwage_fte := log_wagebill_fte - log_workers_fte]
  dd[, wagebill_fte := exp(log_wagebill_fte)]
  dd[, workers_fte := exp(log_workers_fte)]
  dd[, EBITD := exp(log_EBITD)]
  dd[, costsold := 0.5*EBITD]
  dd[, log_costsold := log(costsold)]
  years <- dd[,unique(year)]
  dd[, win_yr := sample(c(years,NA),1), firm_ID]
  write.csv(dd, file=sprintf("%sauction_sample.csv", data_dir), row.names=FALSE)
}
