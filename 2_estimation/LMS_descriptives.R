
get_descriptives <- function(data_dir, intermed_dir, industry='naics2', location='cz', broadmarkets=TRUE, movers=FALSE){

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets)
  flog.info("Beginning descriptives for suffix %s",suffix)
  dd <- data_loader(data_dir=data_dir, industry=industry, location=location, broadmarkets=broadmarkets)
  if(movers){
    dd[, multiple_firms := length(unique(firm_ID))>1, worker_ID]
    dd <- dd[multiple_firms==TRUE]
    dd[, multiple_firms := NULL]
    suffix <- paste0(suffix,'_movers')
  }
  dd[, value_added := exp(va)]
  dd[, firmsize_FTE := exp(firmsize)]
  dd[, firmsize := NULL]
  dd[, wagebill_total := exp(wagebill_total)]
  dd[, firmsize_total := exp(firmsize_total)]
  setnames(dd,c('firmsize_total','wagebill_total','wages','years'),c('firmsize','WB','log_wage_FTE','year'))
  gc()

  descriptives <- data.table()
  if(!broadmarkets){
    descriptives <- LMS_estimate_descriptives(dd)
  }	else {
    descriptives <- data.table()
    for(ii in dd[,unique(broadmarket)]){
      descriptives <- rbind(descriptives, LMS_estimate_descriptives(dd[broadmarket==ii])[, broad_market := ii]  )
    }
  }

  print(descriptives[])
  write.csv(descriptives,file=sprintf("%sdescriptives/LMS_descriptives_%s.csv",intermed_dir,suffix),row.names=F)

  return(NULL)
}


LMS_estimate_descriptives <- function(dd){
  
  flog.info('beginning descriptives')
  flog.info('counting workers, firms, and markets')
  firms <- unique(dd[,list(FTE_workers=length(unique(worker_ID))),list(firm_ID,year)])
  markets <- unique(dd[,list(FTE_workers=length(unique(worker_ID)), firms=length(unique(firm_ID))),list(market,year)])
  
  flog.info('calculating value added and wages per worker')
  ElogWB <- dd[WB > 0, mean(log(WB),na.rm=T)]
  ElogVA <- dd[value_added > 0, mean(log(value_added),na.rm=T)]
  labor_share <- exp(ElogWB - ElogVA)
  EWB_pc <- dd[, mean(WB/firmsize,na.rm=T)]
  EVA_pc <- dd[, mean(value_added/firmsize,na.rm=T)]
  EPi_pc <- EVA_pc - EWB_pc
  
  flog.info('collecting miscellaneous other descriptives')
  descriptives <- data.table(
    ElogWB = ElogWB,
    ElogVA = ElogVA,
    labor_share = exp(ElogWB - ElogVA),
    EWB_pc = EWB_pc,
    EVA_pc = EVA_pc,
    EPi_pc = EPi_pc,
    Ew = dd[, mean(log_wage_FTE)],
    workers = dd[,length(unique(worker_ID))],
    firms = dd[,length(unique(firm_ID))],
    markets = dd[,length(unique(market))],
    workeryears = nrow(dd),
    firmyears = nrow(firms),
    workers_per_firm = firms[,mean(FTE_workers)] ,
    workers_per_market = markets[,mean(FTE_workers)],
    firms_per_market = markets[,mean(firms)]
  )
  
  flog.info('finished descriptives')
  print(descriptives[])
  
  return(descriptives)
  
}

LMS_taxfit <- function(results_dir, data_dir){
  # collect fit of tax function 
  inputdata <- setDT(readRDS(file=sprintf("%sfull_sample_mainvars_withmarkets.rds",data_dir)))
  inputdata <- inputdata[!is.na(netinc) & !is.na(grossinc)]
  tax_coefs <- lm("netinc ~ grossinc", data=inputdata)$coefficient
  inputdata[, netinc_pred := tax_coefs[1] + tax_coefs[2]*grossinc]
  inputdata[, gross_inc_round := round(grossinc*10)/10]
  taxfit <- inputdata[,list(true_log=mean(netinc),pred_log=mean(netinc_pred),N=.N),by=c('gross_inc_round')][order(gross_inc_round)]
  write.csv(taxfit,file=sprintf("%sdescriptives/LMS_taxfit.csv",results_dir),row.names=F)
}



