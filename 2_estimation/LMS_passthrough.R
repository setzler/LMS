
get_passthrough_descriptives <- function(data_dir,results_dir,industry='naics2', location='cz', broadmarkets=FALSE){

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets)
  ddd <- data_loader(data_dir=data_dir, industry=industry, location=location, broadmarkets=broadmarkets)

  descriptives <- data.table()

  if(!broadmarkets){

    descriptives <- lms.passthrough(long_data=ddd,
                           id_var='worker_ID', time_var='years', firm_defn='firm_ID', market_defn='market',
                           spell_length=8, firm_stayer=T, market_stayer=T, workers_per_firm = 10,
                           firms_per_market = 10, covmat_numcores = 1, outcomes=c("wages","va","firmsize"),
                           sample_info_only = TRUE) # this is the key option

  } else {

    for(ii in ddd[,unique(broadmarket)]){

      res <- lms.passthrough(long_data=ddd[broadmarket==ii],
                             id_var='worker_ID', time_var='years', firm_defn='firm_ID', market_defn='market',
                             spell_length=8, firm_stayer=T, market_stayer=T, workers_per_firm = 10,
                             firms_per_market = 10, covmat_numcores = 1, outcomes=c("wages","va","firmsize"),
                             sample_info_only = TRUE) # this is the key option
      res <- res[step=='firmsPerMarket'][,list(step,workeryears,workers,firms,years,spells,Ew,ElogVA,markets,broad_market=ii)]
      descriptives <- rbind(descriptives,res)

    }

  }

  write.csv(descriptives,file=sprintf("%sdescriptives/LMS_descriptives_%s_stayersteps.csv",results_dir,suffix),row.names=F)

}

get_passthrough_parallelized <- function(data_dir,intermed_dir,years=2001:2008, ncores = 4, industry='naics2', location='cz', broadmarkets=FALSE, doDiD=TRUE, gender_subset='', year_subset='', va_var='va', subsample=''){

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var=va_var, subsample=subsample)
  ddd <- data_loader(data_dir=data_dir, industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var=va_var, subsample=subsample)
  market_count = ddd[,length(unique(market))]
  print(paste0("markets: ",market_count))
  gc()

  flog.info("starting parallel")
  out <- parallel::mclapply(years, LMS_collect_passthrough, mc.cores=ncores, dd=ddd, broadmarkets=broadmarkets, doDiD=doDiD)
  flog.info("done parallel")
  print(out)
  CovMat <- data.table()
  DiD <- data.table()
  for(ii in 1:length(out)){
    this_out <- out[[ii]]
    CovMat <- rbind(CovMat,this_out$covmat)
    DiD <- rbind(DiD,this_out$did)
  }
  saveRDS(DiD, file=sprintf("%sDiD/DiD_by_year_%s.csv",intermed_dir,suffix), compress=T)
  saveRDS(CovMat, file=sprintf("%scovmat/CovMat_by_year_%s.csv",intermed_dir,suffix), compress=T)
  flog.info("saved output")
}


get_passthrough_params <- function(results_dir, intermed_dir, industry='naics2', location='cz', broadmarkets=TRUE, doDiD=TRUE, gender_subset='', year_subset='', va_var='va', subsample='', dividedby1k_vars=c('workeryears','workers','firms','firmyears')){

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var=va_var, subsample=subsample)
  if(subsample=='' & va_var=='va'){
    descriptives <- setDT(read.csv(file=sprintf("%sdescriptives/LMS_descriptives_%s.csv",intermed_dir,suffix)))
  }
  if(subsample!='' | va_var!='va'){ # can use any descriptives here, since we only want the passthrough rate for subsamples
    suffix_full <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, year_subset=year_subset)
    descriptives <- setDT(read.csv(file=sprintf("%sdescriptives/LMS_descriptives_%s.csv",intermed_dir,suffix_full)))
  }
  CovMat <- readRDS(file=sprintf("%scovmat/CovMat_by_year_%s.csv",intermed_dir,suffix))
  DiD <- readRDS(file=sprintf("%sDiD/DiD_by_year_%s.csv",intermed_dir,suffix))

  time_opts_ma1 = list(spell_length = 8, onset_event_time = 5, forward_event_time = 2, backward_event_time = 3)
  time_opts_ma2 = list(spell_length = 8, onset_event_time = 5, forward_event_time = 3, backward_event_time = 4)

  if(!broadmarkets){

    COVMAT <- CovMat[,lapply(.SD,mean), by=c('covariate_1','covariate_2','event_1','event_2')][, begin_year := NULL]

    ma2 <- lms.passthrough.collect_IV(COVMAT, time_opts = time_opts_ma2)[, ma := 'ma2']
    ma1 <- lms.passthrough.collect_IV(COVMAT, time_opts = time_opts_ma1)[, ma := 'ma1']
    ma <- rbind(ma1,ma2)[, broad_market := "overall"]

    descriptives[, broad_market := 'overall']
    params_ma1 <- lms.passthrough.collect_params(rr=ma[ma=='ma1'], descriptives=descriptives)[, model := "1inner"]
    params_ma2 <- lms.passthrough.collect_params(rr=ma[ma=='ma2'], descriptives=descriptives)[, model := "2"]
    params <- rbind(params_ma1,params_ma2)

    if(doDiD){
      DID <- DiD[,lapply(.SD,mean),event_time][, begin_year := NULL]
      write.csv(DID,file=sprintf("%sparams/LMS_DiD_%s.csv",results_dir,suffix),row.names=F)
    }

  } else {

    COVMAT <- CovMat[,lapply(.SD,mean), by=c('broad_market','covariate_1','covariate_2','event_1','event_2')][, begin_year := NULL]

    ma <- data.table()
    for(ii in COVMAT[,unique(broad_market)]){

      ma1 <- lms.passthrough.collect_IV(COVMAT[broad_market==ii], time_opts=time_opts_ma1)[, ma := 'ma1'][, broad_market := ii]
      ma <- rbind(ma,ma1)

      ma2 <- lms.passthrough.collect_IV(COVMAT[broad_market==ii], time_opts=time_opts_ma2)[, ma := 'ma2'][, broad_market := ii]
      ma <- rbind(ma,ma2)

    }

    params_ma1 <- lms.passthrough.collect_params(rr=ma[ma=='ma1'], descriptives=descriptives)[, model := "1inner"]
    params_ma2 <- lms.passthrough.collect_params(rr=ma[ma=='ma2'], descriptives=descriptives)[, model := "2"]
    params <- rbind(params_ma1,params_ma2)

  }

  for(var in dividedby1k_vars){
    params[, (var) := get(var)/1e3]
  }
  print(params[])
  write.csv(params,file=sprintf("%sparams/LMS_params_%s.csv",results_dir,suffix),row.names=F)

  return(params)

}


LMS_collect_passthrough <- function(yy,dd,broadmarkets=FALSE,doDiD=TRUE){

  covmat <- data.table()
  did <- data.table()

  if(!broadmarkets){

    flog.info("starting %s",yy)

    res <- lms.passthrough(long_data=dd[years >= yy & years <= (yy+7)] ,
                           id_var='worker_ID', time_var='years', firm_defn='firm_ID', market_defn='market',
                           spell_length=8, firm_stayer=T, market_stayer=T, workers_per_firm = 10,
                           firms_per_market = 10, covmat_numcores = 1, outcomes=c("wages","va","firmsize"))

    covmat <- lms.passthrough.covmat_events(res$covmat)[, begin_year := yy]
    if(doDiD){
      did <- lms.passthrough.collect_diffInDiffs(wide_data = res$wide_data, id_var = "worker_ID")[, begin_year := yy]
    }

  }	else {

    for(ii in dd[,unique(broadmarket)]){

      flog.info("broad market %s year %s",ii,yy)

      res <- lms.passthrough(long_data=dd[years >= yy & years <= (yy+7) & broadmarket==ii] ,
                             id_var='worker_ID', time_var='years', firm_defn='firm_ID', market_defn='market',
                             spell_length=8, firm_stayer=T, market_stayer=T, workers_per_firm = 10,
                             firms_per_market = 10, covmat_numcores = 2, outcomes=c("wages","va","firmsize"))

      covmat <- rbind(covmat,lms.passthrough.covmat_events(res$covmat)[, broad_market := ii][, begin_year := yy] )

    }

  }

  return(list(covmat=covmat, did=did))
}


combine_passthrough_subsamples <- function(results_dir, intermed_dir, industry='naics2', location='cz', broadmarkets=FALSE, gender_subset='', year_subset='', va_set, subsample_set){

  combined_res <- data.table()
  for(subsamples in subsample_set){
    suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var='va', subsample=subsamples)
    file=sprintf("%sparams/LMS_params_%s.csv",intermed_dir,suffix)
    dd <- setDT(read.csv(file=file))[, r := subsamples]
    combined_res <- rbind(combined_res,dd)
  }
  for(va_vars in va_set){
    suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var=va_vars, subsample='')
    file=sprintf("%sparams/LMS_params_%s.csv",intermed_dir,suffix)
    dd <- setDT(read.csv(file=file))[, r := va_vars]
    combined_res <- rbind(combined_res,dd)
  }
  combined_res <-combined_res[,.(r, model, unc_pt, net_pt, market_pt)]
  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var='va', subsample='subsamples')
  write.csv(combined_res,file=sprintf("%sparams/LMS_params_%s.csv",results_dir,suffix),row.names=F)

}




get_passthrough_bootstraps <- function(data_dir,intermed_dir,years=2001:2008, ncores = 4, industry='naics2', location='cz', broadmarkets=FALSE, num_boots=40){

  dir.create(paste0(intermed_dir,'covmat/bootstraps'))
  doDiD=FALSE; gender_subset=''; year_subset=''; va_var='va'; subsample=''

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var=va_var, subsample=subsample)
  dbase <- data_loader(data_dir=data_dir, industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var=va_var, subsample=subsample)

  for(bootstrap_seed in 1:num_boots){
    ddd <- lms.block_sample(copy(dbase), unit_var='market', seed=bootstrap_seed)
    ddd[, worker_ID := .GRP, list(worker_ID,market)]
    ddd[, firm_ID := .GRP, list(firm_ID,market)]
    ddd[, market := .GRP, market]
    ddd[, market := as.integer(market)]
    market_count = ddd[,length(unique(market))]
    print(paste0("markets: ",market_count))
    gc()

    flog.info("starting parallel")
    out <- parallel::mclapply(years, LMS_collect_passthrough, mc.cores=ncores, dd=ddd, broadmarkets=broadmarkets, doDiD=doDiD)
    flog.info("done parallel")
    print(out)
    CovMat <- data.table()
    for(ii in 1:length(out)){
      this_out <- out[[ii]]
      CovMat <- rbind(CovMat,this_out$covmat)
    }
    saveRDS(CovMat, file=sprintf("%scovmat/bootstraps/CovMat_by_year_%s_%s.csv",intermed_dir,suffix,bootstrap_seed), compress=T)
    flog.info("saved output")
  }
}



get_passthrough_params_bootstraps <- function(results_dir, intermed_dir, industry='naics2', location='cz', broadmarkets=TRUE, num_boots=40, dividedby1k_vars=c('workeryears','workers','firms','firmyears'), return_raw=FALSE){

  doDiD=FALSE; gender_subset=''; year_subset=''; va_var='va'; subsample=''

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, va_var=va_var, subsample=subsample)
  if(subsample=='' & va_var=='va'){
    descriptives <- setDT(read.csv(file=sprintf("%sdescriptives/LMS_descriptives_%s.csv",intermed_dir,suffix)))
  }
  if(subsample!='' | va_var!='va'){ # can use any descriptives here, since we only want the passthrough rate for subsamples
    suffix_full <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, year_subset=year_subset)
    descriptives <- setDT(read.csv(file=sprintf("%sdescriptives/LMS_descriptives_%s.csv",intermed_dir,suffix_full)))
  }

  outer_params <- data.table()

  for(bootstrap_seed in 1:num_boots){

    CovMat <- readRDS(file=sprintf("%scovmat/bootstraps/CovMat_by_year_%s_%s.csv",intermed_dir,suffix,bootstrap_seed))

    time_opts_ma1 = list(spell_length = 8, onset_event_time = 5, forward_event_time = 2, backward_event_time = 3)
    time_opts_ma2 = list(spell_length = 8, onset_event_time = 5, forward_event_time = 3, backward_event_time = 4)

    if(!broadmarkets){

      COVMAT <- CovMat[,lapply(.SD,mean), by=c('covariate_1','covariate_2','event_1','event_2')][, begin_year := NULL]

      ma2 <- lms.passthrough.collect_IV(COVMAT, time_opts = time_opts_ma2)[, ma := 'ma2']
      ma1 <- lms.passthrough.collect_IV(COVMAT, time_opts = time_opts_ma1)[, ma := 'ma1']
      ma <- rbind(ma1,ma2)[, broad_market := "overall"]

      descriptives[, broad_market := 'overall']
      params_ma1 <- lms.passthrough.collect_params(rr=ma[ma=='ma1'], descriptives=descriptives)[, model := "1inner"]
      params_ma2 <- lms.passthrough.collect_params(rr=ma[ma=='ma2'], descriptives=descriptives)[, model := "2"]
      params <- rbind(params_ma1,params_ma2)
      params[, boot := bootstrap_seed]
      outer_params <- rbind(outer_params,params)

    } else {

      COVMAT <- CovMat[,lapply(.SD,mean), by=c('broad_market','covariate_1','covariate_2','event_1','event_2')][, begin_year := NULL]

      ma <- data.table()
      for(ii in COVMAT[,unique(broad_market)]){

        ma1 <- lms.passthrough.collect_IV(COVMAT[broad_market==ii], time_opts=time_opts_ma1)[, ma := 'ma1'][, broad_market := ii]
        ma <- rbind(ma,ma1)

        ma2 <- lms.passthrough.collect_IV(COVMAT[broad_market==ii], time_opts=time_opts_ma2)[, ma := 'ma2'][, broad_market := ii]
        ma <- rbind(ma,ma2)

      }

      params_ma1 <- lms.passthrough.collect_params(rr=ma[ma=='ma1'], descriptives=descriptives)[, model := "1inner"]
      params_ma2 <- lms.passthrough.collect_params(rr=ma[ma=='ma2'], descriptives=descriptives)[, model := "2"]
      params <- rbind(params_ma1,params_ma2)
      params[, boot := bootstrap_seed]
      outer_params <- rbind(outer_params,params)

    }

  }

  if(return_raw){
    return(outer_params)
  }

  # other clean-up
  lambda <- 0.9154762
  outer_params[, beta_over_rho := (beta/rho)]
  outer_params[, beta_model_inv := (beta/lambda)^(-1)]
  outer_params[, rts := 1-alpha]
  outer_params[, corr := 1-rho^2]
  for(var in dividedby1k_vars){
    outer_params[, (var) := get(var)/1e3]
  }
  outer_params <- outer_params[model=='1inner']
  outer_params[, model := NULL][, ma := NULL]
  outer_params_avg <- copy(outer_params)[, broad_market := NULL]


  # key step -- take standard deviation across bootstrap draws
  outer_params_avg <- outer_params_avg[,lapply(.SD,weighted.mean,workeryears), boot]
  outer_params_avg <- outer_params_avg[,lapply(.SD,sd)][, broad_market := 'All']
  outer_params <- outer_params[,lapply(.SD,sd),list(broad_market)]
  outer_params <- rbind(outer_params,outer_params_avg)

  needed_vars <- c("broad_market","net_pt","market_pt","beta","beta_over_rho","beta_model_inv","rho","rts","alpha","corr","Rw_firm","Rw_market","Rf_firm","Rf_market","Rw_firm_wageshare","Rw_market_wageshare","Rf_firm_profitshare","Rf_market_profitshare","Rw_firm_share","Rw_market_share")
  outer_params <- outer_params[,.SD,.SDcols=needed_vars]

  print(outer_params[])
  write.csv(outer_params,file=sprintf("%sparams/LMS_params_%s_bootstraps.csv",results_dir,suffix),row.names=F)

  return(outer_params)

}

