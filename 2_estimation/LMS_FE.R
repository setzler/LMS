
FE_estimation <- function(results_dir, intermed_dir, data_dir, industry='naics2', location='cz', broadmarkets=TRUE, gender_subset='', year_subset='early', methods=c('akm_unadjusted','akm_adjusted','blm_unadjusted','blm_adjusted'), post_estimation=TRUE, wagefloor=100, subsample_rate=1.0){
  # industry='naics2'; location='cz'; broadmarkets=TRUE; gender_subset=''; year_subset='early'; methods=c('blm_adjusted'); wagefloor=100; subsample_rate=1.0
  if(wagefloor==100){
    # set up parameters
  	suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset='', year_subset='') # note: this is full sample intentionally, it is only for getting the full-sample parameter values
    params <- setDT(read.csv(file=sprintf("%sparams/LMS_params_%s.csv",results_dir,suffix)))[model=='1inner']
  	params <- params[, list(broadmarket=broad_market,beta_full=weighted.mean(beta,workeryears), rho_r=rho, alpha_r=alpha, log_fte_share_r = ElogVA - ElogWB)]
  	print(params[])
  	# set up data
  	suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset)
    ddd <- data_loader(data_dir=data_dir, industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset)
    market_count = ddd[,length(unique(market))]
    print(paste0("markets: ",market_count))
  	# prepare for FE estimation
    ddd <- merge(ddd, params, by='broadmarket'); gc()
  	setnames(ddd,c('wages','years','va'),c('log_wage','year','log_va_init'))
  	ddd <- ddd[,.SD,.SDcols=c('worker_ID','firm_ID','year','log_wage','age','market','naics2','cz','log_va_init','broadmarket','beta_full','rho_r','alpha_r','log_fte_share_r')]; gc()
  }
	if(wagefloor < 100){
	  flog.info("Allowing wage floor less than 100 percent of minimum wage")
    suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset)
    suffix = paste0(suffix,'_wagefloor',wagefloor)
    ddd <- data_loader(data_dir=data_dir, industry=industry, location=location, broadmarkets=broadmarkets, gender_subset=gender_subset, year_subset=year_subset, wagefloor=wagefloor)
  }
	for(method in methods){
	  LMS_estimate_FE(intermed_dir=intermed_dir,dd=copy(ddd),method=method,suffix=suffix)
	  if(post_estimation){
  	  LMS_estimate_FE_post(intermed_dir=intermed_dir,method=method,suffix=suffix,subsample_rate=subsample_rate)
  	  LMS_estimate_FE_decomp(results_dir=results_dir,intermed_dir=intermed_dir,method=method,suffix=suffix)
	  }
	}
}


LMS_estimate_FE <- function(intermed_dir,dd,method,suffix){
  estimator <- 'blm'
  the_estimator <- copy(lms.fe.blm)
  if(str_detect(method,'akm')){
    estimator <- 'akm'
    the_estimator <- copy(lms.fe.akm)
  }
  va_adjust <- TRUE
  if(str_detect(method,'unadjusted')){
    va_adjust <- FALSE
  }
  output_file <- sprintf("%sFE/%s_%s.rds",intermed_dir,method,suffix)
  flog.info("FE output will be saved to %s",output_file)
  res <- the_estimator(dd, va_adjust=va_adjust)
  saveRDS(file=output_file, res, compress=T)
  dd <- NULL; gc()
  flog.info('done FE')
}

LMS_estimate_FE_post <- function(intermed_dir,method,suffix,subsample_rate=1.0){
  estimator <- 'blm'
  the_estimator <- function(res,va_adjust){
    return(lms.fe.blm.post_estimation(res=res,va_adjust=va_adjust,subsample_rate=subsample_rate))
  }
  if(str_detect(method,'akm')){
    estimator <- 'akm'
    the_estimator <- copy(lms.fe.akm.post_estimation)
  }
  va_adjust <- TRUE
  if(str_detect(method,'unadjusted')){
    va_adjust <- FALSE
  }
  input_file <- sprintf("%sFE/%s_%s.rds",intermed_dir,method,suffix)
  output_file <- sprintf("%sFE/%s_post_%s.rds",intermed_dir,method,suffix)
  flog.info("FE input will be read from %s and post-estimation output saved to %s",input_file,output_file)
  res <- the_estimator(readRDS(file=input_file),va_adjust=va_adjust)
  saveRDS(file=output_file, res, compress=T)
  gc()
  flog.info('done FE post-estimation')
}

LMS_estimate_FE_decomp <- function(results_dir,intermed_dir,method,suffix){
  estimator <- 'blm'
  the_estimator <- copy(lms.fe.blm.decomp)
  if(str_detect(method,'akm')){
    estimator <- 'akm'
    the_estimator <- copy(lms.fe.akm.decomp)
  }
  va_adjust <- TRUE
  if(str_detect(method,'unadjusted')){
    va_adjust <- FALSE
  }
  input_file <- sprintf("%sFE/%s_post_%s.rds",intermed_dir,method,suffix)
  output_file <- sprintf("%sFE/%s_%s.rds",results_dir,method,suffix)
  flog.info("FE post-estimation input will be read from %s and decomp results saved to %s",input_file,output_file)
  res <- the_estimator(readRDS(file=input_file),va_adjust=va_adjust)
  saveRDS(file=output_file, res, compress=T)
  gc()
  flog.info('done FE decomp')
}

