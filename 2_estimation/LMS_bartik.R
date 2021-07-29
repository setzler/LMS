
get_bartik_collapse <- function(intermed_dir, data_dir, lag_length = 1, industry='naics2', location='cz', broadmarkets=TRUE){

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets)
  flog.info("Beginning descriptives for suffix %s",suffix)
  dd <- data_loader(data_dir=data_dir, industry=industry, location=location, broadmarkets=broadmarkets)
  dd <- dd[,.(worker_ID, firm_ID, years, naics2, cz, wages, va)]; gc()
  ddL <- dd[, list(worker_ID, firm_ID, naics2, cz, years=years+lag_length, wagesL=wages, vaL=va)]
  dd3 <- ddL[, list(totalworkersL=.N, totalVAL=exp(vaL)[1]), list(firm_ID,naics2,cz,years)]
  dd3 <- dd3[, list(totalworkersL=sum(totalworkersL), totalVAL=sum(totalVAL)/1e6), list(naics2,cz,years)]
  dd <- merge(dd,ddL,by=c('worker_ID','firm_ID','years','naics2','cz'))
  ddL <- NULL; gc()
  dd2 <- dd[,list(stayers=.N,firms=length(unique(firm_ID)),W=sum(exp(wages))/1e6,WL=sum(exp(wagesL))/1e6,wagesD=mean(wages-wagesL),vaD=mean(va-vaL)),list(naics2,cz,years)][order(naics2,cz,years)]
  dd4 <- dd[, list(VA = exp(va)[1], VAL = exp(vaL)[1]), list(firm_ID, years, naics2, cz)]
  dd4 <- dd4[,list(VA=sum(VA)/1e6,VAL=sum(VAL)/1e6),list(naics2,cz,years)][order(naics2,cz,years)]
  dd2 <- dd2[firms >= 10 & stayers >= 100]
  dd2 <- merge(dd2,dd3,by=c('naics2','cz','years'))
  dd2 <- merge(dd2,dd4,by=c('naics2','cz','years'))

  write.csv(dd2,file=sprintf("%sdescriptives/LMS_bartik_collapse_%s_%s.csv",intermed_dir,lag_length,suffix),row.names=F)

}

get_bartik_results <- function(lag_length = 1,static=TRUE,sharevarname = 'stayers',equally_weight=FALSE,industry='naics2', location='cz', broadmarkets=TRUE){

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets)

  collapsed <- setDT(read.csv(file=sprintf("%sdescriptives/LMS_bartik_collapse_%s_%s.csv",intermed_dir,lag_length,suffix)))
  setnames(collapsed,c('naics2','cz','years'),c('industry','location','year'))
  collapsed[, sharevar := copy(get(sharevarname))]

  if(static){
    N_i <- collapsed[year==2001+lag_length][, list(denom_i = sum(sharevar)), location]
    S_ij <- collapsed[year==2001+lag_length][, list(numer_ij=sharevar), list(location,industry)]
    S_ij <- merge(S_ij,N_i,by='location')
    S_ij <- S_ij[,list(location,industry,S_ij=numer_ij/denom_i)]
    G_j <- collapsed[,list(industry,year,G_ij=vaD,omega_ij=sharevar)]
    G_j <- G_j[, list(G_j = sum(G_ij*omega_ij)/sum(omega_ij)), list(industry,year)]
    Smat <- merge(S_ij,G_j,by='industry',allow.cartesian=TRUE)
    S_ij2 <- unique(Smat[,.(location,industry,year,S_ij)])
  }

  if(!static){
    N_i <- collapsed[, list(denom_i = sum(sharevar)), list(location,year)]
    S_ij <- collapsed[, list(numer_ij=sharevar), list(location,industry,year)]
    S_ij <- merge(S_ij,N_i,by=c('location','year'))
    S_ij <- S_ij[,list(location,industry,year,S_ij=numer_ij/denom_i)]
    G_j <- collapsed[,list(industry,year,G_ij=vaD,omega_ij=sharevar)]
    G_j <- G_j[, list(G_j = sum(G_ij*omega_ij)/sum(omega_ij)), list(industry,year)]
    Smat <- merge(S_ij,G_j,by=c('industry','year'))
    S_ij2 <- unique(Smat[,.(location,industry,year,S_ij)])
  }




  collapsed <- setDT(read.csv(file=sprintf("%sdescriptives/LMS_bartik_collapse_%s_%s.csv",intermed_dir,lag_length,suffix)))
  setnames(collapsed,c('naics2','cz','years'),c('industry','location','year'))
  collapsed[, weights := copy(stayers)]

  outcomes <- collapsed[,list(vaD=weighted.mean(vaD,weights),wagesD=weighted.mean(wagesD,weights),weights=sum(as.numeric(weights)), W=sum(W), WL=sum(WL), VA=sum(VA), VAL=sum(VAL)),list(location,year)]
  outcomes[, logWD := log(W) - log(WL)]
  outcomes[, logVAD := log(VA) - log(VAL)]


  # run Bartik

  run_bartik_wrapper <- function(xvar='vaD',yvar='wagesD'){

    inputs <- SSIV_panelID(location_outcomes=copy(outcomes), industry_shocks=copy(G_j), industry_location_shares=copy(S_ij2), time_variable='year')
    inputs$location_outcomes <- SSIV_outcomes(location_outcomes = inputs$location_outcomes,
                                              time_variable = "year", unit_identifier = "location",
                                              X_var = xvar, Y_var = yvar,  W_var = "weights",
                                              linear_covariates = c(), FE_covariates = c())
    setnames(inputs$location_outcomes, c(xvar,yvar,paste0('W_i_',yvar)), c('X_i','Y_i','W_i'))
    res <- SSIV(location_outcomes = copy(inputs$location_outcomes),
                industry_shocks = copy(inputs$industry_shocks), equally_weighted=equally_weight,
                industry_location_shares=copy(inputs$industry_location_shares), methods = c("BHJ","Uncorrected"))

    return(res)
  }

  res1 <- run_bartik_wrapper(xvar='vaD',yvar='wagesD')[, measured := 'meanlogdiff'][, parameter := 'passthrough']
  res2 <- run_bartik_wrapper(xvar='logVAD',yvar='logWD')[, measured := 'logdiffsum'][, parameter := 'passthrough']
  res3 <- run_bartik_wrapper(yvar='vaD',xvar='wagesD')[, measured := 'meanlogdiff'][, parameter := 'LSelasticity_plus1']
  res4 <- run_bartik_wrapper(yvar='logVAD',xvar='logWD')[, measured := 'logdiffsum'][, parameter := 'LSelasticity_plus1']
  res5 <- rbindlist(list(res1,res2,res3,res4),use.names=T,fill=T)

  # write.csv(res5,file=sprintf("results/params/LMS_bartikIV_lag%s_static%s_sharevar%s_eqwgt%s.csv",lag_length,static,sharevarname,equally_weight),row.names=F)
  # print(res5[])
  return(res5)

}


bartik_gather_results <- function(results_dir, intermed_dir, industry='naics2', location='cz', broadmarkets=TRUE){

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets)
  res1 <- get_bartik_results(lag_length=1,sharevarname='VAL',static=F,equally_weight=T)[, initialweights := FALSE]
  res2 <- get_bartik_results(lag_length=1,sharevarname='VAL',static=T,equally_weight=T)[, initialweights := TRUE]
  res3 <- rbind(res1,res2)
  for(varname in names(res3)){
    if(res3[,class(get(varname))=='numeric']){
      res3[, (varname) := round(get(varname),6)]
    }
  }
  write.csv(res3,file=sprintf("%sparams/LMS_shiftshare_%s.csv",results_dir,suffix),row.names=F)

}



