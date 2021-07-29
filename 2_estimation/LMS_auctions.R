

KLMS.options <- function(spec, outcome_variable = NA, seed=0) {
  # define the default options
  collapse_table <- data.table(a = c("pre", "impact", "post", "before", "after", "before_all", "after_all"), b = c(list(c(-4, -3, -1)), list(0:1), list(2:4), list(-4:-3), list(0:2), list(c(-4, -3, -1)), list(0:4)))
  opts <- list(
    spec = spec,
    outcome_variable = outcome_variable,
    unit_var = "firm_ID",
    cal_time_var = "year",
    onset_time_var = "win_yr",
    cluster_vars = "firm_ID",
    omitted_event_time = -2,
    event_vs_noevent = TRUE,
    add_unit_fes = TRUE,
    never_treat_action = "keep",
    max_control_gap = Inf,
    min_control_gap = 1,
    control_subset_event_time = 0,
    treated_subset_event_time = 0,
    ref_cont_covar_event_time = 0,
    control_subset_var = "bid_active",
    treated_subset_var = NA,
    ref_cont_covars = NULL,
    ref_cont_covar_event_time = 0,
    max_rank = NA,
    max_loss_margin = NA,
    calculate_collapse_estimates = TRUE,
    collapse_inputs = collapse_table,
    cross_sectional_spec = FALSE,
    return_clean_data = FALSE,
    seed = seed
  )
  return(opts)
}


KLMS.dataloader <- function(data_dir,opts) {
  flog.info("Beginning data import.")
  # load data
  inputs <- setDT(read.csv(file = paste0(data_dir,"auction_sample.csv")))
  if(opts$seed > 0){
    flog.info("randomly subsampling with seed %s",opts$seed)
    inputs <- lms.block_sample(long_data = inputs, unit_var='firm_ID', seed=opts$seed)
  }
  # drop if outcome var missing
  inputs <- inputs[!is.na(get(opts$outcome_variable))]
  # finish
  invisible(gc())
  return(inputs)
}


KLMS.wrap_ES <- function(opts, input_data) {
  # execute eventStudy::ES(), could use a match.arg here but seems safer to manually provide arguments
  results <- ES(
    long_data = input_data,
    outcomevar = opts$outcome_variable,
    event_vs_noevent = opts$event_vs_noevent,
    add_unit_fes = opts$add_unit_fes,
    unit_var = opts$unit_var,
    cal_time_var = opts$cal_time_var,
    onset_time_var = opts$onset_time_var,
    cluster_vars = opts$cluster_vars,
    omitted_event_time = opts$omitted_event_time,
    never_treat_action = opts$never_treat_action,
    max_control_gap = opts$max_control_gap,
    min_control_gap = opts$min_control_gap,
    control_subset_var = opts$control_subset_var,
    control_subset_event_time = opts$control_subset_event_time,
    treated_subset_var = opts$treated_subset_var,
    treated_subset_event_time = opts$treated_subset_event_time,
    ref_cont_covars = opts$ref_cont_covars,
    ref_cont_covar_event_time = opts$ref_cont_covar_event_time,
    calculate_collapse_estimates = opts$calculate_collapse_estimates,
    collapse_inputs = opts$collapse_inputs,
    cross_sectional_spec = opts$cross_sectional_spec,
    return_clean_data = opts$return_clean_data
  )
  # keep relevant output and export
  if (opts$calculate_collapse_estimates) {
    results <- results[(ref_event_time >= -6 & ref_event_time <= 4) | !is.na(grouping)]
    results <- results[rn == "att"]
    results <- results[str_detect(ref_onset_time, "Equally-Weighted")]
  } else {
    results <- results[(ref_event_time >= -6 & ref_event_time <= 4)]
    results <- results[rn == "catt"]
  }
  # delete junk columns
  results[, c("rn", "total_unique_units", "t", "pval", "reg_sample_size", "treated", "catt_total_unique_units", "catt_treated_unique_units", "cohort_weight_V1", "cohort_weight_V2") := NULL]
  return(results)
}



KLMS.event_study.baseline <- function(spec, results_dir, data_dir, ncores = 1, seed=0) {
  assertCharacter(spec, len = 1, any.missing = F)
  assertIntegerish(ncores, len = 1, lower = 1, any.missing = F)
  wrap_regression <- function(outcome_variable) {
    flog.info("********* Starting %s, outcome %s *********", spec, outcome_variable)
    opts <- KLMS.options(spec = spec, seed = seed, outcome_variable = outcome_variable)
    # execute the regression for this set of options
    input_data <- KLMS.dataloader(data_dir,opts)
    if(str_detect(spec,'LSelasticity')){
      winners <- input_data[win_yr >= year & win_yr <= year+2 & !is.na(win_yr)]
      median_wagebillperworker <- winners[,median(wagebill_fte/workers_fte,na.rm=T)]
      median_profitperworker <- winners[,median(EBITD/workers_fte,na.rm=T)]
      reg_data <- input_data[bid_active==TRUE & (is.na(win_yr) | (year - win_yr) <= 2)]
      reg_data[, winner := as.numeric(!is.na(win_yr) & year >= win_yr)]
      reg_data[, TFP := log_costsold - 1.089 * log_workers_fte]
      mod <- felm(log_EBITD ~ winner + TFP, data = reg_data)
      dPi <- mod$coefficients['winner',]
    }
    results <- KLMS.wrap_ES(opts = opts, input_data = input_data)
    # record information on the specification
    results$spec <- spec
    results$outcome_variable <- outcome_variable
    if(str_detect(spec,'LSelasticity')){
      results$median_wagebillperworker <- median_wagebillperworker/1e3
      results$median_profitperworker <- median_profitperworker/1e3
      results$dPi <- dPi
    }
    # export
    flog.info("********* Finished %s, outcome %s *********", spec, outcome_variable)
    return(results)
  }
  # execute regressions in parallel, then rbind together
  results <- mclapply(c("log_EBITD","log_wagebill_fte", "log_workers_fte", "log_meanwage_fte"), wrap_regression, mc.cores = ncores)
  results <- rbindlist(results, use.names = T, fill = T)
  
  # finish
  return(results)
}



KLMS.event_study.baseline.SE <- function(spec, results_dir, data_dir, ncores = 1, max_seed=0) {
  
  if(!str_detect(spec,'LSelasticity')){
    stop("Can only bootstrap SEs with KLMS.event_study.baseline.SE for LSelasticity specs.")
  }
  
  # run bootstrap
  the_func <- function(seed){
    ncores=3
    if(str_detect(spec,'forLMS')){
      ncores=4
    }
    res <- KLMS.event_study.baseline(spec=spec,results_dir=results_dir, data_dir=data_dir, ncores = ncores, seed=seed)
    res$seed = seed
    return(res)
  }
  bootstrap_estimates <- mclapply(0:max_seed, the_func, mc.cores=ncores)
  bootstrap_estimates <- rbindlist(bootstrap_estimates, use.names=T, fill=T)
  bootstrap_estimates_before <- bootstrap_estimates[grouping=='before'][,.(seed, outcome_variable, estimate, cluster_se, median_wagebillperworker, median_profitperworker, dPi)]
  bootstrap_estimates <- bootstrap_estimates[grouping=='after'][,.(seed, outcome_variable, estimate, cluster_se, median_wagebillperworker, median_profitperworker, dPi)]
  
  # SEs
  W_standard_errors_before <- bootstrap_estimates_before[seed>0 & outcome_variable=='log_meanwage_fte'][,list(seed, dW_before = estimate)]
  B_standard_errors_before <- bootstrap_estimates_before[seed>0 & outcome_variable=='log_wagebill_fte'][,list(seed, dB_before = estimate)]
  L_standard_errors_before <- bootstrap_estimates_before[seed>0 & outcome_variable=='log_workers_fte'][,list(seed, dL_before = estimate)]
  W_standard_errors <- bootstrap_estimates[seed>0 & outcome_variable=='log_meanwage_fte'][,list(seed, dW = estimate)]
  B_standard_errors <- bootstrap_estimates[seed>0 & outcome_variable=='log_wagebill_fte'][,list(seed, dB = estimate)]
  L_standard_errors <- bootstrap_estimates[seed>0 & outcome_variable=='log_workers_fte'][,list(seed, dL = estimate, WBperL = median_wagebillperworker, PiperL = median_profitperworker, dPi = dPi)]
  standard_errors <- merge(B_standard_errors, L_standard_errors, by='seed')
  standard_errors <- merge(standard_errors, W_standard_errors, by='seed')
  standard_errors <- merge(standard_errors, B_standard_errors_before, by='seed')
  standard_errors <- merge(standard_errors, L_standard_errors_before, by='seed')
  standard_errors <- merge(standard_errors, W_standard_errors_before, by='seed')
  
  if(str_detect(spec,'forLMS')){
    EBITD_standard_errors_before <- bootstrap_estimates_before[seed>0 & outcome_variable=='log_EBITD'][,list(seed, dEBITD_before = estimate)]
    EBITD_standard_errors <- bootstrap_estimates[seed>0 & outcome_variable=='log_EBITD'][,list(seed, dEBITD = estimate)]
    standard_errors <- merge(standard_errors, EBITD_standard_errors, by='seed')
    standard_errors <- merge(standard_errors, EBITD_standard_errors_before, by='seed')
    standard_errors[, passthrough := (dB-dL)/dEBITD]
    standard_errors[, passthroughLS := 1/passthrough - 1]
  }
  
  standard_errors[, oneplustheta := dB/dL]
  standard_errors[, inversetheta := 1/(oneplustheta-1)]
  standard_errors[, markdown := inversetheta/(1+inversetheta)]
  standard_errors[, dRw := WBperL*dB*1/(1+inversetheta)]
  standard_errors[, Rw := WBperL*1/(1+inversetheta)]
  standard_errors[, Rf := PiperL]
  standard_errors[, dRf := PiperL*dPi]
  standard_errors[, Rw_share := Rw/(Rw+Rf)]
  standard_errors[, dRw_share := dRw/(dRw+dRf)]
  
  this_stat <- function(x){sd(x)}
  SEs <- standard_errors[,list(dB_se = this_stat(dB), dL_se = this_stat(dL), dW_se = this_stat(dW), dB_before_se = this_stat(dB_before), dL_before_se = this_stat(dL_before), dW_before_se = this_stat(dW_before), dPi_se = this_stat(dPi), oneplustheta_se = this_stat(oneplustheta), inversetheta_se = this_stat(inversetheta), markdown_se = this_stat(markdown), WBperL_se = this_stat(WBperL), PiperL_se = this_stat(PiperL), dRw_se = this_stat(dRw), Rw_se = this_stat(Rw), dRf_se = this_stat(dRf), Rf_se = this_stat(Rf), Rw_share_se=this_stat(Rw_share), dRw_share_se = this_stat(dRw_share))]
  this_stat <- function(x){as.numeric(quantile(x,.025)[1])}
  LBs <- standard_errors[,list(dB_lb = this_stat(dB), dL_lb = this_stat(dL), dW_lb = this_stat(dW), dB_before_lb = this_stat(dB_before), dL_before_lb = this_stat(dL_before), dW_before_lb = this_stat(dW_before), dPi_lb = this_stat(dPi), oneplustheta_lb = this_stat(oneplustheta), inversetheta_lb = this_stat(inversetheta), markdown_lb = this_stat(markdown), WBperL_lb = this_stat(WBperL), PiperL_lb = this_stat(PiperL), dRw_lb = this_stat(dRw), Rw_lb = this_stat(Rw), dRf_lb = this_stat(dRf), Rf_lb = this_stat(Rf), Rw_share_lb=this_stat(Rw_share), dRw_share_lb = this_stat(dRw_share))]
  this_stat <- function(x){as.numeric(quantile(x,.975)[1])}
  UBs <- standard_errors[,list(dB_ub = this_stat(dB), dL_ub = this_stat(dL), dW_ub = this_stat(dW), dB_before_ub = this_stat(dB_before), dL_before_ub = this_stat(dL_before), dW_before_ub = this_stat(dW_before), dPi_ub = this_stat(dPi), oneplustheta_ub = this_stat(oneplustheta), inversetheta_ub = this_stat(inversetheta), markdown_ub = this_stat(markdown), WBperL_ub = this_stat(WBperL), PiperL_ub = this_stat(PiperL), dRw_ub = this_stat(dRw), Rw_ub = this_stat(Rw), dRf_ub = this_stat(dRf), Rf_ub = this_stat(Rf), Rw_share_ub=this_stat(Rw_share), dRw_share_ub = this_stat(dRw_share))]
  
  if(str_detect(spec,'forLMS')){
    this_stat <- function(x){sd(x)}
    SEs <- standard_errors[,list(dEBITD_se = this_stat(dEBITD), dEBITD_before_se = this_stat(dEBITD_before), passthrough_se = this_stat(passthrough), passthroughLS_se = this_stat(passthroughLS), dB_se = this_stat(dB), dL_se = this_stat(dL), dW_se = this_stat(dW), dB_before_se = this_stat(dB_before), dL_before_se = this_stat(dL_before), dW_before_se = this_stat(dW_before), dPi_se = this_stat(dPi), oneplustheta_se = this_stat(oneplustheta), inversetheta_se = this_stat(inversetheta), markdown_se = this_stat(markdown), WBperL_se = this_stat(WBperL), PiperL_se = this_stat(PiperL), dRw_se = this_stat(dRw), Rw_se = this_stat(Rw), dRf_se = this_stat(dRf), Rf_se = this_stat(Rf), Rw_share_se=this_stat(Rw_share), dRw_share_se = this_stat(dRw_share))]
    this_stat <- function(x){as.numeric(quantile(x,.025)[1])}
    LBs <- standard_errors[,list(dEBITD_lb = this_stat(dEBITD), dEBITD_before_lb = this_stat(dEBITD_before), passthrough_lb = this_stat(passthrough), passthroughLS_lb = this_stat(passthroughLS), dB_lb = this_stat(dB), dL_lb = this_stat(dL), dW_lb = this_stat(dW), dB_before_lb = this_stat(dB_before), dL_before_lb = this_stat(dL_before), dW_before_lb = this_stat(dW_before), dPi_lb = this_stat(dPi), oneplustheta_lb = this_stat(oneplustheta), inversetheta_lb = this_stat(inversetheta), markdown_lb = this_stat(markdown), WBperL_lb = this_stat(WBperL), PiperL_lb = this_stat(PiperL), dRw_lb = this_stat(dRw), Rw_lb = this_stat(Rw), dRf_lb = this_stat(dRf), Rf_lb = this_stat(Rf), Rw_share_lb=this_stat(Rw_share), dRw_share_lb = this_stat(dRw_share))]
    this_stat <- function(x){as.numeric(quantile(x,.975)[1])}
    UBs <- standard_errors[,list(dEBITD_ub = this_stat(dEBITD), dEBITD_before_ub = this_stat(dEBITD_before), passthrough_ub = this_stat(passthrough), passthroughLS_ub = this_stat(passthroughLS), dB_ub = this_stat(dB), dL_ub = this_stat(dL), dW_ub = this_stat(dW), dB_before_ub = this_stat(dB_before), dL_before_ub = this_stat(dL_before), dW_before_ub = this_stat(dW_before), dPi_ub = this_stat(dPi), oneplustheta_ub = this_stat(oneplustheta), inversetheta_ub = this_stat(inversetheta), markdown_ub = this_stat(markdown), WBperL_ub = this_stat(WBperL), PiperL_ub = this_stat(PiperL), dRw_ub = this_stat(dRw), Rw_ub = this_stat(Rw), dRf_ub = this_stat(dRf), Rf_ub = this_stat(Rf), Rw_share_ub=this_stat(Rw_share), dRw_share_ub = this_stat(dRw_share))]
  }
  
  
  
  
  # points
  point_estimates <- bootstrap_estimates[seed==0][,list(outcome_variable, estimate,  cluster_se, WBperL = median_wagebillperworker, PiperL = median_profitperworker, dPi = dPi)]
  point_estimates_before <- bootstrap_estimates_before[seed==0][,list(outcome_variable, estimate,  cluster_se, WBperL = median_wagebillperworker, PiperL = median_profitperworker, dPi = dPi)]
  oneplustheta <- point_estimates[outcome_variable=='log_wagebill_fte']$estimate / point_estimates[outcome_variable=='log_workers_fte']$estimate
  inversetheta <- 1/(oneplustheta-1)
  markdown <- inversetheta/(1+inversetheta)
  dL <- point_estimates[outcome_variable=='log_workers_fte']$estimate
  dB <- point_estimates[outcome_variable=='log_wagebill_fte']$estimate
  dW <- point_estimates[outcome_variable=='log_meanwage_fte']$estimate
  dL_before <- point_estimates_before[outcome_variable=='log_workers_fte']$estimate
  dB_before <- point_estimates_before[outcome_variable=='log_wagebill_fte']$estimate
  dW_before <- point_estimates_before[outcome_variable=='log_meanwage_fte']$estimate
  if(str_detect(spec,'forLMS')){
    dEBITD <- point_estimates[outcome_variable=='log_EBITD']$estimate
    dEBITD_before <- point_estimates_before[outcome_variable=='log_EBITD']$estimate
    passthrough <- (dB-dL)/dEBITD
    passthroughLS <- 1/passthrough - 1
  }
  WBperL <- point_estimates[outcome_variable=='log_workers_fte']$WBperL
  PiperL <- point_estimates[outcome_variable=='log_workers_fte']$PiperL
  dPi <- point_estimates[outcome_variable=='log_workers_fte']$dPi
  dRw <- WBperL * 1/(1+inversetheta) * point_estimates[outcome_variable=='log_wagebill_fte']$estimate
  Rw <- WBperL * 1/(1+inversetheta)
  Rf <- PiperL
  dRf <- PiperL*dPi
  Rw_share <- Rw/(Rw+Rf)
  dRw_share <- dRw/(dRw+dRf)
  
  # combine results
  point_estimates <- data.table(outcome_variable="dL", estimate=dL, cluster_se = SEs$dL_se, lb = LBs$dL_lb, ub = UBs$dL_ub)
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dB", estimate=dB, cluster_se = SEs$dB_se, lb = LBs$dB_lb, ub = UBs$dB_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dW", estimate=dW, cluster_se = SEs$dW_se, lb = LBs$dW_lb, ub = UBs$dW_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dL_before", estimate=dL_before, cluster_se = SEs$dL_before_se, lb = LBs$dL_before_lb, ub = UBs$dL_before_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dB_before", estimate=dB_before, cluster_se = SEs$dB_before_se, lb = LBs$dB_before_lb, ub = UBs$dB_before_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dW_before", estimate=dW_before, cluster_se = SEs$dW_before_se, lb = LBs$dW_before_lb, ub = UBs$dW_before_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dPi", estimate=dPi, cluster_se = SEs$dPi_se, lb = LBs$dPi_lb, ub = UBs$dPi_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="oneplustheta", estimate=oneplustheta, cluster_se = SEs$oneplustheta_se, lb = LBs$oneplustheta_lb, ub = UBs$oneplustheta_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="inversetheta", estimate=inversetheta, cluster_se = SEs$inversetheta_se, lb = LBs$inversetheta_lb, ub = UBs$inversetheta_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="markdown", estimate=markdown, cluster_se = SEs$markdown_se, lb = LBs$markdown_lb, ub = UBs$markdown_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="WBperL", estimate=WBperL, cluster_se = SEs$WBperL_se, lb = LBs$WBperL_lb, ub = UBs$WBperL_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="PiperL", estimate=PiperL, cluster_se = SEs$PiperL_se, lb = LBs$PiperL_lb, ub = UBs$PiperL_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dRw", estimate=dRw, cluster_se = SEs$dRw_se, lb = LBs$dRw_lb, ub = UBs$dRw_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="Rw", estimate=Rw, cluster_se = SEs$Rw_se, lb = LBs$Rw_lb, ub = UBs$Rw_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dRf", estimate=dRf, cluster_se = SEs$dRf_se, lb = LBs$dRf_lb, ub = UBs$dRf_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="Rf", estimate=Rf, cluster_se = SEs$Rf_se, lb = LBs$Rf_lb, ub = UBs$Rf_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="dRw_share", estimate=dRw_share, cluster_se = SEs$dRw_share_se, lb = LBs$dRw_share_lb, ub = UBs$dRw_share_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="Rw_share", estimate=Rw_share, cluster_se = SEs$Rw_share_se, lb = LBs$Rw_share_lb, ub = UBs$Rw_share_ub))
  point_estimates <- rbind(point_estimates, data.table(outcome_variable="bootstraps", estimate=max_seed, cluster_se = NA, lb=NA, ub=NA))
  
  
  if(str_detect(spec,'forLMS')){
    point_estimates <- rbind(point_estimates, data.table(outcome_variable="dEBITD", estimate=dEBITD, cluster_se = SEs$dEBITD_se, lb = LBs$dEBITD_lb, ub = UBs$dEBITD_ub))
    point_estimates <- rbind(point_estimates, data.table(outcome_variable="dEBITD_before", estimate=dEBITD_before, cluster_se = SEs$dEBITD_before_se, lb = LBs$dEBITD_before_lb, ub = UBs$dEBITD_before_ub))
    point_estimates <- rbind(point_estimates, data.table(outcome_variable="passthrough", estimate=passthrough, cluster_se = SEs$passthrough_se, lb = LBs$passthrough_lb, ub = UBs$passthrough_ub))
    point_estimates <- rbind(point_estimates, data.table(outcome_variable="passthroughLS", estimate=passthroughLS, cluster_se = SEs$passthroughLS_se, lb = LBs$passthroughLS_lb, ub = UBs$passthroughLS_ub))
  }
  
  write.csv(point_estimates, file = sprintf("%sparams/results_%s.csv", results_dir, spec), row.names = F)
  
}


