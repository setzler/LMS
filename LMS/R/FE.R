

#' Main AKM application
#' @export
lms.fe.akm <- function(full_data, flatage = 40, min_unique_movers = 2, tol = 0, iter = 100, residualize = T, va_adjust = F) {
  assertDataTable(full_data)
  assertIntegerish(min_unique_movers)
  for (varname in c("firm_ID", "worker_ID", "log_wage", "year")) {
    if (!(varname %in% names(full_data))) {
      stop(sprintf("Must have variable %s in full_data.", varname))
    }
  }
  if (residualize) {
    assertIntegerish(flatage)
    for (varname in c("age")) {
      if (!(varname %in% names(full_data))) {
        stop(sprintf("Must have variable %s in full_data.", varname))
      }
    }
  }
  if (va_adjust) {
    for (varname in c("log_va_init", "market", "beta_full", "rho_r")) {
      if (!(varname %in% names(full_data))) {
        stop(sprintf("Must have variable %s in full_data.", varname))
      }
    }
  }

  # initialize wages_pt
  full_data[, wages_pt := copy(log_wage)]

  sample_info <- data.table(step = "initial", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])
  print(sample_info)
  flog.info("flatage %s, min_unique_movers %s, iter %s, tol %s, residualize %s", flatage, min_unique_movers, iter, tol, residualize)


  # residualize wages
  if (residualize) {
    flog.info("Residualizing wages. Mean wage, var before resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
    full_data[, `:=`(ones = 1, agesq = (age - flatage)^2, agecu = (age - flatage)^3)]
    full_data <- lms.passthrough.transform_residualize(
      input_data = full_data,
      residualize_var = "wages_pt",
      discrete_vars = "year",
      continuous_vars = c("ones", "agesq", "agecu"),
      return_new_vars = T
    )
    full_data[, `:=`(ones = NULL, agesq = NULL, agecu = NULL)]
    full_data[, wages_pt := NULL]
    setnames(full_data, "wages_pt_residual", "wages_pt")
    sample_info <- rbindlist(list(sample_info, data.table(step = "residualize", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])), use.names = T, fill = T)
    flog.info("Done residualizing wages. Mean wage, var after resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
    print(sample_info)
  }

  # adjust for market va
  if (va_adjust) {
    full_data[, log_va := copy(log_va_init)]
    if (residualize) {
      full_data[, log_va := log_va - mean(log_va), list(year)]
    }
    full_data[, log_va_firm_mean := mean(log_va), list(firm_ID)]
    full_data[, log_va_marketyear_mean := mean(log_va), list(market, year)]
    full_data[, adj_term_jt := rho_r / (rho_r + beta_full) * (log_va - log_va_marketyear_mean) + 1 / (1 + beta_full) * log_va_marketyear_mean]
    full_data[, adj_term_jt_res := adj_term_jt - mean(adj_term_jt), list(firm_ID)]
    full_data[, wages_pt_preadjust := copy(wages_pt)]
    full_data[, wages_pt := wages_pt - adj_term_jt_res]
    flog.info("After adjustment. Mean wage, var: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
  }

  # keep only firms with a sufficient number of movers
  flog.info("Finding unique movers")
  long_data <- akm.subset_unique_movers(copy(full_data), min_unique_movers = min_unique_movers)
  full_data <- full_data[firm_ID %in% unique(long_data$firm_ID)]
  invisible(gc())
  sample_info <- rbindlist(list(sample_info, data.table(step = "min_movers", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])), use.names = T, fill = T)
  print(sample_info)

  # find the connected set of firms, keep only workers in that subset
  flog.info("Finding connected set")
  connected_set_of_firms <- akm.connected_set(long_data)
  long_data <- long_data[firm_ID %in% unique(connected_set_of_firms$largest)]
  full_data <- full_data[firm_ID %in% unique(connected_set_of_firms$largest)]
  invisible(gc())
  sample_info <- rbindlist(list(sample_info, data.table(step = "connected_set", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])), use.names = T, fill = T)
  print(sample_info)

  # estimate AKM firm effects
  flog.info("Estimating firm effects")
  firm_effects <- akm.zig_zag(long_data, outcome_var = "wages_pt", tol = tol, max_iterations = iter)
  invisible(gc())

  # merge firm effects back to full data
  flog.info("Merging firm effects back to full data")
  full_data <- merge(full_data, firm_effects$firms_with_fixedeffects, by = "firm_ID")
  full_data[, akm_worker_effect := mean(wages_pt - akm_firm_effect), by = "worker_ID"]
  invisible(gc())
  sample_info <- rbindlist(list(sample_info, data.table(step = "has_firm_effect", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])), use.names = T, fill = T)
  print(sample_info)

  return(list(full_data = full_data, sample_info = sample_info))
}


#' Main BLM
#' @export
lms.fe.blm <- function(full_data, min_unique_movers = 2, firmclusters = 10, flatage = 40, residualize = T, return_preclustered_data = F, cluster_sdata_only = F, three_and_three = FALSE, va_adjust = FALSE) {
  assertDataTable(full_data)
  assertIntegerish(min_unique_movers)
  for (varname in c("firm_ID", "worker_ID", "log_wage", "year")) {
    if (!(varname %in% names(full_data))) {
      stop(sprintf("Must have variable %s in full_data.", varname))
    }
  }
  if (residualize) {
    assertIntegerish(flatage)
    for (varname in c("age")) {
      if (!(varname %in% names(full_data))) {
        stop(sprintf("Must have variable %s in full_data.", varname))
      }
    }
  }

  # initialize wage measure
  full_data[, wages_pt := copy(log_wage)]
  sample_info <- data.table(step = "initial", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])
  print(sample_info)


  flog.info("flatage %s, min_unique_movers %s, residualize %s, return pre-cluster data %s, cluster using only the sdata %s, 3-and-3 spells %s, number of clusters %s", flatage, min_unique_movers, residualize, return_preclustered_data, cluster_sdata_only, three_and_three, firmclusters)

  # residualize wages
  if (residualize) {
    flog.info("Residualizing wages. Mean wage, var before resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
    full_data[, `:=`(ones = 1, agesq = (age - flatage)^2, agecu = (age - flatage)^3)]
    full_data <- lms.passthrough.transform_residualize(
      input_data = full_data,
      residualize_var = "wages_pt",
      discrete_vars = "year",
      continuous_vars = c("ones", "agesq", "agecu"),
      return_new_vars = T
    )
    full_data[, `:=`(ones = NULL, agesq = NULL, agecu = NULL)]
    full_data[, wages_pt := NULL]
    setnames(full_data, "wages_pt_residual", "wages_pt")
    sample_info <- rbindlist(list(sample_info, data.table(step = "residualize", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])), use.names = T, fill = T)
    print(sample_info)
    flog.info("Done residualizing wages. Mean wage, var after resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
  }


  # adjust for market va
  if (va_adjust) {
    for (varname in c("log_va_init", "market", "beta_full", "rho_r")) {
      if (!(varname %in% names(full_data))) {
        stop(sprintf("Must have variable %s in full_data.", varname))
      }
    }
    full_data[, log_va := copy(log_va_init)]
    if (residualize) {
      full_data[, log_va := log_va - mean(log_va), list(year)]
    }
    full_data[, log_va_firm_mean := mean(log_va), list(firm_ID)]
    full_data[, log_va_marketyear_mean := mean(log_va), list(market, year)]
    full_data[, adj_term_jt := rho_r / (rho_r + beta_full) * (log_va - log_va_marketyear_mean) + 1 / (1 + beta_full) * log_va_marketyear_mean]
    full_data[, adj_term_jt_res := adj_term_jt - mean(adj_term_jt), list(firm_ID)]
    full_data[, wages_pt_preadjust := copy(wages_pt)]
    full_data[, wages_pt := wages_pt - adj_term_jt_res]
    flog.info("After adjustment. Mean wage, var after resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
  }

  # create sdata and jdata
  flog.info("Creating sdata and jdata")

  if (three_and_three) {
    timevarying_vars <- c("wages_pt", "firm_ID")

    flog.info("Preparing 3-and-3 spells in wide format")
    DT <- lms.passthrough.reshape_longToWide(
      long_data = full_data,
      id_var = "worker_ID", time_var = "year", constant_vars = NULL,
      timevarying_vars = timevarying_vars, lags = 1:5, balance_panel = T
    )
    DT <- DT[complete.cases(DT)]

    sdata <- DT[firm_ID == firm_ID_L1 & firm_ID == firm_ID_L2 & firm_ID == firm_ID_L3 & firm_ID == firm_ID_L4 & firm_ID == firm_ID_L5]
    jdata <- DT[firm_ID == firm_ID_L1 & firm_ID == firm_ID_L2 & firm_ID_L3 == firm_ID_L4 & firm_ID_L3 == firm_ID_L5 & firm_ID != firm_ID_L3]

    sdata <- sdata[, list(wid = worker_ID, f1 = firm_ID_L1, y1 = wages_pt_L4, y2 = wages_pt_L1)]
    jdata <- jdata[, list(wid = worker_ID, f1 = firm_ID_L4, f2 = firm_ID_L1, y1 = wages_pt_L4, y2 = wages_pt_L1)]
  } else {
    flog.info("Preparing AKM-style mover spells in long format")

    full_data <- full_data[order(worker_ID, year)]
    collapsed <- full_data[, list(wages_pt = mean(wages_pt, na.rm = T)), by = c("worker_ID", "firm_ID")]
    collapsed[, `:=`(spells = .N, spell = 1:.N), worker_ID]

    sdata <- collapsed[spells == 1][, spells := NULL][, spell := NULL]
    sdata <- sdata[, list(wid = worker_ID, f1 = firm_ID, y1 = wages_pt, y2 = wages_pt)]

    jdata <- collapsed[spells > 1][, spells := NULL]
    jdata2 <- copy(jdata)
    jdata2[, spell := spell - 1]
    setnames(jdata2, c("firm_ID", "wages_pt"), c("firm_ID2", "wages_pt2"))
    jdata <- merge(jdata, jdata2, by = c("worker_ID", "spell"))
    jdata <- jdata[, list(wid = worker_ID, f1 = firm_ID, f2 = firm_ID2, y1 = wages_pt, y2 = wages_pt2)]

    jdata2 <- NULL
    collapsed <- NULL
    invisible(gc())
  }


  # deal with unique movers
  if (min_unique_movers > 0) {
    flog.info("Keeping only those with at least %s movers", min_unique_movers)
    full_data[, max_firm_ID := max(firm_ID), worker_ID]
    full_data[, min_firm_ID := min(firm_ID), worker_ID]
    full_data[, mover := as.integer(max_firm_ID != min_firm_ID)]
    data_uniquemovers <- full_data[mover == 1]
    data_uniquemovers[, uniquemovers := length(unique(worker_ID)), by = c("firm_ID")]
    data_uniquemovers <- unique(data_uniquemovers[, .(firm_ID, uniquemovers)])
    full_data[, `:=`(max_firm_ID = NULL, min_firm_ID = NULL, mover = NULL)]

    full_data <- merge(full_data, data_uniquemovers, by = "firm_ID", all.x = T)
    sdata <- merge(sdata, data_uniquemovers, by.x = "f1", by.y = "firm_ID", all.x = T)
    jdata <- merge(jdata, data_uniquemovers, by.x = "f1", by.y = "firm_ID", all.x = T)
    setnames(data_uniquemovers, "uniquemovers", "uniquemovers2")
    jdata <- merge(jdata, data_uniquemovers, by.x = "f2", by.y = "firm_ID", all.x = T)

    # impose restriction
    flog.info("size before minimum movers restriction: full_data %s, sdata %s, jdata %s", nrow(full_data), nrow(sdata), nrow(jdata))
    full_data <- full_data[uniquemovers >= min_unique_movers]
    sdata <- sdata[uniquemovers >= min_unique_movers]
    jdata <- jdata[uniquemovers >= min_unique_movers & uniquemovers2 >= min_unique_movers]

    # clean-up
    full_data[, uniquemovers := NULL]
    sdata[, uniquemovers := NULL]
    jdata[, uniquemovers := NULL]
    invisible(gc())
    flog.info("size after minimum movers restriction: full_data %s, sdata %s, jdata %s", nrow(full_data), nrow(sdata), nrow(jdata))
    sample_info <- rbindlist(list(sample_info, data.table(step = "min_movers", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])), use.names = T, fill = T)
    print(sample_info)
  }

  # find the connected set of firms, keep only workers in that subset
  if (!three_and_three) {
    flog.info("Finding connected set")
    connected_set_of_firms <- akm.connected_set(full_data)
    connected_firms <- unique(connected_set_of_firms$largest)
    full_data <- full_data[firm_ID %in% connected_firms]
    sdata <- sdata[f1 %in% connected_firms]
    jdata <- jdata[f1 %in% connected_firms & f2 %in% connected_firms]
    invisible(gc())
    sample_info <- rbindlist(list(sample_info, data.table(step = "connected_set", workeryears = nrow(full_data), firmyears = nrow(unique(full_data[, .(firm_ID, year)])), unique_workers = full_data[, length(unique(worker_ID))], unique_firms = full_data[, length(unique(firm_ID))], mean_wage = full_data[, mean(wages_pt)], var_wage = full_data[, var(wages_pt)])), use.names = T, fill = T)
    print(sample_info)
  }

  if (return_preclustered_data) {
    return(list(full_data = full_data, sdata = sdata, jdata = jdata))
  }

  # get ready for BLM estimation
  sdata_long <- rbind(sdata[, list(wid, f1, y1)], sdata[, list(wid, f1, y1 = y2)])
  data2 <- full_data[!(worker_ID %in% jdata$wid)]
  setnames(data2, c("worker_ID", "firm_ID", "wages_pt"), c("wid", "f1", "y1"))

  flog.info("running grouping.getMeasures and grouping.classify.once with %s firm clusters", firmclusters)
  ad <- list(sdata = data2, jdata = jdata)
  if (cluster_sdata_only) {
    ad <- list(sdata = sdata_long[, .(wid, f1, y1)], jdata = jdata)
  }
  combined_data <- rbindlist(list(ad$jdata[, list(wid, f1, y1)], ad$sdata[, list(wid, f1, y1)]))
  ms <- grouping.getMeasures(list(sdata = combined_data, jdata = ad$jdata), "ecdf", Nw = 20, y_var = "y1")
  grps <- grouping.classify.once(ms, k = firmclusters, nstart = 100, step = 5)

  flog.info("running grouping.append")
  sdata[, f1 := paste(f1)]
  jdata[, f1 := paste(f1)]
  jdata[, f2 := paste(f2)]
  ad$sdata <- sdata
  ad$jdata <- jdata
  ad <- grouping.append(ad, grps$best_cluster, drop = T)
  ad$sdata$x <- 1

  return(list(full_data = full_data, sdata = ad$sdata, jdata = ad$jdata))
}




#' AKM post-estimation
#' @export
lms.fe.akm.post_estimation <- function(res, va_adjust = FALSE) {
  full_data <- res$full_data
  full_data[, total_firm := mean(log_wage), list(firm_ID)]
  full_data[, total_within := log_wage - total_firm]
  full_data[, mover := as.integer(min(firm_ID) != max(firm_ID)), list(worker_ID)]


  if (!va_adjust) {
    full_data[, WE := akm_worker_effect + wages_pt_predict]
    full_data[, FE := copy(akm_firm_effect)]
    full_data[, WE_firm := mean(WE), list(firm_ID)]
    full_data[, total_firm := mean(log_wage), list(firm_ID)]
    full_data[, WE_within := WE - WE_firm]
    full_data[, total_within := log_wage - total_firm]
    full_data[, predictable := WE + FE]
  }


  if (va_adjust) {
    setnames(full_data, "akm_firm_effect", "akm_firm_effect_static")
    full_data[, akm_firm_effect := akm_firm_effect_static + adj_term_jt_res]
    full_data[, FEbar := copy(akm_firm_effect_static)]
    full_data[, FE := copy(akm_firm_effect)]
    full_data[, WE := copy(akm_worker_effect)]
    full_data[, WE_firm := mean(WE), list(firm_ID)]
    full_data[, WE_within := WE - WE_firm]
    full_data[, FEdev := FE - FEbar]
    full_data[, predictable := WE + FE]
  }

  res$full_data <- full_data
  return(res)
}




#' AKM decomp
#' @export
lms.fe.akm.decomp <- function(res, va_adjust = FALSE) {
  full_data <- res$full_data


  if (!va_adjust) {
    decomp <- with(full_data, data.table(
      total_var = var(log_wage), Rsquared = var(predictable) / var(log_wage),
      total_firm_var = var(total_firm), total_within_var = var(total_within),
      WE_var = var(WE), WE_firm_var = var(WE_firm), WE_within_var = var(WE_within),
      FE_var = var(FE), FE_WE_cov = cov(FE, WE), FE_WE_cor = cor(FE, WE),
      WEonly_var = var(akm_worker_effect), FE_WEonly_cov = cov(FE, akm_worker_effect), FE_WEonly_cor = cor(FE, akm_worker_effect),
      Xb_var = var(wages_pt_predict), FE_Xb_cov = cov(FE, wages_pt_predict), FE_Xb_cor = cor(FE, wages_pt_predict),
      WEonly_Xb_cov = cov(akm_worker_effect, wages_pt_predict), WEonly_Xb_cor = cor(akm_worker_effect, wages_pt_predict)
    ))
  }


  if (va_adjust) {
    decomp <- with(full_data, data.table(
      total_var = var(log_wage), Rsquared = var(predictable) / var(log_wage),
      total_firm_var = var(total_firm), total_within_var = var(total_within),
      WE_var = var(WE), WE_firm_var = var(WE_firm), WE_within_var = var(WE_within),
      FE_var = var(FE), FE_WE_cov = cov(FE, WE), FE_WE_cor = cor(FE, WE),
      FEbar_var = var(FEbar), FEbar_WE_cov = cov(FEbar, WE), FEbar_WE_cor = cor(FEbar, WE),
      FEdev_var = var(FEdev), FEdev_WE_cov = cov(FEdev, WE), FEdev_WE_cor = cor(FEdev, WE)
    ))
  }

  decomp$stayers <- full_data[mover == 0][, length(unique(worker_ID))]
  decomp$movers <- full_data[mover == 1][, length(unique(worker_ID))]
  decomp$firms <- full_data[, length(unique(firm_ID))]

  return(list(decomp = decomp, sample_info = res$sample_info))
}




#' BLM post-estimation
#' @export
lms.fe.blm.post_estimation <- function(res, va_adjust = FALSE, subsample_rate = 1.0) {
  full_data <- res$full_data
  full_data[, total_firm := mean(log_wage), list(firm_ID)]
  full_data[, total_within := log_wage - total_firm]
  full_data[, mover := as.integer(min(firm_ID) != max(firm_ID)), list(worker_ID)]

  # linear
  flog.info("getting linear decomposition")
  blm_linear <- m2.mini.estimate(copy(res$jdata), copy(res$sdata), method = "linear.ss", bigk = 1)
  base_clusters <- unique(res$sdata[, list(firm_ID = f1, cluster = j1)])
  firmeffects_linear <- data.table(cluster = 1:10, FE = blm_linear$A1)
  clusters <- merge(base_clusters, firmeffects_linear, by = "cluster")[order(firm_ID)]
  clusters[, firm_ID := as.numeric(firm_ID)]
  full_data <- full_data[order(firm_ID, year)]
  full_data <- merge(full_data, clusters, by = c("firm_ID"))

  # switch to non-linear
  flog.info("getting non-linear decomposition")
  res2 <- list(sdata = res$sdata, jdata = res$jdata)
  if (subsample_rate < 1) {
    flog.info("in the non-linear decomposition, subsampling at rate %s", subsample_rate)
    set.seed(112233)
    res2$sdata[, index := runif(nrow(res2$sdata))]
    res2$sdata[, qindex := ecdf(index)(index), by = c("j1", "j2")]
    res2$sdata <- res2$sdata[qindex <= subsample_rate]
    res2$jdata[, index := runif(nrow(res2$jdata))]
    res2$jdata[, qindex := ecdf(index)(index), by = c("j1", "j2")]
    res2$jdata <- res2$jdata[qindex <= subsample_rate]
    flog.info("initial movers data: %s. subsampled movers data: %s", nrow(res$jdata), nrow(res2$jdata))
    flog.info("initial stayers data: %s. subsampled stayers data: %s", nrow(res$jdata), nrow(res2$jdata))
  }
  blm_nonlinear <- m2.mini.estimate(copy(res2$jdata), copy(res2$sdata), method = "prof", bigk = 1)
  res2 <- NULL
  gc()
  firmeffects_nonlinear <- data.table(cluster = 1:10, FEnl = blm_nonlinear$A1, gamma = blm_nonlinear$B1)
  clusters <- merge(base_clusters, firmeffects_nonlinear, by = "cluster")[order(firm_ID)]
  clusters[, firm_ID := as.numeric(firm_ID)]
  full_data <- full_data[order(firm_ID, year)]
  full_data <- merge(full_data, clusters, by = c("firm_ID", "cluster"))
  full_data[, gamma_bar := mean(gamma)]
  setnames(full_data, "wages_pt_predict", "Xb")

  qt <- 9
  qvals <- (1:qt) / (qt + 1)
  qts <- qnorm(qvals)
  rr <- data.table(l = 1:length(blm_nonlinear$A1), Em = blm_nonlinear$Em, Esd = blm_nonlinear$Esd, N = blm_nonlinear$Ns, A1 = blm_nonlinear$A1, B1 = blm_nonlinear$B1, A2 = blm_nonlinear$A2, B2 = blm_nonlinear$B2)
  alpha_m <- rr[, wtd.mean(Em, N)]
  alpha_sd <- sqrt(rr[, wtd.mean(Esd^2, N) + wtd.var(Em, N)])
  rr2 <- rr[, list(FEnl = A1, gamma = B1, y1 = (qts * alpha_sd + alpha_m) * B1 + A1, y2 = (qts * alpha_sd + alpha_m) * B2 + A2, k = 1:qt), l]
  rr2 <- melt(rr2, id.vars = c("l", "k"))
  setDT(rr2)
  rr2 <- rr2[variable %in% c("y1", "FEnl", "gamma")]
  # rr2[, variable := NULL]
  qmat <- data.table(k = 1:qt, qk = qvals)
  rr2 <- merge(rr2, qmat, by = "k", all.x = T)
  setnames(rr2, c("l", "k", "qk"), c("FirmCluster", "WorkerEffect", "WorkerEffectQ"))



  if (va_adjust) {
    setnames(full_data, "FE", "FEbar")
    setnames(full_data, "FEnl", "FEbarnl")
    full_data[, FE := FEbar + adj_term_jt_res]
    full_data[, FEnl := FEbarnl + adj_term_jt_res]
    full_data[, FEdev := FE - FEbar]
    full_data[, FEdevnl := FEnl - FEbarnl]

    full_data[, WE := mean((wages_pt_preadjust - FE)), list(worker_ID)]
    full_data[, WE := WE + Xb]
    full_data[, WE_firm := mean(WE), list(firm_ID)]
    full_data[, WE_within := WE - WE_firm]

    full_data[, xnl := mean((wages_pt_preadjust - FEnl) / gamma), list(worker_ID)]
    full_data[, WEbarnl := xnl * gamma_bar + Xb]
    full_data[, WEnl := xnl * gamma + Xb]

    full_data[, predictable := WE + FE]
    full_data[, predictablenl := WEnl + FEnl]

    full_data[, xbar := mean(xnl)]
    full_data[, psi_tilde := FEbarnl + gamma * xbar]
    full_data[, x_tilde := gamma_bar * (xnl - xbar)]
    full_data[, tau := (gamma - gamma_bar) * (xnl - xbar)]
    full_data[, X_tilde := x_tilde + Xb]
  }

  if (!va_adjust) {
    full_data[, WE := mean((wages_pt - FE)), list(worker_ID)]
    full_data[, WE := WE + Xb]
    full_data[, WE_firm := mean(WE), list(firm_ID)]
    full_data[, WE_within := WE - WE_firm]

    full_data[, xnl := mean((wages_pt - FEnl) / gamma), list(worker_ID)]
    full_data[, WEbarnl := xnl * gamma_bar + Xb]
    full_data[, WEnl := xnl * gamma + Xb]
    full_data[, predictablenl := WEnl + FEnl]
    full_data[, predictable := WE + FE]

    full_data[, xbar := mean(xnl)]
    full_data[, psi_tilde := FEnl + gamma * xbar]
    full_data[, x_tilde := gamma_bar * (xnl - xbar)]
    full_data[, tau := (gamma - gamma_bar) * (xnl - xbar)]
    full_data[, X_tilde := x_tilde + Xb]
  }

  rr2 <- rbindlist(list(rr2, data.table(variable = "xbar", value = full_data[, mean(xbar)])), use.names = T, fill = T)

  # get X group shares
  full_data[, Xq := ecdf(x_tilde)(x_tilde)]
  k <- 5
  full_data[, Xgroup := 0]
  for (i in 1:(k - 1) / k) {
    full_data[, Xgroup := Xgroup + as.integer(Xq > i)]
  }
  full_data[, Xgroup := Xgroup + 1]
  cluster_sizes <- full_data[, list(raw_wage = mean(log_wage), resid_wage = mean(wages_pt), size = .N, FEnl = mean(FEnl), gamma = mean(gamma), xnl = mean(xnl), xbar = mean(xbar)), list(cluster, Xgroup)][order(cluster, Xgroup)]


  res$full_data <- full_data
  res$interactions <- rr2
  res$FEnonlinear <- firmeffects_nonlinear
  res$cluster_sizes <- cluster_sizes
  return(res)
}





#' BLM decomp
#' @export
lms.fe.blm.decomp <- function(res, va_adjust = FALSE) {
  full_data <- res$full_data

  if (va_adjust) {
    decomp <- with(full_data, data.table(
      total_var = var(log_wage), Rsquared = var(predictable) / var(log_wage), Rsquarednl = var(predictablenl) / var(log_wage),
      total_firm_var = var(total_firm), total_within_var = var(total_within),
      WE_var = var(WE), WE_firm_var = var(WE_firm), WE_within_var = var(WE_within),
      FE_var = var(FE), FE_WE_cov = cov(FE, WE), FE_WE_cor = cor(FE, WE),
      FEbar_var = var(FEbar), FEbar_WE_cov = cov(FEbar, WE), FEbar_WE_cor = cor(FEbar, WE),
      FEdev_var = var(FEdev), FEdev_WE_cov = cov(FEdev, WE), FEdev_WE_cor = cor(FEdev, WE),
      psi_tilde_var = var(psi_tilde), x_tilde_var = var(X_tilde), tau_var = var(tau),
      x_plus_psi_tilde_with_tau_cov = cov(X_tilde + psi_tilde, tau),
      x_with_psi_tilde_cov = cov(X_tilde, psi_tilde),
      interactions = var(FEnl - FEbarnl) + 2 * cov(FEnl - FEbarnl, X_tilde + tau)
    ))
  }

  if (!va_adjust) {
    decomp <- with(full_data, data.table(
      total_var = var(log_wage), Rsquared = var(predictable) / var(log_wage), Rsquarednl = var(predictablenl) / var(log_wage),
      total_firm_var = var(total_firm), total_within_var = var(total_within),
      WE_var = var(WE), WE_firm_var = var(WE_firm), WE_within_var = var(WE_within),
      FE_var = var(FE), FE_WE_cov = cov(FE, WE), FE_WE_cor = cor(FE, WE),
      psi_tilde_var = var(psi_tilde), x_tilde_var = var(X_tilde), tau_var = var(tau),
      x_plus_psi_tilde_with_tau_cov = cov(X_tilde + psi_tilde, tau),
      x_with_psi_tilde_cov = cov(X_tilde, psi_tilde)
    ))
  }

  decomp$stayers <- res$sdata[, length(unique(wid))]
  decomp$movers <- res$jdata[, length(unique(wid))]
  decomp$firms <- res$full_data[, length(unique(firm_ID))]

  mstats <- res$jdata[, list(y1 = mean(y1), y2 = mean(y2), .N), list(j1, j2)][order(j1, j2)]
  mstats <- mstats[N >= 100]
  cstats <- res$sdata[, list(y1 = mean(y1), y2 = mean(y2), .N), list(j1)][order(j1)]
  cstats <- cstats[N >= 100]

  return(list(decomp = decomp, interactions = res$interactions, FEnonlinear = res$FEnonlinear, cluster_sizes = res$cluster_sizes, cstats = cstats, mstats = mstats))
}






#' Cluster markets and group worker effects by quantile bins
#' @export
lms.fe.cluster_markets <- function(long_data, marketclusters = 10) {
  long_data <- long_data[order(worker_ID, year)]
  collapsed <- long_data[, list(wages_pt = mean(wages_pt, na.rm = T)), by = c("worker_ID", "market")]
  collapsed[, `:=`(spells = .N, spell = 1:.N), worker_ID]

  sdata <- collapsed[spells == 1][, spells := NULL][, spell := NULL]
  sdata <- sdata[, list(wid = worker_ID, f1 = market, y1 = wages_pt, y2 = wages_pt)]

  jdata <- collapsed[spells > 1][, spells := NULL]
  jdata2 <- copy(jdata)
  jdata2[, spell := spell - 1]
  setnames(jdata2, c("market", "wages_pt"), c("market2", "wages_pt2"))
  jdata <- merge(jdata, jdata2, by = c("worker_ID", "spell"))
  jdata <- jdata[, list(wid = worker_ID, f1 = market, f2 = market2, y1 = wages_pt, y2 = wages_pt2)]

  jdata2 <- NULL
  collapsed <- NULL
  invisible(gc())

  ad_o <- list(sdata = sdata, jdata = jdata)
  ms_o <- grouping.getMeasures(ad_o, "ecdf", Nw = 20, y_var = "y1")
  grps_o <- grouping.classify.once(ms_o, k = marketclusters, nstart = 100, step = 5)

  sdata[, f1 := paste(f1)]
  sdata[, f2 := paste(f1)]
  jdata[, f1 := paste(f1)]
  jdata[, f2 := paste(f2)]

  ad <- list()
  ad$sdata <- sdata
  ad$jdata <- jdata
  ad <- grouping.append(ad, grps_o$best_cluster, drop = T)

  ad$sdata[, type := "sdata"]
  ad$jdata[, type := "jdata"]
  res <- rbindlist(ad, use.names = T)

  base_clusters <- unique(rbind(res[, list(market = f1, market_cluster = j1)], res[, list(market = f2, market_cluster = j2)]))
  base_clusters[, market := as.numeric(market)]
  long_data <- merge(long_data, base_clusters, by = c("market"))

  return(long_data)
}







#' Main AKM application, subsampling for limited mobility bias analysis
#' @export
lms.fe.akm.subsample <- function(full_data, flatage = 40, min_unique_movers = 15, iter = 100, tol = 0, residualize = T, shares = c(0.25, .5, .75), num_random = 2, num_cores = 1) {
  flog.info("flatage %s, min_unique_movers %s, iter %s, tol %s, residualize %s", flatage, min_unique_movers, iter, tol, residualize)

  assertDataTable(full_data)
  assertIntegerish(min_unique_movers)
  for (varname in c("firm_ID", "worker_ID", "log_wage", "year")) {
    if (!(varname %in% names(full_data))) {
      stop(sprintf("Must have variable %s in full_data.", varname))
    }
  }
  if (residualize) {
    assertIntegerish(flatage)
    for (varname in c("age")) {
      if (!(varname %in% names(full_data))) {
        stop(sprintf("Must have variable %s in full_data.", varname))
      }
    }
  }

  full_data[, wages_pt := copy(log_wage)]

  # residualize wages
  if (residualize) {
    flog.info("Residualizing wages. Mean wage, var before resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
    full_data[, `:=`(ones = 1, agesq = (age - flatage)^2, agecu = (age - flatage)^3)]
    full_data <- lms.passthrough.transform_residualize(
      input_data = full_data,
      residualize_var = "wages_pt",
      discrete_vars = "year",
      continuous_vars = c("ones", "agesq", "agecu"),
      return_new_vars = T
    )
    full_data[, `:=`(ones = NULL, agesq = NULL, agecu = NULL)]
    full_data[, wages_pt := NULL]
    setnames(full_data, "wages_pt_residual", "wages_pt")
    flog.info("Done residualizing wages. Mean wage, var after resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
  }

  # keep only firms with a sufficient number of movers
  flog.info("Finding unique movers")
  long_data <- akm.subset_unique_movers(copy(full_data), min_unique_movers = min_unique_movers)
  full_data_full <- full_data[firm_ID %in% unique(long_data$firm_ID)]
  invisible(gc())

  long_data_full <- long_data[min_firm_ID != max_firm_ID]
  long_data <- NULL
  full_data <- NULL
  invisible(gc())


  all_res <- NULL
  for (share_mover_keep in shares) {
    random_subsample <- function(i) {
      flog.info("share %s, sample %s", share_mover_keep, i)

      long_data <- long_data_full[rank(runif(.N)) / .N <= share_mover_keep]
      invisible(gc())

      # find the connected set of firms, keep only workers in that subset
      flog.info("Finding connected set")
      connected_set_of_firms <- akm.connected_set(long_data)
      long_data <- long_data[firm_ID %in% unique(connected_set_of_firms$largest)]
      invisible(gc())

      # estimate AKM firm effects
      flog.info("Estimating firm effects")
      firm_effects <- akm.zig_zag(long_data, outcome_var = "wages_pt", tol = tol, max_iterations = iter)
      invisible(gc())

      # merge firm effects back to full data
      flog.info("Merging firm effects back to full data")
      full_data <- merge(copy(full_data_full), firm_effects$firms_with_fixedeffects, by = "firm_ID")
      invisible(gc())

      # computes AKM worker effects, given by average deviation from firm effect
      flog.info("Constructing worker effects and idiosyncratic term")
      full_data[, akm_worker_effect := mean(wages_pt - akm_firm_effect), by = "worker_ID"]
      full_data[, akm_idiosyncratic := wages_pt - akm_worker_effect - akm_firm_effect]

      res <- akm.extract_statistics(full_data, outcome_var = "wages_pt", outcome_predicted = "wages_pt_predict")
      res$keep_share <- share_mover_keep
      res$random_draw <- i
      res$N_jdata <- nrow(long_data)
      res$N_sdata <- nrow(full_data)
      return(res)
    }

    res <- rbindlist(parallel::mclapply(1:num_random, random_subsample, mc.cores = num_cores), use.names = T)
    all_res <- rbindlist(list(all_res, res), use.names = T)
  }

  return(all_res)
}





#' BLM subsample application, subsampling for limited mobility bias analysis
#' @export
lms.application.blm.subsample <- function(full_data, min_unique_movers = 15, firmclusters = 10, flatage = 40, residualize = T, cluster_sdata_only = F, shares = c(0.25, .5, .75), num_random = 2, num_cores = 2) {
  flog.info("flatage %s, min_unique_movers %s, residualize %s, cluster using only the sdata %s", flatage, min_unique_movers, residualize, cluster_sdata_only)

  assertDataTable(full_data)
  assertIntegerish(min_unique_movers)
  for (varname in c("firm_ID", "worker_ID", "log_wage", "year")) {
    if (!(varname %in% names(full_data))) {
      stop(sprintf("Must have variable %s in full_data.", varname))
    }
  }
  if (residualize) {
    assertIntegerish(flatage)
    for (varname in c("age")) {
      if (!(varname %in% names(full_data))) {
        stop(sprintf("Must have variable %s in full_data.", varname))
      }
    }
  }

  # prepare wage measure
  full_data[, wages_pt := copy(log_wage)]

  # residualize wages
  if (residualize) {
    flog.info("Residualizing wages. Mean wage, var before resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
    full_data[, `:=`(ones = 1, agesq = (age - flatage)^2, agecu = (age - flatage)^3)]
    full_data <- lms.passthrough.transform_residualize(
      input_data = full_data,
      residualize_var = "wages_pt",
      discrete_vars = "year",
      continuous_vars = c("ones", "agesq", "agecu"),
      return_new_vars = F
    )
    full_data[, `:=`(ones = NULL, agesq = NULL, agecu = NULL)]
    flog.info("Done residualizing wages. Mean wage, var after resid: %s, %s", full_data[, mean(wages_pt)], full_data[, var(wages_pt)])
  }

  # create sdata and jdata
  flog.info("Preparing spells in wide format")
  DT <- lms.passthrough.reshape_longToWide(
    long_data = full_data,
    id_var = "worker_ID", time_var = "year", constant_vars = NULL,
    timevarying_vars = c("wages_pt", "firm_ID"), lags = 1:5, balance_panel = T
  )
  DT <- DT[complete.cases(DT)]

  flog.info("Creating sdata and jdata")
  sdata <- DT[firm_ID == firm_ID_L1 & firm_ID == firm_ID_L2 & firm_ID == firm_ID_L3 & firm_ID == firm_ID_L4 & firm_ID == firm_ID_L5]
  jdata <- DT[firm_ID == firm_ID_L1 & firm_ID == firm_ID_L2 & firm_ID_L3 == firm_ID_L4 & firm_ID_L3 == firm_ID_L5 & firm_ID != firm_ID_L3]

  # deal with unique movers
  if (min_unique_movers > 0) {
    flog.info("Keeping only those with at least %s movers", min_unique_movers)
    full_data[, max_firm_ID := max(firm_ID), worker_ID]
    full_data[, min_firm_ID := min(firm_ID), worker_ID]
    full_data[, mover := as.integer(max_firm_ID != min_firm_ID)]
    data_uniquemovers <- full_data[mover == 1]
    data_uniquemovers[, uniquemovers := length(unique(worker_ID)), by = c("firm_ID")]
    data_uniquemovers <- unique(data_uniquemovers[, .(firm_ID, uniquemovers)])
    full_data[, `:=`(max_firm_ID = NULL, min_firm_ID = NULL, mover = NULL)]

    full_data <- merge(full_data, data_uniquemovers, by = "firm_ID", all.x = T)
    sdata <- merge(sdata, data_uniquemovers, by = "firm_ID", all.x = T)
    jdata <- merge(jdata, data_uniquemovers, by = "firm_ID", all.x = T)
    setnames(data_uniquemovers, "uniquemovers", "uniquemovers_L4")
    jdata <- merge(jdata, data_uniquemovers, by.x = "firm_ID_L4", by.y = "firm_ID", all.x = T)

    # impose restriction
    flog.info("size before minimum movers restriction: full_data %s, sdata %s, jdata %s", nrow(full_data), nrow(sdata), nrow(jdata))
    full_data <- full_data[uniquemovers >= min_unique_movers]
    sdata <- sdata[uniquemovers >= min_unique_movers]
    jdata <- jdata[uniquemovers >= min_unique_movers & uniquemovers_L4 >= min_unique_movers]

    # clean-up
    full_data[, uniquemovers := NULL]
    sdata[, uniquemovers := NULL]
    jdata[, uniquemovers := NULL]
    invisible(gc())
    flog.info("size after minimum movers restriction: full_data %s, sdata %s, jdata %s", nrow(full_data), nrow(sdata), nrow(jdata))
  }

  sdata_full <- copy(sdata)
  jdata_full <- copy(jdata)


  all_res <- NULL
  for (share_mover_keep in shares) {
    random_subsample <- function(i) {
      jdata <- jdata_full[rank(runif(.N)) / .N <= share_mover_keep]

      # get ready for BLM estimation
      sdata_long <- rbind(sdata[, list(wid = worker_ID, f1 = firm_ID_L1, y1 = wages_pt_L1)], sdata[, list(wid = worker_ID, f1 = firm_ID_L4, y1 = wages_pt_L4)])
      sdata_wide <- sdata[, list(wid = worker_ID, f1 = firm_ID_L1, y1 = wages_pt_L4, y2 = wages_pt_L1)]
      jdata_sub <- jdata[, list(wid = worker_ID, f1 = firm_ID_L4, f2 = firm_ID_L1, y1 = wages_pt_L4, y2 = wages_pt_L1)]
      data2 <- full_data[!(worker_ID %in% jdata_sub$wid)]
      setnames(data2, c("worker_ID", "firm_ID", "wages_pt"), c("wid", "f1", "y1"))

      flog.info("running grouping.getMeasures and grouping.classify.once with %s firm clusters", firmclusters)
      ad <- list(sdata = data2, jdata = jdata_sub)
      if (cluster_sdata_only) {
        ad <- list(sdata = sdata_long[, .(wid, f1, y1)], jdata = jdata_sub)
      }
      ms <- grouping.getMeasures(ad, "ecdf", Nw = 20, y_var = "y1")
      grps <- grouping.classify.once(ms, k = firmclusters, nstart = 100, step = 5)

      flog.info("running grouping.append")
      sdata_wide[, f1 := paste(f1)]
      jdata_sub[, f1 := paste(f1)]
      jdata_sub[, f2 := paste(f2)]
      ad$sdata <- sdata_wide
      ad$jdata <- jdata_sub
      ad <- grouping.append(ad, grps$best_cluster, drop = T)
      ad$sdata$x <- 1

      # run BLM estimation
      flog.info("running m2.mini.estimate with method='linear.ss'")
      blm_res <- m2.mini.estimate(ad$jdata, ad$sdata, method = "linear.ss", bigk = 1)
      res_mat <- as.data.table(blm_res$vdec$cc)
      res_mat$var <- names(res_mat)
      res_mat$keep_share <- share_mover_keep
      res_mat$random_draw <- i
      res_mat$N_sdata <- nrow(ad$sdata)
      res_mat$N_jdata <- nrow(ad$jdata)

      return(res_mat)
    }

    res <- rbindlist(parallel::mclapply(1:num_random, random_subsample, mc.cores = num_cores), use.names = T)
    all_res <- rbindlist(list(all_res, res), use.names = T)
  }

  return(all_res)
}
