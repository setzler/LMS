


#' Apply all passthrough methods from this package.
#' @param long_data Data in long format.
#' @param id_var ID variable.
#' @param time_var Time variable.
#' @param firm_defn Firm identifier.
#' @param market_defn Market identifier.
#' @param spell_length Length of a spell.
#' @param firm_stayer Flag for whether or not workers must stay in same firm over spell_length.
#' @param market_stayer Flag for whether or not workers must stay in same market over spell_length.
#' @param workers_per_firm Number of id_var spells per firm_defn spells.
#' @param firms_per_market Number of firm_defn spells per market_defn spells.
#' @param workers_per_market Number of id_var spells per market_defn spells.
#' @param sample_counts Collects sample size counts (e.g., unique id_var values) if TRUE.
#' @param sample_info_only Returns sample info and skips estimation if TRUE.
#' @param covmat_numcores Number of cores to use in computing the covariance matrix.
#' @param outcomes Must include "wages". Can also include "firmsize".
#' @param collapse_to_firm Indicator for collapsing to firm-level between residualizing and constructing market means.
#' @export
lms.passthrough <- function(long_data, id_var, time_var, firm_defn, market_defn,
                            spell_length, firm_stayer, market_stayer,
                            workers_per_firm = NA, firms_per_market = NA, workers_per_market = NA,
                            sample_counts = T, sample_info_only = F,
                            covmat_numcores = 2, outcomes = c("wages", "va", "firmsize"),
                            collapse_to_firm = FALSE) {


  ######### 1. assertions #########

  # checks of required arguments
  assertDataTable(long_data)
  assertCharacter(id_var, len = 1)
  assertCharacter(time_var, len = 1)
  assertCharacter(outcomes)
  assertIntegerish(spell_length, lower = 1)
  stopifnot("wages" %in% outcomes)
  stopifnot("va" %in% outcomes)

  # firms
  if (!is.na(firm_defn)) {
    assertCharacter(firm_defn, len = 1)
  }

  # markets
  if (!is.na(market_defn)) {
    assertCharacter(market_defn, len = 1)
  }

  # counts per group
  if (!is.na(workers_per_firm)) {
    assertIntegerish(workers_per_firm, lower = 0)
  }
  if (!is.na(firms_per_market)) {
    assertIntegerish(firms_per_market, lower = 0)
  }
  if (!is.na(workers_per_market)) {
    assertIntegerish(workers_per_market, lower = 0)
  }
  assertFlag(sample_counts)

  # required variables
  required_vars <- c(id_var, time_var, outcomes, "age")
  if (!is.na(firm_defn)) {
    required_vars <- c(required_vars, firm_defn)
  }
  if (!is.na(market_defn)) {
    required_vars <- c(required_vars, market_defn)
  }
  for (req_var in required_vars) {
    if (!(req_var %in% names(long_data))) {
      stop(sprintf("`long_data` must include variable %s", req_var))
    }
  }
  long_data <- long_data[, .SD, .SDcols = required_vars]
  long_data <- na.omit(long_data)
  invisible(gc())


  ######### 2. sample construction #########

  flog.info("Imposing sample restrictions")
  long_data <- long_data %>% lms.passthrough.select_init()
  if (sample_counts) {
    full_sample_info <- cbind(step = "initial", get_sample_info(long_data, id_var, time_var, firm_defn, market_defn))
  }

  # require worker-spells to be stayers within firm-spells
  if (firm_stayer) {
    flog.info("Imposing firm stayer in %s spell", firm_defn)
    long_data <- long_data %>% lms.passthrough.select_stayers(id_var = id_var, time_var = time_var, stayer_var = firm_defn, spell_length = spell_length)
    if (sample_counts) {
      full_sample_info <- rbindlist(list(full_sample_info, cbind(step = "firmStayer", get_sample_info(long_data, id_var, time_var, firm_defn, market_defn))), use.names = T)
    }
  }

  # require worker-spells to be stayers within market-spells
  if (market_stayer) {
    flog.info("Imposing market stayer in %s spell", market_defn)
    long_data <- long_data %>% lms.passthrough.select_stayers(id_var = id_var, time_var = time_var, stayer_var = market_defn, spell_length = spell_length)
    if (sample_counts) {
      full_sample_info <- rbindlist(list(full_sample_info, cbind(step = "marketStayer", get_sample_info(long_data, id_var, time_var, firm_defn, market_defn))), use.names = T)
    }
  }

  # require sufficient worker-spells per firm-spell
  if (!is.na(workers_per_firm) & (workers_per_firm > 0)) {
    flog.info("Imposing %s worker-spells per firm-spell", workers_per_firm)
    long_data <- long_data %>% lms.passthrough.select_unitCount(id_var = id_var, time_var = time_var, stayer_var = firm_defn, spell_length = spell_length, min_units = workers_per_firm)
    if (sample_counts) {
      full_sample_info <- rbindlist(list(full_sample_info, cbind(step = "workersPerFirm", get_sample_info(long_data, id_var, time_var, firm_defn, market_defn))), use.names = T)
    }
  }

  # require sufficient firm-spells per market-spell
  if (!is.na(firms_per_market) & (firms_per_market > 0)) {
    flog.info("Imposing %s firm-spells per market-spell", firms_per_market)
    long_data <- long_data %>% lms.passthrough.select_groupCount(id_var = id_var, time_var = time_var, stayer_var = firm_defn, group_var = market_defn, spell_length = spell_length, min_units = firms_per_market)
    if (sample_counts) {
      full_sample_info <- rbindlist(list(full_sample_info, cbind(step = "firmsPerMarket", get_sample_info(long_data, id_var, time_var, firm_defn, market_defn))), use.names = T)
    }
  }

  # require sufficient worker-spells per market-spell
  if (!is.na(workers_per_market) & (workers_per_market > 0)) {
    flog.info("Imposing %s worker-spells per market-spell", workers_per_market)
    long_data <- long_data %>% lms.passthrough.select_unitCount(id_var = id_var, time_var = time_var, stayer_var = market_defn, spell_length = spell_length, min_units = workers_per_market)
    if (sample_counts) {
      full_sample_info <- rbindlist(list(full_sample_info, cbind(step = "workersPerMarket", get_sample_info(long_data, id_var, time_var, firm_defn, market_defn))), use.names = T)
    }
  }

  invisible(gc())
  if (sample_counts) {
    flog.info("Sample size information:")
    print(full_sample_info)
    if (sample_info_only) {
      return(full_sample_info)
    }
  }


  ######### 4. variable construction #########

  # residualize
  flog.info("Residualizing wages and va")
  long_data[, `:=`(agesq = (age - 40)^2, agecu = (age - 40)^3), ]
  long_data <- long_data %>% lms.passthrough.transform_residualize(residualize_var = "wages", discrete_vars = time_var, continuous_vars = c("agesq", "agecu"), return_new_vars = F)
  if ("meanwage" %in% outcomes) {
    long_data <- long_data %>% lms.passthrough.transform_residualize(residualize_var = "meanwage", discrete_vars = time_var, continuous_vars = c("agesq", "agecu"), return_new_vars = F)
  }
  if ("wagebill" %in% outcomes) {
    long_data <- long_data %>% lms.passthrough.transform_residualize(residualize_var = "wagebill", discrete_vars = time_var, continuous_vars = c("agesq", "agecu"), return_new_vars = F)
  }
  if ("residsize" %in% outcomes) {
    long_data <- long_data %>% lms.passthrough.transform_residualize(residualize_var = "residsize", discrete_vars = time_var, continuous_vars = c("agesq", "agecu"), return_new_vars = F)
  }
  long_data[, `:=`(age = NULL, agesq = NULL, agecu = NULL), ]
  long_data[, va := va - mean(va), by = time_var]
  if ("firmsize" %in% outcomes) {
    flog.info("Residualizing firmsize")
    long_data[, firmsize := firmsize - mean(firmsize), by = time_var]
  }
  invisible(gc())


  # optional collapse to firm-level
  if (collapse_to_firm) {
    id_var <- firm_defn # the ID is now the firm, not the worker
    long_data <- long_data[, lapply(.SD, mean), .SDcols = outcomes, by = c(id_var, market_defn, time_var)]
  }

  if (!is.na(market_defn)) {
    # form market aggregates
    flog.info("Forming market mean of wages")
    long_data[, `:=`(wagesmean = mean(wages), vamean = mean(va)), by = c(market_defn, time_var)]
    long_data[, `:=`(wagesnet = wages - wagesmean, vanet = va - vamean)]
    outcomes <- c(outcomes, "wagesnet", "wagesmean", "vanet", "vamean")
    if ("firmsize" %in% outcomes) {
      flog.info("Forming market mean of firmsize")
      long_data[, `:=`(firmsizemean = mean(firmsize)), by = c(market_defn, time_var)]
      long_data[, `:=`(firmsizenet = firmsize - firmsizemean)]
      outcomes <- c(outcomes, "firmsizenet", "firmsizemean")
    }
    if ("wagebill" %in% outcomes) {
      flog.info("Forming market mean of wagebill")
      long_data[, `:=`(wagebillmean = mean(wagebill)), by = c(market_defn, time_var)]
      long_data[, `:=`(wagebillnet = wagebill - wagebillmean)]
      outcomes <- c(outcomes, "wagebillnet", "wagebillmean")
    }
    if ("meanwage" %in% outcomes) {
      flog.info("Forming market mean of meanwage")
      long_data[, `:=`(meanwagemean = mean(meanwage)), by = c(market_defn, time_var)]
      long_data[, `:=`(meanwagenet = meanwage - meanwagemean)]
      outcomes <- c(outcomes, "meanwagenet", "meanwagemean")
    }
    if ("residsize" %in% outcomes) {
      flog.info("Forming market mean of residsize")
      long_data[, `:=`(residsizemean = mean(residsize)), by = c(market_defn, time_var)]
      long_data[, `:=`(residsizenet = residsize - residsizemean)]
      outcomes <- c(outcomes, "residsizenet", "residsizemean")
    }
  }


  ######### 5. collect covariances and export #########

  # flip wide
  wide_data <- lms.passthrough.reshape_longToWide(long_data = long_data, id_var, time_var, constant_vars = NULL, timevarying_vars = outcomes, lags = 1:(spell_length - 1), balance_panel = T)
  long_data <- NULL
  invisible(gc())

  # take differences
  for (outcome in outcomes) {
    wide_data <- wide_data %>% lms.passthrough.transform_difference_wide(difference_var = outcome, lags = 1:(spell_length - 1))
  }

  # get covariance matrix
  flog.info("computing covariance using lms.passthrough.cov")
  covmat <- lms.passthrough.cov(wide_data = copy(wide_data), id_var = id_var, time_var = time_var, weight_var = NA, number_cores = covmat_numcores)

  return_list <- list(wide_data = wide_data, covmat = covmat)
  if (sample_counts) {
    return_list$sample_info <- full_sample_info
  }

  return(return_list)
}



#' @export
get_sample_info <- function(long_data, id_var, time_var, firm_defn, market_defn) {
  sample_info <- NULL
  flog.info("Collecting sample size information")
  sample_info <- data.table(
    workeryears = nrow(long_data),
    workers = long_data[, length(unique(get(id_var)))],
    firms = long_data[, length(unique(get(firm_defn)))],
    firmyears = nrow(unique(long_data[spell_end == 1][, .SD, .SDcols = c(firm_defn, time_var)])),
    years = long_data[, length(unique(get(time_var)))],
    spells = long_data[, sum(spell_end)],
    Ew = long_data[, mean(wages)],
    ElogVA = long_data[, mean(va)]
  )
  if(!is.na(market_defn)){
    sample_info$markets = long_data[, length(unique(get(market_defn)))]
  }
  return(sample_info)
}


#' draw a block-bootstrap sample (donated by Michael Graber)
#'
#' \code{lms.block_sample} draws a block-bootstrap sample from a given panel data set
#'
#' @param data    panel in data.table format
#' @param id      character; indicating the column name of the individual index
#' @param time    character; indicating the column name of the time index
#' @return        a data.table with a block bootstrap sample
#' @export
lms.block_sample <- function(long_data, unit_var, seed=1){
  flog.info("beginning bootstrap draw with seed %s",seed)
  # vector of unique cluster IDs and total number of clusters
  set.seed(seed)
  ids <- unique(long_data[, get(unit_var)])
  N   <- length(ids)
  # sample ID's with replacement:
  # we sample ID's with replacement, then collapse the resulting draws to
  # a vector of unique IDs with a column representing the number of times the cluster
  # has been sampled. e.g. if unit_var = c("A","B") and count = c(1,3) then
  # cluster "A" has been sampled once, while cluster "B" has been sampled three times.
  clusters <- data.table(sample(ids, size = N, replace = TRUE))
  names(clusters) <- unit_var
  setkeyv(clusters, unit_var)
  clusters[,count := .N, by = get(unit_var)]
  clusters <- unique(clusters)
  # repeated merges to create block bootstrap sample (bs)
  max_iter <- max(clusters$count)
  bs       <- data.table()
  for (i in 1 : max_iter) {
    tmp <- merge(clusters[count >= i,], long_data, by = unit_var, all.x = TRUE,
                 all.y = FALSE, sort = FALSE)
    tmp[, eval(unit_var) := paste0(get(unit_var), "-", as.character(i))]  # create new unique ID: original ID "-" index of draw
    bs <- rbind(bs, tmp)
  }
  bs[,count := NULL]
  gc()

  return(bs)
}


#' @export
lms.passthrough_bootstrap <- function(long_data, id_var, time_var, firm_defn, market_defn,
                                      spell_length, firm_stayer, market_stayer,
                                      workers_per_firm = NA, firms_per_market = NA, workers_per_market = NA,
                                      covmat_numcores = 1, boot_numcores = 1, num_boots, block_defn) {
  get_boot <- function(ii) {
    flog.info("This is boot %s", ii)
    # create block boostrap sample
    boot <- block_sample(data = copy(long_data), id = block_defn, time = time_var)
    boot[, (paste0(block_defn, "_temp")) := as.integer(.GRP), list(get(block_defn))]
    boot[, (block_defn) := NULL]
    setnames(boot, paste0(block_defn, "_temp"), block_defn)
    # get covariance matrix
    covmat <- lms.passthrough(
      long_data = boot, id_var = id_var, time_var = time_var,
      firm_defn = firm_defn, market_defn = market_defn,
      spell_length = spell_length, firm_stayer = firm_stayer, market_stayer = market_stayer,
      workers_per_firm = workers_per_firm, firms_per_market = firms_per_market, workers_per_market = workers_per_market,
      sample_counts = F, sample_info_only = F, covmat_numcores = covmat_numcores, outcomes = c("wages", "va")
    )$covmat
    covmat[, boot := ii]
    return(covmat)
  }

  res <- rbindlist(mclapply(1:num_boots, get_boot, mc.cores = boot_numcores), use.names = T)
  return(res)
}





#' Infer event times from long covariance matrix
#' @param covmat Covariance matrix produced by lms.passthrough.
#' @param vars Variables to include in the event study.
#' @export
lms.passthrough.covmat_events <- function(covmat, vars = c("wages", "wagesnet", "wagesmean", "va", "vanet", "vamean", "firmsize", "firmsizenet", "firmsizemean")) {

  # clean up covariate names
  covmat[, covariate_1 := as.character(covariate_1)]
  covmat[, covariate_2 := as.character(covariate_2)]

  for (var in vars) {
    covmat[covariate_1 == var, covariate_1 := paste0(var, "_L0")]
    covmat[covariate_2 == var, covariate_2 := paste0(var, "_L0")]
  }

  # construct event times
  covmat[, event_1 := as.numeric(substr(covariate_1, nchar(covariate_1), nchar(covariate_1)))]
  covmat[, event_2 := as.numeric(substr(covariate_2, nchar(covariate_2), nchar(covariate_2)))]
  covmat[, event_1 := min(event_1) - event_1]
  covmat[, event_2 := min(event_2) - event_2]
  covmat[, covariate_1 := substr(covariate_1, 1, nchar(covariate_1) - 1)]
  covmat[, covariate_2 := substr(covariate_2, 1, nchar(covariate_2) - 1)]

  # deal with levels vs differences
  covmat[substr(covariate_1, nchar(covariate_1) - 2, nchar(covariate_1)) == "_DL", covariate_1 := paste0(substr(covariate_1, 1, nchar(covariate_1) - 3), "D")]
  covmat[substr(covariate_2, nchar(covariate_2) - 2, nchar(covariate_2)) == "_DL", covariate_2 := paste0(substr(covariate_2, 1, nchar(covariate_2) - 3), "D")]
  covmat[substr(covariate_1, nchar(covariate_1) - 1, nchar(covariate_1)) == "_L", covariate_1 := substr(covariate_1, 1, nchar(covariate_1) - 2)]
  covmat[substr(covariate_2, nchar(covariate_2) - 1, nchar(covariate_2)) == "_L", covariate_2 := substr(covariate_2, 1, nchar(covariate_2) - 2)]

  return(covmat)
}


#' IV estimation from covariance restrictions
#' @param covmat Covariance matrix produced by lms.passthrough
#' @export
lms.passthrough.covmat_IV <- function(covmat, spell_length = 8, onset_event_time = 5, forward_event_time = 3, backward_event_time = 4, y_name, x_name, z_name) {
  y1 <- covmat[covariate_1 == y_name & event_1 == (onset_event_time - backward_event_time - spell_length) & covariate_2 == z_name & event_2 == (onset_event_time - spell_length)]$covariance_estimate
  y2 <- covmat[covariate_1 == y_name & event_1 == (onset_event_time + forward_event_time - spell_length) & covariate_2 == z_name & event_2 == (onset_event_time - spell_length)]$covariance_estimate
  x1 <- covmat[covariate_1 == x_name & event_1 == (onset_event_time - backward_event_time - spell_length) & covariate_2 == z_name & event_2 == (onset_event_time - spell_length)]$covariance_estimate
  x2 <- covmat[covariate_1 == x_name & event_1 == (onset_event_time + forward_event_time - spell_length) & covariate_2 == z_name & event_2 == (onset_event_time - spell_length)]$covariance_estimate

  return(data.table(onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, reg = (y1 - y2) / (x1 - x2)))
}


#' IV estimation from covariance restrictions
#' @param covmat Covariance matrix produced by lms.passthrough
#' @param time_opts Determine the event times to use
#' @export
lms.passthrough.collect_IV <- function(covmat, time_opts = list(spell_length = 8, onset_event_time = 5, forward_event_time = 3, backward_event_time = 4)) {
  # read IV event time specification
  spell_length <- time_opts$spell_length
  onset_event_time <- time_opts$onset_event_time
  forward_event_time <- time_opts$forward_event_time
  backward_event_time <- time_opts$backward_event_time
  # determine available data
  has_market <- "vamean" %in% covmat[, unique(c(covariate_1, covariate_2))]
  has_firmsize <- "firmsize" %in% covmat[, unique(c(covariate_1, covariate_2))]
  has_wagebill <- "wagebill" %in% covmat[, unique(c(covariate_1, covariate_2))]
  has_meanwage <- "meanwage" %in% covmat[, unique(c(covariate_1, covariate_2))]
  has_residsize <- "residsize" %in% covmat[, unique(c(covariate_1, covariate_2))]
  # estimate IV by case
  rr <- data.table()
  rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "wages", x_name = "va", z_name = "vaD")[, param := "unc_pt"])

  if(has_market){
    rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "wagesnet", x_name = "vanet", z_name = "vanetD")[, param := "net_pt"])
    rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "vamean", x_name = "va", z_name = "vaD")[, param := "selectioncoef"])
    rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "wagesmean", x_name = "vamean", z_name = "vameanD")[, param := "market_pt"])
  }

  if (has_firmsize) {
    rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "firmsize", x_name = "va", z_name = "vaD")[, param := "sizeunc_pt"])
    if(has_market){
      rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "firmsizenet", x_name = "vanet", z_name = "vanetD")[, param := "sizenet_pt"])
      rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "firmsizemean", x_name = "vamean", z_name = "vameanD")[, param := "sizemarket_pt"])
    }
  }

  if (has_wagebill) {
    rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "wagebill", x_name = "va", z_name = "vaD")[, param := "wagebillunc_pt"])
    if(has_market){
      rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "wagebillnet", x_name = "vanet", z_name = "vanetD")[, param := "wagebillnet_pt"])
      rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "wagebillmean", x_name = "vamean", z_name = "vameanD")[, param := "wagebillmarket_pt"])
    }
  }

  if (has_meanwage) {
    rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "meanwage", x_name = "va", z_name = "vaD")[, param := "meanwageunc_pt"])
    if(has_market){
      rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "meanwagenet", x_name = "vanet", z_name = "vanetD")[, param := "meanwagenet_pt"])
      rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "meanwagemean", x_name = "vamean", z_name = "vameanD")[, param := "meanwagemarket_pt"])
    }
  }

  if (has_residsize) {
    rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "residsize", x_name = "va", z_name = "vaD")[, param := "residsizeunc_pt"])
    if(has_market){
      rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "residsizenet", x_name = "vanet", z_name = "vanetD")[, param := "residsizenet_pt"])
      rr %<>% rbind(lms.passthrough.covmat_IV(covmat = covmat, spell_length = spell_length, onset_event_time = onset_event_time, forward_event_time = forward_event_time, backward_event_time = backward_event_time, y_name = "residsizemean", x_name = "vamean", z_name = "vameanD")[, param := "residsizemarket_pt"])
    }
  }

  # finish
  var_names <- rr$param
  rr <- as.data.table(t(rr[, .(reg)]))
  setnames(rr, var_names)
  return(rr)
}


#' DiD
#' @param wide_data wide_data from lms.passthrough output
#' @param id_var identifier
#' @param firmsize whether or not it is included
#' @export
lms.passthrough.collect_diffInDiffs <- function(wide_data, id_var, firmsize = FALSE) {
  net_vars <- "wagesnet"
  unc_vars <- "wages"
  if (firmsize) {
    net_vars <- c(net_vars, "firmsizenet")
    unc_vars <- c(unc_vars, "firmsize")
  }

  rr <- lms.passthrough.diffInDiffs_binarize(dd = wide_data, id_var = id_var, time_var = "years", timevarying_vars = unc_vars, split_var = "va", lags = 0:6, number_cores = 2)[order(tshock, t)]
  rrnet <- lms.passthrough.diffInDiffs_binarize(dd = wide_data, id_var = id_var, time_var = "years", timevarying_vars = net_vars, split_var = "vanet", lags = 0:6, number_cores = 2)[order(tshock, t)]
  rrmean <- lms.passthrough.diffInDiffs_binarize(dd = wide_data, id_var = id_var, time_var = "years", timevarying_vars = c("wagesmean"), split_var = "vamean", lags = 0:6, number_cores = 2)[order(tshock, t)]
  rr <- merge(rr, rrnet, by = c("t", "tshock"))
  rr <- merge(rr, rrmean, by = c("t", "tshock"))
  rr[, event_time := t - tshock]
  rr[, t := NULL][, tshock := NULL]
  rr <- rr[, lapply(.SD, mean), event_time][order(event_time)]
  return(rr)
}

#' Model parameters
#' @param rr passthrough results.
#' @param descriptives descriptive statistics.
#' @param rho_to_1 use for the rho=1 counterfactual.
#' @export
lms.passthrough.collect_params <- function(rr, descriptives, rho_to_1 = FALSE) {
  rr <- merge(rr, descriptives, by = "broad_market")

  helper.getPsiFirm <- function(a, b, r) {
    br <- b / r
    expo <- -(1 - a) * br / (1 + a * br)
    mult <- a * (1 + br) / (1 + a * br)
    base <- br / (1 + br)
    return(1 - mult * (base)^expo)
  }

  helper.getPsiMarket <- function(a, b, r) {
    br <- b / r
    expo <- -(1 - a) * b / (1 + a * b) # this differs from Firm-level
    mult <- a * (1 + br) / (1 + a * br)
    base <- br / (1 + br)
    return(1 - mult * (base)^expo)
  }

  # main parameters
  rr[, market_pt_het := copy(market_pt)]
  rr[, market_pt := weighted.mean(market_pt, workeryears)]
  rr[, beta_het := 1 / market_pt_het - 1]
  rr[, beta := 1 / market_pt - 1]
  rr[, rho := beta * net_pt / (1 - net_pt)]
  if (rho_to_1) {
    rr[, rho := 1.0]
  }
  rr[, alpha := 1 - exp(log(labor_share) - log(beta / rho / (1 + beta / rho)))]
  rr[, PsiFirm := helper.getPsiFirm(alpha, beta, rho)]
  rr[, PsiMarket := helper.getPsiMarket(alpha, beta, rho)]
  rr[, Rw_firm := EWB_pc * 1 / (1 + beta / rho)]
  rr[, Rw_market := EWB_pc * 1 / (1 + beta)]
  rr[, Rf_firm := EPi_pc * PsiFirm]
  rr[, Rf_market := EPi_pc * PsiMarket]

  # shares
  rr[, Rw_firm_wageshare := Rw_firm / EWB_pc]
  rr[, Rw_market_wageshare := Rw_market / EWB_pc]
  rr[, Rf_firm_profitshare := Rf_firm / EPi_pc]
  rr[, Rf_market_profitshare := Rf_market / EPi_pc]
  rr[, Rw_firm_share := Rw_firm / (Rw_firm + Rf_firm)]
  rr[, Rw_market_share := Rw_market / (Rw_market + Rf_market)]

  return(rr)
}






#' Reshape data from long to wide
#' @param long_data Data in long format
#' @param id_var ID variable
#' @param time_var Time variable
#' @param timevarying_vars Variables that we wish to reshape to wide
#' @param constant_vars Variables that shouldn't be reshaped because they
#'  do not change with time.
#' @param lags Vector of whole numbers indicating the time at which lags
#'  are to be reshaped to wide.
#' @param check_adjacent_times Verifies that time_var has increments of 1.
#' @param check_duplicates Verify that there are no duplicates in the data.
#' @param balance_panel Returns a balanced panel, i.e., use only id_var values
#'  for which an observation is available at all values of lags.
#' @export
lms.passthrough.reshape_longToWide <- function(long_data, id_var, time_var,
                                               timevarying_vars, constant_vars,
                                               lags = 1,
                                               check_adjacent_times = T,
                                               check_duplicates = T,
                                               balance_panel = F) {
  assertDataTable(long_data)
  assertCharacter(id_var, len = 1)
  assertCharacter(time_var, len = 1)
  assertCharacter(timevarying_vars)
  if (!is.null(constant_vars)) {
    assertCharacter(constant_vars)
  }
  assertIntegerish(lags)
  assertFlag(check_adjacent_times)
  assertFlag(check_duplicates)
  assertFlag(balance_panel)

  # find and check time periods
  time_periods <- long_data[, sort(unique(get(time_var)))]
  if (check_adjacent_times) {
    diff_time_periods <- diff(time_periods)
    if (!(min(diff_time_periods) == 1 & max(diff_time_periods) == 1)) {
      stop("`time_var' must have increments of 1 only.\n
             Suggest long_data[,sort(unique(get(time_var)))]
             to look for unusual jumps in time.")
    }
  }

  # check duplicates
  if (check_duplicates) {
    duplicate_vector <- duplicated(long_data[, .SD, .SDcols = c(id_var, time_var)])
    if (sum(duplicate_vector) > 0) {
      duplicate_rows <- long_data[duplicate_vector]
      duplicate_ids <- duplicate_rows[, unique(get(id_var))]
      duplicate_ids_count <- length(duplicate_ids)
      stop(sprintf(
        "Must have no duplicates in `long_data'.
         For the same `id_var' value, the same `time_var` value exists on multiple rows for %i of the `id_vars' values.
         The first three `id_var' values with this problem are %s, %s, and %s.",
        duplicate_ids_count, duplicate_ids[1], duplicate_ids[2], duplicate_ids[3]
      ))
    }
  } else {
    print("Warning: `check_duplicates=T' is strongly recommended. If there are duplicates, the output could be non-sense or crash.")
  }

  # drop extraneous variables
  all_vars <- c(id_var, time_var, timevarying_vars, constant_vars)
  long_data <- long_data[, .SD, .SDcols = all_vars]
  invisible(gc())


  # reminder about sample size (only print on first iteration)
  message_balanceT <- "decrease"
  message_balanceF <- "stay the same"
  if (balance_panel) {
    message_balance <- message_balanceT
  } else {
    message_balance <- message_balanceF
  }
  sprintf(
    "Warning: You chose option balance_panel=%s so sample size should %s on each lag.",
    balance_panel, message_balance
  )


  # perform long-to-wide reshaping
  wide_data <- copy(long_data)
  initial_size <- nrow(long_data)
  flog.info("Beginning lags for lms.passthrough.reshape_longToWide")
  for (lag in lags) {

    # extract id, time, and changing variables
    long_data_lag <- long_data[, .SD, .SDcols = c(id_var, time_var, timevarying_vars)]

    # shift time by lag
    long_data_lag[, (time_var) := get(time_var) + lag]

    # set name of lagged changing variables
    if (lag > 0) {
      new_extension <- paste0("_L", lag)
    } else {
      new_extension <- paste0("_F", abs(lag))
    }
    setnames(long_data_lag, timevarying_vars, paste0(timevarying_vars, new_extension))


    # perform merge
    if (balance_panel) {
      wide_data <- merge(wide_data, long_data_lag, by = c(id_var, time_var))
    } else {
      wide_data <- merge(wide_data, long_data_lag, by = c(id_var, time_var), all.x = T)
    }
    invisible(gc())


    # check sample size
    current_size <- nrow(wide_data)
    flog.info(
      "`long_data` initial size was %i; size after merging in lag %i is %i.",
      initial_size, lag, current_size
    )
    if (current_size > initial_size) {
      stop("`long_data' must have no duplicates.
           Current size is greater than initial size, indicating
           duplicates in the supplied `long_data'.")
    }
  }

  return(wide_data[order(get(id_var), get(time_var))])
}




#' Reshape data from wide to long
#' @param wide_data Data in wide format
#' @param id_var ID variable
#' @param time_var Time variable
#' @param timevarying_vars Variables that we wish to reshape to lon
#' @param constant_vars Variables that shouldn't be reshaped because they
#'  do not change with time.
#' @param lags Vector of whole numbers indicating the time at which lags
#'  are to be reshaped to wide.
#' @export
lms.passthrough.reshape_wideToLong <- function(wide_data, id_var, time_var,
                                               timevarying_vars, constant_vars,
                                               lags = 1) {
  assertDataTable(wide_data)
  assertCharacter(id_var, len = 1)
  assertCharacter(time_var, len = 1)
  assertCharacter(timevarying_vars)
  if (!is.null(constant_vars)) {
    assertCharacter(constant_vars)
  }
  assertIntegerish(lags)


  # check lags
  if (abs(sum(lags %% 1)) != 0 | 0 %in% lags) {
    stop("`lags' must include only whole non-zero numbers, like `lags=c(-2,-1,1,2)'.")
  }

  # perform long-to-wide reshaping
  long_data <- NULL
  always_vars <- c(id_var, time_var, constant_vars)
  initial_size <- nrow(wide_data)
  lags <- c(0, lags)

  flog.info("Beginning lags for lms.passthrough.reshape_wideToLong")
  for (lag in lags) {

    # set name of lagged changing variables
    if (lag == 0) {
      old_extension <- ""
    } else {
      if (lag > 0) {
        old_extension <- paste0("_L", lag)
      } else {
        old_extension <- paste0("_F", abs(lag))
      }
    }

    # extract id, time, and changing variables
    timevarying_vars_lag <- paste0(timevarying_vars, old_extension)
    wide_data_lag <- wide_data[, .SD, .SDcols = c(always_vars, timevarying_vars_lag)]

    # shift time by lag
    wide_data_lag[, (time_var) := get(time_var) - lag]

    # set name of lagged changing variables
    setnames(wide_data_lag, timevarying_vars_lag, timevarying_vars)

    # perform merge
    long_data <- rbindlist(list(long_data, wide_data_lag), use.names = T)
    long_data <- unique(long_data)
    invisible(gc())

    current_size <- nrow(long_data)
    flog.info(
      "`wide_data` initial size was %i; size after appending in lag %i is %i.",
      initial_size, lag, current_size
    )
  }

  return(unique(long_data[order(get(id_var), get(time_var))]))
}


#' Alternative to lms.passthrough.reshape_longToWide with balance=TRUE built on data.table::shift
#' @param long_data Data in long format.
#' @param id_var ID variable.
#' @param time_var Time variable.
#' @param timevarying_vars Variables that we wish to reshape to wide.
#' @param lags Vector of whole numbers indicating the time at which lags
#'  are to be reshaped to wide.
#' @export
lms.passthrough.reshape_longToWide_shift <- function(long_data, id_var, time_var, timevarying_vars, lags) {
  flog.info("Caution: this function is in development and may not work right.")
  flog.info("starting balanced reshape long-to-wide built on data.table::shift")

  long_data <- long_data[!is.na(get(time_var))]

  timevarying_vars <- c(time_var, timevarying_vars)

  new_cols <- NULL
  for (timevarying_var in timevarying_vars) {
    for (lag in lags) {
      new_cols <- c(new_cols, paste0(timevarying_var, "_L", lag))
    }
  }

  long_data[order(get(time_var)), (new_cols) := shift(.SD, n = lags, type = "lag"),
            .SDcols = timevarying_vars, by = id_var
            ]


  for (lag in lags) {
    time_lag <- paste0(time_var, "_L", lag)
    long_data <- long_data[!is.na(get(time_lag))]
    long_data <- long_data[get(time_lag) == get(time_var) - lag]
    long_data[, (time_lag) := NULL]
  }

  flog.info("finished data.table::shift")

  return(long_data)
}


#' Alternative to lms.passthrough.reshape_longToWide built on data.table::melt
#' @param wide_data Data in wide format.
#' @param id_var ID variable.
#' @param time_var Time variable.
#' @param timevarying_vars Variables that we wish to reshape to long.
#' @export
lms.passthrough.reshape_wideToLong_melt <- function(wide_data, id_var, time_var, timevarying_vars) {
  flog.info("Caution: this function is in development and may not work right.")
  flog.info("starting balanced reshape wide-to-long built on data.table::melt")

  setnames(wide_data, timevarying_vars, paste0(timevarying_vars, "_L0"))

  long_data2 <- melt(wide_data,
                     measure = patterns(paste0("^", timevarying_vars)),
                     value.name = timevarying_vars
  )

  setDT(long_data2)
  long_data2[, (time_var) := get(time_var) - as.integer(variable) + 1]

  long_data2[, variable := NULL]
  long_data2 <- unique(long_data2)[order(get(id_var), get(time_var))]

  flog.info("finished data.table::melt")

  return(long_data2)
}




#' Initialize spell data
#' @param long_data Data in long format.
#' @export
lms.passthrough.select_init <- function(long_data) {

  # type checks
  assertDataTable(long_data)

  # initialize spell_end
  long_data[, spell_end := 1]

  return(long_data)
}



#' Select the sample of stayers on a given variable when data is wide
#' @param long_data Data in long format.
#' @param id_var ID variable.
#' @param time_var Time variable.
#' @param stayer_var Variable with respect to which the observation
#'  must be a stayer (i.e., same value across lags).
#' @param spell_length Length of a spell.
#' @param drop Drop units that do not satisfy the restriction.
#' @export
lms.passthrough.select_stayers <- function(long_data, id_var, time_var, stayer_var, spell_length, drop = T) {

  # type checks
  assertCharacter(id_var, len = 1)
  assertCharacter(time_var, len = 1)
  assertCharacter(stayer_var, len = 1)
  assertIntegerish(spell_length, lower = 1)
  assertFlag(drop)
  if (!("spell_end" %in% names(long_data))) {
    stop("`spell_end` must be a column name in `long_data`. Use `lms.passthrough.select_init`.")
    assertNumeric(long_data$spell_end)
  }

  # impose that the worker is a stayer, must be ordered correctly
  setorderv(long_data, cols = c(id_var, time_var))
  long_data[, tmp_spell_time := computeSpellTime(get(id_var), get(time_var), stayer_var = get(stayer_var))]
  long_data[, spell_end := spell_end * (tmp_spell_time >= spell_length)]

  if (drop) {
    long_data[, tmp_spell_dist := computeDistanceToSpellEnd(
      get(id_var), get(time_var),
      stayer_var = get(stayer_var), spell_end
    )]
    flog.info("Before imposing stayer restriction size %i", nrow(long_data))
    long_data <- long_data[tmp_spell_dist != -1]
    flog.info("After imposing stayer restriction size %i", nrow(long_data))
  }

  tmp_var_names <- names(long_data)[substr(names(long_data), 1, 10) == "tmp_spell_"]
  long_data[, (tmp_var_names) := NULL]
  invisible(gc())

  return(long_data)
}


#' Select the sample of stayers on a given variable when data is wide
#' @param long_data Data in long format.
#' @param id_var ID variable.
#' @param time_var Time variable.
#' @param stayer_var Variable with respect to which the observation
#'  must be a stayer (i.e., same value across lags).
#' @param spell_length Length of a spell.
#' @param min_units Minimum number of id_var spells per stayer_var spell.
#' @param drop Drop units that do not satisfy the restriction.
#' @export
lms.passthrough.select_unitCount <- function(long_data, id_var, time_var, stayer_var, spell_length, min_units = 1, drop = T) {

  # type checks
  assertCharacter(id_var, len = 1)
  assertCharacter(time_var, len = 1)
  assertCharacter(stayer_var, len = 1)
  assertIntegerish(min_units, lower = 1)
  assertFlag(drop)
  if (!("spell_end" %in% names(long_data))) {
    stop("`spell_end` must be a column name in `long_data`. Use `lms.passthrough.select_init`.")
  }


  setorderv(long_data, cols = c(id_var, time_var))
  long_data[
    , spell_end := as.numeric(spell_end & (sum(spell_end) >= min_units)),
    list(get(stayer_var), get(time_var))
    ]

  if (drop) {
    # define distance
    long_data[, tmp_spell_dist := computeDistanceToSpellEnd(
      get(id_var), get(time_var),
      stayer_var = get(stayer_var), spell_end
    )]
    long_data[tmp_spell_dist >= spell_length, tmp_spell_dist := -1]

    # subset
    flog.info("Before imposing unitCount restriction size %i", nrow(long_data))
    long_data <- long_data[tmp_spell_dist != -1]
    flog.info("After imposing unitCount restriction size %i", nrow(long_data))

    # clean-up
    tmp_var_names <- names(long_data)[substr(names(long_data), 1, 10) == "tmp_spell_"]
    long_data[, (tmp_var_names) := NULL]
    invisible(gc())
  }

  return(long_data)
}



#' Select the sample of stayers on a given variable when data is wide
#' @param long_data Data in long format.
#' @param id_var ID variable.
#' @param time_var Time variable.
#' @param stayer_var Variable with respect to which the observation
#'  must be a stayer (i.e., same value across lags).
#' @param spell_length Length of a spell.
#' @param group_var Variable that defines groups in the stayer_var.
#' @param min_units Minimum number of stayer_var spells per group_var spell.
#' @param drop Drop units that do not satisfy the restriction.
#' @export
lms.passthrough.select_groupCount <- function(long_data, id_var, time_var, stayer_var, group_var, spell_length, min_units = 1, drop = T) {

  # type checks
  assertCharacter(id_var, len = 1)
  assertCharacter(time_var, len = 1)
  assertCharacter(stayer_var, len = 1)
  assertCharacter(group_var, len = 1)
  assertIntegerish(min_units, lower = 1)
  assertFlag(drop)
  if (!("spell_end" %in% names(long_data))) {
    stop("`spell_end` must be a column name in `long_data`. Use `lms.passthrough.select_init`.")
  }

  # impose that there are enough firm-spells per market-spell
  setorderv(long_data, cols = c(id_var, time_var))
  long_data[, spell_end := 1 * ((length(unique(get(stayer_var)[spell_end == 1])) >= min_units) & spell_end), list(get(group_var), get(time_var))]

  if (drop) {
    # distance
    long_data[, tmp_spell_dist := computeDistanceToSpellEnd(
      get(id_var), get(time_var),
      stayer_var = get(stayer_var), spell_end
    )]
    long_data[tmp_spell_dist >= spell_length, tmp_spell_dist := -1]

    # drop
    flog.info("Before imposing groupCount restriction size %i", nrow(long_data))
    long_data <- long_data[tmp_spell_dist != -1]
    flog.info("After imposing groupCount restriction size %i", nrow(long_data))

    # clean-up
    tmp_var_names <- names(long_data)[substr(names(long_data), 1, 10) == "tmp_spell_"]
    long_data[, (tmp_var_names) := NULL]
    invisible(gc())
  }

  return(long_data)
}





#' Select the sample of stayers on a given variable when data is wide
#' @param wide_data Data in wide format
#' @param stayer_var Variable with respect to which the observation
#'  must be a stayer (i.e., same observation value across lags).
#' @param lags Lags at which the observation must be a stayer.
#' @param current_time Impose stayer at the current time (i.e., lag=0).
#' @export
lms.passthrough.select_stayers_wide <- function(wide_data, stayer_var, lags, current_time = T) {
  assertDataTable(wide_data)
  assertCharacter(stayer_var, len = 1)
  assertIntegerish(lags)
  assertFlag(current_time)

  # add time 0 if current_time
  if (current_time) {
    lags <- sort(unique(c(lags, 0)))
  }

  lag_count <- length(lags)
  initial_size <- nrow(wide_data)

  # pick two lags, remove if missing on either, or if they aren't equal
  for (lag_counter in 1:(lag_count - 1)) {
    lag1 <- lags[lag_counter]
    lag2 <- lags[lag_counter + 1]

    flog.info("Starting lag pair (%i,%i)", lag1, lag2)

    # set name of lagged changing variables
    if (lag1 == 0) {
      old_extension1 <- ""
    } else {
      if (lag1 > 0) {
        old_extension1 <- paste0("_L", lag1)
      } else {
        old_extension1 <- paste0("_F", abs(lag1))
      }
    }

    # set name of lagged changing variables
    if (lag2 == 0) {
      old_extension2 <- ""
    } else {
      if (lag2 > 0) {
        old_extension2 <- paste0("_L", lag2)
      } else {
        old_extension2 <- paste0("_F", abs(lag2))
      }
    }

    # stayer variable at two lags
    stayer_var_lag1 <- paste0(stayer_var, old_extension1)
    stayer_var_lag2 <- paste0(stayer_var, old_extension2)
    wide_data <- wide_data[!is.na(get(stayer_var_lag1)) &
                             get(stayer_var_lag1) == get(stayer_var_lag2)]
    invisible(gc())

    flog.info("Finished iteration %i of %i, initial size %i, remaining size %i", lag_counter, lag_count - 1, initial_size, nrow(wide_data))
  }

  return(wide_data)
}



#' Select the sample with a minimum count of observations per group.
#' @param wide_data Data in wide format.
#' @param inner_group_var Name of variable that defines the inner group.
#' @param outer_group_vars Name(s) of variables that define the outer group.
#' @param min_count Minimum count per group.
#' @export
lms.passthrough.select_countPerGroup_wide <- function(wide_data, inner_group_var, outer_group_vars, min_count) {
  assertDataTable(wide_data)
  assertCharacter(inner_group_var, len = 1)
  assertCharacter(outer_group_vars)
  assertNumber(min_count)

  flog.info("Size before restriction: %i", nrow(wide_data))

  wide_data[, size_per_group := length(unique(get(inner_group_var))), by = outer_group_vars]
  wide_data <- wide_data[size_per_group >= min_count]
  wide_data[, size_per_group := NULL]

  flog.info("Size after restriction: %i", nrow(wide_data))

  return(wide_data)
}


#' Select the sample that is not missing on a given variable and its lags
#' @param wide_data Data in wide format
#' @param nonmissing_var Variable with respect to which the observation
#'  must not be missing.
#' @param lags Lags at which the observation must be a stayer.
#' @param current_time Impose stayer at the current time (i.e., lag=0).
#' @export
lms.passthrough.select_nonmissing_wide <- function(wide_data, nonmissing_var, lags, current_time = T) {
  assertDataTable(wide_data)
  assertCharacter(nonmissing_var, len = 1)
  assertIntegerish(lags)
  assertFlag(current_time)

  # add time 0 if current_time
  if (current_time) {
    lags <- unique(c(lags, 0))
  }

  initial_size <- nrow(wide_data)

  # pick two lags, remove if missing on either, or if they aren't equal
  for (lag in lags) {

    # set name of lagged changing variables
    if (lag == 0) {
      old_extension <- ""
    } else {
      if (lag > 0) {
        old_extension <- paste0("_L", lag)
      } else {
        old_extension <- paste0("_F", abs(lag))
      }
    }

    nonmissing_var_lag <- paste0(nonmissing_var, old_extension)
    wide_data <- wide_data[!is.na(get(nonmissing_var_lag))]

    flog.info("Finished lag %i, initial size %i, remaining size %i", lag, initial_size, nrow(wide_data))
  }

  return(wide_data)
}






#' Fast covariances in parallel, returned in long format
#'
#' @description
#'  This takes an input the data structure returned from reshape.longtowide,
#'   which includes leads and lags of variables. It supports parallelization.
#'   It uses the fast covariance function covar from package coop.
#'
#' @param wide_data Data in wide format.
#' @param id_var ID variable.
#' @param time_var Time variable.
#' @param weight_var Covariance weight variable (optional).
#' @param number_cores Number of parallel cores to use for the covariances.
#' @export
lms.passthrough.cov <- function(wide_data, id_var, time_var, weight_var = NA, number_cores = 1) {
  library(coop)
  assertDataTable(wide_data)
  assertIntegerish(number_cores, len = 1)
  if (number_cores > 1) {
    library(parallel)
  }
  if (any(is.na(wide_data))) {
    stop("Must be no missing values in `wide_data`.")
  }
  if (sum(duplicated(names(wide_data)))) {
    stop("Must be no duplicate column names in `wide_data`.")
  }
  if (sum(duplicated(wide_data[, .SD, .SDcols = c(id_var, time_var)]))) {
    stop("Must be no duplicates in `(id_var,time_var)` in `wide_data`.")
  }

  if (!is.na(weight_var)) {
    cov_weights <- wide_data[, get(weight_var)]
    wide_data[, c(weight_var) := NULL]
  }
  wide_data[, c(id_var, time_var) := NULL]
  names_cov_variables <- names(wide_data)
  number_names_cov_variables <- length(names_cov_variables)

  # prepare results holder
  covariate_combinations <- NULL
  for (counter_1 in 1:number_names_cov_variables) {
    for (counter_2 in counter_1:number_names_cov_variables) {
      covariate_1 <- names_cov_variables[counter_1]
      covariate_2 <- names_cov_variables[counter_2]
      new_row <- data.table(covariate_1 = covariate_1, covariate_2 = covariate_2)
      covariate_combinations <- rbind(covariate_combinations, new_row)
    }
  }

  # set up unique covariate pairs
  collect_covariance <- function(index_counter) {
    covariate_1 <- covariate_combinations[index_counter, ]$covariate_1
    covariate_2 <- covariate_combinations[index_counter, ]$covariate_2
    covariance_estimate <- wide_data[, covar(get(covariate_1), get(covariate_2))]
    return(
      data.table(
        covariate_1 = covariate_1,
        covariate_2 = covariate_2,
        covariance_estimate = covariance_estimate
      )
    )
  }

  # set up unique covariate pairs
  collect_covariance_weighted <- function(index_counter) {
    covariate_1 <- covariate_combinations[index_counter, ]$covariate_1
    covariate_2 <- covariate_combinations[index_counter, ]$covariate_2
    cov_vars_dt <- wide_data[, .(get(covariate_1), get(covariate_2))]
    covariance_estimate <- cov.wt(cov_vars_dt, wt = cov_weights)
    return(
      data.table(
        covariate_1 = covariate_1,
        covariate_2 = covariate_2,
        covariance_estimate = covariance_estimate
      )
    )
  }


  flog.info("`wide_data` size when computing covariances: %i", nrow(wide_data))

  if (is.na(weight_var)) {
    covariance_estimates <- rbindlist(
      mclapply(1:nrow(covariate_combinations),
               collect_covariance,
               mc.cores = number_cores
      ),
      use.names = T
    )
  } else {
    covariance_estimates <- rbindlist(
      mclapply(1:nrow(covariate_combinations),
               collect_covariance_weighted,
               mc.cores = number_cores
      ),
      use.names = T
    )
  }

  return(covariance_estimates)
}


# this is in progress
lms.passthrough.cov_longToWide <- function(covmat) {


  # build names of lag or lead variables
  covariance_estimates[, lag_1 := 0]
  covariance_estimates[, lag_2 := 0]
  if (!lag_not_diff & current_time) {
    for (timevarying_var in timevarying_vars) {
      var_lag <- paste0(timevarying_var, "_DL0")
      covariance_estimates[covariate_1 == var_lag, covariate_1 := timevarying_var]
      covariance_estimates[covariate_2 == var_lag, covariate_2 := timevarying_var]
    }
  }

  for (timevarying_var in timevarying_vars) {
    for (lag in lags) {
      if (lag != 0) {

        # for each lag, build the variable name
        if (lag_not_diff) {
          if (lag > 0) {
            new_extension <- paste0("_L", lag)
          } else {
            new_extension <- paste0("_F", abs(lag))
          }
        } else {
          if (lag > 0) {
            new_extension <- paste0("_DL", lag)
          } else {
            new_extension <- paste0("_DF", abs(lag))
          }
        }

        # set the time
        var_lag <- paste0(timevarying_var, new_extension)
        covariance_estimates[covariate_1 == var_lag, lag_1 := lag]
        covariance_estimates[covariate_2 == var_lag, lag_2 := lag]
        covariance_estimates[covariate_1 == var_lag, covariate_1 := timevarying_var]
        covariance_estimates[covariate_2 == var_lag, covariate_2 := timevarying_var]
      }
    }
  }

  covariance_estimates[, event_time_1 := -lag_1]
  covariance_estimates[, event_time_2 := -lag_2]

  covariance_estimates <- covariance_estimates[, .(
    event_time_1, event_time_2, covariate_1, covariate_2, covariance_estimate
  )]

  return(covariance_estimates[order(covariate_1, covariate_2, event_time_1, event_time_2)])
}




#' Convert a continuous variable into a binary variable and perform differences-in-differences
#' @param dd Data in wide format.
#' @param id_var ID variable.
#' @param time_var Time variable.
#' @param timevarying_vars Variables whose response to split_var is to be estimated.
#' @param split_var Variable to binarize on its differences.
#' @param lags Lags at which the differences have been provided.
#' @param number_cores Number of parallel cores to use for the covariances.
#' @export
lms.passthrough.diffInDiffs_binarize <- function(dd, id_var, time_var, timevarying_vars, split_var, lags, number_cores = 1) {
  assertDataTable(dd)
  assertCharacter(id_var, len = 1)
  assertCharacter(time_var, len = 1)
  assertCharacter(timevarying_vars)
  assertCharacter(split_var, len = 1)
  assertIntegerish(lags)
  if (number_cores > 1) {
    library(parallel)
  }

  flog.info("starting data.table::melt and cumulating the diffs")

  keep_vars <- c(id_var, time_var)
  for (lag in lags) {
    keep_var <- paste0(c(timevarying_vars, split_var), "_DL", lag)
    keep_vars <- c(keep_vars, keep_var)
  }

  dd <- dd[, .SD, .SDcols = keep_vars]
  invisible(gc())

  flog.info("running data.table::melt")

  dd <- melt(dd, id.vars = c(id_var, time_var))
  setDT(dd)
  invisible(gc())
  dd[, variable := as.character(variable)]
  dd[, t := substr(variable, nchar(variable), nchar(variable))]
  dd[, t := max(lags) - as.integer(t)]
  dd[, variable := substr(variable, 1, nchar(variable) - 4)]
  dd <- dd[order(get(id_var), get(time_var), variable, t)]

  flog.info("cumulating the diffs")
  dd2 <- dd[, list(value = cumsum(value), t = t), by = c(id_var, time_var, "variable")]
  dd[, variable := paste0(variable, "D")]
  dd <- rbind(dd, dd2)
  dd2 <- NULL
  invisible(gc())

  flog.info("reshaping back to wide")
  cast_formula <- as.formula(paste0(id_var, "+", time_var, "+t ~ variable"))
  ddw <- data.table(data.table::dcast(dd, cast_formula))
  setkeyv(ddw, c(id_var, time_var, "t"))
  ddw <- na.omit(ddw)
  dd <- NULL
  invisible(gc())

  rr <- data.frame()
  diff_byt <- function(cur_t) {
    flog.info("splitting shocks at lag %i", cur_t)
    p0 <- proc.time()[3]
    # construct split_varD index
    ff <- ddw[t == cur_t, ecdf(get(paste0(split_var, "D")))]
    ddw <- ddw[, index := ff(get(paste0(split_var, "D"))[t == cur_t]), list(get(id_var), get(time_var))]
    ddw <- ddw[(index >= .5), grp := 2]
    ddw <- ddw[(index < .5), grp := 1]
    # take the mean of the two groups
    rr_b <- ddw[, lapply(.SD, mean, na.rm = F), by = c("t", "grp"), .SDcols = c(timevarying_vars, split_var)]
    rr_b$tshock <- cur_t
    return(as.data.table(rr_b))
  }
  rr <- rbindlist(
    mclapply(lags,
             diff_byt,
             mc.cores = number_cores
    ),
    use.names = T
  )

  # take difference across groups for each variable
  rr1 <- rr[grp == 1][, grp := NULL]
  rr2 <- rr[grp == 2][, grp := NULL]
  setnames(rr1, c(timevarying_vars, split_var), paste0(c(timevarying_vars, split_var), "1"))
  setnames(rr2, c(timevarying_vars, split_var), paste0(c(timevarying_vars, split_var), "2"))
  rrm <- merge(rr1, rr2, by = c("t", "tshock"))
  for (var in c(timevarying_vars, split_var)) {
    rrm[, (var) := get(paste0(var, "2")) - get(paste0(var, "1"))]
  }
  rrm <- rrm[, .SD, .SDcols = c("t", "tshock", timevarying_vars, split_var)]

  return(rrm)
}




#' felm-based function to residualize out covariates from a given dataset.
#'
#' @description
#'  This function regresses an outcome on a given set of covariates and
#'  obtains and stores the residual for each observation.
#'
#' @param input_data The data containing the outcome and covariates (data.table).
#' @param residualize_var The name of the outcome variable (character).
#' @param discrete_vars A vector of discrete variable names (character).
#' @param continuous_vars A vector of continuous variable names (character).
#' @param return_new_vars If TRUE, returns new variables called _residual and _predicted.
#'  If FALSE, overwrites the residualize_var with its residuals.
#' @export
lms.passthrough.transform_residualize <- function(input_data, residualize_var, discrete_vars, continuous_vars = c(1), return_new_vars = F) {
  library(lfe)
  assertDataTable(input_data)
  assertCharacter(residualize_var, len = 1)
  assertFlag(return_new_vars)
  if (is.null(discrete_vars) & is.null(continuous_vars)) {
    stop(print("At least one of `discrete_vars' and `continuous_vars' must not be NULL."))
  }

  felm_formula <- paste(c(
    residualize_var,
    "~",
    paste(continuous_vars, collapse = " + "),
    "|",
    paste(discrete_vars, collapse = " + ")
  ),
  collapse = " "
  )

  flog.info("lfe::felm residualization formula: \n%s", felm_formula)

  felm_model <- felm(formula = as.formula(felm_formula), data = input_data)

  # run fast regression based on .lm.fit
  output_vector <- felm_model$residuals
  if (return_new_vars) {
    flog.info("Finished regression; putting results in %s_residual and %s_predict.", residualize_var, residualize_var)
    input_data[, (paste0(residualize_var, "_residual")) := output_vector]
    input_data[, (paste0(residualize_var, "_predict")) := get(residualize_var) - output_vector]
  } else {
    flog.info("Finished regression; overwriting %s with its residuals.", residualize_var)
    input_data[, (residualize_var) := output_vector]
  }

  return(input_data)
}




#' felm-based function to residualize out market-specific linear time trends from a specified variable.
#'
#' @description
#'  This function regresses an outcome on each market's linear time trend.
#'
#' @param input_data The data containing the outcome and covariates (data.table).
#' @param residualize_var The name of the outcome variable (character).
#' @param time_var The name of the time variable (character).
#' @param market_defn The definition of a market (character).
#' @export
lms.passthrough.transform_detrendMarkets <- function(input_data, residualize_var, time_var, market_defn) {
  library(lfe)
  assertDataTable(input_data)
  assertCharacter(residualize_var, len = 1)
  assertCharacter(time_var, len = 1)
  assertCharacter(market_defn, len = 1)

  felm_formula <- as.formula(paste0(residualize_var, "~", time_var))

  market_defns <- input_data[, unique(get(market_defn))]
  markets_n <- length(market_defns)
  for (market_counter in 1:markets_n) {
    if ((market_counter - 1) %% 10 == 0) {
      flog.info("market %i of %i", market_counter, markets_n)
    }
    market <- market_defns[market_counter]
    felm_model <- felm(formula = felm_formula, data = input_data[get(market_defn) == market])
    output_vector <- felm_model$residuals
    input_data[get(market_defn) == market, (residualize_var) := output_vector]
  }

  return(input_data)
}





#' lmfit-based function to residualize out covariates from a given dataset.
#'
#' @description
#'  This function regresses an outcome on a given set of covariates and
#'  obtains and stores the residual for each observation.
#'
#' @param input_data The data containing the outcome and covariates (data.table).
#' @param residualize_var The name of the outcome variable (character).
#' @param discrete_vars A vector of discrete variable names (character).
#' @param continuous_vars A vector of continuous variable names (character).
#' @param return_new_vars If TRUE, returns new variables called _residual and _predicted.
#'  If FALSE, overwrites the residualize_var with its residuals.
#' @export
lms.passthrough.transform_residualize_lmfit <- function(input_data, residualize_var, discrete_vars, continuous_vars, return_new_vars = F) {
  library(mltools)
  assertDataTable(input_data)
  assertCharacter(residualize_var, len = 1)
  assertFlag(return_new_vars)
  if (is.null(discrete_vars) & is.null(continuous_vars)) {
    stop(print("At least one of `discrete_vars' and `continuous_vars' must not be NULL."))
  }

  flog.info("Constructing covariate matrix to residualize %s.", residualize_var)

  # extractions
  covariates <- c(discrete_vars, continuous_vars)
  input_vector <- input_data[, get(residualize_var)]
  covariate_matrix <- input_data[, .SD, .SDcols = covariates]

  # check for missings
  check_missing_input <- sum(is.na(input_vector))
  if (check_missing_input > 0) {
    stop(sprintf("`residualize_var' must have 0 missing values but has %i missing values", check_missing_input))
  }
  check_missing_covariates <- sum(is.na(covariate_matrix))
  if (check_missing_covariates > 0) {
    stop(sprintf("Must have no missing values in `discrete_vars' or `continuous_vars'; there are %i.", check_missing_covariates))
  }
  invisible(gc())

  # prepare covariate matrix using mltools::one_hot for the discrete variables
  for (discrete_var in discrete_vars) {
    covariate_matrix[, (discrete_var) := as.factor(get(discrete_var))]
    covariate_matrix <- one_hot(covariate_matrix, cols = discrete_var)
  }
  covariate_matrix[, ones := 1]
  covariate_matrix <- as.matrix(covariate_matrix)
  invisible(gc())

  flog.info("Finished constructing covariate matrix; running regression.")

  # run fast regression based on .lm.fit
  output_vector <- .lm.fit(covariate_matrix, input_vector)$residuals
  if (return_new_vars) {
    flog.info("Finished regression; putting results in %s_residual and %s_predict.", residualize_var, residualize_var)
    input_data[, (paste0(residualize_var, "_residual")) := output_vector]
    input_data[, (paste0(residualize_var, "_predict")) := get(residualize_var) - output_vector]
  } else {
    flog.info("Finished regression; overwriting %s with its residuals.", residualize_var)
    input_data[, (residualize_var) := output_vector]
  }


  return(input_data)
}






#' Take first differences at the given lags
#' @param wide_data Data in long format.
#' @param difference_var Variable to difference.
#' @param lags Vector of whole numbers indicating the time at which lags
#'  are to be reshaped to wide.
#' @param current_time Include the current time period.
#' @export
lms.passthrough.transform_difference_wide <- function(wide_data, difference_var, lags = 1, current_time = T) {
  assertDataTable(wide_data)
  assertCharacter(difference_var, len = 1)
  assertIntegerish(lags)
  assertFlag(current_time)

  # add time 0 if current_time
  if (current_time) {
    lags <- sort(unique(c(lags, 0)))
  }

  lag_count <- length(lags)

  # pick two lags, remove if missing on either, or if they aren't equal
  for (lag_counter in 1:(lag_count - 1)) {
    lag1 <- lags[lag_counter]
    lag2 <- lags[lag_counter + 1]

    # set name of lagged changing variables
    if (lag1 == 0) {
      old_extension1 <- ""
    } else {
      if (lag1 > 0) {
        old_extension1 <- paste0("_L", lag1)
      } else {
        old_extension1 <- paste0("_F", abs(lag1))
      }
    }

    # set name of lagged changing variables
    if (lag2 == 0) {
      old_extension2 <- ""
    } else {
      if (lag2 > 0) {
        old_extension2 <- paste0("_L", lag2)
      } else {
        old_extension2 <- paste0("_F", abs(lag2))
      }
    }

    # stayer variable at two lags
    var_lag1 <- paste0(difference_var, old_extension1)
    var_lag2 <- paste0(difference_var, old_extension2)
    if (lag1 >= 0) {
      new_var <- paste0(difference_var, "_DL", lag1)
    }
    if (lag1 < 0) {
      new_var <- paste0(difference_var, "_DF", lag1)
    }
    wide_data[, (new_var) := get(var_lag1) - get(var_lag2)]
    invisible(gc())
  }
  flog.info("Finished constructing %i differences of variable %s", lag_count - 1, difference_var)

  return(wide_data)
}
