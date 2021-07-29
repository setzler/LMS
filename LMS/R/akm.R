
#' Extract the largest connected set from a data
#' @export
akm.connected_set <- function(data, ncat = 1, threshold = 0.9) {
  setkey(data, worker_ID)
  data[, t := 1:.N, worker_ID]
  setkey(data, worker_ID, t)
  data[, firm_ID2 := data[J(worker_ID, t - 1), firm_ID]]

  dadj <- unique(data[!is.na(firm_ID2), list(f1 = firm_ID, f2 = firm_ID2)])

  dadj <- dadj[f1 != f2]
  dadj <- rbind(dadj, dadj[, list(f1 = f2, f2 = f1)])
  dadj <- dadj[f1 != f2]
  flog.info("number of firms connections via movers: %i", nrow(dadj))

  # compute connected set
  setkey(dadj, f1)
  dadj[, grp := 0]
  dadj[, proc := 0]
  # start with first obs

  # cur_worker_ID = data[1,worker_ID]
  cur_grp <- 1
  cur_firm_ID <- dadj[1, f1]

  cur_firm_ID <- dadj[, .N, f1][.N > 50][, sample(dadj$f1, 1)]
  dadj[, grp := 0]

  i <- 0
  while (TRUE) {
    i <- i + 1

    # get all siblings of cur_firm_ID
    firm_IDs <- dadj[f1 %in% cur_firm_ID, unique(f2)]

    # extract the one that are new (we need to get their sibblings)
    cur_firm_ID <- dadj[f1 %in% firm_IDs][grp == 0, unique(f1)]
    if (length(cur_firm_ID) == 0) {
      cur_grp <- cur_grp + 1
      # cur_firm_ID = dadj[grp==0,f1[1]]
      cur_firm_ID <- dadj[grp == 0][, .N, f1][order(-N)][, f1[1]]

      if (dadj[, mean(grp > 0)] > threshold) {
        flog.info("passed threshold, we stop.")
        break
      }
      if ((length(cur_firm_ID) == 0) | (is.na(cur_firm_ID))) break # we are done here
    }

    # set these firms to cur_group
    dadj[f1 %in% cur_firm_ID, grp := cur_grp]
    perc <- dadj[, mean(grp > 0)]

    if ((i %% ncat) == 0) flog.info("[%i] %f%% done, cur_grp=%i, nfirms_grp=%i", i, 100 * perc, cur_grp, length(cur_firm_ID))
  }

  # extract firms in largest connected set
  large_grp <- dadj[, .N, grp][order(-N)][1, grp]
  fids <- dadj[grp == large_grp, unique(c(f1, f2))]

  # compute share of workers
  share <- data[, mean(firm_ID %in% fids)]

  return(list(largest = fids, share = share, all = dadj))
}



#' Function to estimate the AKM firm effects using the "zig zag" approach.
#'
#' @description
#'  This function takes in data on workers, firms, and wages to estimate
#'   the AKM firm effects on the workers' wages. It uses the "zig zag" approach,
#'   in which
#'
#' @param wage_data A data.table containing worker IDs, the firm IDs where
#'  the workers are employed, the outcome variable of interest, and a time variable.
#'  The corresponding columns of the data.table should be called "worker_ID",
#'  "firm_ID", "year", and the name of the column containing the outcome
#'  variable of interest (see outcome_var argument) (data.table).
#' @param outcome_var The name of the column in the wage_data data.table that
#'   contains the outcome variable of interest (character).
#' @param max_iterations Maximum number of "zig zag" iterations to use in the
#'  AKM estimation; breaks when max_iterations number of iterations is
#'  completed (numeric).
#' @param tol Tolerance for convergence of "zig zag" algorithm AKM estimation;
#'  breaks when tol level of mean squared deviation in the worker effects plus
#'  mean squared deviation in the firm effects is reached (numeric).
#' @param report_frequency Determines how many iterations to wait between
#'  printing output. In particular, report_frequency=k means to print a report
#'  every k iterations (numeric).
#' @export
akm.zig_zag <- function(wage_data, outcome_var,
                        max_iterations = 100, tol = 1e-20, report_frequency = 1) {
  if (!("count" %in% names(wage_data))) {
    wage_data <- wage_data[, count := 1]
  }

  setkey(wage_data, worker_ID)
  # replace firm id with an integer
  if ("firm_ID_int" %in% names(wage_data)) wage_data[, firm_ID_int := NULL]
  wage_data[, firm_ID_int := .GRP - 1, firm_ID] # counters start at 0 in c++!

  # initialize tmp variables
  R <- wage_data[, rep(0, .N)]
  fval <- wage_data[, rep(0.0, length(unique(firm_ID)))]
  fwei <- copy(fval)
  fwei <- fwei * 0
  fval <- fval * 0

  # initialize fixed effects
  wage_data[, akm_firm_effect := mean(get(outcome_var)), firm_ID]
  wage_data[, akm_worker_effect := mean(get(outcome_var) - akm_firm_effect), worker_ID]

  # other tmp vars
  counter <- 0
  dist <- 1
  logg <- NULL

  # perform zig zag steps
  for (counter in 1:max_iterations) {
    fwei <- fwei * 0 # make sure tmp are 0
    fval <- fval * 0 # make sure tmp are 0

    # update worker fixed-effect (zig)
    wage_data[, akm_worker_effect_new := workerFEupdate(list(yr = get(outcome_var) - akm_firm_effect, tin = worker_ID, res = R, count = count))]

    # update firm fixed-effect (zag)
    wage_data[, akm_firm_effect_new := firmFEupdate(list(yr = get(outcome_var) - akm_worker_effect_new, payer_tin = firm_ID_int, res = R, weight = count, fval = fval, fwei = fwei))]

    # force first firm to 0
    wage_data[firm_ID == 0, akm_firm_effect_new := 0]

    # update effects
    wage_data[, akm_worker_effect := akm_worker_effect_new][, akm_firm_effect := akm_firm_effect_new]

    # extract moments
    akm_firm_effect_bar <- wage_data[, akm.weighted_mean(akm_firm_effect, count)]
    akm_worker_effect_bar <- wage_data[, akm.weighted_mean(akm_worker_effect, count)]
    akm_firm_effect_var <- wage_data[, akm.weighted_mean((akm_firm_effect - akm_firm_effect_bar)^2, count)]
    akm_worker_effect_var <- wage_data[, akm.weighted_mean((akm_worker_effect - akm_worker_effect_bar)^2, count)]
    cov_akm_firm_effect_akm_worker_effect <- wage_data[, akm.weighted_mean((akm_worker_effect - akm_worker_effect_bar) * (akm_firm_effect - akm_firm_effect_bar), count)]

    # check fit (distance from convergence)
    dist <- wage_data[, mean((akm_worker_effect - akm_worker_effect_new)^2) + mean((akm_firm_effect - akm_firm_effect_new)^2)]

    # wrap loop
    logg <- rbind(logg, c(akm_firm_effect_bar, akm_worker_effect_bar, akm_firm_effect_var, akm_worker_effect_var, cov_akm_firm_effect_akm_worker_effect))
    if ((counter %% report_frequency) == 0) flog.info("[%3i] p-chg=%e var(akm_firm_effect)=%f var(akm_worker_effect)=%f", counter, dist, akm_firm_effect_var, akm_worker_effect_var)

    if (dist < tol) break
  }
  flog.info("[%3i][final] p-chg=%e var(akm_firm_effect)=%f var(akm_worker_effect)=%f", counter, dist, akm_firm_effect_var, akm_worker_effect_var)
  colnames(logg) <- c("akm_firm_effect_bar", "akm_worker_effect_bar", "akm_firm_effect_var", "akm_worker_effect_var", "cov_akm_firm_effect_akm_worker_effect")

  firms_with_fixedeffects <- unique(wage_data[, .(firm_ID, akm_firm_effect)])

  return(list(firms_with_fixedeffects = firms_with_fixedeffects, log = logg))
}





#' Compute the weighted mean
#'
#' @description
#'  Compute the weighted mean
#'
#' @param variable_of_interest Variable to take the mean of (numeric).
#' @param weights Weights, which will be rescaled to sum to one (numeric).
#' @export
akm.weighted_mean <- function(variable_of_interest, weights) {
  # rescale weights to sum to 1
  weights <- weights / sum(weights)
  # take the weighted sum
  sum(variable_of_interest * weights)
}



#' Function to subset firms with a miniumum number of unique movers
#'
#' @description
#'  This function takes in data on matched worker and firm ID, counts
#'   the number of unique movers within a firm, then returns the IDs
#'   of firms with a sufficient number of movers and the workers who move.
#'
#' @param wage_data A data.table containing worker IDs and the firm IDs where
#'  the workers are employed. The corresponding columns of the data.table
#'  should be called "worker_ID" and "firm_ID" (data.table).
#' @param min_unique_movers The minimum number of unique movers
#'  indicating the number of unique movers a firm must have to remain
#'  in the sample for the estimation (numeric).
#' @export
akm.subset_unique_movers <- function(wage_data, min_unique_movers) {
  assertDataTable(wage_data)
  assertNumeric(min_unique_movers, lower = 1)
  for (varname in c("worker_ID", "firm_ID")) {
    stopifnot(varname %in% names(wage_data))
  }

  # find and keep movers
  wage_data[, max_firm_ID := max(firm_ID), worker_ID]
  wage_data[, min_firm_ID := min(firm_ID), worker_ID]
  wage_data[, mover := as.integer(max_firm_ID != min_firm_ID)]
  wage_data <- wage_data[mover == 1]
  wage_data[, mover := NULL]

  # keep if sufficient movers per firm
  wage_data[, uniquemovers := length(unique(worker_ID)), by = "firm_ID"]
  wage_data <- wage_data[uniquemovers >= min_unique_movers]
  wage_data[, uniquemovers := NULL]
  invisible(gc())

  return(wage_data)
}





#' Function to extract AKM-related statistics
#'
#' @description
#'   This function extracts AKM-related statistics, including variance of
#'    firm effects, variance of worker effects, covariance between firm and
#'    worker effects
#'
#' @param wage_data A data.table with information on workers, their wages, which
#'   firm they work at, and the time period. There should be columns called
#'   "firm_ID" and "worker_ID" (data.table).
#' @param outcome_var Wage measure on which AKM was computed (e.g., residualized).
#' @param outcome_predicted If there is a predicted outcome in the wage_data dataset,
#'   this is the name of that predicted outcome (character).

akm.extract_statistics <- function(wage_data, outcome_var, outcome_predicted = NA) {
  assertDataTable(wage_data)
  assertCharacter(outcome_var)
  for (varname in c("firm_ID", "worker_ID", "akm_firm_effect", "akm_worker_effect")) {
    stopifnot(varname %in% names(wage_data))
  }
  if (!is.na(outcome_predicted)) {
    stopifnot(outcome_predicted %in% names(wage_data))
  }

  wage_data[, firm_mean_wage := mean(log_wage), list(firm_ID)]
  wage_data[, firm_mean_workereffect := mean(akm_worker_effect), list(firm_ID)]

  akm_statistics <- list(
    firmeffect_var = wage_data[, var(akm_firm_effect)],
    workereffect_var = wage_data[, var(akm_worker_effect)],
    idiosyncratic_var = wage_data[, var(akm_idiosyncratic)],
    workereffect_firmeffect_cov = wage_data[, cov(akm_firm_effect, akm_worker_effect)],
    idiosyncratic_firmeffect_cov = wage_data[, cov(akm_firm_effect, akm_idiosyncratic)],
    workereffect_idiosyncratic_cov = wage_data[, cov(akm_idiosyncratic, akm_worker_effect)]
  )
  akm_statistics$workereffect_firmeffect_cor <- with(
    akm_statistics,
    workereffect_firmeffect_cov / (sqrt(workereffect_var) * sqrt(firmeffect_var))
  )

  if (is.na(outcome_predicted)) {
    akm_statistics$total_var <- with(akm_statistics, workereffect_var + firmeffect_var +
      idiosyncratic_var + 2 * workereffect_firmeffect_cov +
      2 * idiosyncratic_firmeffect_cov + 2 * workereffect_idiosyncratic_cov)
  } else {
    akm_statistics$predictable_var <- wage_data[, var(get(outcome_predicted))]
    akm_statistics$firmeffect_predictable_cov <- wage_data[, cov(get(outcome_predicted), akm_firm_effect)]
    akm_statistics$workereffect_predictable_cov <- wage_data[, cov(get(outcome_predicted), akm_worker_effect)]
    akm_statistics$total_var <- with(akm_statistics, workereffect_var + firmeffect_var + predictable_var + idiosyncratic_var +
      2 * workereffect_firmeffect_cov + 2 * idiosyncratic_firmeffect_cov + 2 * workereffect_idiosyncratic_cov +
      2 * workereffect_predictable_cov + 2 * firmeffect_predictable_cov)


    with(akm_statistics, workereffect_var + firmeffect_var + idiosyncratic_var
      + predictable_var + 2 * workereffect_firmeffect_cov
      + 2 * workereffect_predictable_cov + 2 * firmeffect_predictable_cov)
    akm_statistics$workertotal_var <- with(akm_statistics, workereffect_var + predictable_var + 2 * workereffect_predictable_cov)
    akm_statistics$workertotal_firmeffect_cov <- with(akm_statistics, workereffect_firmeffect_cov + firmeffect_predictable_cov)
    akm_statistics$workertotal_firmeffect_cor <- with(
      akm_statistics,
      workertotal_firmeffect_cov / (sqrt(workertotal_var) * sqrt(firmeffect_var))
    )
    akm_statistics <- akm_statistics[c(
      "workereffect_firmeffect_cor", "firmeffect_var", "workereffect_var", "idiosyncratic_var",
      "workereffect_firmeffect_cov", "predictable_var", "firmeffect_predictable_cov",
      "workereffect_predictable_cov", "total_var",
      "workertotal_var", "workertotal_firmeffect_cov", "workertotal_firmeffect_cor"
    )]
  }

  akm_statistics$total_var_direct <- wage_data[, var(log_wage)]
  akm_statistics$between_wage_var <- wage_data[, var(firm_mean_wage)]
  akm_statistics$between_workereffect_var <- wage_data[, var(firm_mean_workereffect)]

  return(akm_statistics)
}
