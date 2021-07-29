

#' Collect terms required for model decompositions
#' @export
lms.model.prepare <- function(full_data, lambda = .9154762, marketclusters = 10, workergroups = 10) {

  # rename variables
  long_data <- full_data[, list(worker_ID, firm_ID, year, cluster, market, broad_market, y_jt = log_va_init, w_it = log_wage, wages_pt, x_i = xnl * gamma, FEbarnl, adj_term_jt_res, adj_term_jt, psi_jt_init = FEnl, theta_j_init = gamma, theta_bar = gamma_bar, beta_full, rho_r, alpha_r, log_fte_share_r)]

  # set up market clusters
  long_data <- lms.fe.cluster_markets(long_data, marketclusters = marketclusters)

  # set up x
  long_data[, x_base := x_i / theta_j_init]
  long_data[, x_base_mean := mean(x_base)]
  long_data[, x_i := theta_bar * (x_base - x_base_mean)]
  long_data[, Xq := ecdf(x_i)(x_i)]
  k <- workergroups
  long_data[, X := 0]
  for (i in 1:(k - 1) / k) {
    long_data[, X := X + as.integer(Xq > i)]
  }
  long_data[, X := X + 1]
  long_data[, xbar := mean(x_i), X]

  # set up psi
  long_data[, psi_tilde_j := FEbarnl + adj_term_jt_res - adj_term_jt]
  long_data[, psi_bar := psi_jt_init - adj_term_jt_res]
  long_data[, psi_bar := psi_bar + theta_j_init * x_base_mean]
  long_data[, psi_bar := psi_bar + mean(w_it) - mean(psi_bar)]
  long_data[, psi_jt := psi_bar + adj_term_jt_res]
  long_data[, psi_jt_init := copy(psi_jt)]

  # set up theta
  long_data[, theta_j := theta_j_init / theta_bar]

  # set up observables
  long_data[, Xb := w_it - wages_pt]
  long_data[, l_jt := log(sum(exp(x_i)^theta_j)), list(firm_ID, year)]
  long_data[, wagebill := log(sum(exp(w_it))), list(firm_ID, year)]

  # run market decomposition
  long_data <- lms.model.market_decomp(long_data, method = 2)

  # construct level terms
  long_data[, H_j := exp(h_j)]
  long_data[, A_rt := exp(a_rt)]
  long_data[, A_tilde := exp(a_tilde)]
  long_data[, c_r := log((1 - alpha_r) * (beta_full / rho_r) / (1 + beta_full / rho_r))]
  long_data$lambda <- lambda
  long_data[, beta_model := beta_full / lambda]
  long_data[, Xbar := exp(xbar)]

  return(long_data)
}

#' Estimate amenities G
#' @export
lms.model.G <- function(long_data) {
  fxdata <- long_data[, list(.N), list(
    X, broad_market, market_cluster, market, cluster, firm_ID,
    year, a_rt, A_rt, a_tilde, A_tilde, h_j, H_j, beta_full, beta_model, lambda,
    rho_r, alpha_r, c_r, xbar, psi_jt, psi_jt_init, y_jt, y_jt_pred, theta_j, adj_term_jt_res,
    l_jt, wagebill, wagebill_pred, log_fte_share_r
  )]

  fxdata <- lms.model.get_Gjx(fxdata)
  setnames(fxdata, "N", "size")
  fxdata[, Xbar := exp(xbar)]
  fxdata[, g_jX := log(G_jX)]
  fxdata[, g_jX_btw := weighted.mean(g_jX, w = Pr_jX), list(firm_ID)]

  g_var <- cov.wt(fxdata[, .(g_jX - g_jX_btw)], wt = fxdata$Pr_jX)$cov

  return(list(fxdata = fxdata, g_var = g_var))
}


#' Estimate amenities G
#' @param long_data
#' @export
lms.model.decomp <- function(long_data) {

  # a terms
  long_data[, a_rt_bar := mean(a_rt)]
  long_data[, atotal := 1 / (1 + alpha_r * lambda * beta_model / rho_r) * c_r + 1 / (1 + alpha_r * lambda * beta_model) * (a_rt - a_rt_bar) + 1 / (1 + alpha_r * lambda * beta_model / rho_r) * a_tilde]
  long_data[, abar_outer := mean(atotal), list(broad_market, year)]
  long_data[, atilde_outer := atotal - abar_outer]
  long_data[, abar_inner := mean(atilde_outer), list(market, year)]
  long_data[, atilde := atilde_outer - abar_inner]

  # h terms
  long_data[, htotal := -alpha_r * (1 / (1 + alpha_r * lambda * beta_model / rho_r) * h_j + lambda * beta_model / (1 + alpha_r * lambda * beta_model) * a_rt_bar)]
  long_data[, hbar_outer := mean(htotal), list(broad_market, year)]
  long_data[, htilde_outer := htotal - hbar_outer]
  long_data[, hbar_inner := mean(htilde_outer), list(market, year)]
  long_data[, htilde := htilde_outer - hbar_inner]

  # x terms
  long_data[, xbar_outer := mean(x_i), list(broad_market, year)]
  long_data[, xtilde_outer := x_i - xbar_outer]
  long_data[, xbar_inner := mean(xtilde_outer), list(market, year)]
  long_data[, xtilde := xtilde_outer - xbar_inner]
  long_data[, xtilde := mean(xtilde), list(firm_ID, year)]

  # psi terms
  long_data[, psi_outer := mean(psi_jt), list(broad_market, year)]
  long_data[, psitilde_outer := psi_jt - psi_outer]
  long_data[, psibar_inner := mean(psitilde_outer), list(market, year)]
  long_data[, psitilde := psitilde_outer - psibar_inner]

  # interaction terms
  long_data[, theta_raw := theta_j * theta_bar]
  long_data[, interactions := (theta_raw - theta_bar) * (x_base - x_base_mean)]
  long_data[, interactions_outer := mean(interactions), list(broad_market, year)]
  long_data[, interactionstilde_outer := interactions - interactions_outer]
  long_data[, interactionsbar_inner := mean(interactionstilde_outer), list(market, year)]
  long_data[, interactionstilde := interactionstilde_outer - interactionsbar_inner]

  # model decomp
  decomp <- with(long_data, list(
    var_w = var(w_it),
    var_psi = var(psi_jt),
    var_x = var(x_i),
    var_psi_outer = var(psi_outer),
    var_abar_outer = var(abar_outer),
    var_hbar_outer = var(hbar_outer),
    var_xbar_outer = var(xbar_outer),
    var_psi_inner = var(psibar_inner),
    var_abar_inner = var(abar_inner),
    var_hbar_inner = var(hbar_inner),
    var_xbar_inner = var(xbar_inner),
    var_psitilde = var(psitilde),
    var_atilde = var(atilde),
    var_htilde = var(htilde),
    var_xtilde = var(xtilde),
    cov_abar_hbar_outer = cov(abar_outer, hbar_outer),
    cov_abar_hbar_inner = cov(abar_inner, hbar_inner),
    cov_atilde_htilde = cov(atilde, htilde),
    cov_psi_x = cov(psi_jt, x_i),
    cov_abar_xbar_outer = cov(abar_outer, xbar_outer),
    cov_hbar_xbar_outer = cov(hbar_outer, xbar_outer),
    cov_abar_xbar_inner = cov(abar_inner, xbar_inner),
    cov_hbar_xbar_inner = cov(hbar_inner, xbar_inner),
    cov_atilde_xtilde = cov(atilde, xtilde),
    cov_htilde_xtilde = cov(htilde, xtilde),
    interactions_outer = var(interactions_outer) + 2 * cov(interactions_outer, psi_outer + xbar_outer),
    interactions_inner = var(interactionsbar_inner) + 2 * cov(interactionsbar_inner, psibar_inner + xbar_inner),
    interactions_tilde = var(interactionstilde) + 2 * cov(interactionstilde, psitilde + xtilde)
  ))

  thenames <- names(decomp)
  decomp <- as.numeric(decomp)
  decomp <- as.data.table(t(decomp))
  setnames(decomp, thenames)

  return(decomp)
}


#' Compare true and predicted values across size distribution
#' @param fxdata
#' @export
lms.model.size_fit_grid <- function(fxdata) {
  fxdata[, y_jt_fte_pred := y_jt_pred + (1 - alpha_r) * log_fte_share_r]
  fdata <- fxdata[, list(psi_jt_true = mean(psi_jt_init), psi_jt_pred = mean(psi_jt), y_jt_true = mean(y_jt), y_jt_pred = mean(y_jt_fte_pred), l_jt_true = mean(l_jt), l_jt_pred = mean(l_jt_pred), wagebill_true = mean(wagebill), wagebill_pred = mean(wagebill_pred), size_true = log(sum(size)), size_pred = log(sum(Pr_j_given_Xt_pred * Pr_X)), xbar_pred = weighted.mean(xbar, Pr_j_given_Xt_pred * Pr_X), xbar_true = weighted.mean(xbar, Pr_jX)), list(firm_ID, year, broad_market)]
  fdata[, cor(size_true, size_pred)]
  fdata[, cor(xbar_true, xbar_pred)]
  fdata[, size_pred := size_pred - mean(size_pred) + mean(size_true)]

  dec <- 4
  fdata[, size_pred_round := round(size_pred * dec) / dec]
  fdata[, size_true_round := round(size_true * dec) / dec]

  grid_true <- fdata[, list(type = "true", psi_jt = mean(psi_jt_true), y_jt = mean(y_jt_true), l_jt = mean(l_jt_true), wagebill = mean(wagebill_true), xbar = mean(xbar_true), N = .N), list(size = size_true_round)]
  grid_pred <- fdata[, list(type = "pred", psi_jt = mean(psi_jt_pred), y_jt = mean(y_jt_pred), l_jt = mean(l_jt_pred), wagebill = mean(wagebill_pred), xbar = mean(xbar_pred), N = .N), list(size = size_pred_round)]
  grid <- rbind(grid_true, grid_pred)
  grid <- grid[order(type, size)]
  # grid <- grid[N >= 100 & size > 0]

  return(grid)
}



#' Compare true and predicted values across size distribution
#' @param fxdata
#' @param fxdata_solved
#' @export
lms.model.h_fit_grid <- function(fxdata, fxdata_solved) {
  fxdata <- fxdata[, list(size_true = log(sum(size)), h_j_true = h_j[1]), firm_ID]
  fxdata_solved <- fxdata_solved[, list(size_pred = log(sum(Pr_jX_ct)), h_j_pred = h_j_ct[1]), firm_ID]
  fdata <- merge(fxdata, fxdata_solved, by = "firm_ID")
  fdata[, size_pred := size_pred - mean(size_pred) + mean(size_true)]
  dec <- 4
  fdata[, size_pred_round := round(size_pred * dec) / dec]
  fdata[, size_true_round := round(size_true * dec) / dec]
  grid_true <- fdata[, list(type = "true", h_j = mean(h_j_true), N = .N), list(size = size_true_round)]
  grid_pred <- fdata[, list(type = "pred", h_j = mean(h_j_pred), N = .N), list(size = size_pred_round)]
  grid <- rbind(grid_true, grid_pred)
  grid <- grid[order(type, size)]

  return(grid)
}


lms.model.compdiffs <- function(long_data, num_firms = 1e4) {
  long_data[, x_base_mean := mean(x_base)]
  long_data[, x_i := theta_bar * (x_base - x_base_mean)]
  long_data[, psi_bar := psi_jt - adj_term_jt_res]
  long_data[, psi_bar := psi_bar + theta_j * x_base_mean]
  long_data[, psi_jt := psi_bar + adj_term_jt_res]
  long_data[, interactions := (theta_j - theta_bar) * (x_base - x_base_mean)]
  x_base_deciles <- long_data[, quantile(x_base, 1:9 / 10)]
  x_base_mean <- long_data[, unique(x_base_mean)]
  theta_bar <- long_data[, unique(theta_bar)]
  intercept <- long_data[, mean(w_it - x_i - psi_jt - interactions)]

  nn <- nrow(long_data)
  id1 <- round(runif(num_firms) * nn)
  f1 <- long_data[id1][, list(f1 = firm_ID, m1 = market, psi_jt1 = psi_jt, theta_j1 = theta_j)]
  id2 <- round(runif(num_firms) * nn)
  f2 <- long_data[id2][, list(f2 = firm_ID, m2 = market, psi_jt2 = psi_jt, theta_j2 = theta_j)]
  firm_pairs <- cbind(f1, f2)

  out <- 1:9
  outd <- 1:9
  for (xx in 1:length(x_base_deciles)) {
    xval <- x_base_deciles[xx]
    firm_pairs[, comp_diff := (psi_jt1 + (theta_j1 - theta_bar) * (xval - x_base_mean)) - (psi_jt2 + (theta_j2 - theta_bar) * (xval - x_base_mean))]
    out[xx] <- firm_pairs[, mean(abs(comp_diff))]
    firm_pairs[, comp_diff_dollars := exp(intercept + theta_bar * (xval - x_base_mean) + psi_jt1 + (theta_j1 - theta_bar) * (xval - x_base_mean)) - exp(intercept + theta_bar * (xval - x_base_mean) + psi_jt2 + (theta_j2 - theta_bar) * (xval - x_base_mean))]
    outd[xx] <- firm_pairs[, mean(abs(comp_diff_dollars))]
  }



  nn <- nrow(long_data)
  id1 <- round(runif(num_firms) * nn)
  f1 <- long_data[id1][, list(f1 = firm_ID, m1 = market, psi_jt1 = psi_jt, theta_j1 = theta_j)]
  firm_pairs_tilde <- NULL
  for (ii in 1:nrow(f1)) {
    this_f1 <- f1[ii]
    m <- this_f1$m1
    sub <- long_data[market == m]
    id2 <- round(runif(1) * nrow(sub))
    this_f2 <- sub[id2][, list(f2 = firm_ID, m2 = market, psi_jt2 = psi_jt, theta_j2 = theta_j)]
    firm_pairs_tilde <- rbindlist(list(firm_pairs_tilde, cbind(this_f1, this_f2)), use.names = T)
    if (nrow(firm_pairs_tilde) %% 100 == 0) {
      print(nrow(firm_pairs_tilde))
    }
  }


  out2 <- 1:9
  out2d <- 1:9
  for (xx in 1:length(x_base_deciles)) {
    xval <- x_base_deciles[xx]
    firm_pairs_tilde[, comp_diff := (psi_jt1 + (theta_j1 - theta_bar) * (xval - x_base_mean)) - (psi_jt2 + (theta_j2 - theta_bar) * (xval - x_base_mean))]
    out2[xx] <- firm_pairs_tilde[, mean(abs(comp_diff))]
    firm_pairs_tilde[, comp_diff_dollars := exp(intercept + theta_bar * (xval - x_base_mean) + psi_jt1 + (theta_j1 - theta_bar) * (xval - x_base_mean)) - exp(intercept + theta_bar * (xval - x_base_mean) + psi_jt2 + (theta_j2 - theta_bar) * (xval - x_base_mean))]
    out2d[xx] <- firm_pairs_tilde[, mean(abs(comp_diff_dollars))]
  }


  res <- data.table(x_decile = 1:9, market = out, within = out2, market_dollars = outd, within_dollars = out2d)

  return(res)
}







#' AKM-based decomposition
#'
#' Construct the different elements for a variance decomposition including market level values. It takes as inputs
#' inputs: market, year, y_jt, psi_tilde_j, beta_full, alpha_r, rho_r
#'   where psi_tilde_j is the firm fixed effect coming out of an AKM regression on adjusted wages.
#' outputs: psi_jt, ...
#'
#' the method argument chooses the approach to construct the time varying part
#' method 0 uses time variation in y_jt
#' method 1 uses time variation in l_jt
#' method 2 uses psi_jt as an input
#'
#' @export
lms.model.market_decomp <- function(long_data, method = 0) {

  # formulas here are from direct messages/images from Tibo on Slack on 11/26/2018

  for (varname in c("market", "year", "y_jt", "psi_tilde_j", "beta_full", "alpha_r", "rho_r")) {
    if (!(varname %in% names(long_data))) {
      stop(flog.info("Must have variable %s in long_data", varname))
    }
  }

  # prepare market means of inputs
  long_data[, c_r := log((1 - alpha_r) * (beta_full / rho_r) / (1 + beta_full / rho_r))]
  long_data[, y_rt := mean(y_jt), by = c("market", "year")]
  long_data[, psi_tilde_r := mean(psi_tilde_j), by = "market"]

  # intermediate parameters for psi_tilde decomposition
  long_data[, xi_psitilde_cr := (1 + beta_full - (1 - alpha_r) * beta_full / rho_r) / ((1 + beta_full) * (1 + alpha_r * beta_full / rho_r))]
  long_data[, xi_psitilde_hr := -(1 - alpha_r) * (1 - rho_r) * (beta_full / rho_r) / ((1 + beta_full) * (1 + alpha_r * beta_full / rho_r) * (1 + beta_full / rho_r))]
  long_data[, xi_psitilde_hj := -1 / (1 + beta_full / rho_r)]

  # intermediate parameters for y_j decomposition
  long_data[, xi_yj_cr := (1 - alpha_r) * (beta_full / rho_r) / (1 + alpha_r * beta_full / rho_r)] # intercept in the y_j decomposition
  long_data[, xi_yj_hj := (1 - alpha_r) / (1 + alpha_r * beta_full / rho_r)] # coefficient on h_j in the y_j decomposition
  long_data[, xi_yj_atilde := (1 + beta_full / rho_r) / (1 + alpha_r * beta_full / rho_r)] # coefficient on a_tilde in the y_j decomposition
  long_data[, xi_yj_ar := (1 + beta_full) / (1 + alpha_r * beta_full)] # coefficient on a_r in the y_j decomposition

  # intermediate parameters for psi_j from the w_j wage equation
  long_data[, w_j__intercept := c_r / (1 + alpha_r * beta_full / rho_r)] # intercept in the y_j decomposition
  long_data[, w_j__h_j_coef := -alpha_r / (1 + alpha_r * beta_full / rho_r)] # coefficient on h_j in the y_j decomposition
  long_data[, w_j__a_tilde_coef := 1 / (1 + alpha_r * beta_full / rho_r)] # coefficient on a_tilde in the y_j decomposition
  long_data[, w_j__a_r_coef := 1 / (1 + alpha_r * beta_full)] # coefficient on a_r in the y_j decomposition

  # get amenities (h) using the psi_tilde decomposition into h_j and h_r terms
  long_data[, h_r := (psi_tilde_r - xi_psitilde_cr * c_r) / (xi_psitilde_hr + xi_psitilde_hj)] # inverting E[psi_tilde] equation
  long_data[, h_j := (psi_tilde_j - xi_psitilde_cr * c_r - xi_psitilde_hr * h_r) / xi_psitilde_hj] # inverting psi_tilde equation

  long_data[, ff := 1 / (1 + alpha_r * beta_full / rho_r)] # firm factor
  long_data[, mf := 1 / (1 + alpha_r * beta_full)] # market factor

  if (method == 0) {
    # get TFP (a) using the y_j decomposition into h_j, a_tilde (which is a_j-a_r), and a_r terms
    long_data[, a_rt := (y_rt - xi_yj_cr * c_r - xi_yj_hj * h_r) / xi_yj_ar] # inverting y_jt equation
    long_data[, a_jt := a_rt + (y_jt - xi_yj_cr * c_r - xi_yj_hj * h_j - xi_yj_ar * a_rt) / xi_yj_atilde] # inverting E[psi_tilde] equation
    long_data[, a_tilde := a_jt - a_rt]
  } else if (method == 1) {
    # get TFP (a) using the l_jt equation
    # long_data[, l_jt := log(sum(Xbar^theta_j)), list(firm_ID,year)]
    long_data[, a_rt := (y_rt - xi_yj_cr * c_r - xi_yj_hj * h_r) / xi_yj_ar] # inverting y_jt equation
    long_data[, a_jt := a_rt + (y_jt - xi_yj_cr * c_r - xi_yj_hj * h_j - xi_yj_ar * a_rt) / xi_yj_atilde] # inverting E[psi_tilde] equation
    long_data[, a_tilde := a_jt - a_rt]

    long_data[, l_j := mean(l_jt), list(firm_ID)]
    long_data[, l_r := mean(l_j), list(market, year)]
    long_data[, a_r := (l_r - w_j__a_tilde_coef * h_r) / xi_yj_ar] # inverting l_rt equation
    long_data[, a_j := a_r + (l_j - xi_yj_ar * a_r - w_j__a_tilde_coef * h_j) / xi_yj_atilde] # inverting l_jt equation

    long_data[, a_rt := a_rt - mean(a_rt) + a_r, list(firm_ID)]
    long_data[, a_jt := a_jt - mean(a_jt) + a_j, list(firm_ID)]
    long_data[, a_tilde := a_jt - a_rt]
  } else if (method == 2) {
    # get TFP (a) using the l_jt equation
    # long_data[, l_jt := log(sum(Xbar^theta_j)), list(firm_ID,year)]
    long_data[, l_rt := mean(l_jt), list(market, year)]
    long_data[, psi_rt := mean(psi_jt), list(market, year)]

    # get h_r, a_rt
    long_data[, h_r := mean(l_rt - beta_full * psi_rt - beta_full * (1 / rho_r - 1) * ff * c_r) / (ff * (1 + alpha_r * beta_full)), firm_ID]
    long_data[, a_rt := alpha_r * l_rt + psi_rt - c_r, list(firm_ID, year)]

    # get h_j, a_jt
    long_data[, l_res := l_jt - beta_full * mf * a_rt - ff * beta_full / rho_r * c_r, list(firm_ID, year)]
    long_data[, psi_res := psi_jt - mf * a_rt - ff * c_r, list(firm_ID, year)]

    long_data[, h_j := mean(l_res - beta_full / rho_r * psi_res), list(firm_ID, year)]
    long_data[, a_tilde := alpha_r * l_res + psi_res, list(firm_ID, year)]

    # get individual level a_j, h_j
    long_data[, a_jt := a_rt + a_tilde]
  }

  # get the psi_j
  long_data[, psi_jt := ff * c_r - alpha_r * ff * h_j + w_j__a_r_coef * a_rt + w_j__a_tilde_coef * a_tilde]
  long_data[, psi_rt := mean(psi_jt), by = c("market", "year")]

  long_data[, l_jt_pred := (beta_full / rho_r) * ff * c_r + ff * h_j + (beta_full) * mf * a_rt + (beta_full / rho_r) * ff * a_tilde]

  long_data[, y_jt_pred := (1 - alpha_r) * ff * (beta_full / rho_r * c_r + h_j) +
    (1 + beta_full) * mf * a_rt + (1 + beta_full / rho_r) * ff * a_tilde]

  long_data[, wagebill_pred := ff * ((1 + beta_full / rho_r) * c_r + (1 - alpha_r) * h_j) +
    (1 + beta_full) * mf * a_rt + (1 + beta_full / rho_r) * ff * a_tilde]

  long_data[, w_it_pred := x_i * theta_j + ff * (c_r - alpha_r * h_j + a_tilde) + mf * a_rt]

  # terms needed for decompositions
  long_data[, a_tilde_mod := (1) / (1 + alpha_r * beta_full / rho_r) * a_tilde]
  long_data[, a_rt_mod := (1) / (1 + alpha_r * beta_full) * a_rt]
  long_data[, a_jt_mod := a_tilde_mod + a_rt_mod]
  long_data[, h_j_mod := (-alpha_r) / (1 + alpha_r * beta_full / rho_r) * h_j]
  long_data[, c_r_mod := c_r / (1 + alpha_r * beta_full / rho_r)]

  # delete terms not needed
  long_data[, names(long_data)[str_detect(names(long_data), "xi_")] := NULL]
  long_data[, names(long_data)[str_detect(names(long_data), "w_j_")] := NULL]

  return(long_data)
}





#' Model decomposition
#' @export
lms.model_moments <- function(long_data, bymarket = F, covmat_numcores = 1) {
  vars <- c(
    "y_jt", "y_rt", "y_tilde",
    "w_it", "w_rt",
    "x_i", "x_r", "x_tilde",
    "psi_tilde_j", "psi_tilde_r",
    "psi_jt", "psi_rt",
    "h_j", "h_r", "h_tilde", "h_j_mod",
    "a_jt", "a_rt", "a_tilde",
    "a_tilde_mod", "a_jt_mod", "a_rt_mod",
    "c_r_mod", "beta_full", "alpha_r", "rho_r", "true_pt", "true_whreg"
  )


  long_data[, x_tilde := x_i - x_r]
  long_data[, h_tilde := h_j - h_r]
  long_data[, y_tilde := y_jt - y_rt]
  long_data[, true_pt := 1 / (1 + beta_full / rho_r)]
  long_data[, true_whreg := -alpha_r / (1 + alpha_r * beta_full / rho_r)]

  for (varname in c("market", vars)) {
    if (!(varname %in% names(long_data))) {
      stop(flog.info("variable %s is missing", varname))
    }
  }

  get_logdollar_decomp <- function(long_data) {
    subdata <- long_data[, .SD, .SDcols = c("worker_ID", "year", vars)]
    covmat <- pt_cov(subdata, id_var = "worker_ID", time_var = "year", number_cores = covmat_numcores)
    setnames(covmat, "covariance_estimate", "estimate")
    return(covmat)
  }

  get_logdollar_means <- function(long_data) {
    means <- NULL
    for (v in vars) {
      res <- data.table(covariate_1 = v, covariate_2 = "mean", estimate = long_data[, mean(get(v))])
      means <- rbindlist(list(means, res))
    }
    return(means)
  }

  logdollar_decomp <- get_logdollar_decomp(long_data)
  means_decomp <- get_logdollar_means(long_data)
  logdollar_decomp <- rbindlist(list(logdollar_decomp, means_decomp), use.names = T)
  logdollar_decomp <- rbindlist(list(logdollar_decomp, data.table(covariate_1 = "ObsYears", covariate_2 = "N", estimate = nrow(long_data) / 1e6)), use.names = T)
  logdollar_decomp <- rbindlist(list(logdollar_decomp, data.table(covariate_1 = "Workers", covariate_2 = "N", estimate = long_data[, length(unique(worker_ID))])), use.names = T)
  logdollar_decomp <- rbindlist(list(logdollar_decomp, data.table(covariate_1 = "Firms", covariate_2 = "N", estimate = long_data[, length(unique(firm_ID))])), use.names = T)
  logdollar_decomp$market <- "All"

  if (bymarket) {
    unique_markets <- long_data[, unique(broad_market)]
    print("unique markets:")
    print(unique_markets)
    for (mm in unique_markets) {
      res <- get_logdollar_decomp(long_data[broad_market == mm])
      means <- get_logdollar_means(long_data[broad_market == mm])
      res <- rbindlist(list(res, means), use.names = T)
      res <- rbindlist(list(res, data.table(covariate_1 = "ObsYears", covariate_2 = "N", estimate = nrow(long_data[broad_market == mm]) / 1e6)), use.names = T)
      res <- rbindlist(list(res, data.table(covariate_1 = "Workers", covariate_2 = "N", estimate = long_data[broad_market == mm, length(unique(worker_ID))])), use.names = T)
      res <- rbindlist(list(res, data.table(covariate_1 = "Firms", covariate_2 = "N", estimate = long_data[broad_market == mm, length(unique(firm_ID))])), use.names = T)
      res$market <- mm
      logdollar_decomp <- rbindlist(list(logdollar_decomp, res), use.names = T)
    }
  }

  return(logdollar_decomp)
}






#' Collect G_jx
#' @export
lms.model.get_Gjx <- function(fxdata) {
  flog.info("Preparing intermediates")

  check.frame(fxdata,
    requires = c(
      "X", "market_cluster", "market", "cluster", "firm_ID",
      "year", "a_rt", "A_rt", "a_tilde", "A_tilde", "h_j", "H_j", "beta_full", "beta_model", "lambda",
      "rho_r", "alpha_r", "c_r", "xbar", "psi_jt", "theta_j",
      "l_jt"
    ),
    uses = c("N_X", "Pr_X", "Nw_t", "N_X_and_m", "N_m", "Pr_X_given_m"),
    returns = c()
  )

  # We start by collpsing the data at the firm,year level
  # fxdata = long_data[,list(.N),list(X, broad_market, market_cluster, market, cluster, firm_ID,
  #                                   year, a_rt, A_rt, a_tilde, A_tilde, h_j, H_j, beta_full, beta_model, lambda,
  #                                   rho_r, alpha_r, c_r, xbar, psi_jt, psi_jt_init, y_jt, y_jt_pred, theta_j,
  #                                   l_jt, wagebill, wagebill_pred)]
  fxdata[, ff := 1 / (1 + alpha_r * beta_full / rho_r)] # firm factor
  fxdata[, mf := 1 / (1 + alpha_r * beta_full)] # market factor

  # Pr(X)
  fxdata[, N_X := sum(N), list(X)]
  fxdata[, Pr_X := N_X / sum(N)]
  fxdata[, Nw_t := sum(N), list(year)]

  # Pr(X|m) - Probability of X given market cluster m
  fxdata[, N_X_and_m := sum(N), list(X, market_cluster)]
  fxdata[, N_m := sum(N), list(market_cluster)]
  fxdata[, Pr_X_given_m := N_X_and_m / N_m]
  fxdata[, `:=`(N_X_and_m = NULL, N_m = NULL)]
  invisible(gc())

  # Pr(r|t) #
  fxdata[, N_r_and_t := sum(N), list(year, market)]
  fxdata[, N_t := sum(N), list(year)]
  fxdata[, Pr_r_given_t := N_r_and_t / N_t]
  fxdata[, `:=`(N_r_and_t = NULL, N_t = NULL)]
  invisible(gc())

  # Pr(X|k) # this is good
  fxdata[, N_X_and_k := sum(N), list(X, cluster)]
  fxdata[, N_k := sum(N), list(cluster)]
  fxdata[, Pr_X_given_k := N_X_and_k / N_k]
  fxdata[, `:=`(N_X_and_k = NULL, N_k = NULL)]
  invisible(gc())

  # Pr(j|t,r) # this is good
  fxdata[, N_j_and_tr := sum(N), list(firm_ID, year, market)]
  fxdata[, N_tr := sum(N), list(year, market)]
  fxdata[, Pr_j_given_tr := N_j_and_tr / N_tr]
  fxdata[, `:=`(N_j_and_tr = NULL, N_tr = NULL)]
  invisible(gc())

  # constants
  fxdata[, Nf_rkt := length(unique(firm_ID)), list(market, cluster, year)]
  fxdata[, Nr_m := length(unique(market)), list(market_cluster)]
  fxdata[, Nt := length(unique(year))]

  flog.info("Preparing I and G")
  # -------- Within market updating ---------- #
  fxdata[, G_kX := exp(xbar)^(-lambda * theta_j) * (Pr_X_given_m / Pr_X)^(1 / beta_model) * (Pr_X_given_k / Pr_X_given_m)^(rho_r / beta_model)]
  fxdata[, G_j2 := exp(-lambda * psi_jt) * (Pr_j_given_tr)^(rho_r / beta_model)]

  iters <- 10
  for (iter in 1:iters) {

    # Computing stuff
    fxdata[, term_V_Xtj := ((G_kX * G_j2)^(1 / lambda) * exp(psi_jt) * exp(xbar)^(theta_j))^(beta_full / rho_r)]
    fxdata[, term_S_Xtr := sum(term_V_Xtj), list(market, X, year)]
    fxdata[, pred := sum(term_V_Xtj / term_S_Xtr * Pr_X_given_m), list(market, year, firm_ID)]

    # check it sums to one
    # assert_that( all(abs(fxdata[, list(pred[1]), list(market,year,firm_ID)][,sum(V1),market][,V1]-1)<0.001) , msg="the probabilities Pr[j|r,t] don't sum to 1")

    # Check current fit
    flog.info("[%i/%i]correlation in Pr[j|r,t] %f", iter, iters, fxdata[, cor(pred, Pr_j_given_tr)]) # check fit of the prediction

    # update G_j
    fxdata[, G_j2 := G_j2 * (Pr_j_given_tr / pred)^(rho_r / beta_model)]
  }

  # -------- Between market updating ---------- #
  fxdata[, G_r2 := (Pr_r_given_t)^(1 / beta_model)]
  rdata <- fxdata[, list(
    term_S_Xtr = term_S_Xtr[1], rho_r = rho_r[1], G_r2 = G_r2[1], Pr_X = Pr_X[1],
    beta_full = beta_full[1], beta_model = beta_model[1],
    Pr_r_given_t = Pr_r_given_t[1]
  ), by = list(market, X, year)]


  iters <- 20
  for (iter in 1:iters) {
    rdata[, term_V_Xtr := G_r2^beta_model * (term_S_Xtr)^rho_r]
    rdata[, term_S_Xt := sum(term_V_Xtr), list(year, X)]
    rdata[, pred := sum(term_V_Xtr / term_S_Xt * Pr_X), list(market, year)]

    # check it sums to one
    # assert_that( all(abs(rdata[, list(pred[1]), list(market,year)][,sum(V1),year][,V1]-1)<0.001) , msg="the probabilities Pr[r|t] doen't sum to 1")

    # Check current fit
    flog.info("[%i/%i]correlation in Pr[r|t] %f", iter, iters, rdata[, cor(pred, Pr_r_given_t)]) # check fit of the prediction

    # update G_r
    rdata[, G_r2 := G_r2 * (Pr_r_given_t / pred)^(1 / beta_model)]
  }

  # --------- Comnbining within and between ----- #
  rdata <- rdata[, .(year, X, market, term_S_Xt, term_V_Xtr, G_r2)]
  # fxdata[, term_S_Xt := NULL][, term_V_Xtr := NULL][, G_r2 := NULL]
  fxdata[, G_r2 := NULL]
  fxdata <- merge(fxdata, rdata, by = c("market", "year", "X"))
  fxdata[, G_jX := G_kX * G_j2 * G_r2]

  # compute predicted probabilities
  fxdata[, Pr_j_given_Xt_pred := (term_V_Xtr / term_S_Xt) * (term_V_Xtj / term_S_Xtr)]

  # check predicted L_jt, and predicted H_jt
  fxdata[, l_jt_pred := log(sum(exp(xbar)^(theta_j) * Pr_j_given_Xt_pred * Pr_X)) + log(Nw_t), list(firm_ID, year)]
  fxdata[, h_j_pred := (1 / ff) * mean(l_jt_pred - ff * (beta_full / rho_r) * c_r - mf * (beta_model * lambda) * a_rt - ff * (beta_model * lambda / rho_r) * a_tilde), list(firm_ID)]

  # gather true vs predicted size
  # fxdata[, sum(Pr_j_given_Xt_pred), list(X,year)] # check that this is 1
  fxdata[, Pr_size := Pr_r_given_t * Pr_j_given_tr]
  fxdata[, Pr_size_pred := sum(Pr_j_given_Xt_pred * Pr_X), list(firm_ID, year)]
  fxdata[, Pr_size_log := log(Pr_size)]
  fxdata[, Pr_size_pred_log := log(Pr_size_pred)]

  fxdata[, Pr_jX := Pr_X_given_k * Pr_j_given_tr * Pr_r_given_t]

  invisible(gc())
  flog.info("Done")

  return(fxdata)
}

#' Model checks
#' @export
lms.check_Gjx <- function(long_data) {
  flog.info("Preparing true values of Pr_r_given_Xt and Pr_k_given_Xrt")
  long_data[, `:=`(Pr_r_and_Xt, .N), list(market, X, year)]
  long_data[, `:=`(Pr_Xt, .N), list(X, year)]
  long_data[, `:=`(Pr_r_given_Xt, Pr_r_and_Xt / Pr_Xt)]
  long_data[, `:=`(N_k_and_Xrt, .N), list(cluster, X, market, year)]
  long_data[, `:=`(N_Xrt, .N), list(X, market, year)]
  long_data[, `:=`(Pr_k_given_Xrt, N_k_and_Xrt / N_Xrt)]

  flog.info("Preparing Pr_r_given_Xt_pred")
  subdata_forI <- long_data[, lapply(.SD, mean),
    by = c("market", "X", "year"),
    .SDcols = c("I_rX", "A_rt", "beta_full", "alpha_r", "rho_r")
  ]
  subdata_forI[, `:=`(Pr_r_given_Xt_pred, A_rt^(beta_full / (1 + alpha_r * beta_full / rho_r)) * I_rX^beta_full)]
  subdata_forI[, Pr_r_given_Xt_pred := Pr_r_given_Xt_pred / sum(Pr_r_given_Xt_pred), list(X, year)]
  long_data <- merge(long_data, subdata_forI[, .(market, X, year, Pr_r_given_Xt_pred)], by = c("market", "X", "year"))

  flog.info("Preparing Pr_k_given_Xrt_pred")
  subdata_forG <- long_data[, lapply(.SD, mean),
    by = c("firm_ID", "X", "market", "cluster", "year"),
    .SDcols = c("H_j", "A_tilde", "xbar", "beta_full", "rho_r", "alpha_r", "exponent_denom")
  ]
  subdata_forG <- subdata_forG[, list(inner_term = sum((H_j^(-alpha_r / exponent_denom) * A_tilde^(1 / exponent_denom) * xbar)^(beta_full / rho_r))), list(X, market, cluster, year)]
  long_data <- merge(long_data, subdata_forG, by = c("market", "X", "cluster", "year"))
  long_data[, `:=`(Pr_k_given_Xrt_pred, ((G_kX / I_rX) * C_r^(1 / exponent_denom))^(beta_full / rho_r) * inner_term)]

  flog.info("done")
  return(long_data)
}

#' Solving for Hj as a function of the fixed point given
#' primitives of the model. Requires a few columns from fxdata: GjX,a_rt,a_tilde....
#' @param relax set to 0 for no relaxing, 0.9 is recommended
#' @export
lms.model.solve_Hj <- function(fxdata, num_steps = 10, threshold, relax = 0, price_takers = FALSE) {
  check.frame(fxdata,
    requires = c("tau_ct", "lambda_ct", "beta_model", "alpha_r", "rho_r", "h_j", "G_jX", "xbar", "Pr_X", "Nw_t", "a_rt", "a_tilde", "size", "l_jt"),
    uses = c("psi_jt", "term_V_Xtj", "term_S_Xtr", "l_jt_bis"),
    returns = c("h_j_ct", "y_jt_ct", "Pr_jX_ct")
  )

  # prepare some constants
  fxdata[, ff_ct := 1 / (1 + alpha_r * beta_model * lambda_ct / rho_r)] # firm factor
  fxdata[, mf_ct := 1 / (1 + alpha_r * beta_model * lambda_ct)] # market factor
  if (!price_takers) {
    fxdata[, c_r_ct := log((1 - alpha_r) * (beta_model * lambda_ct / rho_r) / (1 + beta_model * lambda_ct / rho_r))]
  } else {
    fxdata[, c_r_ct := log((1 - alpha_r) * (beta_model) / (1 + beta_model))]
  }

  # use h_j as starting value
  fxdata[, h_j_ct := copy(h_j)]

  for (step in 1:num_steps) {

    # we start with the provided value of h_j
    # 0) we construct psi_jt
    fxdata[, psi_jt_ct := c_r_ct * ff - alpha_r * ff_ct * h_j_ct + mf_ct * a_rt + ff_ct * a_tilde]

    # 1) compute within market constants
    fxdata[, term_V_Xtj := ((tau_ct * G_jX)^(1 / lambda_ct) * exp(psi_jt_ct + xbar * theta_j))^(beta_model * lambda_ct / rho_r)]
    fxdata[, term_S_Xtr := sum(term_V_Xtj), list(market, X, year)]

    # 2) compute between market constants
    rdata <- fxdata[, list(
      term_S_Xtr = term_S_Xtr[1], rho_r = rho_r[1],
      beta_model = beta_model[1]
    ), by = list(market, X, year)]

    rdata[, term_V_Xtr := (term_S_Xtr)^rho_r]
    rdata[, term_S_Xt := sum(term_V_Xtr), list(year, X)]

    # 3) merge
    rdata <- rdata[, .(year, X, market, term_S_Xt, term_V_Xtr)]
    fxdata[, term_S_Xt := NULL][, term_V_Xtr := NULL]
    fxdata <- merge(fxdata, rdata, by = c("market", "year", "X"))

    # 4) evaluate l_jt and h_jt
    fxdata[, Pr_j_given_Xt_pred := (term_V_Xtr / term_S_Xt) * (term_V_Xtj / term_S_Xtr)]
    setkey(fxdata, firm_ID, year)
    fxdata[, l_jt_ct := log(sum(exp(xbar * theta_j) * Pr_j_given_Xt_pred * Pr_X)) + log(Nw_t), list(firm_ID, year)]
    setkey(fxdata, firm_ID)
    fxdata[, h_j_ct_update := (1 / ff_ct) * mean(l_jt_ct - ff_ct * (beta_model * lambda_ct / rho_r) * c_r_ct - mf_ct * (beta_model * lambda_ct) * a_rt - ff_ct * (beta_model * lambda_ct / rho_r) * a_tilde), list(firm_ID)]

    # size
    fxdata[, sizepred := Pr_j_given_Xt_pred * Pr_X * Nw_t]

    # check distance
    h_dist_to_init <- fxdata[, round(mean((h_j_ct_update - h_j)^2), 3)]
    h_dist_to_prev <- fxdata[, round(mean((h_j_ct_update - h_j_ct)^2), 3)]

    # update
    fxdata[, h_j_ct := relax * h_j_ct + (1 - relax) * copy(h_j_ct_update)] # apply a relaxation

    # check h info
    h_mean_truesize <- fxdata[, round(weighted.mean(h_j_ct, size), 3)]
    h_mean_predsize <- fxdata[, round(weighted.mean(h_j_ct, sizepred), 3)]
    aa <- cov.wt(fxdata[, .(h_j, h_j_ct)], wt = fxdata$size)$cov
    h_corr <- round(aa[1, 2] / sqrt(aa[1, 1] * aa[2, 2]), 3)
    h_var_truesize <- round(aa[2, 2], 3)
    aa <- cov.wt(fxdata[, .(h_j, h_j_ct)], wt = fxdata$sizepred)$cov
    h_corr_predsize <- round(aa[1, 2] / sqrt(aa[1, 1] * aa[2, 2]), 3)
    h_var_predsize <- round(aa[2, 2], 3)

    # check l
    l_mean_truesize <- fxdata[, round(weighted.mean(l_jt_ct, size), 3)]
    l_mean_predsize <- fxdata[, round(weighted.mean(l_jt_ct, sizepred), 3)]
    aa <- cov.wt(fxdata[, .(l_jt, l_jt_ct)], wt = fxdata$size)$cov
    l_corr <- round(aa[1, 2] / sqrt(aa[1, 1] * aa[2, 2]), 3)
    l_var_truesize <- round(aa[2, 2], 3)
    l_var_predsize <- round(cov.wt(fxdata[, .(l_jt_ct)], wt = fxdata$sizepred)$cov[1, 1], 3)


    # check psi
    psi_mean_truesize <- fxdata[, round(weighted.mean(psi_jt_ct, size), 3)]
    psi_mean_predsize <- fxdata[, round(weighted.mean(psi_jt_ct, sizepred), 3)]
    aa <- cov.wt(fxdata[, .(psi_jt, psi_jt_ct)], wt = fxdata$size)$cov
    psi_corr <- round(aa[1, 2] / sqrt(aa[1, 1] * aa[2, 2]), 3)
    psi_var_truesize <- round(aa[2, 2], 3)
    psi_var_predsize <- round(cov.wt(fxdata[, .(psi_jt_ct)], wt = fxdata$sizepred)$cov[1, 1], 3)

    # check size
    size_corr <- fxdata[, round(cor(size, sizepred), 3)]
    sizelog_corr <- fxdata[, round(cor(log(size), log(sizepred)), 3)]

    # log results
    flog.info("[%i/%i] h_j: Dist from prev %s, dist from init %s, corr w/init %s, corr w/init pred size %s", step, num_steps, h_dist_to_prev, h_dist_to_init, h_corr, h_corr_predsize)
    flog.info("[%i/%i] h_j: Mean w/true vs pred size %s/%s, var w/true vs pred size %s/%s", step, num_steps, h_mean_truesize, h_mean_predsize, h_var_truesize, h_var_predsize)
    flog.info("[%i/%i] l_j: corr w/ init %s, Mean w/true vs pred size %s/%s, var w/true vs pred size %s/%s", step, num_steps, l_corr, l_mean_truesize, l_mean_predsize, l_var_truesize, l_var_predsize)
    flog.info("[%i/%i] psi_j: corr w/ init %s, Mean w/true vs pred size %s/%s, var w/true vs pred size %s/%s", step, num_steps, psi_corr, psi_mean_truesize, psi_mean_predsize, psi_var_truesize, psi_var_predsize)
    flog.info("[%i/%i] size: corr in levels/logs %s/%s", step, num_steps, size_corr, sizelog_corr)

    if (h_dist_to_prev < threshold) {
      break
    }
    # Finally we loop
  }

  # compute other outcomes
  fxdata[, Pr_jX_ct := Pr_j_given_Xt_pred * Pr_X]
  fxdata[, y_jt_ct := (1 - alpha_r) * (beta_model * lambda_ct / rho_r) * ff_ct * c_r_ct + (1 - alpha_r) * ff_ct * h_j_ct + (1 + beta_model * lambda_ct) * mf_ct * a_rt + (1 + beta_model * lambda_ct / rho_r) * ff_ct * a_tilde]
  return(fxdata)
}

#' Checking the columns of a data.frame
#' @param requires are columns that are required by the function
#' @param uses are temporary columns the data.frame will create
#' @param returns are the outputs
#' @export
check.frame <- function(dd, requires = c(), uses = c(), returns = c()) {

  # checking requirements
  names <- setdiff(requires, colnames(dd))
  if (length(names) > 0) flog.error("input is missing columns %s", paste(names, collapse = ","))

  # checking column names that will be overwritten
  names <- intersect(uses, colnames(dd))
  if (length(names) > 0) flog.warn("function will overwrite columns %s", paste(names, collapse = ","))

  # checking column names that will be overwritten by results
  names <- intersect(returns, colnames(dd))
  if (length(names) > 0) flog.warn("function will overwrite returns in columns %s", paste(names, collapse = ","))
}


#' We solve for a counterfactual allocation when we shut shrink the
#' non-linearity in X in theta_j and G_jX.
#' @param shrink_theta shrinking theta to common value across firms, if 0, nothing happens, if 1, same for everyone
#' @param shrink_g shrking the G_jX, if 0 nothing happens, if 1 G_jX is log-additive in j and X
#' @export
lms.model.solve_theta_ct <- function(fxdata, shrink_theta = 0, shrink_g = 0) {
  check.frame(fxdata,
    requires = c("lambda", "beta_model", "alpha_r", "rho_r", "psi_jt", "G_jX", "xbar", "Pr_X", "theta_j"),
    uses = c("term_V_Xtj", "term_S_Xtr"),
    returns = c("Pr_jX_ct", "w_jt_ct")
  )

  # shrink theta and g
  fxdata[, theta_j_ct := (shrink_theta * mean(theta_j) + (1 - shrink_theta) * theta_j)]
  fxdata[, g_jx := log(G_jX)]
  fxdata[, g_j := mean(g_jx), firm_ID]
  fxdata[, g_jx_ct := g_j + shrink_g * mean(g_jx - g_j) + (1 - shrink_g) * (g_jx - g_j), X]
  fxdata[, G_jX_ct := exp(g_jx_ct)]

  # 1) compute within market constants
  fxdata[, term_V_Xtj := ((G_jX_ct)^(1 / lambda) * exp(psi_jt + theta_j_ct * xbar))^(beta_model * lambda / rho_r)]
  fxdata[, term_S_Xtr := sum(term_V_Xtj), list(market, X, year)]

  # 2) compute between market constants
  rdata <- fxdata[, list(
    term_S_Xtr = term_S_Xtr[1], rho_r = rho_r[1],
    beta_model = beta_model[1]
  ), by = list(market, X, year)]

  rdata[, term_V_Xtr := (term_S_Xtr)^rho_r]
  rdata[, term_S_Xt := sum(term_V_Xtr), list(year, X)]

  # 3) merge
  rdata <- rdata[, .(year, X, market, term_S_Xt, term_V_Xtr)]
  fxdata[, term_S_Xt := NULL][, term_V_Xtr := NULL]
  fxdata <- merge(fxdata, rdata, by = c("market", "year", "X"))

  # 4) evaluate allocation
  fxdata[, Pr_jX_ct := (term_V_Xtr / term_S_Xt) * (term_V_Xtj / term_S_Xtr) * Pr_X]
  fxdata[, w_jt_ct := psi_jt + theta_j_ct * xbar]

  return(fxdata)
}


#' Model counterfactual moments
#' @export
lms.model.extract_ct_moments <- function(fxdata, G_constant, price_takers = FALSE) {
  fxdata[, w_xjt := theta_j * xbar + psi_jt_ct] # gross wage
  fxdata[, w_jt_ct := copy(w_xjt)]
  fxdata[, w_jt_ct_net := lambda_ct * w_jt_ct] # net wage

  fxdata[, psi_j_ct := psi_jt_ct - adj_term_jt_res]
  fxdata[, x_mean := weighted.mean(xbar, Pr_jX_ct)]
  fxdata[, interactions := (xbar - x_mean) * (theta_j - 1)] # the xbar and theta_j are already in tilde units

  aa <- cov.wt(fxdata[, .(xbar, psi_j_ct, w_jt_ct, y_jt_ct, w_jt_ct_net, interactions, adj_term_jt_res)], wt = fxdata$Pr_jX_ct)$cov
  dd <- data.table(
    x_var = aa[1, 1],
    psi_var = aa[2, 2],
    psi_x_cov = aa[1, 2],
    w_var = aa[3, 3],
    y_var = aa[4, 4],
    net_w_var = aa[5, 5],
    interactions_var = aa[6, 6],
    interactions_cov_with_x_plus_psi = aa[6, 1] + aa[6, 2],
    timevarying = aa[7, 7] + 2 * aa[7, 1] + 2 * aa[7, 6]
  )
  dd$w_mean <- fxdata[, weighted.mean(w_jt_ct, Pr_jX_ct)]

  # storing some constants
  dd$c_r <- fxdata[, mean(c_r_ct)]
  dd$Nw_t <- fxdata[, mean(Nw_t)]
  dd$beta_model <- fxdata[, mean(beta_model)]
  dd$lambda_ct <- fxdata[, mean(lambda_ct)]
  dd$G_constant <- G_constant # normalizing constant

  # welfare due to preferences: paid wages and amenities
  dd$welfare_prefs_pw <- fxdata[, weighted.mean(log(term_S_Xt), Pr_jX_ct)] # we save it per workers

  # firm profit per worker
  fdata <- fxdata[, list(y_jt_ct = y_jt_ct[1], profits = (exp(y_jt_ct) - exp(y_jt_ct + c_r_ct))[1]), firm_ID]
  dd$profits_pw <- fdata[, sum(profits / dd$Nw_t)]

  dd$y_mean <- fdata[, mean(y_jt_ct)]
  dd$y_pw <- fdata[, sum(exp(y_jt_ct) / dd$Nw_t)]

  # storing other average firm level outcomes
  dd$mean_log_profits <- fdata[, mean(log(profits))]

  # tax revenue
  dd$gvtRevenue_pw <- fxdata[, weighted.mean(exp(w_xjt) - exp(lambda_ct * w_xjt), Pr_jX_ct)]
  dd$netWageBill_pw <- fxdata[, weighted.mean(exp(lambda_ct * w_xjt), Pr_jX_ct)]

  # also get within-X g_jX variance
  weighted.variance <- function(var, wt) {
    df <- data.table(vv = var)
    return(cov.wt(df, wt)$cov[1, 1])
  }
  dd$withinX_var_gjX <- fxdata[, weighted.variance(log(G_jX), Pr_jX_ct), by = list(X)][, mean(V1)]
  dd$mean_gjX <- fxdata[, weighted.mean(log(G_jX), Pr_jX_ct)]

  # implied welfare
  dd[, welfare_wage := welfare_prefs_pw - G_constant]
  dd[, welfare_taxesAndProfits := beta_model * log(profits_pw / netWageBill_pw)]
  dd[, welfare_total := welfare_wage + welfare_taxesAndProfits]

  # other info

  helper.getPsiFirm <- function(a, b_model, r, lambda) {
    br <- lambda * b_model / r
    expo <- -(1 - a) * br / (1 + a * br)
    mult <- a * (1 + br) / (1 + a * br)
    base <- br / (1 + br)
    return(1 - mult * (base)^expo)
  }

  helper.getPsiMarket <- function(a, b_model, r, lambda) {
    br <- lambda * b_model / r
    expo <- -(1 - a) * lambda * b_model / (1 + a * lambda * b_model) # this differs from Firm-level
    mult <- a * (1 + br) / (1 + a * br)
    base <- br / (1 + br)
    return(1 - mult * (base)^expo)
  }

  helper.getPassthroughFirm <- function(a, b_model, r, lambda) {
    br <- lambda * b_model / r
    return(1 / (1 + br))
  }

  helper.getPassthroughMarket <- function(a, b_model, r, lambda) {
    return(1 / (1 + lambda * b_model))
  }

  fxdata[, Psif := helper.getPsiFirm(alpha_r, beta_model, rho_r, lambda_ct)]
  fxdata[, ptf := helper.getPassthroughFirm(alpha_r, beta_model, rho_r, lambda_ct)]
  fxdata[, Psim := helper.getPsiMarket(alpha_r, beta_model, rho_r, lambda_ct)]
  fxdata[, ptm := helper.getPassthroughMarket(alpha_r, beta_model, rho_r, lambda_ct)]
  fxdata[, wedge := 1 + rho_r / (beta_model * lambda_ct)]

  dd$Psif <- fxdata[, weighted.mean(Psif, Pr_jX_ct)]
  dd$ptf <- fxdata[, weighted.mean(ptf, Pr_jX_ct)]
  dd$Psim <- fxdata[, weighted.mean(Psim, Pr_jX_ct)]
  dd$ptm <- fxdata[, weighted.mean(ptm, Pr_jX_ct)]
  dd$wedge <- fxdata[, weighted.mean(wedge, Pr_jX_ct)]
  if (price_takers) {
    dd$wedge <- 1.0
  }


  return(dd)
}



#' All of the model-based results from LMS
#' @param dd input data produced by lms.application.blm
#' @param relax convergence tolerance parameter
#' @param focal_year select a year on which to estimate model counterfactuals
#' @export
lms.model <- function(dd, relax = 0.9, focal_year = 2005) {
  model_results <- list()

  aa <- lms.model.prepare(copy(dd))

  # main model estimation
  model_results$decomp <- lms.model.decomp(copy(aa))

  # solution with G
  fxdata <- lms.model.G(copy(aa))$fxdata

  # model solution at baseline
  fxdatayr <- copy(fxdata[year == focal_year])[, tau_ct := 1.0]
  fxdatayr_baseline <- lms.model.solve_Hj(copy(fxdatayr)[, lambda_ct := copy(lambda)], num_steps = 10000, threshold = 0.01, relax = relax, price_taker = FALSE)

  # goodness of fit
  grid <- lms.model.size_fit_grid(copy(fxdatayr))
  gridh <- lms.model.h_fit_grid(copy(fxdatayr), copy(fxdatayr_baseline))
  gridboth <- merge(grid, gridh, by = c("size", "type"))
  setnames(gridboth, c("N.x", "N.y"), c("size_main", "size_h"))
  model_results$grid <- gridboth

  # welfare counterfactuals
  G_constant <- fxdatayr_baseline[, weighted.mean(beta_model * log(G_jX), Pr_jX_ct)]
  welfare_baseline <- lms.model.extract_ct_moments(copy(fxdatayr_baseline), G_constant)[, model := "baseline"]
  welfare_priceTaker <- lms.model.extract_ct_moments(lms.model.solve_Hj(copy(fxdatayr)[, lambda_ct := 1.0], num_steps = 10000, threshold = 0.01, relax = relax, price_takers = TRUE), G_constant, price_takers = TRUE)[, model := "priceTaker"]
  model_results$welfare <- rbind(welfare_baseline, welfare_priceTaker)

  # FE and sorting in shrinks
  FEsort <- fxdatayr_baseline[, Xq := ceiling(X / 2)][, list(size = sum(Pr_jX_ct), FE = mean(psi_jt), WE = mean(xbar)), list(Xq, cluster)][order(Xq, cluster)][, type := "baseline"][, shrink := 0.0]
  fxdatayr_shrinkTheta <- lms.model.solve_theta_ct(copy(fxdatayr), shrink_theta = 0.5, shrink_g = 0)
  FEsort <- rbind(FEsort, fxdatayr_shrinkTheta[, Xq := ceiling(X / 2)][, list(size = sum(Pr_jX_ct), FE = mean(psi_jt), WE = mean(xbar)), list(Xq, cluster)][order(Xq, cluster)][, type := "theta"][, shrink := 0.5])
  fxdatayr_shrinkG <- lms.model.solve_theta_ct(copy(fxdatayr), shrink_theta = 0, shrink_g = 0.5)
  model_results$shrink_FEsort <- rbind(FEsort, fxdatayr_shrinkG[, Xq := ceiling(X / 2)][, list(size = sum(Pr_jX_ct), FE = mean(psi_jt), WE = mean(xbar)), list(Xq, cluster)][order(Xq, cluster)][, type := "G"][, shrink := 0.5])

  # shrink patterns
  res_shrink <- data.table()
  for (ii in 0:10 / 10) {
    res_shrink <- rbind(res_shrink, lms.model.extract_ct_moments(lms.model.solve_theta_ct(copy(fxdatayr)[, `:=`(lambda_ct = copy(lambda), psi_jt_ct = copy(psi_jt), y_jt_ct = copy(y_jt), c_r_ct = copy(c_r))], shrink_theta = ii, shrink_g = 0), G_constant)[, type := "theta"][, shrink := ii])
    res_shrink <- rbind(res_shrink, lms.model.extract_ct_moments(lms.model.solve_theta_ct(copy(fxdatayr)[, `:=`(lambda_ct = copy(lambda), psi_jt_ct = copy(psi_jt), y_jt_ct = copy(y_jt), c_r_ct = copy(c_r))], shrink_theta = 0, shrink_g = ii), G_constant)[, type := "G"][, shrink := ii])
  }
  model_results$shrink_levels <- res_shrink

  # compensating differentials
  model_results$compdiffs <- lms.model.compdiffs(copy(aa), num_firms = 1000)

  return(model_results)
}
