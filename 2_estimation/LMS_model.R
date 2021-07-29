
collect_model <- function(results_dir,intermed_dir,industry='naics2', location='cz', broadmarkets=TRUE){
  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets)
  dd <- setDT(readRDS(file=sprintf("%sFE/blm_adjusted_post_%s.rds",intermed_dir,suffix))$full_data)
  setnames(dd,'broadmarket','broad_market')
  model_results <- lms.model(dd)
  # save results into separate CSV files
  write.csv(model_results$decomp,file=sprintf("%smodel/LMS_psidecomp_%s.csv",results_dir,suffix),row.names=F)
  write.csv(model_results$grid,file=sprintf("%smodel/LMS_fitgrid_%s.csv",results_dir,suffix),row.names=F)
  write.csv(model_results$shrink_levels,file=sprintf("%smodel/LMS_shrinks_%s.csv",results_dir,suffix),row.names=F)
  write.csv(model_results$shrink_FEsort,file=sprintf("%smodel/LMS_shrinksort_%s.csv",results_dir,suffix),row.names=F)
  write.csv(model_results$compdiffs,file=sprintf("%smodel/LMS_compdiffs_%s.csv",results_dir,suffix),row.names=F)
  write.csv(model_results$welfare,file=sprintf("%smodel/LMS_welfare_%s.csv",results_dir,suffix),row.names=F)
}

collect_model_descriptives <- function(results_dir,intermed_dir,industry='naics2', location='cz', broadmarkets=TRUE){

  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets)
  dd <- setDT(readRDS(file=sprintf("%sFE/blm_adjusted_post_%s.rds",intermed_dir,suffix))$full_data)
  setnames(dd,'broadmarket','broad_market')
  dd <- lms.model.prepare(dd); gc()

  dd[, `:=`(a_rt_bar, mean(a_rt))]
  dd[, `:=`(atotal, 1/(1 + alpha_r * lambda * beta_model/rho_r) * c_r + 1/(1 + alpha_r * lambda * beta_model) * (a_rt -a_rt_bar) + 1/(1 + alpha_r * lambda * beta_model/rho_r) * a_tilde)]
  dd[, `:=`(htotal, -alpha_r * (1/(1 + alpha_r * lambda * beta_model/rho_r) * h_j + lambda * beta_model/(1 + alpha_r * lambda * beta_model) * a_rt_bar))]
  dd[, `:=`(abar_market, mean(atotal)), list(market, year)]
  dd[, `:=`(abar_market_bar = abar_market - mean(abar_market)), list(market)]
  dd[, `:=`(a_tilde_mod_bar, mean(a_tilde_mod)), list(firm_ID)]

  descriptives <- data.table( x_unique = dd[,length(unique(worker_ID))])
  descriptives$x_var <- dd[,var(x_i)]
  descriptives$psi_unique <- nrow(unique(dd[,.(firm_ID,year)]))
  descriptives$psi_var <- dd[,var(psi_jt_init)]
  descriptives$theta_unique <- dd[, length(unique(theta_j))]
  descriptives$theta_var <- dd[,var(theta_j)]
  descriptives$h_unique <- dd[, length(unique(firm_ID))]
  descriptives$h_var <- dd[, var(htotal)]
  descriptives$lambda_val <- dd[, unique(lambda)]
  descriptives$beta_val <- dd[, unique(beta_model)]
  descriptives$rho_mean <- dd[, mean(rho_r)]
  descriptives$rho_unique <- dd[, length(unique(rho_r))]
  descriptives$alpha_mean <- dd[, mean(alpha_r)]
  descriptives$alpha_unique <- dd[, length(unique(alpha_r))]
  descriptives$TFP_unique <- nrow(unique(dd[,.(firm_ID,year)]))
  descriptives$TFP_var <- dd[,var(a_tilde_mod)]
  descriptives$TFPbar_unique <- dd[, length(unique(firm_ID))]
  descriptives$TFPbar_var <- dd[,var(a_tilde_mod_bar)]
  descriptives$marketTFP_unique <- nrow(unique(dd[,.(market,year)]))
  descriptives$marketTFP_var <- dd[, var(abar_market)]
  descriptives$marketTFPbar_unique <- dd[, length(unique(market))]
  descriptives$marketTFPbar_var <- dd[, var(abar_market_bar)]

  fxdata <- lms.model.G(copy(dd))$fxdata
  descriptives$g_unique <- nrow(fxdata[year==2005])
  descriptives$g_var <- fxdata[, weighted.var(g_jX - g_jX_btw,Pr_jX)]

  write.csv(descriptives, file=sprintf("%smodel/LMS_model_descriptives_%s.csv",results_dir,suffix), row.names=F)

}

collect_decomp_AKM <- function(results_dir, intermed_dir, industry='naics2', location='cz', broadmarkets=TRUE){
  suffix <- get_suffix(industry=industry, location=location, broadmarkets=broadmarkets)
  dd <- setDT(readRDS(file=sprintf("%sFE/akm_adjusted_post_%s.rds",intermed_dir,suffix))$full_data)
  setnames(dd,'broadmarket','broad_market')
  dd[, gamma := 1.0]
  dd[, gamma_bar := 1.0]
  dd[, cluster := copy(firm_ID)]
  setnames(dd,c('FEbar','FE','WE'),c('FEbarnl','FEnl','xnl'))
  dd <- lms.model.prepare(dd); gc()
  decomp <- lms.model.decomp(dd)
  write.csv(decomp, file=sprintf("%smodel/LMS_psidecomp_AKM_%s.csv",results_dir,suffix), row.names=F)
}

# collect_model()
# collect_decomp_AKM()
# collect_model_descriptives()
