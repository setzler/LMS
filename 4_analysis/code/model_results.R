

table.4 <- function() {
  decomp <- setDT(read.csv(file=sprintf("%smodel/LMS_psidecomp_naics2_cz_broadmarket.csv",input_dir)))
  decomp2 <- setDT(read.csv(sprintf("%smodel/LMS_psidecomp_AKM_naics2_cz_broadmarket.csv",input_dir)))

  tt <- TR(c(" ", "\\textbf{Between Broad Markets}", "\\textbf{Within Broad Markets}"), c(1, 1, 2)) +
    tt_rule_mid_partial(list(c(2, 2), c(3, 4))) +
    TR(c(" ", "", "\\textbf{Between}", "\\textbf{Within}"), c(1, 1, 1, 1)) +
    TR(c(" ", "", "\\textbf{Detailed Markets}", "\\textbf{Detailed Markets}"), c(1, 1, 1, 1)) +
    tt_rule_mid() +
    TR("") %:% TR("\\textbf{Preferred Specification}", 3) +
    TR("Total") %:% TR(decomp[, c(var_psi_outer, var_psi_inner, var_psitilde) / var_w * 100], dec = 1, percentage = T) +
    TR("Decomposition:") +
    TR("$\\quad$ Amenity Differences") %:% TR(decomp[, c(var_hbar_outer, var_hbar_inner, var_htilde) / var_w * 100], dec = 1, percentage = T) +
    TR("$\\quad$ TFP Differences") %:% TR(decomp[, c(var_abar_outer, var_abar_inner, var_atilde) / var_w * 100], dec = 1, percentage = T) +
    TR("$\\quad$ Amenity-TFP Covariance") %:% TR(decomp[, c(2 * cov_abar_hbar_outer, 2 * cov_abar_hbar_inner, 2 * cov_atilde_htilde) / var_w * 100], dec = 1, percentage = T) +
    tt_rule_mid() +
    TR("") %:% TR("\\textbf{Log-additive Fixed Effects Specification}", 3) +
    TR("Total") %:% TR(decomp2[, c(var_psi_outer, var_psi_inner, var_psitilde) / var_w * 100], dec = 1, percentage = T) +
    TR("Decomposition:") +
    TR("$\\quad$ Amenity Differences") %:% TR(decomp2[, c(var_hbar_outer, var_hbar_inner, var_htilde) / var_w * 100], dec = 1, percentage = T) +
    TR("$\\quad$ TFP Differences") %:% TR(decomp2[, c(var_abar_outer, var_abar_inner, var_atilde) / var_w * 100], dec = 1, percentage = T) +
    TR("$\\quad$ Amenity-TFP Covariance") %:% TR(decomp2[, c(2 * cov_abar_hbar_outer, 2 * cov_abar_hbar_inner, 2 * cov_atilde_htilde) / var_w * 100], dec = 1, percentage = T)

  TS(tt, file = "table4", header = paste0(c("l", rep("c", 3))), output_path = output_dir)
}


figure.A4abcd_A5 <- function() {
  dd <- setDT(read.csv(file=sprintf("%smodel/LMS_fitgrid_naics2_cz_broadmarket.csv",input_dir)))
  dd[type == "true", type := "Observed"]
  dd[type == "pred", type := "Predicted"]
  dd <- dd[size >= 1 & size <= 8]
  
  max_check <- dd[,max(size)]

  gg <- ggplot(aes(x = (size), y = y_jt, linetype = type), data = dd) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 16) +
    labs(x = "Log Size", y = "Mean Log VA", linetype = " ") +
    scale_y_continuous(breaks = pretty_breaks()) 
  if(max_check>5){
    gg <- gg + xlim(1, 8)
  }
  ggsave(gg, file = sprintf("%sfigureA4a.pdf",output_dir), width = 8, height = 5)

  gg <- ggplot(aes(x = (size), y = wagebill, linetype = type), data = dd) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 16) +
    labs(x = "Log Size", y = "Mean Log Wage bill", linetype = " ") +
    scale_y_continuous(breaks = pretty_breaks())
  if(max_check>5){
    gg <- gg + xlim(1, 8)
  }
  ggsave(gg, file = sprintf("%sfigureA4d.pdf",output_dir), width = 8, height = 5)

  gg <- ggplot(aes(x = (size), y = psi_jt, linetype = type), data = dd) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 16) +
    labs(x = "Log Size", y = "Mean Firm Effect", linetype = " ") +
    scale_y_continuous(breaks = pretty_breaks())
  if(max_check>5){
    gg <- gg + xlim(1, 8)
  }
  ggsave(gg, file = sprintf("%sfigureA4b.pdf",output_dir), width = 8, height = 5)

  gg <- ggplot(aes(x = (size), y = (l_jt), linetype = type), data = dd) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 16) +
    labs(x = "Log Size", y = "Mean Log Efficiency Units of Labor", linetype = " ") +
    scale_y_continuous(breaks = pretty_breaks()) 
  if(max_check>5){
    gg <- gg + xlim(1, 8)
  }
  ggsave(gg, file = sprintf("%sfigureA4c.pdf",output_dir), width = 8, height = 5)

  dd <- setDT(read.csv(file=sprintf("%smodel/LMS_fitgrid_naics2_cz_broadmarket.csv",input_dir)))
  dd[type == "true", type := "Baseline"]
  dd[type == "pred", type := "Equilibrium"]
  dd <- dd[size >= 1 & size <= 8]

  gg <- ggplot(aes(x = (size), y = h_j, linetype = type), data = dd) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 16) +
    labs(x = "Log Size", y = expression("Mean h"[j]), linetype = " ") +
    scale_y_continuous(breaks = pretty_breaks()) 
  if(max_check>5){
    gg <- gg + xlim(1, 8)
  }
  ggsave(gg, file = sprintf("%sfigureA5.pdf",output_dir), width = 8, height = 5)
}


figure.A7ab <- function() {

  resid_var <- 0.04484871 # this comes from interacted adjusted BLM
  dd <- setDT(read.csv(file=sprintf("%smodel/LMS_shrinks_naics2_cz_broadmarket.csv",input_dir)))
  types <- c("G", "theta")
  dd <- dd[type %in% types]
  dd[, type := factor(type, types)]
  dd <- dd[order(type, shrink)]
  dd <- dd[shrink <= 0.5]

  gg <- ggplot(data = dd, aes(x = shrink, y = 2 * psi_x_cov / (w_var + resid_var) * 100, linetype = type)) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 16) +
    labs(x = "Shrink Rate", y = "Sorting Covariance (%)", linetype = "Parameter\nShrunk:") +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_linetype_discrete(labels = c("G" = expression("g"[j](x)), "theta" = expression(theta[j])))
  ggsave(gg, file = sprintf("%sfigureA7a.pdf",output_dir), width = 8, height = 5)

  gg <- ggplot(data = dd, aes(x = shrink, y = psi_x_cov / sqrt(psi_var * x_var), linetype = type)) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 16) +
    labs(x = "Shrink Rate", y = "Sorting Correlation", linetype = "Parameter\nShrunk:") +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_linetype_discrete(labels = c("G" = expression("g"[j](x)), "theta" = expression(theta[j])))
  ggsave(gg, file = sprintf("%sfigureA7b.pdf",output_dir), width = 8, height = 5)
}


figure.3abcd <- function() {
  dd <- readRDS(sprintf("%sFE/blm_adjusted_naics2_cz_broadmarket.rds",input_dir))$cluster_sizes
  dd[, raw_wage_mean := weighted.mean(raw_wage, size), cluster]
  dd <- dd[order(raw_wage_mean)]
  dd[, cluster_ordered := .GRP, list(raw_wage_mean)]
  cluster_mapping <- unique(dd[, .(cluster, cluster_ordered)])
  setnames(dd, c("size", "Xgroup"), c("N", "WEq"))
  dd[, total := sum(N), list(cluster_ordered)]
  dd[, share := N / total * 100]
  dd[, WEq := factor(WEq, levels = 10:1)]
  dd <- dd[order(cluster_ordered, WEq)]

  gg <- ggplot(aes(x = factor(cluster_ordered), y = share, fill = WEq), data = dd) +
    geom_col() +
    theme_bw(base_size = 18) +
    scale_fill_grey(start = 0.0, end = .8) +
    theme(legend.position = "bottom") +
    labs(x = "Firm Type (ordered by mean log earnings)", y = "Share of Workers (%)", fill = "Worker Effect Quintile") +
    guides(fill = guide_legend(reverse = T))
  ggsave(gg, file = sprintf("%sfigure3a.pdf",output_dir), width = 8, height = 5)

  dd <- setDT(read.csv(file=sprintf("%smodel/LMS_shrinksort_naics2_cz_broadmarket.csv",input_dir)))[type == "baseline"]
  dd <- merge(dd, cluster_mapping, by = "cluster")
  setnames(dd, c("size", "Xq"), c("N", "WEq"))
  dd[, total := sum(N), list(cluster_ordered)]
  dd[, share := N / total * 100]
  dd <- dd[order(cluster_ordered, WEq)]
  dd[, WEq := factor(WEq, levels = 10:1)]
  dd <- dd[order(cluster_ordered, WEq)]

  gg <- ggplot(aes(x = factor(cluster_ordered), y = share, fill = WEq), data = dd) +
    geom_col() +
    theme_bw(base_size = 18) +
    scale_fill_grey(start = 0.0, end = .8) +
    theme(legend.position = "bottom") +
    labs(x = "Firm Type (ordered by mean log earnings)", y = "Share of Workers (%)", fill = "Worker Quality Quintile") +
    guides(fill = guide_legend(reverse = T))
  ggsave(gg, file = sprintf("%sfigure3b.pdf",output_dir), width = 8, height = 5)

  dd <- setDT(read.csv(file=sprintf("%smodel/LMS_shrinksort_naics2_cz_broadmarket.csv",input_dir)))[type == "theta"]
  dd <- merge(dd, cluster_mapping, by = "cluster")
  setnames(dd, c("size", "Xq"), c("N", "WEq"))
  dd[, total := sum(N), list(cluster_ordered)]
  dd[, share := N / total * 100]
  dd <- dd[order(cluster_ordered, WEq)]
  dd[, WEq := factor(WEq, levels = 10:1)]
  dd <- dd[order(cluster_ordered, WEq)]

  gg <- ggplot(aes(x = factor(cluster_ordered), y = share, fill = WEq), data = dd) +
    geom_col() +
    theme_bw(base_size = 18) +
    scale_fill_grey(start = 0.0, end = .8) +
    theme(legend.position = "bottom") +
    labs(x = "Firm Type (ordered by mean log earnings)", y = "Share of Workers (%)", fill = "Worker Quality Quintile") +
    guides(fill = guide_legend(reverse = T))
  ggsave(gg, file = sprintf("%sfigure3d.pdf",output_dir), width = 8, height = 5)

  dd <- setDT(read.csv(file=sprintf("%smodel/LMS_shrinksort_naics2_cz_broadmarket.csv",input_dir)))[type == "G"]
  dd <- merge(dd, cluster_mapping, by = "cluster")
  setnames(dd, c("size", "Xq"), c("N", "WEq"))
  dd[, total := sum(N), list(cluster_ordered)]
  dd[, share := N / total * 100]
  dd <- dd[order(cluster_ordered, WEq)]
  dd[, WEq := factor(WEq, levels = 10:1)]
  dd <- dd[order(cluster_ordered, WEq)]

  gg <- ggplot(aes(x = factor(cluster_ordered), y = share, fill = WEq), data = dd) +
    geom_col() +
    theme_bw(base_size = 18) +
    scale_fill_grey(start = 0.0, end = .8) +
    theme(legend.position = "bottom") +
    labs(x = "Firm Type (ordered by  mean log earnings)", y = "Share of Workers (%)", fill = "Worker Quality Quintile") +
    guides(fill = guide_legend(reverse = T))
  ggsave(gg, file = sprintf("%sfigure3c.pdf",output_dir), width = 8, height = 5)
}


figure.A6 <- function() {
  dd <- setDT(read.csv(file=sprintf("%smodel/LMS_compdiffs_naics2_cz_broadmarket.csv",input_dir)))
  dd[, x_decile := as.integer(x_decile)]
  dd <- melt(dd, id.vars = "x_decile")
  setDT(dd)
  dd[variable == "market", variable := "Overall"]
  dd[variable == "within", variable := "Within market"]
  dd <- dd[variable %in% c("Overall", "Within market")]
  overall_mean <- dd[variable == "Overall", mean(value)]
  within_mean <- dd[variable == "Within market", mean(value)]
  
  max_check <- dd[,max(value)]

  gg <- ggplot(aes(x = x_decile, y = value, linetype = variable), data = dd) +
    geom_line() +
    geom_point() +
    theme_bw(base_size = 16) +
    geom_hline(yintercept = overall_mean, linetype = "solid") +
    geom_hline(yintercept = within_mean, linetype = "dashed") +
    scale_x_continuous(breaks = pretty_breaks()) +
    labs(x = "Decile of Worker Effects", y = "Mean Compensating Differential", linetype = " ")
  if(max_check < 0.35){
    gg <- gg + ylim(0, .35)
  }
  ggsave(gg, file = sprintf("%sfigureA6.pdf",output_dir), width = 8, height = 5)
}

table.5 <- function(){
  dd <- setDT(read.csv(file=sprintf("%smodel/LMS_welfare_naics2_cz_broadmarket.csv",input_dir)))
  dd[, welfare_total := welfare_total/beta_model]
  dd[, y_pw := log(y_pw)]
  dd[, psi_x_cor := psi_x_cov/sqrt(psi_var*x_var)]
  dd <- dd[,.(y_pw,welfare_total,psi_x_cor,wedge,ptf,ptm)]
  dd2 <- rbind(dd,dd[2,] - dd[1,])

  tt <- with(dd2,
             TR(c(""," ","(1)","(2)","Difference")) +
               TR(c(""," ","Monopsonistic","No Labor","between")) +
               TR(c(" "," ","Labor Market","or Tax Wedges","(1) and (2)")) +
               tt_rule_mid() +
               TR("Log of Expected Output") %:% TR("$\\log \\mathbb{E}[Y_{jt}]$") %:% TR(y_pw,dec=2)  +
               TR("Total Welfare (log dollars)") %:% TR(" ") %:% TR(welfare_total,dec=2)  +
               TR("Sorting Correlation")  %:% TR("$Cor(\\psi_{jt},x_i)$") %:% TR(psi_x_cor,dec=2)  +
               TR("Labor Wedges") %:% TR("$1+\\frac{\\rho_r}{\\beta \\lambda}$") %:% TR(wedge,dec=2)  +
               TR("Worker Rents (as share of earnings):") +
               TR("$\\quad$ Firm-level") %:% TR("$\\frac{\\rho_r}{\\rho_r+\\beta\\lambda}$") %:% TR(ptf*100,dec=1,percentage=T)  +
               TR("$\\quad$ Market-level") %:% TR("$\\frac{1}{1+\\beta\\lambda}$") %:% TR(ptm*100,dec=1,percentage=T)
  )

  TS(tt, file = "table5", header = c("l","l","c","c","c"), output_path = output_dir)

}



table.1 <- function(){

  dd <- setDT(read.csv(sprintf("%smodel/LMS_model_descriptives_naics2_cz_broadmarket.csv",input_dir)))
  lambda <- dd$lambda_val
  dd2 <- setDT(read.csv(sprintf("%sparams/LMS_params_naics2_cz_broadmarket.csv",input_dir)))[model == "1inner"]
  rho <- dd2[, weighted.mean(rho,workeryears)]
  alpha <- dd2[, weighted.mean(alpha,workeryears)]
  broad_markets <- dd2[, length(unique(broad_market))]
  beta <- dd2[, weighted.mean(beta,workeryears)]/lambda


  gammaM_label <- "$\\frac{\\mathbb{E}\\left[\\Delta \\bar{y}_{rt}\\left(\\bar{w}_{rt+e}-\\bar{w}_{rt-e'}\\right)|S_{i}{=}1\\right]}{\\mathbb{E}\\left[\\Delta \\bar{y}_{rt}\\left(\\bar{y}_{rt+e}-\\bar{y}_{rt-e'}\\right)|S_{i}{=}1\\right]}$"
  gamma_label <- "$\\frac{\\mathbb{E}\\left[\\Delta \\tilde{y}_{jt}\\left(\\tilde{w}_{it+e}-\\tilde{w}_{it-e'}\\right)|S_{i}{=}1,r(j)=r\\right]}{\\mathbb{E}\\left[\\Delta \\tilde{y}_{jt}\\left(\\tilde{y}_{jt+e}-\\tilde{y}_{jt-e'}\\right)|S_{i}{=}1,r(j)=r\\right]}$"

  theta_label <- "$\\mathbb{E}[w_{it+1}|j \\rightarrow j'] - \\mathbb{E}[w_{it}|j' \\rightarrow j]$"
  theta_label2 <- "$\\mathbb{E}[w_{it}|j' \\rightarrow j] - \\mathbb{E}[w_{it+1}|j \\rightarrow j']$"

  akm_label1 <- "$\\mathbb{E}[w_{it}{-}\\frac{1}{1+\\lambda\\beta}\\bar{y}_{r,t}$"
  akm_label2 <- "$\\qquad {-}\\frac{\\rho_{r}}{\\rho_{r}+\\lambda\\beta}\\tilde{y}_{j,t} | r(j)=r]$"

  tt <- with(dd,
             TR("Name") %:% TR(" ") %:% TR("Unique Parameters") %:% TR("Mean Estimate") %:% TR("Moments of the Data",2) +
               tt_rule_mid_partial(list(c(1,4),c(5,6))) +
               TR("\\textbf{Panel A. Rents and Scale}") +
               TR("Idiosyncratic Taste Parameter") %:% TR("$\\beta$") %:% TR(1,dec=0) %:% TR(beta,dec=2) %:%
               TR("Market Passthrough") %:% TR(gammaM_label) +
               TR("Taste Correlation Parameter") %:% TR("$\\rho_r$") %:% TR(broad_markets,dec=0) %:% TR(rho,dec=2) %:%
               TR("Net Passthrough") %:% TR(gamma_label) +
               TR("Returns to Scale Parameter") %:% TR("$\\alpha_r$") %:% TR(broad_markets,dec=0) %:% TR(alpha,dec=2) %:%
               TR("Labor Share") %:% TR("$\\mathbb{E}[b_{j(i,t)} - y_{j(i,t)}|r(j)=r]$") + tt_spacer_row(5) +
               tt_rule_mid() +
               TR("Name") %:% TR(" ") %:% TR("Unique Parameters") %:% TR("Var. Estimate") %:% TR("Moments of the Data",2) +
               tt_rule_mid_partial(list(c(1,4),c(5,6))) +

               TR("\\textbf{Panel B. Firm and Worker Heterogeneity}",3,center="l") +

               TR("Time-varying Firm Premium") %:% TR("$\\psi_{jt}$") %:% TR(psi_unique,dec=0) %:% TR(psi_var,dec=2)  %:%
               TR("Structural Wage Equation") %:% TR(akm_label1) +
               TR("Firm-specific Technology Parameter") %:% TR("$\\theta_j$") %:% TR(theta_unique,dec=0) %:% TR(theta_var,dec=2) %:%
               TR(" ") %:% TR(akm_label2) +
               TR("Worker Quality") %:% TR("$x_i$") %:% TR(x_unique,dec=0) %:% TR(x_var,dec=2)  %:%
               TR("Wage Changes around Moves") %:% TR(theta_label) +

               TR("Amenity Efficiency Units at Neutral TFP") %:% TR("$h_j$") %:% TR(h_unique,dec=0) %:% TR(0.13795,dec=2)  %:%
               TR(" ") %:% TR(theta_label2) +
               tt_spacer_row(10) +

               TR("Time-varying Firm-specific TFP") %:% TR("$\\tilde{a}_{jt}$") %:% TR(TFP_unique,dec=0) %:% TR(h_var,dec=2) %:%
               TR("Total Labor Input \\&") %:% TR("$\\ell_{jt}=\\log \\sum X_i^{\\theta_j}$ and $\\psi_{jt}$") +
               TR("Time-varying Market-specific TFP") %:% TR("$\\overline{a}_{rt}$") %:% TR(marketTFP_unique,dec=0) %:% TR(marketTFP_var,dec=2) %:%
               TR("Time-varying Firm Premium") %:% TR("") +
               tt_spacer_row(5) +
               tt_rule_mid() +

               TR("Name") %:% TR(" ") %:% TR("Unique Parameters") %:% TR("Var. Estimate") %:% TR("Moments of the Data",2) +
               tt_rule_mid_partial(list(c(1,4),c(5,6))) +
               TR("\\textbf{Panel C. Model Counterfactuals}") +
               TR("Preferences for amenities for:") %:% TR("$g_{j}(X)$") %:% TR(g_unique,dec=0) %:% TR(g_var,dec=2) %:%
               TR("Firm Size \\&") %:% TR("$\\Pr[j]$")  +
               TR("Firm $j$ for workers of quality $X$") %:% TR(" ") %:% TR(NA,dec=0) %:% TR(NA,dec=2) %:%
               TR("Firm Composition \\&") %:% TR("$\\Pr[x | k(j)=k]$") +
               TR("Market $r$ for workers of quality $X$") %:% TR(" ") %:% TR(" ") %:% TR(" ") %:%
               TR("Market Composition") %:% TR("$\\Pr[x | r(j)=r]$")
  )

  TS(tt, file='table1', header=c("l","l","c","c","l","l"), output_path = output_dir)

}


