

figure.2 <- function() {

  dd <- readRDS(sprintf("%sFE/blm_adjusted_naics2_cz_broadmarket.rds",input_dir))$cluster_sizes
  dd[, raw_wage_mean := weighted.mean(raw_wage, size), cluster]
  dd <- dd[order(raw_wage_mean)]
  dd[, cluster_ordered := .GRP, list(raw_wage_mean)]
  cluster_mapping <- unique(dd[, list(FirmCluster=cluster, cluster_ordered)])

  dd2 <- unique(readRDS(sprintf("%sFE/blm_adjusted_naics2_cz_broadmarket.rds",input_dir))$interaction[variable == "y1"][, list(WorkerEffectQ, FirmCluster, MeanWage = value)])
  dd2[, WorkerEffect := as.integer(WorkerEffectQ * 100)]
  dd2 <- dd2[, .(FirmCluster, WorkerEffect, MeanWage)]
  dd2 <- merge(dd2, cluster_mapping, by='FirmCluster')

  max_check <- dd2[, max(MeanWage)]
  
  gg2 <- ggplot(aes(x = cluster_ordered, y = MeanWage, linetype = factor(WorkerEffect)), data = dd2) +
    geom_line() +
    geom_point() +
    theme_bw(base_size = 16) +
    ylab("Predicted Log Earnings") +
    xlab("Firm Type (ordered by mean log earnings)") +
    labs(linetype = "Worker Quantile") +
    scale_x_continuous(breaks = 1:10) +
    theme(panel.grid.minor.x = element_blank())
  if(max_check < 1.2){
    gg2 <- gg2 + scale_y_continuous(breaks = pretty_breaks(), limits = c(-1.2, 1.2))
  }
  ggsave(gg2, file = sprintf("%sfigure2.pdf",output_dir), width = 8, height = 5)

}



table.A6 <- function() {
  dd <- rbindlist(list(
    readRDS(sprintf("%sFE/blm_unadjusted_naics2_cz_broadmarket.rds",input_dir))$decomp[, adjusted := FALSE],
    readRDS(sprintf("%sFE/blm_adjusted_naics2_cz_broadmarket.rds",input_dir))$decomp[, adjusted := TRUE]
  ), use.names = T, fill = T)

  tab <- TR("Share explained by:")

  tab <- tab + TR("$\\quad$ i) Worker Quality") %:% TR("$Var(\\tilde{x}_i)$") %:%
    TR(dd[adjusted == TRUE]$x_tilde_var / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(dd[adjusted == TRUE]$WE_var / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(dd[adjusted == FALSE]$x_tilde_var / dd[adjusted == FALSE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(dd[adjusted == FALSE]$WE_var / dd[adjusted == FALSE]$total_var * 100, dec = 1, percentage = T)

  tab <- tab + TR("$\\quad$ ii) Firm Effects") %:% TR("$Var(\\tilde{\\psi}_{j(i)})$") %:%
    TR(dd[adjusted == TRUE]$psi_tilde_var / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(dd[adjusted == TRUE]$FEbar_var / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(dd[adjusted == FALSE]$psi_tilde_var / dd[adjusted == FALSE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(dd[adjusted == FALSE]$FE_var / dd[adjusted == FALSE]$total_var * 100, dec = 1, percentage = T)

  tab <- tab + TR("$\\quad$ iii) Sorting") %:% TR("$2Cov(\\tilde{x}_i,\\tilde{\\psi}_{j(i)})$") %:%
    TR(2 * dd[adjusted == TRUE]$x_with_psi_tilde_cov / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(2 * dd[adjusted == TRUE]$FEbar_WE_cov / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(2 * dd[adjusted == FALSE]$x_with_psi_tilde_cov / dd[adjusted == FALSE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(2 * dd[adjusted == FALSE]$FE_WE_cov / dd[adjusted == FALSE]$total_var * 100, dec = 1, percentage = T)

  tab <- tab + TR("$\\quad$ iv) Interactions") %:% TR("$Var(\\varrho_{ij})   + 2Cov(x_i + \\psi_{j(i)}, \\varrho_{ij})$") %:%
    TR((dd[adjusted == TRUE][, tau_var + 2 * x_plus_psi_tilde_with_tau_cov]) / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(" ") %:%
    TR((dd[adjusted == FALSE][, tau_var + 2 * x_plus_psi_tilde_with_tau_cov]) / dd[adjusted == FALSE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(" ")


  tab <- tab + TR("$\\quad$ v) Time-varying Effects") %:% TR("$Var(\\psi^a_{j(i),t}) + 2Cov(x_i, \\psi^a_{j(i),t})$") %:%
    TR(dd[adjusted == TRUE]$interactions / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR((dd[adjusted == TRUE]$FEdev_var + 2 * dd[adjusted == TRUE]$FEdev_WE_cov) / dd[adjusted == TRUE]$total_var * 100, dec = 1, percentage = T) %:%
    TR(" ") %:%
    TR(" ")

  tab <- tab + tt_spacer_row(5)

  tab <- tab + TR("Sorting Correlation:") %:% TR("$Cor(x_i,\\psi_{j(i)})$") %:%
    TR(dd[adjusted == TRUE][, x_with_psi_tilde_cov / sqrt(x_tilde_var * psi_tilde_var)], dec = 2) %:%
    TR(dd[adjusted == TRUE]$FEbar_WE_cor, dec = 2) %:%
    TR(dd[adjusted == FALSE][, x_with_psi_tilde_cov / sqrt(psi_tilde_var * x_tilde_var)], dec = 2) %:%
    TR(dd[adjusted == FALSE]$FE_WE_cor, dec = 2)

  tab <- tab + TR("Variance Explained:") %:% TR("$R^2$") %:%
    TR(dd[adjusted == TRUE]$Rsquarednl, dec = 2) %:%
    TR(dd[adjusted == TRUE]$Rsquared, dec = 2) %:%
    TR(dd[adjusted == FALSE]$Rsquarednl, dec = 2) %:%
    TR(dd[adjusted == FALSE]$Rsquared, dec = 2)

  tab <- tab + tt_spacer_row(5) +
    TR("Specification:") +
    TR("$\\quad$ Firm-Worker Interactions") %:% TR("") %:% TR(c("\\checkmark", "\\ding{55}", "\\checkmark", "\\ding{55}")) +
    TR("$\\quad$ Time-varying Firm Effects") %:% TR("") %:% TR(c("\\checkmark", "\\checkmark", "\\ding{55}", "\\ding{55}"))


  tab <- TR(c("", "\\textbf{Model Specifications}"), c(2, 4)) +
    midrulep(list(c(3, 6))) + TR(c("", "\\textbf{Main}", "\\textbf{Alternatives}"), c(2, 1, 3)) +
    midrulep(list(c(3, 3), c(4, 6))) + TR(c("", "", "", "$\\theta_j=\\bar{\\theta}$", "$\\gamma_r=\\Upsilon=0$", "$\\theta_j=\\bar{\\theta}$ and")) +
    TR(c("", "", "", " ", " ", "$\\gamma_r=\\Upsilon=0$")) + tt_rule_mid() + tab

  TS(tab, file = "tableA6", header = c("l", "l", "r", "r", "r", "r"), output_path = output_dir)

}


