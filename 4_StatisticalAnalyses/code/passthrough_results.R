
read_passthrough_params <- function(industry = "naics2", location = "cz", broadmarkets = TRUE, DiD = FALSE, moreoutcomes = FALSE) {
  suffix <- paste0(industry, "_", location)
  if (broadmarkets) {
    suffix <- paste0(suffix, "_broadmarket")
  }
  if (moreoutcomes) {
    suffix <- paste0(suffix, "_moreoutcomes")
  }
  paramtype <- "params"
  if (DiD) {
    paramtype <- "DiD"
  }
  ff <- sprintf("%sparams/LMS_%s_%s.csv", input_dir, paramtype, suffix)
  dd <- setDT(read.csv(ff))
  dd[, industry_defn := industry]
  dd[, location_defn := location]
  return(dd)
}

figure.1 <- function() {
  helper.wages_DiD_plot <- function(dd, yaxis2_scale, market = FALSE, net = TRUE, limits = c(-.07, .36), ylab = "Log Earnings Difference (Dashed)", remove_unc = FALSE) {
    gg <- ggplot(dd, aes(x = event_time))
    if (!remove_unc) {
      gg <- gg + geom_line(aes(y = va), linetype = "solid", color = "black")
    }
    if (net) {
      gg <- gg + geom_line(aes(y = vanet), linetype = "solid", color = "red")
    }
    if (market) {
      gg <- gg + geom_line(aes(y = vamean), linetype = "solid", color = "blue")
    }
    if (!remove_unc) {
      gg <- gg + geom_line(aes(y = wages * yaxis2_scale), linetype = "dashed", color = "black")
    }
    if (net) {
      gg <- gg + geom_line(aes(y = wagesnet * yaxis2_scale), linetype = "dashed", color = "red")
    }
    if (market) {
      gg <- gg + geom_line(aes(y = wagesmean * yaxis2_scale), linetype = "dashed", color = "blue")
    }
    gg <- gg + theme_bw(base_size = 15) +
      scale_x_continuous("Years from Event", minor_breaks = NULL) +
      geom_vline(xintercept = 0, linetype = 2, color = "black") +
      scale_y_continuous("Log VA Difference (Solid)", limits = limits, breaks = -1:4 / 10, minor_breaks = NULL, sec.axis = sec_axis(~ . * (1 / yaxis2_scale), name = ylab))
    gg <- gg + geom_rect(aes(xmin = -2, xmax = 1, ymin = -Inf, ymax = Inf), alpha = .025)

    return(gg)
  }

  dd <- read_passthrough_params(industry = "naics2", location = "cz", DiD = TRUE, broadmarkets = FALSE)
  gg <- helper.wages_DiD_plot(dd, yaxis2_scale = 2.5, market = TRUE, remove_unc = TRUE)
  ggsave(gg, file = sprintf("%sfigure1.pdf", output_dir), height = 5, width = 8)
}


figure.A3ab <- function() {
  lambda <- 0.9154762
  dd <- read_passthrough_params(industry = "naics2", location = "cz", broadmarkets = TRUE)[model == "1inner"]
  dd[, wedge := 1 + rho / beta]
  dd[, LSelasticity_firm := beta / (rho * lambda)]
  dd <- dd[, list(r = broad_market, wedge, LSelasticity_firm, N = workeryears)]
  dd[, sector := "Goods"]
  dd[str_detect(r, "Services"), sector := "Services"]
  dd[, r := str_replace(r, "_Goods", "")]
  dd[, r := str_replace(r, "_Services", "")]
  dd[, r := factor(r, levels = rev(c("Northeast", "South", "Midwest", "West")))]
  dd[, sector := factor(sector, levels = c("Goods", "Services"))]
  dd <- dd[order(sector, r)]

  avg <- dd[, weighted.mean(wedge, N)]
  shifter <- 1
  gg <- ggplot(data = dd, aes(x = sector, y = wedge - shifter, fill = r)) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(x = "Broad Market", y = "Labor Wedge", fill = " ") +
    theme_bw(base_size = 16) +
    scale_fill_grey(start = 0.2, end = 0.8) +
    scale_y_continuous(breaks = c(0, .1, avg - shifter, .2, .3), labels = c(1, 1.1, "Mean", 1.2, 1.3), limit = c(0, .3)) +
    geom_hline(yintercept = avg - shifter)
  ggsave(gg, file = sprintf("%sfigureA3b.pdf", output_dir), height = 5, width = 8)

  avg <- dd[, weighted.mean(LSelasticity_firm, N)]
  shifter <- 0
  gg <- ggplot(data = dd, aes(x = sector, y = LSelasticity_firm - shifter, fill = r)) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(x = "Broad Market", y = "Firm-level Labor Supply Elasticity", fill = " ") +
    theme_bw(base_size = 16) +
    scale_fill_grey(start = 0.2, end = 0.8) +
    scale_y_continuous(breaks = c(0, 2, 4, 6, avg, 8, 10) - shifter, labels = c(0, 2, 4, 6, "Mean", 8, 10), limit = c(0, 11 - shifter)) +
    geom_hline(yintercept = avg - shifter)
  ggsave(gg, file = sprintf("%sfigureA3a.pdf", output_dir), height = 5, width = 8)
}

table.A5 <- function() {

  lambda <- 0.9154762

  dd <- rbind(
    read_passthrough_params(industry = "naics2", location = "cz", broadmarkets = TRUE)[model == "1inner"],
    read_passthrough_params(industry = "naics2", location = "state", broadmarkets = TRUE)[model == "1inner"],
    read_passthrough_params(industry = "naics2", location = "county", broadmarkets = TRUE)[model == "1inner"],
    read_passthrough_params(industry = "naics3", location = "cz", broadmarkets = TRUE)[model == "1inner"],
    read_passthrough_params(industry = "supersector", location = "cz", broadmarkets = TRUE)[model == "1inner"]
  )
  dd2 <- read_passthrough_params(industry = "naics2", location = "cz", broadmarkets = FALSE)[model == "1inner"]

  rr <- dd[, list(
    workers_per_market = weighted.mean(workers_per_market, markets) / 1e3, firms_per_market = weighted.mean(firms_per_market, markets) / 1e3,
    market_pt = unique(market_pt), net_pt = weighted.mean(net_pt, workeryears), beta = unique(beta / lambda), bad_rho = sum(rho < 0 | rho > 1),
    markets = sum(markets), rho = weighted.mean(rho, workeryears), alpha = weighted.mean(alpha, workeryears), min_rho = min(rho),
    Rw_firm_share = weighted.mean(Rw_firm_share, workeryears),
    Rw_market_share = weighted.mean(Rw_market_share, workeryears)
  ), list(industry_defn, location_defn)][, corr := 1 - rho^2]

  rr2 <- dd2[, list(
    workers_per_market = weighted.mean(workers_per_market, markets) / 1e3, firms_per_market = weighted.mean(firms_per_market, markets) / 1e3,
    market_pt = unique(market_pt), net_pt = weighted.mean(net_pt, workeryears), beta = unique(beta / lambda), bad_rho = sum(rho < 0 | rho > 1),
    markets = sum(markets), rho = weighted.mean(rho, workeryears), alpha = weighted.mean(alpha, workeryears), min_rho = min(rho),
    Rw_firm_share = weighted.mean(Rw_firm_share, workeryears),
    Rw_market_share = weighted.mean(Rw_market_share, workeryears)
  ), list(industry_defn, location_defn)][, corr := 1 - rho^2]


  tt <- TR("") %:% TR("Market Count ", 2) %:% TR("", 2) %:% TR("Average of the", 3) %:% TR("Workers' Share of Rents", 2)
  tt <- tt + TR("") %:% TR("(in 1,000)", 2) %:% TR("Passthrough Rate", 2) %:% TR("Model Parameters", 3) %:% TR("Firm-level") %:% TR("Market-level")
  tt <- tt + midrulep(list(c(2, 3), c(4, 5), c(6, 8), c(9, 10)))
  tt <- tt + TR("") %:% TR("Workers") %:% TR("Firms") %:% TR("Market") %:% TR("Firm") %:% TR("$\\beta$") %:% TR("$1-\\rho^2_r$") %:% TR("$1-\\alpha_r$") %:% TR("$\\frac{R^w}{R^w + R^f}$") %:% TR("$\\frac{R^{wm}}{R^{wm} + R^{fm}}$") + midrule()

  tt <- tt + midrule()
  tt <- tt + TR("Baseline (NAICS 2-digit, commuting zone)") %:% TR(rr[industry_defn == "naics2" & location_defn == "cz", c(workers_per_market, firms_per_market, market_pt, net_pt, beta, corr, 1 - alpha, Rw_firm_share, Rw_market_share)], dec = 2)
  tt <- tt + vspace(5)
  tt <- tt + TR("Shutdown broad market heterogeneity") %:% TR(rr2[industry_defn == "naics2" & location_defn == "cz", c(workers_per_market, firms_per_market, market_pt, net_pt, beta, corr, 1 - alpha, Rw_firm_share, Rw_market_share)], dec = 2)
  tt <- tt + TR("$\\qquad (\\rho_r=\\overline{\\rho},\\alpha_r=\\overline{\\alpha})$") + vspace(5)
  tt <- tt + TR("Alternative detailed markets:")
  tt <- tt + TR("$\\quad$ Finer geography (county)") %:% TR(rr[industry_defn == "naics2" & location_defn == "county", c(workers_per_market, firms_per_market, market_pt, net_pt, beta, corr, 1 - alpha, Rw_firm_share, Rw_market_share)], dec = 2)
  tt <- tt + TR("$\\quad$ Finer industry (NAICS 3-digit)") %:% TR(rr[industry_defn == "naics3" & location_defn == "cz", c(workers_per_market, firms_per_market, market_pt, net_pt, beta, corr, 1 - alpha, Rw_firm_share, Rw_market_share)], dec = 2)
  tt <- tt + TR("$\\quad$ Coarser geography (state)") %:% TR(rr[industry_defn == "naics2" & location_defn == "state", c(workers_per_market, firms_per_market, market_pt, net_pt, beta, corr, 1 - alpha, Rw_firm_share, Rw_market_share)], dec = 2)
  tt <- tt + TR("$\\quad$ Coarser industry (NAICS supersector)") %:% TR(rr[industry_defn == "supersector" & location_defn == "cz", c(workers_per_market, firms_per_market, market_pt, net_pt, beta, corr, 1 - alpha, Rw_firm_share, Rw_market_share)], dec = 2)

  TS(tt, file = "tableA5", header = c("l", rep("r", 15)), output_path = output_dir)
}




figure.A1ab <- function(){
  
  dd_all <- setDT(read.csv(sprintf("%sparams/LMS_params_naics2_cz_broadmarket.csv",input_dir)))[model == "1inner"][,list(model="All",unc=weighted.mean(unc_pt,workeryears),net=weighted.mean(net_pt,workeryears),market=weighted.mean(market_pt,workeryears))]
  dd <- setDT(read.csv(sprintf("%sparams/LMS_params_naics2_cz_subsamples.csv",input_dir)))[model == "1inner"][,list(model=r,unc=unc_pt,net=net_pt,market=market_pt)]
  dd <- rbindlist(list(dd_all,dd),use.names=T)
  max_check <- dd[,max(unc)]
  
  dd[, market := market - unc] # this is because geom_col will sum them later
  dd[, unc := unc - net] # this is because geom_col will sum them later
  dd2 <- melt(dd,id.vars="model")
  setDT(dd2)
  
  dd2[variable == "net",variable := "Net of Market"]
  dd2[variable == "unc",variable := "Firm Only"]
  dd2[variable == "market",variable := "Market"]
  dd2[, variable := factor(variable,levels=c("Market","Firm Only","Net of Market"))]
  setnames(dd2,"variable","Passthrough")
  
  dd2[, `:=`(firm_restrictions=FALSE,worker_restrictions=FALSE)]
  dd2[model %in% c("All","ebitd","operprofits","VAlessthan10B","vanet","noMNE","smallFirm"), firm_restrictions := T]
  dd2[model %in% c("All","older","men","highFirmWage","highWorkerWage","sometenure"), worker_restrictions := T]
  
  dd2[model=="sometenure", model := "Some Tenure"]
  dd2[model=="older", model := "Age > 45"]
  dd2[model=="men", model := "Men"]
  dd2[model=="highWorkerWage", model := "High Wage"]
  
  dd2[model=="noMNE", model := "No MNE"]
  dd2[model=="VAlessthan10B", model := "VA < 10B"]
  dd2[model=="operprofits", model := "Oper. Profits"]
  dd2[model=="vanet", model := "VA - Deprec."]
  dd2[model=="ebitd", model := "EBITD"]
  dd2[model=="smallFirm", model := "Size < 100"]
  dd2[model=="highFirmWage", model := "High Wage Firm"]
  
  levels=c("All","Age > 45","Younger than 45","Some Tenure","No Tenure","Men","Women","Oper. Profits","EBITD","VA - Deprec.","VA < 10B","No MNE","Size < 100","High Wage","High Wage Firm")
  dd2[, model := factor(model, levels=levels)]
  
  dd <- dd2[Passthrough != "Market"]
  
  
  p <- ggplot(data = dd[worker_restrictions == T & !is.na(model)], aes(x = model, y = value, fill = Passthrough))
  p <- p + geom_col(width=.4) + theme_bw(base_size = 14) + scale_fill_grey(start=.7, end=.3) + theme(legend.position="bottom", axis.text.x = element_text(angle = 0)) + ylab("") + xlab("")
  if(max_check < 0.25){
    p <- p + scale_y_continuous(breaks= pretty_breaks(), limits=c(0,.25))
  }
  ggsave(p,file=sprintf("%sfigureA1a.pdf",output_dir),height=5,width=7)
  
  p <- ggplot(data = dd[firm_restrictions == T & !is.na(model)], aes(x = model, y = value, fill = Passthrough))
  p <- p + geom_col(width=.4) + theme_bw(base_size = 14) + scale_fill_grey(start=.7, end=.3) + theme(legend.position="bottom", axis.text.x = element_text(angle = 0)) + ylab("") + xlab("")
  if(max_check < 0.25){
    p <- p + scale_y_continuous(breaks= pretty_breaks(), limits=c(0,.25))
  }
  ggsave(p,file=sprintf("%sfigureA1b.pdf",output_dir),height=5,width=7)
  
}


table.2_A4 <- function(){

  dd <- setDT(read.csv(file=sprintf("%sparams/LMS_params_naics2_cz_broadmarket.csv",input_dir)))[model=='1inner']
  dd[, broad_market := NULL][, model := NULL][, ma := NULL]
  dd <- dd[,lapply(.SD,weighted.mean,workeryears)]
  bb <- setDT(read.csv(file=sprintf("%sparams/LMS_params_naics2_cz_broadmarket_bootstraps.csv",input_dir)))[broad_market=='All']
  LMS_firmlevel <- data.table(passthrough=dd$net_pt, passthrough_se=bb$net_pt, LSelasticity=dd[,beta/rho], LSelasticity_se=bb$beta_over_rho)
  LMS_marketlevel <- data.table(passthrough=dd$market_pt, passthrough_se=bb$market_pt, LSelasticity=dd[,beta], LSelasticity_se=bb$beta)

  bartik <- setDT(read.csv(file=sprintf("%sparams/LMS_shiftshare_naics2_cz_broadmarket.csv",input_dir)))[initialweights==TRUE]
  bartik <- bartik[method=='BHJ'][measured=='logdiffsum']
  bartikLS <- bartik[parameter=='LSelasticity_plus1']
  bartikpt <- bartik[parameter=='passthrough']
  bartikLS[, secondstage_coef := secondstage_coef - 1] # to convert from (1+beta) to (beta)

  auctions_base <- setDT(read.csv(file=sprintf("%sparams/results_forLMS_LSelasticity.csv",input_dir)))
  auctions <- auctions_base[outcome_variable=='passthrough'][,list(passthrough=estimate,passthrough_se=cluster_se)]
  auctions <- cbind(auctions,auctions_base[outcome_variable=='passthroughLS'][,list(LSelasticity=estimate,LSelasticity_se=cluster_se)])
  auctions <- cbind(auctions,auctions_base[outcome_variable=='dEBITD'][,list(firststage=estimate,firststage_se=cluster_se)])
  auctions <- cbind(auctions,auctions_base[outcome_variable=='dW'][,list(reducedform=estimate,reducedform_se=cluster_se)])

  tab <-
    TR("\\textbf{Panel A.}") %:% TR("\\textbf{Firm-level Estimation}",2) +
    tt_rule_mid_partial(list(c(2,3))) +
    TR(c('\\textbf{Instrumental Variable}','\\textbf{Passthrough ($\\mathbb{E}[\\gamma_r]$)}','\\textbf{Implied Elasticity}')) +
    tt_rule_mid() +
    vspace(-8) +
    TR("\\textbf{Internal instrument: }") %:% TR(LMS_firmlevel[,c(passthrough, LSelasticity)],dec=c(2,2)) +
    vspace(3) +
    TR('Lagged firm-level value added shock under MA(1) errors') %:% TR(LMS_firmlevel[,c(passthrough_se, LSelasticity_se)],dec=c(2,2),se=T) +
    vspace(10) +
    TR("\\textbf{External instrument: }") %:% TR(auctions[,c(passthrough, LSelasticity)],dec=c(2,2)) +
    TR('Procurement auction shock at firm-level') %:% TR(auctions[,c(passthrough_se, LSelasticity_se)],dec=c(2,2),se=T)

  tab <- tab +
    vspace(6) +
    tt_rule_mid() +
    TR("\\textbf{Panel B.}") %:% TR("\\textbf{Market-level Estimation}",2) +
    tt_rule_mid_partial(list(c(2,3))) +
    TR(c('\\textbf{Instrumental Variable}','\\textbf{Passthrough ($\\Upsilon$)}','\\textbf{Implied Elasticity}')) +
    tt_rule_mid() +
    vspace(-8) +
    TR("\\textbf{Internal instrument: }") %:% TR(LMS_marketlevel[,c(passthrough, LSelasticity)],dec=c(2,2)) +
    vspace(3) +
    TR('Lagged market-level value added shock under MA(1) errors') %:% TR(LMS_marketlevel[,c(passthrough_se, LSelasticity_se)],dec=c(2,2),se=T) +
    vspace(10) +
    TR("\\textbf{External instrument:}") %:% TR(c(bartikpt$secondstage_coef,bartikLS$secondstage_coef),dec=c(2,2)) +
    vspace(3) +
    TR('Shift-share industry value added shock') %:% TR(c(bartikpt$secondstage_se,bartikLS$secondstage_se),dec=c(2,2),se=T) +
    vspace(2)

  TS(tab,file="table2",header = c(rep("l",1),rep("c",2)), output_path = output_dir)



  tab <- TR(c("Outcome Sample","First Stage","Reduced Form","Second Stage")) +
    TR(c("","(Std. Error)","(Std. Error)","(Std. Error)")) +
    midrulep(list(c(1,4))) +
    TR(c("\\textbf{Procurement auction shock at firm-level}"),c(4)) +
    midrulep(list(c(1,4))) +
    TR("8,677 unique auction bidders") %:% TR(auctions$firststage, dec=3) %:% TR(auctions$reducedform, dec=3) %:% TR(auctions$passthrough, dec=3) +
    TR("") %:% TR(auctions$firststage_se, dec=3, se=T) %:% TR(auctions$reducedform_se, dec=3, se=T) %:% TR(auctions$passthrough_se, dec=3, se=T) +
    midrulep(list(c(1,4))) +
    TR(c("\\textbf{Shift-share industry value added shock}"),c(4)) +
    midrulep(list(c(1,4))) +
    TR(sprintf("%s unique commuting zones",667)) %:% TR(bartikpt$firststage_coef, dec=3) %:% TR(bartikpt$reducedform_coef, dec=3) %:% TR(bartikpt$secondstage_coef, dec=3) +
    TR("")  %:%                                           TR(bartikpt$firststage_se, dec=3, se=T)  %:% TR(bartikpt$reducedform_se, dec=3, se=T) %:% TR(bartikpt$secondstage_se, dec=3, se=T)

  TS(tab,file="tableA4",header = c(rep("l",1),rep("c",3)), output_path = output_dir)

}



table.3 <- function(){

  dd <- setDT(read.csv(file=sprintf("%sparams/LMS_params_naics2_cz_broadmarket.csv",input_dir)))[model=='1inner']
  dd[, broad_market := NULL][, model := NULL][, ma := NULL]
  dd <- dd[,lapply(.SD,weighted.mean,workeryears)]
  dd[, Rw_firm_share := Rw_firm/(Rw_firm + Rf_firm)]
  dd[, Rw_market_share := Rw_market/(Rw_market + Rf_market)]
  dd[, Rf_firm_profitshare := Rf_firm/EPi_pc]
  dd[, Rf_market_profitshare:= Rf_market/EPi_pc]
  bb <- setDT(read.csv(file=sprintf("%sparams/LMS_params_naics2_cz_broadmarket_bootstraps.csv",input_dir)))
  bb <- bb[broad_market=='All']

  tab <-
    TR(" ") %:% TR("\\textbf{Rents and Rent Shares}",4) +
    tt_rule_mid_partial(list(c(2,5))) +
    TR(c("","\\textbf{Firm level}","\\textbf{Market level}"),c(1,2,2)) +
    tt_rule_mid_partial(list(c(2,3),c(4,5))) +
    TR("\\textbf{Workers' Rents:}") +
    TR("$\\quad$ Per-worker Dollars") %:% TR(dd$Rw_firm,dec=0) %:% TR(bb$Rw_firm,dec=0,se=T) %:% TR(dd$Rw_market,dec=0) %:% TR(bb$Rw_market,dec=0,se=T)  +
    TR("$\\quad$ Share of Earnings") %:% TR(dd$Rw_firm_wageshare*100,dec=0,percentage=T) %:% TR(bb$Rw_firm_wageshare*100,dec=0,percentage=T,se=T)  %:% TR(dd$Rw_market_wageshare*100,dec=0,percentage=T)  %:% TR(bb$Rw_market_wageshare*100,dec=0,percentage=T,se=T) +
    tt_spacer_row(5) +
    TR("\\textbf{Firms' Rents:}") +
    TR("$\\quad$ Per-worker Dollars") %:% TR(dd[,c(Rf_firm)],dec=0) %:% TR(bb[,c(Rf_firm)],dec=0,se=T) %:% TR(dd[,c(Rf_market)],dec=0)  %:% TR(bb[,c(Rf_market)],dec=0,se=T)  +
    TR("$\\quad$ Share of Profits") %:% TR(dd[,c(Rf_firm_profitshare)*100],dec=0,percentage=T) %:% TR(bb[,c(Rf_firm_profitshare)*100],dec=0,percentage=T,se=T) %:% TR(dd[,c(Rf_market_profitshare)*100],dec=0,percentage=T) %:% TR(bb[,c(Rf_market_profitshare)*100],dec=0,percentage=T,se=T) +
    tt_spacer_row(10) +
    TR("\\textbf{Workers' Share of Rents}") %:% TR(dd[,c(Rw_firm_share)*100],dec=0,percentage=T) %:% TR(bb[,c(Rw_firm_share)*100],dec=0,percentage=T,se=T) %:% TR(dd[,c(Rw_market_share)*100],dec=0,percentage=T) %:% TR(bb[,c(Rw_market_share)*100],dec=0,percentage=T,se=T) + tt_spacer_row(5)

  TS(tab, file="table3", header=c("l",rep("c",4)), output_path = output_dir)

}



table.A7 <- function(){

  # prepare point estimates
  lambda <- 0.9154762
  ddinput <- setDT(read.csv(file=sprintf("%sparams/LMS_params_naics2_cz_broadmarket.csv",input_dir)))[model=='1inner']
  ddinput[, beta_model_inv := (beta/lambda)^(-1)]
  ddinput[, rts := 1-alpha]
  ddinput[, corr := 1-rho^2]
  
  # prepare standard errors
  bb <- setDT(read.csv(file=sprintf("%sparams/LMS_params_naics2_cz_broadmarket_bootstraps.csv",input_dir)))
  bb <- bb[broad_market != 'All']
  
  # only keep the needed variables
  needed_vars <- c("broad_market","beta_model_inv","rho","rts","alpha","corr","Rw_firm","Rw_market","Rf_firm","Rf_market","Rw_firm_wageshare","Rw_market_wageshare","Rf_firm_profitshare","Rf_market_profitshare","Rw_firm_share","Rw_market_share")
  ddinput <- ddinput[,.SD,.SDcols=needed_vars]
  bb <- bb[,.SD,.SDcols=needed_vars]

  regions <- c("Midwest","Northeast","South","West")
  industries <- c("Goods","Services")
  for(loc in regions){
    ddinput[str_detect(broad_market,loc), location := loc]
    bb[str_detect(broad_market,loc), location := loc]
  }
  for(ind in industries){
    ddinput[str_detect(broad_market,ind), industry := ind]
    bb[str_detect(broad_market,ind), industry := ind]
  }

  ddinput <- ddinput[order(industry,location)]
  bb <- bb[order(industry,location)]

  
  panel_width <- ddinput[,length(unique(broad_market))]
  span_top <- c(panel_width/2,panel_width/2)
  partial_top <- list(c(2,panel_width/2+1),c(panel_width/2+2,panel_width+1))
  panel_partial <- list(c(2,panel_width+1))

  tab <- TR("") %:% TR(as.character(unique(ddinput$industry)),span_top)
  tab <- tab + tt_rule_mid_partial(partial_top) +
    TR("") %:% TR(as.character(ddinput$location)) + tt_spacer_row(5)

  tab <- tab +
    tt_rule_mid() +
    TR("\\textbf{Panel A.}") %:% TR("\\textbf{Model Parameters}",panel_width)  +
    tt_rule_mid_partial(panel_partial) +
    TR("Idyosinctratic taste parameter ($\\beta^{-1}$)") %:% TR(ddinput[,unique(beta_model_inv)],c(panel_width),dec=3) +
    TR(" ") %:% TR(bb[,unique(beta_model_inv)],panel_width,dec=3,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("Taste correlation parameter ($\\rho$)") %:% TR(ddinput[,rho],dec=3) +
    TR(" ") %:% TR(bb[,rho],dec=3,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("Returns to scale ($1-\\alpha$)") %:% TR(ddinput[,rts],dec=3) +
    TR(" ") %:% TR(bb[,rts],dec=3,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    tt_rule_mid()

  tab <- tab +
    TR("\\textbf{Panel B.}") %:% TR("\\textbf{Firm-level Rents and Rent Shares}",panel_width)  +
    tt_rule_mid_partial(panel_partial) +
    TR("Workers' Rents:") +
    TR("$\\quad$ Per-worker Dollars") %:% TR(ddinput$Rw_firm,dec=0) +
    TR(" ") %:% TR(bb$Rw_firm,dec=0,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("$\\quad$ Share of Earnings") %:% TR(ddinput$Rw_firm_wageshare*100,dec=0,percentage=T) +
    TR(" ") %:% TR(bb$Rw_firm_wageshare*100,dec=0,percentage=T,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("Firms' Rents:") +
    TR("$\\quad$ Per-worker Dollars") %:% TR(ddinput$Rf_firm,dec=0)  +
    TR(" ") %:% TR(bb$Rf_firm,dec=0,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("$\\quad$ Share of Profits") %:% TR(ddinput$Rf_firm_profitshare*100,dec=0,percentage=T) +
    TR(" ") %:% TR(bb$Rf_firm_profitshare*100,dec=0,percentage=T,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("Workers' Share of Rents") %:% TR(ddinput$Rw_firm_share*100,dec=0,percentage=T) +
    TR(" ") %:% TR(bb$Rw_firm_share*100,dec=0,percentage=T,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    tt_rule_mid()

  tab <- tab +
    TR("\\textbf{Panel C.}") %:% TR("\\textbf{Market-level Rents and Rent Shares}",panel_width)  +
    tt_rule_mid_partial(panel_partial) +
    TR("Workers' Rents:") +
    TR("$\\quad$ Per-worker Dollars") %:% TR(ddinput$Rw_market,dec=0) +
    TR(" ") %:% TR(bb$Rw_market,dec=0,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("$\\quad$ Share of Earnings") %:% TR(ddinput$Rw_market_wageshare*100,dec=0,percentage=T) +
    TR(" ") %:% TR(bb$Rw_market_wageshare*100,dec=0,percentage=T,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("Firms' Rents:") +
    TR("$\\quad$ Per-worker Dollars") %:% TR(ddinput$Rf_market,dec=0)  +
    TR(" ") %:% TR(bb$Rf_market,dec=0,surround = "{(\\footnotesize %s)}")  +
    tt_spacer_row(5) +
    TR("$\\quad$ Share of Profits") %:% TR(ddinput$Rf_market_profitshare*100,dec=0,percentage=T) +
    TR(" ") %:% TR(bb$Rf_market_profitshare*100,dec=0,percentage=T,surround = "{(\\footnotesize %s)}") +
    tt_spacer_row(5) +
    TR("Workers' Share of Rents") %:% TR(ddinput$Rw_market_share*100,dec=0,percentage=T) +
    TR(" ") %:% TR(bb$Rw_market_share*100,dec=0,percentage=T,surround = "{(\\footnotesize %s)}")

  TS(tab,file="tableA7",header = c("l",rep("r",8)), output_path = output_dir)

}


helper.matlab.after <- function(results,bootstrap=FALSE){

  id.vars <- c("model","MAorder")
  if(bootstrap){
    id.vars <- c("model","MAorder","boot")
  }

  # convert results into a clean data.table
  dd = NULL
  results_names = rownames(results[[1]])
  for(i in 1:length(results_names)){
    this_results = as.data.table(results[[1]][i][[1]])
    setnames(this_results,c('t1_y','t2_y','mu_y','e_y','t1_w','t2_w','mu_w','e_w','gap0','gat'))
    dd = rbindlist(list(dd,this_results))
  }
  dd$model = rownames(results[[1]])
  for(i in 1:5){ # need to do this a few times to catch them all for some reason
    dd[,model := str_replace(model,"[.]","_")]
  }

  # clean up MA order info
  dd[str_detect(model,"ma1"), MAorder := 1]
  dd[str_detect(model,"ma2"), MAorder := 2]
  dd[, model := str_replace(model,"_ma2","")]
  dd[, model := str_replace(model,"_ma1","")]

  # clean up bootstrap or gross/net income indicators
  if(bootstrap){
    dd[, boot := gsub("[^0-9\\.]", "", model) ]
  }

  # clean up names
  setnames(dd,c('mu_w','mu_y','e_w','e_y'),c('permvar_w','permvar_y','transshockvar_w','transshockvar_y'))
  setnames(dd,c('t1_w','t1_y','t2_w','t2_y'),c('macoef1_w','macoef1_y','macoef2_w','macoef2_y'))
  setnames(dd,c('gap0','gat'),c('perm_pt','trans_pt'))

  # construct implied variables
  dd[,`:=`(permsd_w=sqrt(permvar_w),permsd_y=sqrt(permvar_y), transshocksd_w=sqrt(transshockvar_w),transshocksd_y=sqrt(transshockvar_y))]
  dd[,`:=`(transtotalvar_y = transshockvar_y*(1 + (macoef1_y-1)^2 + (macoef1_y-macoef2_y)^2 + macoef2_w^2 ), transtotalvar_w = transshockvar_w*(1 + (macoef1_w-1)^2 + (macoef1_w-macoef2_w)^2 + macoef2_w^2 ))]
  dd[, pttransvar_w := trans_pt^2*transtotalvar_y]
  dd[, ptpermvar_w := perm_pt^2*permvar_y]
  dd[,`:=`(totalvar_w = transtotalvar_w + permvar_w + pttransvar_w + ptpermvar_w,totalvar_y = transtotalvar_y + permvar_y)]
  dd[,`:=`(totalsd_y = sqrt(totalvar_y), totalsd_w = sqrt(totalvar_w), transtotalsd_y = sqrt(transtotalvar_y), transtotalsd_w = sqrt(transtotalvar_w), ptpermsd_w = sqrt(ptpermvar_w), pttranssd_w = sqrt(pttransvar_w))]

  # flip long
  dd2 <- setDT(melt(dd,id.vars=id.vars))

  if(bootstrap){
    dd2[, model := str_replace(model,"_"," ")]
    dd2[, model := word(model,1)]
    dd2 <- dd2[,list(value = sd(value)), list(model,MAorder,variable)]
  }

  dd2[model=="market" & variable %in% c("pttranssd_w","ptpermsd_w","pttransvar_w","ptpermvar_w","perm_pt","trans_pt"), `:=`(variable = paste0("market",variable), model = "net")]
  dd2 <- dd2[model != "market"]

  return(dd2)
}


matlab_process_output <- function(results_dir,intermed_dir){

  dd <- readMat(sprintf("%smatlab/overall_passthrough_matlab_results.mat",intermed_dir))
  ddall <- helper.matlab.after(results=copy(dd))
  write.csv(ddall, file=sprintf("%smatlab/matlab_output.csv",results_dir), row.names=F)

  dd1 <- readMat(sprintf("%smatlab/bootstrap_passthrough_matlab_results.mat",intermed_dir))
  bb1 <- helper.matlab.after(results=copy(dd1),bootstrap=TRUE)
  dd2 <- readMat(sprintf("%smatlab/bootstrap_passthrough_matlab_results_MA2.mat",intermed_dir))
  bb2 <- helper.matlab.after(results=copy(dd2),bootstrap=TRUE)
  bb <- rbind(bb1,bb2)
  write.csv(bb, file=sprintf("%smatlab/matlab_output_bootstraps.csv",results_dir), row.names=F)

}

table.A3 <- function(){

  get_param <- function(pp="totalsd"){
    c(dd[variable==paste0(pp,"_y")]$value[1], dd[variable==paste0(pp,"_w")]$value[1], dd[variable==paste0(pp,"_y")]$value[2], dd[variable==paste0(pp,"_w")]$value[2])
  }

  get_param_boot <- function(pp="totalsd"){
    c(bb[variable==paste0(pp,"_y")]$value[1], bb[variable==paste0(pp,"_w")]$value[1], bb[variable==paste0(pp,"_y")]$value[2], bb[variable==paste0(pp,"_w")]$value[2])
  }
  
  matlab_process_output(results_dir=input_dir,intermed_dir=input_dir)

  ddall <- setDT(read.csv(sprintf("%smatlab/matlab_output.csv",input_dir)))
  ddall <- ddall[, model := factor(model,c("unc","net"))]
  ddall <- ddall[order(model)]

  bball <- setDT(read.csv(sprintf("%smatlab/matlab_output_bootstraps.csv",input_dir)))
  bball <- bball[, model := factor(model,c("unc","net"))]
  bball <- bball[order(model)]

  tab <- TR(c(" ","\\textbf{GMM Estimates of Joint Process}"),c(1,4)) +
    tt_rule_mid_partial(list(c(2,5))) +
    TR(c("",rep(c("Firm Only","Accounting for Markets"),1)),c(1,rep(2,2))) +
    tt_rule_mid_partial(list(c(2,5))) +
    TR(c("",rep(c("Log Value Added","Log Earnings"),2)))


  dd <- ddall[MAorder==1]
  bb <- bball[MAorder==1]

  tab <- tab + tt_rule_mid() +
    TR('\\textbf{Panel A.}') %:% TR(c("\\textbf{Process: MA(1)}"),c(4)) +
    tt_rule_mid_partial(list(c(2,5))) +
    TR('Total Growth (Std. Dev.)') %:% TR( get_param("totalsd"),dec=2) +
    TR(' ') %:% TR( get_param_boot("totalsd"),dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR('Permanent Shock (Std. Dev.)') %:% TR( get_param("permsd"), dec=2) +
    TR(' ') %:% TR( get_param_boot("permsd"), dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR('Transitory Shock (Std. Dev.)') %:% TR( get_param("transshocksd"), dec=2) +
    TR(' ') %:% TR( get_param_boot("transshocksd"), dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR("MA Coefficient, Lag 1") %:% TR( get_param("macoef1"), dec=2) +
    TR(" ") %:% TR( get_param_boot("macoef1"), dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR("MA Coefficient, Lag 2") %:% TR( get_param("macoef2"), dec=2) +
    TR(" ") %:% TR( get_param_boot("macoef2"), dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR("Permanent Passthrough Coefficient") %:% TR(c( NA, dd[variable=="perm_pt"]$value[1], NA, dd[variable=="perm_pt"]$value[2] ), dec=2) +
    TR(" ") %:% TR(NA) %:% TR(bb[variable=="perm_pt"]$value[1], dec=2, surround = "{(\\footnotesize %s)}") %:% TR(NA) %:% TR(bb[variable=="perm_pt"]$value[2], dec=2, surround = "{(\\footnotesize %s)}") + tt_spacer_row(5) +
    TR("Transitory Passthrough Coefficient") %:% TR(c( NA, dd[variable=="trans_pt"]$value[1], NA, dd[variable=="trans_pt"]$value[2] ), dec=2) +
    TR(" ") %:% TR(NA) %:% TR(bb[variable=="trans_pt"]$value[1], dec=2, surround = "{(\\footnotesize %s)}") %:% TR(NA) %:% TR(bb[variable=="trans_pt"]$value[2] , dec=2, surround = "{(\\footnotesize %s)}") + tt_spacer_row(5) +
    TR("Market Passthrough Coefficient") %:% TR(c( NA, NA, NA, dd[variable=="marketperm_pt"]$value[1] ), dec=2) +
    TR(" ") %:% TR(c( NA, NA, NA)) %:% TR(bb[variable=="marketperm_pt"]$value[1] , dec=2, surround = "{(\\footnotesize %s)}")

  dd <- ddall[MAorder==2]
  bb <- bball[MAorder==2]

  tab <- tab + tt_rule_mid() +
    TR('\\textbf{Panel B.}') %:% TR(c("\\textbf{Process: MA(2)}"),c(4)) +
    tt_rule_mid_partial(list(c(2,5))) +
    TR('Total Growth (Std. Dev.)') %:% TR( get_param("totalsd"),dec=2) +
    TR(' ') %:% TR( get_param_boot("totalsd"),dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR('Permanent Shock (Std. Dev.)') %:% TR( get_param("permsd"), dec=2) +
    TR(' ') %:% TR( get_param_boot("permsd"), dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR('Transitory Shock (Std. Dev.)') %:% TR( get_param("transshocksd"), dec=2) +
    TR(' ') %:% TR( get_param_boot("transshocksd"), dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR("MA Coefficient, Lag 1") %:% TR( get_param("macoef1"), dec=2) +
    TR(" ") %:% TR( get_param_boot("macoef1"), dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR("MA Coefficient, Lag 2") %:% TR( get_param("macoef2"), dec=2) +
    TR(" ") %:% TR( get_param_boot("macoef2"), dec=2, surround = "{(\\footnotesize %s)}")  + tt_spacer_row(5)+
    TR("Permanent Passthrough Coefficient") %:% TR(c( NA, dd[variable=="perm_pt"]$value[1], NA, dd[variable=="perm_pt"]$value[2] ), dec=2) +
    TR(" ") %:% TR(NA) %:% TR(bb[variable=="perm_pt"]$value[1], dec=2, surround = "{(\\footnotesize %s)}") %:% TR(NA) %:% TR(bb[variable=="perm_pt"]$value[2], dec=2, surround = "{(\\footnotesize %s)}") + tt_spacer_row(5) +
    TR("Transitory Passthrough Coefficient") %:% TR(c( NA, dd[variable=="trans_pt"]$value[1], NA, dd[variable=="trans_pt"]$value[2] ), dec=2) +
    TR(" ") %:% TR(NA) %:% TR(bb[variable=="trans_pt"]$value[1], dec=2, surround = "{(\\footnotesize %s)}") %:% TR(NA) %:% TR(bb[variable=="trans_pt"]$value[2] , dec=2, surround = "{(\\footnotesize %s)}") + tt_spacer_row(5) +
    TR("Market Passthrough Coefficient") %:% TR(c( NA, NA, NA, dd[variable=="marketperm_pt"]$value[1] ), dec=2) +
    TR(" ") %:% TR(c( NA, NA, NA)) %:% TR(bb[variable=="marketperm_pt"]$value[1] , dec=2, surround = "{(\\footnotesize %s)}")

  TS(tab,file="tableA3",header = c("l",rep("c",4)), output_path = output_dir)

}




