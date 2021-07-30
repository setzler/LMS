
table.A1_A2 <- function(){
  
  regionsectors <- c("Midwest_Goods","Northeast_Goods","South_Goods","West_Goods","Midwest_Services","Northeast_Services","South_Services","West_Services","All")
  
  full0 <- setDT(read.csv(sprintf("%sdescriptives/LMS_descriptives_naics2_cz.csv",input_dir)))[,broad_market := 'All']
  full1 <- setDT(read.csv(sprintf("%sdescriptives/LMS_descriptives_naics2_cz_broadmarket.csv",input_dir)))
  full <- rbind(full0,full1)
  full[, broad_market := factor(broad_market,levels=regionsectors)]
  full <- full[order(broad_market)]
  
  movers0 <- setDT(read.csv(sprintf("%sdescriptives/LMS_descriptives_naics2_cz_movers.csv",input_dir)))[,broad_market := 'All']
  movers1 <- setDT(read.csv(sprintf("%sdescriptives/LMS_descriptives_naics2_cz_broadmarket_movers.csv",input_dir)))
  movers <- rbind(movers0,movers1)
  movers[, broad_market := factor(broad_market,levels=regionsectors)]
  movers <- movers[order(broad_market)]
  
  stayers0 <- setDT(read.csv(sprintf("%sdescriptives/LMS_descriptives_naics2_cz_stayersteps.csv",input_dir)))[,broad_market := 'All']
  stayers1 <- setDT(read.csv(sprintf("%sdescriptives/LMS_descriptives_naics2_cz_broadmarket_stayersteps.csv",input_dir)))
  stayers <- rbindlist(list(stayers0,stayers1),use.names=T,fill=T)
  stayers[, broad_market := factor(broad_market,levels=regionsectors)]
  stayers <- stayers[order(broad_market)]
  
  regions <- c("Midwest","Northeast","South","West")
  industries <- c("Goods","Services")
  for(loc in c(regions,"All")){
    full[str_detect(broad_market,loc), location := loc]
    movers[str_detect(broad_market,loc), location := loc]
    stayers[str_detect(broad_market,loc), location := loc]
  }
  for(ind in c(industries,"All")){
    full[str_detect(broad_market,ind), industry := ind]
    movers[str_detect(broad_market,ind), industry := ind]
    stayers[str_detect(broad_market,ind), industry := ind]
  }
  
  
  reps <- full[,length(unique(broad_market))]
  
  tab_small <- TR(c("","\\textbf{Workers}","\\textbf{Firms}"),c(1,2,2)) + tt_rule_mid() +
    TR(c("\\textbf{Panel A.}","\\textbf{Baseline Sample}"),c(1,4)) + tt_rule_mid_partial(list(c(2,5))) +
    TR(c("","\\textbf{Unique}","\\textbf{Observation-Years}","\\textbf{Unique}","\\textbf{Observation-Years}")) +
    TR("Full Sample:") %:% TR(full[broad_market=="All",c(workers,workeryears,firms,firmyears)],dec=0) +
    tt_rule_mid() + TR(c("\\textbf{Panel B.}","\\textbf{Movers Sample}"),c(1,4)) + tt_rule_mid_partial(list(c(2,5))) +
    TR(c("","\\textbf{Unique}","\\textbf{Observation-Years}","\\textbf{Unique}","\\textbf{Observation-Years}")) +
    TR("Movers Only:") %:% TR(movers[broad_market=="All",c(workers,workeryears,firms,firmyears)],dec=0) +
    tt_rule_mid() + TR(c("\\textbf{Panel C.}","\\textbf{Stayers Sample}"),c(1,4)) + tt_rule_mid_partial(list(c(2,5))) +
    TR(c("","\\textbf{Unique}","\\textbf{6 Year Spells}","\\textbf{Unique}","\\textbf{6 Year Spells}")) +
    TR("Complete Stayer Spells:") %:% TR(stayers[broad_market=="All" & step=="marketStayer",c(workers,spells,firms,firmyears)],dec=0) +
    TR("10 Stayers per Firm:") %:% TR(stayers[broad_market=="All" & step=="workersPerFirm",c(workers,spells,firms,firmyears)],dec=0) +
    TR("10 Firms per Market:") %:% TR(stayers[broad_market=="All" & step=="firmsPerMarket",c(workers,spells,firms,firmyears)],dec=0)
  
  TS(tab_small, file='tableA1', header=c("l",rep("r",4)), output_path = output_dir)
  
  
  
  
  
  tab <-
    TR("") %:% TR(as.character(unique(full$industry)),c(reps/2-0.5,reps/2-0.5,1)) +
    tt_rule_mid_partial(list(c(2,reps/2+0.5),c(reps/2+1.5,reps))) +
    TR("") %:% TR(as.character(full$location)) +
    tt_spacer_row(5) +
    tt_rule_mid() +
    TR("\\textbf{Panel A.}") %:% TR("\\textbf{Full Sample}",reps)  +
    tt_rule_mid_partial(list(c(2,reps+1))) +
    TR("Observation Counts:") +
    TR("$\\quad$ Number of FTE Worker-Years") %:% TR(full$workeryears,dec=0) +
    TR("$\\quad$ Number of Unique FTE Workers") %:% TR(full$workers,dec=0) +
    TR("$\\quad$ Number of Unique Firms with FTE Workers") %:% TR(full$firms,dec=0) +
    TR("$\\quad$ Number of Unique Markets with FTE Workers") %:% TR(full$markets,dec=0) +
    TR("Group Counts:") +
    TR("$\\quad$ Mean Number of FTE Workers per Firm") %:% TR(full$workers_per_firm,dec=1) +
    TR("$\\quad$ Mean Number of FTE Workers per Market") %:% TR(full$workers_per_market,dec=1) +
    TR("$\\quad$ Mean Number of Firms per Market with FTE Workers") %:% TR(full$firms_per_market,dec=1) +
    TR("Outcome Variables in Log \\$:") +
    TR("$\\quad$ Mean Log Wage for FTE Workers") %:% TR(full$Ew,dec=2) +
    TR("$\\quad$ Mean Value Added for FTE Workers") %:% TR(full$ElogVA,dec=2) +
    TR("Firm Aggregates in \\$1,000:") +
    TR("$\\quad$ Wage Bill per Worker") %:% TR(full$EWB,dec=1) +
    TR("$\\quad$ Value Added per Worker") %:% TR(full$EVA,dec=1) +
    tt_spacer_row(5) +
    tt_rule_mid() +
    TR("\\textbf{Panel B.}") %:% TR("\\textbf{Movers Sample}",reps)  +
    tt_rule_mid_partial(list(c(2,reps+1))) +
    TR("Observation Counts:") +
    TR("$\\quad$ Number of FTE Mover-Years") %:% TR(movers$workeryears,dec=0) +
    TR("$\\quad$ Number of Unique FTE Movers") %:% TR(movers$workers,dec=0) +
    TR("$\\quad$ Number of Unique Firms with FTE Movers") %:% TR(movers$firms,dec=0) +
    TR("$\\quad$ Number of Unique Markets with FTE Movers") %:% TR(movers$markets,dec=0) +
    TR("Group Counts:") +
    TR("$\\quad$ Mean Number of FTE Movers per Firm with FTE Movers") %:% TR(movers$workers_per_firm,dec=1) +
    TR("$\\quad$ Mean Number of Movers per Market with FTE Movers") %:% TR(movers$workers_per_market,dec=1) +
    TR("$\\quad$ Mean Number of Firms per Market with FTE Movers") %:% TR(movers$firms_per_market,dec=1) +
    TR("Outcome Variables in Log \\$:") +
    TR("$\\quad$ Mean Log Wage for FTE Movers") %:% TR(movers$Ew,dec=2) +
    TR("$\\quad$ Mean Value Added for FTE Movers") %:% TR(movers$ElogVA,dec=2) +
    tt_spacer_row(5) +
    tt_rule_mid() +
    TR("\\textbf{Panel C.}") %:% TR("\\textbf{Stayers Sample}",reps)  +
    tt_rule_mid_partial(list(c(2,reps+1))) +
    TR("Sample Counts:") +TR("$\\quad$ Number of 8-year Worker-Firm Stayer Spells") %:% TR(stayers[step=="firmsPerMarket"]$spells,dec=0) +
    TR("$\\quad$ Number of Unique FTE Stayers in Firms with 10 FTE Stayers") %:% TR(stayers[step=="firmsPerMarket"]$workers,dec=0) +
    TR("$\\quad$ Number of Unique Firms with 10 FTE Stayers") %:% TR(stayers[step=="firmsPerMarket"]$firms,dec=0) +
    TR("$\\quad$ Number of Unique Markets with 10 Firms with 10 FTE Stayers") %:% TR(stayers[step=="firmsPerMarket"]$markets,dec=0) +
    TR("Outcome Variables in Log \\$:") +
    TR("$\\quad$ Mean Log Wage for FTE Stayers") %:% TR(stayers[step=="firmsPerMarket"]$Ew,dec=2) +
    TR("$\\quad$ Mean Log Value Added for FTE Stayers") %:% TR(stayers[step=="firmsPerMarket"]$ElogVA,dec=2)
  
  TS(tab, file='tableA2', header=c("l",rep("r",reps)), output_path = output_dir)
  
  
}



figure.A2 <- function(){
  
  taxfit <- setDT(read.csv(sprintf("%sdescriptives/LMS_taxfit.csv",input_dir)))
  dd <- melt(taxfit,id=c("gross_inc_round","N"))
  setDT(dd)
  dd[variable=="true_log",variable:="Observed"]
  dd[variable=="pred_log",variable:="Predicted"]
  
  gg <- ggplot(aes(y=value,x=gross_inc_round,linetype=variable),data=dd[variable %in% c("Observed","Predicted")]) +
    geom_line() + geom_point() + theme_bw(base_size=16) +
    scale_x_continuous(breaks= pretty_breaks()) + scale_y_continuous(breaks= pretty_breaks()) +
    labs(x="Log Gross Income Bin",y="",linetype="Log Net Income")
  ggsave(gg, file=sprintf("%sfigureA2.pdf",output_dir),width=8,height=5)
  
}

