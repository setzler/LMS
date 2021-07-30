
rm(list=ls())

# set run type
# runtype = "local-simulation"      ## make figures for simulated results on Brad's Linux machine
# runtype = "CodeOcean-simulation"    ## make figures for simulated results on a CodeOcean capsule
runtype = "local-real"            ## make figures for real IRS results on Brad's Linux machine
# runtype = "CodeOcean-real"        ## make figures for real IRS results on a CodeOcean capsule

# make sure all packages are installed and load them
list.of.packages <- c("data.table", "ggplot2", "stringr", "magrittr", "futile.logger", "scales", "R.matlab", "textables")
lapply(list.of.packages, library, character.only = TRUE) # load all packages

# set up directories
if(runtype=='local-real'){
  code_dir <- "~/github/LMS/4_analysis/code/"
  input_dir <- "../data/" 
  output_dir <- "../results/" # output_dir <- "~/Dropbox/IRS_Firm_Worker/paper-final/tables_and_figures/"
}
if(runtype=='local-simulation'){
  code_dir <- "~/github/LMS/4_analysis/code/"
  input_dir <- "~/Downloads/tmp/results/"
  output_dir <- "~/Downloads/tmp/output/"
}
if(runtype=='CodeOcean-simulation' | runtype=='CodeOcean-real'){
  code_dir <- paste0(getwd(),"/")
  input_dir <- "../data/" 
  output_dir <- "../results/"
}
setwd(code_dir)
dir.create(output_dir)
flog.info("results output to %s",output_dir)

# pass-through results
source(sprintf("%spassthrough_results.R", code_dir)) # import the formatting functions for passthrough results
table.2_A4() # external IV tables
figure.1() # DiD event study plot
table.3() # overall rents
figure.A1ab() # passthrough robustness to alternate measures and subsamples
figure.A3ab() # heterogeneity in the passthrough estimates across region-sector
table.A3() # process GMM estimates for other parameters
table.A5() # passthrough robustness to market definition
table.A7() # rent heterogeneity by market

# fixed-effect results
source(sprintf("%sFE_results.R", code_dir)) # import the formatting functions for FE results
figure.2() # plot of FE interactions
table.A6() # table comparing FE specifications

# model results
source(sprintf("%smodel_results.R", code_dir)) # import the formatting functions for model results
table.1() # model summary
figure.3abcd() # shares of worker types by firm cluster
table.4() # psi variance decomposition
table.5() # welfare counterfactuals
figure.A4abcd_A5() # goodness of fit
figure.A6() # comp diff estimates
figure.A7ab() # counterfactuals that shrink G or theta variance

# other results
source(sprintf("%sdescriptives_results.R", code_dir)) # import the formatting functions for other results
table.A1_A2() # descriptive statistics
figure.A2() # fit of the tax function
