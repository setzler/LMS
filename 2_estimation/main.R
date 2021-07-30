
rm(list=ls())

# set run type
# runtype = "local-simulation"      ## run the raw data simulation on Brad's Linux machine
# runtype = "CodeOcean-simulation"    ## run the raw data simulation on a CodeOcean capsule
runtype = "IRS-real"              ## run the raw data real analysis on the IRS server

# configure
start_time <- Sys.time()
set.seed(112233)

library(LMS)
setDTthreads(threads = 5)

# configure directories
if(runtype=="local-simulation"){
  data_dir <- "~/Downloads/tmp/" 
  code_dir <- "~/github/LMS/2_estimation/" 
  intermed_dir <- "~/Downloads/tmp/intermed/" 
  results_dir <- "~/Downloads/tmp/results/" 
  for(subdir in c(data_dir,intermed_dir,results_dir)){
    dir.create(subdir)
  }
  source(sprintf('%sLMS_simulation.R', code_dir ))
  run_simulation(data_dir)
  run_simulation_auction(data_dir)
  year_set <- 2001
  ncores <- 1
  passthrough_bootstraps <- 4
  auction_bootstraps <- 4
}
if(runtype=="CodeOcean-simulation"){
  data_dir <- "../data/" 
  code_dir <- "../code/" 
  intermed_dir <- "../data/" 
  results_dir <- "../results/" 
  source(sprintf('%sLMS_simulation.R', code_dir ))
  run_simulation(data_dir)
  run_simulation_auction(data_dir)
  year_set <- 2001
  ncores <- 1
  passthrough_bootstraps <- 4
  auction_bootstraps <- 4
}
if(runtype=="IRS-real"){
  data_dir <- "~/LMSdir/data/revision3/"
  code_dir <- "~/LMSdir/data/revision3/"
  intermed_dir <- "~/LMSdir/data/revision3/intermediates/"
  results_dir <- "~/LMSdir/data/revision3/results/"
  year_set <- 2001:2008
  ncores <- 8
  passthrough_bootstraps <- 40
  auction_bootstraps <- 20
}

# create subdirectories
dir.create(intermed_dir)
for(subdir in c('covmat','descriptives','DiD','FE','params','matlab')){
  dir.create(paste0(intermed_dir,subdir))
}
dir.create(results_dir)
for(subdir in c('model','descriptives','FE','params','matlab')){
  dir.create(paste0(results_dir,subdir))
}



# import run commands
source(sprintf('%sLMS_helpers.R', code_dir))

# clean data and export in expected format
source(sprintf('%sLMS_sample.R', code_dir)) # import the clean_mainvars_data function
clean_mainvars_data(data_dir)

# descriptives
source(sprintf('%sLMS_descriptives.R', code_dir)) # import the get_descriptives function
get_descriptives(data_dir=data_dir, intermed_dir=results_dir, industry='naics2', location='cz', broadmarkets=FALSE)
get_descriptives(data_dir=data_dir, intermed_dir=results_dir, industry='naics2', location='cz', broadmarkets=TRUE)
get_descriptives(data_dir=data_dir, intermed_dir=results_dir, industry='naics2', location='cz', broadmarkets=FALSE, movers=TRUE)
get_descriptives(data_dir=data_dir, intermed_dir=results_dir, industry='naics2', location='cz', broadmarkets=TRUE, movers=TRUE)

get_descriptives(data_dir=data_dir, intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=TRUE)
get_descriptives(data_dir=data_dir, intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=FALSE)
for(ii in 1:nrow(market_combinations)){
  get_descriptives(data_dir=data_dir, intermed_dir=intermed_dir, industry=market_combinations[ii]$ind, location=market_combinations[ii]$loc, broadmarkets=market_combinations[ii]$br)
}
LMS_taxfit(results_dir=results_dir, data_dir=data_dir)

# passthrough -- main results, with broad markets
source(sprintf('%sLMS_passthrough.R', code_dir)) # import the parallel_output and read_params functions
get_passthrough_descriptives(data_dir=data_dir, results_dir=results_dir, broadmarkets=TRUE)
get_passthrough_descriptives(data_dir=data_dir, results_dir=results_dir, broadmarkets=FALSE)

get_passthrough_parallelized(data_dir=data_dir, intermed_dir=intermed_dir, years=year_set, ncores = ncores, industry='naics2', location='cz', broadmarkets=TRUE)
get_passthrough_params(results_dir=results_dir, intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=TRUE)

get_passthrough_parallelized(data_dir=data_dir, intermed_dir=intermed_dir, years=year_set, ncores = ncores, industry='naics2', location='cz', broadmarkets=FALSE)
get_passthrough_params(results_dir=results_dir, intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=FALSE)

# passthrough - standard errors
get_passthrough_bootstraps(data_dir,intermed_dir,years=year_set, ncores = ncores, industry='naics2', location='cz', broadmarkets=TRUE, num_boots=passthrough_bootstraps)
get_passthrough_params_bootstraps(results_dir=results_dir, intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=TRUE, num_boots=passthrough_bootstraps)

get_passthrough_bootstraps(data_dir,intermed_dir,years=year_set, ncores = ncores, industry='naics2', location='cz', broadmarkets=FALSE, num_boots=passthrough_bootstraps)
get_passthrough_params_bootstraps(results_dir=results_dir, intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=FALSE, num_boots=passthrough_bootstraps)

# format passthrough output for matlab polynomial model
source(sprintf('%sLMS_prematlab.R', code_dir)) # import matlab_preparation, matlab_preparation_bootstraps
library(R.matlab)
matlab_preparation(results_dir,intermed_dir)
matlab_preparation_bootstraps(results_dir,intermed_dir,passthrough_bootstraps)

# FE
source(sprintf('%sLMS_FE.R', code_dir)) # import FE_estimation
FE_estimation(results_dir=results_dir, intermed_dir=intermed_dir, data_dir=data_dir, industry="naics2", location="cz", broadmarkets=TRUE, methods=c("blm_adjusted","blm_unadjusted","akm_adjusted"))

# model
source(sprintf('%sLMS_model.R', code_dir)) # import collect_model, collect_model_descriptives, collect_decomp_AKM
collect_decomp_AKM(results_dir, intermed_dir)
collect_model_descriptives(results_dir, intermed_dir)
collect_model(results_dir,intermed_dir)



# passthrough -- alternative market definitions
for(ii in 1:nrow(market_combinations)){
  get_passthrough_parallelized(data_dir=data_dir, intermed_dir=intermed_dir, years=year_set, ncores = ncores, industry=market_combinations[ii]$ind, location=market_combinations[ii]$loc, broadmarkets=market_combinations[ii]$br)
  get_passthrough_params(results_dir=results_dir, intermed_dir=intermed_dir, industry=market_combinations[ii]$ind, location=market_combinations[ii]$loc, broadmarkets=market_combinations[ii]$br)
}

# passthrough -- robustness checks
subsample_set <- c('sometenure','men','VAlessthan10B','noMNE','smallFirm','highWorkerWage','highFirmWage','older')
for(subsamples in subsample_set){
  get_passthrough_parallelized(data_dir=data_dir, intermed_dir=intermed_dir, years=year_set, ncores = ncores, industry='naics2', location='cz', broadmarkets=FALSE, doDiD=FALSE, subsample=subsamples)
  get_passthrough_params(results_dir=intermed_dir,intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=FALSE, doDiD=FALSE, subsample=subsamples)
}
va_set <- c('ebitd','vanet','operprofits')
for(va_vars in va_set){
  get_passthrough_parallelized(data_dir=data_dir, intermed_dir=intermed_dir, years=year_set, ncores = ncores, industry='naics2', location='cz', broadmarkets=FALSE, doDiD=FALSE, va_var=va_vars)
  get_passthrough_params(results_dir=intermed_dir,intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=FALSE, doDiD=FALSE, va_var=va_vars)
}
combine_passthrough_subsamples(results_dir=results_dir, intermed_dir=intermed_dir, industry='naics2', location='cz', broadmarkets=FALSE, va_set=va_set, subsample_set=subsample_set)

# passthrough -- bartik IV
source(sprintf('%sLMS_bartik.R', code_dir)) # import bartik_gather_results
library(ShiftShareIV)
get_bartik_collapse(intermed_dir, data_dir)
bartik_gather_results(results_dir,intermed_dir)

# passthrough -- auction IV
source(sprintf('%sLMS_auctions.R', code_dir)) # import bartik_gather_results
library(eventStudy)
KLMS.event_study.baseline.SE(spec='forLMS_LSelasticity',results_dir=results_dir, data_dir=data_dir, ncores = ncores, max_seed=auction_bootstraps) 

# done
end_time <- Sys.time()
flog.info("Finished all estimation. Total time: %f", (end_time - start_time)) # takes about 6mins
