

helper.square_covariance <- function(long_cov){
  
  # make symmetric in long format (construct the identical mirrored upper and lower parts of symmetric matrix)
  long_cov2 <- copy(long_cov)
  long_cov2$covariate_2 <- long_cov$covariate_1 
  long_cov2$covariate_1 <- long_cov$covariate_2
  long_cov2$event_2 <- long_cov$event_1
  long_cov2$event_1 <- long_cov$event_2
  long_cov <- rbind(long_cov,long_cov2)
  long_cov <- unique(long_cov)
  long_cov <- long_cov[order(covariate_1,covariate_2,event_1,event_2)]
  
  # shift to counting forward, combine variable names with event times
  long_cov[, event_1 := event_1 + 7]
  long_cov[, event_2 := event_2 + 7]
  long_cov[, var_1 := paste0(covariate_1,"_",event_1)]
  long_cov[, var_2 := paste0(covariate_2,"_",event_2)]
  long_cov[,c("event_1","event_2") := NULL]
  long_cov = unique(long_cov[,.(var_1,var_2,covariance_estimate)])
  
  # reshape and tidy up
  square_cov <- reshape(copy(long_cov), v.names = "covariance_estimate", idvar = "var_1",
                        timevar = "var_2", direction = "wide")
  setnames(square_cov,str_replace(names(square_cov),"covariance_estimate.",""))
  square_cov = square_cov[order(var_1)]
  ordered_names <- square_cov$var_1
  square_cov[, var_1 := NULL]
  square_cov = square_cov[,.SD,.SDcols=ordered_names]
  
  return(square_cov)
  
}

helper.matlab_covariance <- function(square_cov){
  
  # covert data.table to matrix with rownames = colnames
  square_cov = as.matrix(square_cov)
  rownames(square_cov) = colnames(square_cov)
  
  # subset squares from the full covariance matrix
  subsetter <- function(fullset,var1,var2){
    indexer = str_detect(rownames(fullset),var1)|str_detect(rownames(fullset),var2)
    return(fullset[indexer,indexer])
  }
  
  # get the passthrough-specific covariance matrices
  matlab_ready <- NULL
  matlab_ready[["unc"]] <- subsetter(square_cov,'vaD','wagesD') # unconditional passthrough
  matlab_ready[["net"]] <- subsetter(square_cov,'vanetD','wagesnetD') # net-of-market passthrough
  matlab_ready[["market"]] <- subsetter(square_cov,'vameanD','wagesmeanD') # market passthrough
  
  return(matlab_ready)
  
}

helper.write_to_matlab <- function(con,arglist) {
  arglist[["con"]] = con
  do.call(writeMat,arglist)
}

matlab_preparation <- function(results_dir,intermed_dir){
  
  suffix <- get_suffix(industry='naics2', location='cz', broadmarkets=FALSE)
  
  # this is the mean of the overall samples across year intervals
  dd <- readRDS(file=sprintf("%scovmat/CovMat_by_year_%s.csv",intermed_dir,suffix))
  dd[, event_1 := min(event_1) - event_1] # counting from 1 instead of 0
  dd[, event_2 := min(event_2) - event_2] # counting from 1 instead of 0
  dd <- dd[,list(covariance_estimate = mean(covariance_estimate)),list(covariate_1,covariate_2,event_1,event_2)] # collapse across cohorts
  
  # now we convert it to a form matlab can read
  dd <- helper.square_covariance(dd)
  matlab_list <- helper.matlab_covariance(dd) # contains 3 different 14 x 14 matrices in list format
  helper.write_to_matlab(sprintf('%smatlab/matlab_CovMat.dat',results_dir),matlab_list)
  
  # get the passthrough rates
  pt <- setDT(read.csv(file=sprintf("%sparams/LMS_params_%s.csv",results_dir,suffix)))[model=="1inner",list(unc=unc_pt,net=net_pt,marketIV=market_pt)]
  matlab_list_pt <- list(unc = pt$unc, net = pt$net, market=pt$marketIV) # contains just the 3 passthrough rate estimates under MA(1) assumption
  helper.write_to_matlab(sprintf("%smatlab/matlab_ptMA1.dat",results_dir),matlab_list_pt)
  
  pt <- setDT(read.csv(file=sprintf("%sparams/LMS_params_%s.csv",results_dir,suffix)))[model=="2",list(unc=unc_pt,net=net_pt,marketIV=market_pt)]
  matlab_list_pt <- list(unc = pt$unc, net = pt$net, market=pt$marketIV) # contains just the 3 passthrough rate estimates under MA(2) assumption
  helper.write_to_matlab(sprintf("%smatlab/matlab_ptMA2.dat",results_dir),matlab_list_pt)
  
}


matlab_preparation_bootstraps <- function(results_dir,intermed_dir,passthrough_bootstraps){
  
  suffix <- get_suffix(industry='naics2', location='cz', broadmarkets=FALSE)
  
  # we will sequentially fill in the matlab list with each of the bootstrap covariance matrices
  matlab_list <- list()
  for(bb in 1:passthrough_bootstraps){
    
    # this is the mean of the overall samples across year intervals
    dd <- readRDS(file=sprintf("%scovmat/bootstraps/CovMat_by_year_%s_%s.csv",intermed_dir,suffix,bb))
    dd[, event_1 := min(event_1) - event_1] # counting from 1 instead of 0
    dd[, event_2 := min(event_2) - event_2] # counting from 1 instead of 0
    dd <- dd[,list(covariance_estimate = mean(covariance_estimate)),list(covariate_1,covariate_2,event_1,event_2)] # collapse across cohorts
    
    dd <- helper.square_covariance(dd)
    matlab_list2 <- helper.matlab_covariance(dd) # contains 3 different 14 x 14 matrices in list format
    names(matlab_list2) <- paste0(names(matlab_list2),sprintf("_boot%s",bb))
    matlab_list <- append(matlab_list,matlab_list2)
  }
  helper.write_to_matlab(sprintf('%smatlab/matlab_CovMat_bootstraps.dat',results_dir),matlab_list)
  
  # get the passthrough
  pt_full <- get_passthrough_params_bootstraps(results_dir, intermed_dir, industry='naics2', location='cz', broadmarkets=FALSE, num_boots=passthrough_bootstraps, return_raw=TRUE)
  
  pt <- pt_full[model=="1inner",list(boot,unc_pt,net_pt,market_pt)]
  matlab_list_pt <- list()
  for(bb in pt[,unique(boot)]){
    matlab_list_pt[[paste0("unc_boot",bb)]] <- pt[boot==bb]$unc_pt
    matlab_list_pt[[paste0("net_boot",bb)]] <- pt[boot==bb]$net_pt
    matlab_list_pt[[paste0("market_boot",bb)]] <- pt[boot==bb]$market_pt
  }
  helper.write_to_matlab(sprintf("%smatlab/matlab_bootstraps_ptMA1.dat",results_dir),matlab_list_pt)
  
  pt <- pt_full[model=="2",list(boot,unc_pt,net_pt,market_pt)]
  matlab_list_pt <- list()
  for(bb in pt[,unique(boot)]){
    matlab_list_pt[[paste0("unc_boot",bb)]] <- pt[boot==bb]$unc_pt
    matlab_list_pt[[paste0("net_boot",bb)]] <- pt[boot==bb]$net_pt
    matlab_list_pt[[paste0("market_boot",bb)]] <- pt[boot==bb]$market_pt
  }
  helper.write_to_matlab(sprintf("%smatlab/matlab_bootstraps_ptMA2.dat",results_dir),matlab_list_pt)

}



