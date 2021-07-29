

wt.mean <- function(x,w){
  return(weighted.mean(x,w))
}

get_suffix <- function(industry='naics2', location='cz', broadmarkets=FALSE, gender_subset='', year_subset='', va_var='va', subsample=''){
  suffix = paste0(industry,'_',location)
  if(broadmarkets){
    suffix <- paste0(suffix,"_broadmarket")
  }
  if(gender_subset != ''){
    suffix <- paste0(suffix,"_gender",gender_subset)
  }
  if(year_subset == 'late'){
    suffix <- paste0(suffix,"_late")
  }
  if(va_var != 'va'){
    suffix <- paste0(suffix,"_",va_var)
  }
  if(subsample != ''){
    suffix <- paste0(suffix,"_",subsample)
  }
  flog.info("suffix %s",suffix)
  return(suffix)
}

data_setup_rules <- function(dd, industry='naics2', location='cz', broadmarkets=FALSE, gender_subset='', year_subset='', va_var='va', subsample=''){
  if(industry=='naics3'){
    dd[, naics3 := substr(naics,1,3)]
  }
  if(industry=='naics4'){
    dd[, naics4 := substr(naics,1,4)]
  }
  if(industry=='supersector'){
    dd[, supersector := ""]
    dd[naics2 %in% c(21,23,11), supersector := "const_mining"]
    dd[naics2 %in% c(31,32,33), supersector := "manuf"]
    dd[naics2 %in% c(42,44,45, 48,49,22), supersector := "trade_transport"]
    dd[naics2 %in% c(51), supersector := "information"]
    dd[naics2 %in% c(52,53), supersector := "fire"]
    dd[naics2 %in% c(54,55,56), supersector := "prof_manage"]
    dd[naics2 %in% c(61,62), supersector := "educ_health"]
    dd[naics2 %in% c(71,72), supersector := "leisure"]
    dd[naics2 %in% c(81), supersector := "other_services"]
  }
  dd[, market := .GRP, by=c(industry,location)]

  if(year_subset=='early'){
    flog.info("size before keeping only early years: %s",nrow(dd))
    dd <- dd[years %in% 2001:2008]
    flog.info("size after keeping only early years: %s",nrow(dd))
  }
  if(year_subset=='late'){
    flog.info("size before keeping only late years: %s",nrow(dd))
    dd <- dd[years %in% 2008:2015]
    flog.info("size after keeping only late years: %s",nrow(dd))
  }

  if(gender_subset != ''){
    flog.info("size before keeping only gender %s: %s",gender_subset, nrow(dd))
    dd <- dd[gender==gender_subset]
    flog.info("size after keeping only gender %s: %s",gender_subset, nrow(dd))
  }
  
  if(va_var != 'va'){ # can be ebitd, operprofits, vanet
    flog.info("size before replacing va with other variable: %s",nrow(dd))
    dd[, va := copy(get(va_var))] # this replaces the variable called "va" with another variable (in logs)
    dd <- dd[!is.na(va)] # just in case the outcome is missing (in logs)
    flog.info("size after replacing va with other variable: %s",nrow(dd))
  }
  
  if(subsample != ''){
    flog.info("size before subsample %s: %s", subsample, nrow(dd))
    if(subsample=='VAlessthan10B'){
      dd <- dd[exp(va) <= 1e10]
    }
    if(subsample=='noMNE'){
      dd <- dd[foreign_tax == 0 | is.na(foreign_tax)] # require that the firm currently pays a foreign tax
    }
    if(subsample=='smallFirm'){
      dd <- dd[minfirmsize < 100]
    }
    if(subsample=='highWorkerWage'){
      dd <- dd[init_wage >= median(init_wage)]
    }
    if(subsample=='highFirmWage'){
      dd <- dd[mean_firm_wage >= median(mean_firm_wage)]
    }
    if(subsample=='sometenure'){
      dd <- dd[total_tenure %in% c(8,9,10) & begin_tenure > min(years)]
    }
    if(subsample=='older'){
      dd <- dd[age > (45-8)] # in an 8-year spell, this ensures at least age 45 in the 8th year
    }
    if(subsample=='men'){
      dd <- dd[gender=='M'] 
    }
    flog.info("size after subsample %s: %s", subsample, nrow(dd))
  }
  
  
  return(dd)
}

data_loader <- function(data_dir="~/LMSdir/data/revision3/", industry='naics2',location='cz',broadmarkets=TRUE, gender_subset='', year_subset='', va_var='va', subsample='', wagefloor=100){
  flog.info('load data')
  if(wagefloor==100){
    dd <- setDT(readRDS(file=sprintf("%sfull_sample_mainvars_withmarkets.rds",data_dir)))
    flog.info('data loaded')
    dd <- data_setup_rules(dd,industry=industry,location=location,broadmarkets=broadmarkets,gender_subset=gender_subset,year_subset=year_subset, va_var=va_var, subsample=subsample)
  }
  return(dd)
}

market_combinations <- rbindlist(list(
  data.table(ind='naics3',loc='cz',br=TRUE),
  data.table(ind='supersector',loc='cz',br=TRUE),
  data.table(ind='naics2',loc='county',br=TRUE),
  data.table(ind='naics2',loc='state',br=TRUE)
))


