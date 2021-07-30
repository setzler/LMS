
# nohup R CMD BATCH --no-save --no-restore LMS_sample.R logs/LMS_sample.txt &

clean_mainvars_data <- function(data_dir = '~/LMSdir/LMS/revision3'){
  
  # define valid states + DC
  valid_states = c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA",
                   "ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN",
                   "MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA",
                   "RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY"
  )
  
  # define 2-digit NAICS codes of interest
  valid_naics2 = c("11","21","22","23","31","32","33","42","44","45","48","49","51","52","53","54","55","56","61","62","71","72","81")
  
  # define 3-digit NAICS codes of interest
  valid_naics3 = as.character(c(
    111:115,
    211:213,221,236:238,
    311:316,321:327,331:337,339,
    423:425,441:448,451:454,481:488,491:493,
    511,512,515,517:519,521:525,531:533,541,551,561,562,
    611,621:624,
    711:713,721,722,
    811:814
  ))

  # note: dollar variables were already converted to 2015 USD using CPI, then converted to log-units
  # note: cz variable was constructed using 1990 commuting zone definition linked to firm's zipcode
  # note: variables were renamed so that true IRS names are not disclosed
  flog.info('begin loading data')
  inputdata = setDT(readRDS(file=sprintf("%sfull_sample.rds",data_dir))) 
  inputdata <- inputdata[,list(worker_ID,firm_ID,years,naics,cz,county,state,age,gender,wages,va,grossinc,netinc,wagebill_total,firmsize_total,foreign_tax,ebitd,vanet,operprofits)]; gc()
  inputdata <- inputdata[!is.na(wages) & !is.na(va) & !is.na(naics) & !is.na(cz) & !is.na(state) & !is.na(age)]
  
  # define industries
  inputdata[, naics2 := substr(naics,1,2)]
  inputdata[, naics3 := substr(naics,1,3)]
  inputdata[, goods := as.numeric(naics2 %in% c('11','21','23','31','32','33'))]
  inputdata[goods==1, sector := "Goods"]
  inputdata[goods==0, sector := "Services"]
  
  # keep only observations in valid states and industries
  inputdata = inputdata[state %in% valid_states]
  inputdata = inputdata[naics2 %in% valid_naics2]
  inputdata = inputdata[naics3 %in% valid_naics3]
  
  # define regions
  inputdata[state %in% c('CT','MA','ME','NH','RI','VT'), region := 'NewEngland']
  inputdata[state %in% c('NJ','NY','PA'), region := 'MidAtlantic']
  inputdata[state %in% c('IL','IN','MI','OH','WI'), region := 'MidwestEast']
  inputdata[state %in% c('IA','KS','MN','MO','ND','NE','SD'), region := 'MidwestWest']
  inputdata[state %in% c('DC','DE','FL','GA','MD','NC','SC','VA','WV'), region := 'SouthAtlantic']
  inputdata[state %in% c('AL','KY','MS','TN'), region := 'SouthEast']
  inputdata[state %in% c('AR','LA','OK','TX'), region := 'SouthWest']
  inputdata[state %in% c('AZ','CO','ID','MT','NM','NV','UT','WY'), region := 'WestMountain']
  inputdata[state %in% c('AK','CA','HI','OR','WA'), region := 'WestPacific']
  inputdata[region %in% c('NewEngland','MidAtlantic'), superRegion := 'Northeast']
  inputdata[region %in% c('MidwestEast','MidwestWest'), superRegion := 'Midwest']
  inputdata[region %in% c('SouthEast','SouthWest','SouthAtlantic'), superRegion := 'South']
  inputdata[region %in% c('WestPacific','WestMountain'), superRegion := 'West']

  # construct a few other useful variables
  flog.info("Imposed valid states and industry codes, sample size %s",nrow(inputdata))
  inputdata[, firmsize := .N, list(firm_ID,years)]
  inputdata[, firmsize := as.numeric(firmsize)]
  inputdata[, firmsize := log(firmsize)]
  inputdata[, broadmarket := paste0(superRegion,'_',sector)]
  
  # variables used for subsetting
  inputdata <- inputdata[order(worker_ID,years)]
  inputdata[, n := 1:.N, by='worker_ID']
  inputdata[, init_wage := wages[n==1], by='worker_ID']
  inputdata[, tenure := cumsum(!is.na(wages)), by=c('worker_ID','firm_ID')]
  inputdata[, `:=`(maxfirmsize = max(exp(firmsize)), minfirmsize = min(exp(firmsize))), by='firm_ID']
  inputdata[, bigfirm := (minfirmsize >= 100)]
  inputdata[, begin_tenure := min(years), list(worker_ID,firm_ID)]
  inputdata[, total_tenure := max(tenure), list(worker_ID,firm_ID)]
  inputdata[, mean_firm_wage := mean(wages), list(firm_ID)]

  # export
  flog.info('cleaned variables, drop unnecessary info, final sample size %s',nrow(inputdata))
  saveRDS(inputdata,file=sprintf("%sfull_sample_mainvars_withmarkets.rds",data_dir),compress=F)
  flog.info('saved final data, done.')

}


