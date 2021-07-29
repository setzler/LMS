% requires installaltion of 
% - http://homepages.laas.fr/henrion/software/gloptipoly/
% - https://github.com/sqlp/sedumi
% - SOS tools (optional)


tic()
dd        = importdata('../data/matlab_CovMat.dat');
fields = fieldnames(dd);
disp(fields)

pt_ma1        = importdata('../data/matlab_ptMA1.dat');
pt_ma2        = importdata('../data/matlab_ptMA2.dat');


opts.nweight=1;
opts.ptrans=1;
all_res = {};

for idx = 1:length(fields)
    
   field_name = fields{idx};
   
   % MA2
   opts.ma1=0;
   opts.ma2=0;
   covmat = dd.(field_name);
   gap0 = pt_ma2.(field_name);
   res=process_estimate_givenpt(covmat,gap0,opts);
   all_res.([field_name,'_ma2']) = res;
   
   % MA1
   opts.ma1=1;
   opts.ma2=0;
   covmat = dd.(field_name);
   gap0 = pt_ma1.(field_name);
   res=process_estimate_givenpt(covmat,gap0,opts);
   all_res.([field_name,'_ma1']) = res;
   
end

save('../results/overall_passthrough_matlab_results.mat','all_res')
toc()




%%%%%%%%%%%%% Bootstraps


tic()
dd        = importdata('../data/matlab_CovMat_bootstraps.dat');
fields = fieldnames(dd);
disp(fields)

pt_ma1        = importdata('../data/matlab_bootstraps_ptMA1.dat');
pt_ma2        = importdata('../data/matlab_bootstraps_ptMA2.dat');


opts.nweight=1;
opts.ptrans=1;
all_res = {};

for idx = 1:length(fields)
    
   field_name = fields{idx};

   % MA1
   opts.ma1=1;
   opts.ma2=0;
   covmat = dd.(field_name);
   gap0 = pt_ma1.(field_name);
   res=process_estimate_givenpt(covmat,gap0,opts);
   all_res.([field_name,'_ma1']) = res;
   
end

save('../results/bootstrap_passthrough_matlab_results.mat','all_res')
toc()

opts.nweight=1;
opts.ptrans=1;
all_res = {};

for idx = 1:length(fields)
    
   field_name = fields{idx};

   % MA2
   opts.ma1=0;
   opts.ma2=0;
   covmat = dd.(field_name);
   gap0 = pt_ma2.(field_name);
   res=process_estimate_givenpt(covmat,gap0,opts);
   all_res.([field_name,'_ma2']) = res;
   
end

save('../results/bootstrap_passthrough_matlab_results_MA2.mat','all_res')
toc()




