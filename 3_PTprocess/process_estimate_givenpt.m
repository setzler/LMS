function [par] = process_estimate_givenpt(M1,gap0,opts)
% We estimate the VA and earnings processes, 
% taking the permanent pass-through as given.

mpol t1_y t2_y t1_w t2_w mu_y mu_w e_y e_w gat ; 
nlag = 5;

% the matrix size should be nlag + nlag + 3
% where ty1 is theta1 for y
%     mu1, mu2, mu3, eps1,    eps2,       eps3,        eps4         eps5 

Amu   = [];
Aeps  = [];
for nl=1:nlag
  Amu = [Amu;zeros(1,nl-1) 1, zeros(1,nlag-nl)];
  Aeps= [Aeps;  zeros(1,nl-1) ,1, t1_y-1,  t2_y-t1_y, -t2_y,zeros(1,nlag-nl)];
end
A = [Amu Aeps];

Omega = mpol(zeros(2*nlag+3));
for nl=1:nlag
  Omega(nl,nl) = mu_y;    
end
for nl=(nlag+1):(2*nlag+3)
    Omega(nl,nl) = e_y;
end


if (opts.ma1==1)
    t2_y=0;
    Amu   = [];
    Aeps  = [];
    for nl=1:nlag
      Amu = [Amu;zeros(1,nl-1) 1, zeros(1,nlag-nl)];
      Aeps= [Aeps;  zeros(1,nl-1) ,1, t1_y-1,  -t1_y, 0,zeros(1,nlag-nl)];
    end
    A = [Amu Aeps];
end

if (opts.ma2==1)
    t2_y=0;
    t1_y=0;
    Amu   = [];
    Aeps  = [];
    for nl=1:nlag
      Amu = [Amu;zeros(1,nl-1) 1, zeros(1,nlag-nl)];
      Aeps= [Aeps;  zeros(1,nl-1) ,1, 0-1,  -0, 0,zeros(1,nlag-nl)];
    end
    A = [Amu Aeps];
end

      
Mm    = A * Omega * A';

% extract VA components from cov matrix
Md    = M1(2:6,2:6); 

% compute Normal distribution weights
WW = Md;
for i = 1:size(WW,1)
    for j = 1:size(WW,1)
        WW(i,j) = Md(i,j)^2+ Md(i,i)*Md(j,j);
    end
end
WW = WW/mean(WW(:));

if (opts.nweight)
    ALL = (Mm - Md).^2 .* WW.^-1;
else 
    ALL = (Mm - Md).^2 ;
end

K = [e_y>=0; mu_y>=0]; 


F = sum( ALL(:));
P = msdp(min(F),K);
msol(P);
par1 = double([t1_y t2_y mu_y e_y ])
double(Mm);
Md;

%% Earning process with firm pass-through

% here we need to construct in addition the earning process
% it is very similar and includes the muy shock

% 1) building the process 
Amu2   = [];
Aeps2  = [];
for nl=1:nlag
  Amu2 = [Amu2;zeros(1,nl-1) 1, zeros(1,nlag-nl)];
  Aeps2= [Aeps2;  zeros(1,nl-1) ,1, t1_w-1,  t2_w-t1_w, -t2_w,zeros(1,nlag-nl)];
end

Omega2 = mpol(zeros(2*nlag+3));
for nl=1:nlag
  Omega2(nl,nl) = mu_w;    
end
for nl=(nlag+1):(2*nlag+3)
    Omega2(nl,nl) = e_w;
end

% 2) Extend the firm process matrix with 0 for the worker shocks
if (opts.ptrans==0)
    gat=0;
end

if (opts.ma1==1)
    t2_w=0;
    Amu2   = [];
    Aeps2  = [];
    for nl=1:nlag
      Amu2 = [Amu2;zeros(1,nl-1) 1, zeros(1,nlag-nl)];
      Aeps2= [Aeps2;  zeros(1,nl-1) ,1, t1_w-1,  0-t1_w, -0,zeros(1,nlag-nl)];
    end
end

if (opts.ma2==1)
    t2_w=0;
    t1_w=0;
    Amu2   = [];
    Aeps2  = [];
    for nl=1:nlag
      Amu2 = [Amu2;zeros(1,nl-1) 1, zeros(1,nlag-nl)];
      Aeps2= [Aeps2;  zeros(1,nl-1) ,1, 0-1,  0-0, -0,zeros(1,nlag-nl)];
    end
end


A = [Amu      double(Aeps)      zeros(nlag,2*nlag+3);     
     Amu*gap0  double(Aeps)*gat Amu2 Aeps2];        




Om = [ double(Omega) 0*Omega; 0*Omega Omega2];
Mm    = A * Om * A';

WW = M1;
for i = 1:size(WW,1)
    for j = 1:size(WW,1)
        WW(i,j) = M1(i,j)^2+ M1(i,i)*M1(j,j);
    end
end


WW = WW( 9:13,[2:6,9:13]);
WW = WW/mean(WW(:));
Md = M1( 9:13,[2:6,9:13]);

% sub select moments
Mm2 = Mm((nlag+1):(2*nlag),1:(2*nlag));

if (opts.nweight)
    ALL = (Mm2 - Md).^2 .* WW.^-1 ;
else
    ALL = (Mm2 - Md).^2 ;
end

F = sum( ALL(:));

% adding constraints
K = [e_w>=0; mu_w>=0]; 

P = msdp(min(F),K);
msol(P);

par2 = double([t1_w t2_w mu_w e_w gap0 gat]);
double(Mm2);
fprintf('[wages]      theta1=%4.4f theta2=%4.4f mu_var=%4.4f eps_var=%4.4f gamma_p=%4.4f gamma_t=%4.4f\n', par2 );
fprintf('[valueadded] theta1=%4.4f theta2=%4.4f mu_var=%4.4f eps_var=%4.4f\n', par1 );

par = [par1 par2];