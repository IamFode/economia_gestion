%
% Status : main Dynare file
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

if isoctave || matlab_ver_less_than('8.6')
    clear all
else
    clearvars -global
    clear_persistent_variables(fileparts(which('dynare')), false)
end
tic0 = tic;
% Define global variables.
global M_ options_ oo_ estim_params_ bayestopt_ dataset_ dataset_info estimation_info ys0_ ex0_
options_ = [];
M_.fname = 'ejemplo1';
M_.dynare_version = '5.0';
oo_.dynare_version = '5.0';
options_.dynare_version = '5.0';
%
% Some global variables initialization
%
global_initialization;
M_.exo_names = cell(1,1);
M_.exo_names_tex = cell(1,1);
M_.exo_names_long = cell(1,1);
M_.exo_names(1) = {'e'};
M_.exo_names_tex(1) = {'e'};
M_.exo_names_long(1) = {'e'};
M_.endo_names = cell(7,1);
M_.endo_names_tex = cell(7,1);
M_.endo_names_long = cell(7,1);
M_.endo_names(1) = {'c'};
M_.endo_names_tex(1) = {'c'};
M_.endo_names_long(1) = {'c'};
M_.endo_names(2) = {'k'};
M_.endo_names_tex(2) = {'k'};
M_.endo_names_long(2) = {'k'};
M_.endo_names(3) = {'h'};
M_.endo_names_tex(3) = {'h'};
M_.endo_names_long(3) = {'h'};
M_.endo_names(4) = {'y'};
M_.endo_names_tex(4) = {'y'};
M_.endo_names_long(4) = {'y'};
M_.endo_names(5) = {'i'};
M_.endo_names_tex(5) = {'i'};
M_.endo_names_long(5) = {'i'};
M_.endo_names(6) = {'yh'};
M_.endo_names_tex(6) = {'yh'};
M_.endo_names_long(6) = {'yh'};
M_.endo_names(7) = {'zeta'};
M_.endo_names_tex(7) = {'zeta'};
M_.endo_names_long(7) = {'zeta'};
M_.endo_partitions = struct();
M_.param_names = cell(9,1);
M_.param_names_tex = cell(9,1);
M_.param_names_long = cell(9,1);
M_.param_names(1) = {'gam'};
M_.param_names_tex(1) = {'gam'};
M_.param_names_long(1) = {'gam'};
M_.param_names(2) = {'beta'};
M_.param_names_tex(2) = {'beta'};
M_.param_names_long(2) = {'beta'};
M_.param_names(3) = {'delta'};
M_.param_names_tex(3) = {'delta'};
M_.param_names_long(3) = {'delta'};
M_.param_names(4) = {'alfa'};
M_.param_names_tex(4) = {'alfa'};
M_.param_names_long(4) = {'alfa'};
M_.param_names(5) = {'n'};
M_.param_names_tex(5) = {'n'};
M_.param_names_long(5) = {'n'};
M_.param_names(6) = {'sg'};
M_.param_names_tex(6) = {'sg'};
M_.param_names_long(6) = {'sg'};
M_.param_names(7) = {'rho'};
M_.param_names_tex(7) = {'rho'};
M_.param_names_long(7) = {'rho'};
M_.param_names(8) = {'sge'};
M_.param_names_tex(8) = {'sge'};
M_.param_names_long(8) = {'sge'};
M_.param_names(9) = {'ZETA'};
M_.param_names_tex(9) = {'ZETA'};
M_.param_names_long(9) = {'ZETA'};
M_.param_partitions = struct();
M_.exo_det_nbr = 0;
M_.exo_nbr = 1;
M_.endo_nbr = 7;
M_.param_nbr = 9;
M_.orig_endo_nbr = 7;
M_.aux_vars = [];
M_ = setup_solvers(M_);
M_.Sigma_e = zeros(1, 1);
M_.Correlation_matrix = eye(1, 1);
M_.H = 0;
M_.Correlation_matrix_ME = 1;
M_.sigma_e_is_diagonal = true;
M_.det_shocks = [];
M_.surprise_shocks = [];
M_.heteroskedastic_shocks.Qvalue_orig = [];
M_.heteroskedastic_shocks.Qscale_orig = [];
options_.linear = false;
options_.block = false;
options_.bytecode = false;
options_.use_dll = false;
M_.orig_eq_nbr = 7;
M_.eq_nbr = 7;
M_.ramsey_eq_nbr = 0;
M_.set_auxiliary_variables = exist(['./+' M_.fname '/set_auxiliary_variables.m'], 'file') == 2;
M_.epilogue_names = {};
M_.epilogue_var_list_ = {};
M_.orig_maximum_endo_lag = 1;
M_.orig_maximum_endo_lead = 1;
M_.orig_maximum_exo_lag = 0;
M_.orig_maximum_exo_lead = 0;
M_.orig_maximum_exo_det_lag = 0;
M_.orig_maximum_exo_det_lead = 0;
M_.orig_maximum_lag = 1;
M_.orig_maximum_lead = 1;
M_.orig_maximum_lag_with_diffs_expanded = 1;
M_.lead_lag_incidence = [
 0 3 10;
 1 4 0;
 0 5 0;
 0 6 11;
 0 7 0;
 0 8 0;
 2 9 0;]';
M_.nstatic = 3;
M_.nfwrd   = 2;
M_.npred   = 2;
M_.nboth   = 0;
M_.nsfwrd   = 2;
M_.nspred   = 2;
M_.ndynamic   = 4;
M_.dynamic_tmp_nbr = [3; 0; 0; 0; ];
M_.model_local_variables_dynamic_tt_idxs = {
};
M_.equations_tags = {
  1 , 'name' , '1' ;
  2 , 'name' , '2' ;
  3 , 'name' , 'y' ;
  4 , 'name' , 'i' ;
  5 , 'name' , '5' ;
  6 , 'name' , '6' ;
  7 , 'name' , 'yh' ;
};
M_.mapping.c.eqidx = [1 2 5 ];
M_.mapping.k.eqidx = [1 3 4 ];
M_.mapping.h.eqidx = [3 5 7 ];
M_.mapping.y.eqidx = [1 2 3 5 7 ];
M_.mapping.i.eqidx = [2 4 ];
M_.mapping.yh.eqidx = [7 ];
M_.mapping.zeta.eqidx = [3 6 ];
M_.mapping.e.eqidx = [6 ];
M_.static_and_dynamic_models_differ = false;
M_.has_external_function = false;
M_.state_var = [2 7 ];
M_.exo_names_orig_ord = [1:1];
M_.maximum_lag = 1;
M_.maximum_lead = 1;
M_.maximum_endo_lag = 1;
M_.maximum_endo_lead = 1;
oo_.steady_state = zeros(7, 1);
M_.maximum_exo_lag = 0;
M_.maximum_exo_lead = 0;
oo_.exo_steady_state = zeros(1, 1);
M_.params = NaN(9, 1);
M_.endo_trends = struct('deflator', cell(7, 1), 'log_deflator', cell(7, 1), 'growth_factor', cell(7, 1), 'log_growth_factor', cell(7, 1));
M_.NNZDerivatives = [23; -1; -1; ];
M_.static_tmp_nbr = [3; 0; 0; 0; ];
M_.model_local_variables_static_tt_idxs = {
};
M_.params(1) = 0.0139;
gam = M_.params(1);
M_.params(2) = 0.9575;
beta = M_.params(2);
M_.params(3) = 0.0262;
delta = M_.params(3);
M_.params(4) = 0.3;
alfa = M_.params(4);
M_.params(5) = 0.0173;
n = M_.params(5);
M_.params(6) = 2.5213;
sg = M_.params(6);
M_.params(7) = 0.95;
rho = M_.params(7);
M_.params(8) = 0.0079;
sge = M_.params(8);
VLzeta=(sge^2)/(1-(rho^2)); 
zetaee=1.9174; 
M_.params(9) = log(zetaee)-VLzeta/2;
ZETA = M_.params(9);
%
% INITVAL instructions
%
options_.initval_file = false;
oo_.steady_state(2) = 2;
oo_.steady_state(1) = 0.6;
oo_.steady_state(3) = 0.2;
oo_.steady_state(4) = 0.8;
oo_.steady_state(5) = 0.4;
oo_.steady_state(6) = 4;
oo_.steady_state(7) = 1.4;
if M_.exo_nbr > 0
	oo_.exo_simul = ones(M_.maximum_lag,1)*oo_.exo_steady_state';
end
if M_.exo_det_nbr > 0
	oo_.exo_det_simul = ones(M_.maximum_lag,1)*oo_.exo_det_steady_state';
end
steady;
oo_.dr.eigval = check(M_,options_,oo_);
kh=(((1+gam)*(1+n)-(beta*(1-delta)))/(beta*alfa*zetaee))^(1/(alfa-1));
a1=(zetaee*kh^(alfa-1))-((1+gam)*(1+n))+(1-delta);
a2=(1-alfa)*(kh^alfa)*zetaee/sg;
a3=(1-alfa)*(kh^(alfa-1))*zetaee/sg;
ke=a2/(a1+a3);
ce=a1*ke;
he=ke/kh;
ye=zetaee*(ke^alfa)*(he^(1-alfa));
ie=((1+gam)*(1+n)*ke)-((1-delta)*ke);
yhe=ye/he;
%
% SHOCKS instructions
%
M_.exo_det_length = 0;
M_.Sigma_e(1, 1) = M_.params(8)^2;
options_.irf = 30;
options_.order = 1;
options_.periods = 200;
var_list_ = {};
[info, oo_, options_, M_] = stoch_simul(M_, options_, oo_, var_list_);


oo_.time = toc(tic0);
disp(['Total computing time : ' dynsec2hms(oo_.time) ]);
if ~exist([M_.dname filesep 'Output'],'dir')
    mkdir(M_.dname,'Output');
end
save([M_.dname filesep 'Output' filesep 'ejemplo1_results.mat'], 'oo_', 'M_', 'options_');
if exist('estim_params_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'ejemplo1_results.mat'], 'estim_params_', '-append');
end
if exist('bayestopt_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'ejemplo1_results.mat'], 'bayestopt_', '-append');
end
if exist('dataset_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'ejemplo1_results.mat'], 'dataset_', '-append');
end
if exist('estimation_info', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'ejemplo1_results.mat'], 'estimation_info', '-append');
end
if exist('dataset_info', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'ejemplo1_results.mat'], 'dataset_info', '-append');
end
if exist('oo_recursive_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'ejemplo1_results.mat'], 'oo_recursive_', '-append');
end
if ~isempty(lastwarn)
  disp('Note: warning(s) encountered in MATLAB/Octave code')
end
