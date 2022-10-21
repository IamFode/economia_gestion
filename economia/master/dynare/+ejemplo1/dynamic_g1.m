function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
% function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T             [#temp variables by 1]     double   vector of temporary terms to be filled by function
%   y             [#dynamic variables by 1]  double   vector of endogenous variables in the order stored
%                                                     in M_.lead_lag_incidence; see the Manual
%   x             [nperiods by M_.exo_nbr]   double   matrix of exogenous variables (in declaration order)
%                                                     for all simulation periods
%   steady_state  [M_.endo_nbr by 1]         double   vector of steady state values
%   params        [M_.param_nbr by 1]        double   vector of parameter values in declaration order
%   it_           scalar                     double   time period for exogenous variables for which
%                                                     to evaluate the model
%   T_flag        boolean                    boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   g1
%

if T_flag
    T = ejemplo1.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(7, 12);
g1(1,3)=(-((1+params(1))*(1+params(5))))/(y(3)*y(3));
g1(1,10)=(-((1+params(4)*y(11)/y(4)-params(3))*(-params(2))/(y(10)*y(10))));
g1(1,4)=(-(params(2)/y(10)*(-(params(4)*y(11)))/(y(4)*y(4))));
g1(1,11)=(-(params(2)/y(10)*params(4)/y(4)));
g1(2,3)=1;
g1(2,6)=(-1);
g1(2,7)=1;
g1(3,1)=(-(T(3)*y(9)*getPowerDeriv(y(1),params(4),1)));
g1(3,5)=(-(T(2)*getPowerDeriv(y(5),1-params(4),1)));
g1(3,6)=1;
g1(3,9)=(-(T(1)*T(3)));
g1(4,1)=1-params(3);
g1(4,4)=(-((1+params(1))*(1+params(5))));
g1(4,7)=1;
g1(5,3)=params(6)/(1-y(5));
g1(5,5)=y(3)*params(6)/((1-y(5))*(1-y(5)))-(-(y(6)*(1-params(4))))/(y(5)*y(5));
g1(5,6)=(-((1-params(4))/y(5)));
g1(6,2)=(-(params(7)*1/y(2)));
g1(6,9)=1/y(9);
g1(6,12)=(-1);
g1(7,5)=(-((-y(6))/(y(5)*y(5))));
g1(7,6)=(-(1/y(5)));
g1(7,8)=1;

end
