var c k h y i yh zeta; //variables endogenas 
/*
c -> Consumo
k -> Capital
h -> Horas trabajadas
y -> Output (Producción)
i -> Inversión
yh -> Productividad
zeta -> Variables estocástica, choque de productividad.
*/

varexo e ; //variables exogenas 
// e -> Epsilon del autoregresivo.

parameters gam beta delta alfa n sg rho sge ZETA; // parametros

gam=0.0139; //Tasa de crecimiento de la productividad per-cápita.
beta=0.9575; //Factor de descuento
delta=0.0262; //Factor de depreciación o tasa de depreciación del capital
alfa=0.3; //Parámetro de función de producción
n=0.0173; //Tasa de crecimiento de la población
sg=2.5213; //Parametro de las preferencias que mide el peso del ocio en el bienestar de nuestros agentes.
rho=0.95; //Parametro autoregresivo del proceso autoregresivo.
sge=0.0079; //Desviación típica de la variable exogena
VLzeta=(sge^2)/(1-(rho^2)); //Varianza de z, la varianza de ruido sobre 1 menos 
zetaee=1.9174; //Constante del autoregresivo.
ZETA=log(zetaee)-(VLzeta/2); 
//ZETA=1.0916;

model;
(1+gam)*(1+n)/c=(beta/c(+1))*((alfa*y(+1)/k)+1-delta);
c+i=y;
y=zeta*(k(-1)^alfa)*(h^(1-alfa));
i=((1+gam)*(1+n)*k)-((1-delta)*k(-1));
sg*c/(1-h)=(1-alfa)*y/h;
log(zeta)=(rho*log(zeta(-1)))+((1-rho)*ZETA)+e;
yh=y/h;
end;

initval;
k=2;
c=0.6;
h=0.2;
y=0.8;
i=1-0.6;
yh=0.8/0.2;
zeta=1.4;
/*
k=2.9;
c=0.8329;
h=0.25;
y=1;
i=1-0.8329;
yh=1/0.25;
zeta=1.9174;
*/
end;

steady;
check;

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



shocks;
var e = sge^2;
end;

stoch_simul(order=1,periods=200,irf=30);

