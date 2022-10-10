"""
######################################################
############### FUNCIÓN DE PRODUCCIÓN. ###############
######################################################

    Y_t = AF[K_t,e^(gamma*t)*L_t] 

- Y -> Producción. Se obtiene usando:
- A>0 -> Productividad total de los factores. Variable exógena. Se insiste en endogeneizar esta variable.
- F -> Función de producción neoclásica.
- K -> Capital 
- gamma -> Tasa de crecimiento de la eficiencia. Refleja el progreso tecnológico.
- t -> tiempo.
- e^(gamma*t)>=0 -> Eficiencia. Cuanto mayor sea, más outputs se puede conseguir a partir del mismo input trabajo. Cuando aumenta t, este término se hace más grande.  
- e^(gamma t) L_t -> El trabajo medido en unidades de eficiencia o trabajo efectivo. Significa que la eficiencia del trabajo aumenta tras pasado el tiempo. La eficiencia del trabajo aumenta cuando gamma aumenta. Lo que hacemos  aquí, es ver como el progreso tecnológico aumenta la eficiencia del trabajo.
- L -> Trabajo.

    Suponemos que la fuerza de trabajo es un variable exógena. El número de trabajadores en el tiempo t es igual a e^(nt)L_0 donde L_0 es una constante inicial. 

    L_t = e^(nt)L_0

    Supongamos n=0, entonces L_t = L_0. Es decir, el número de trabajadores en el tiempo inicial.

- n -> Tasa de crecimiento de la población.
        Si n=0.1 y t=años 
        nt indica que la población está creciendo al 1% anual.


######################################################
######## EL TIEMPO Y LA TASA DE CRECIMIENTO. #########
######################################################

Supondremos que el tiempo será continuo. 
Y las variables son diferenciables en el tiempo.

    X_t = dX_t/dt = Variación de X_t en el tiempo = X_(t+1) - X_t

La tasa de crecimiento del trabajo es:

    L_t = e^(nt)L_0 => L_t = ne^(nt)L_0 = nL_t => L_t/L_t=n.

La tasa de progreso tecnológico es:

    T_t = e^(gamma t) => T_t = gamma e^(gamma t) = gamma T_t => T_t/T_t=gamma.

La tasa de crecimiento del trabajo efectivo es:

    E_t / E_t = T_t/T_t + L_t/L_t = gamma + n


######################################################
#### OTROS DOS TIPOS DE PROGRESO TÉCNICO NEUTRAL. ####
######################################################
Hemos supuesto que lo que aumenta con el tiempo es la eficiencia del factor trabajo. Es decir,

    E_t = e^(gamma t) L_t

Podemos definir otras hipótesis alternativas. Podemos suponer que:

    Y_t=AF[e^(gamma t)*K_t, L_t]

La eficiencia aumenta el factor capital.

O, 
    Y_t=e^(gamma t)AF[K_t, L_t]

Aumenta la eficiencia de todos los factores de producción.


######################################################
###### PROPIEDADES DE LA FUNCIÓN DE PRODUCCIÓN. ######
######################################################

#-----------------------------------------------------
######### RENDIMIENTOS CONSTANTES A ESCALA. ##########
#-----------------------------------------------------

- La función de producción es homogénea de grado uno.

    AF(mu*K_t,mu*e^(gamma t)L_t) = mu*AF(K_t,e^(gamma t)L_t) para todo mu>0.

La hipótesis significa que si aumentamos o disminuimos en una determina proporción la cantidad usada de todos los inputs productivos, entonces la escala de la producción aumenta o disminuye en la misma proporción. Esto significa que:
    - Puedo producir replicar una misma empresa. 
    - En el largo plazo suponemos rendimientos constantes a escala. 
    - Tengo una fábrica que produce 1000 productos, podría duplicar la fábrica para producir más quesos o invertir más en una fábrica para producir más quesos. Es decir, no importa como os producen solo miro el agregado. La cantidad es lo que importa. Por lo que suponga sólo existe una única empresa. 

#-----------------------------------------------------
############## LA PRODUCTIVIDAD MARGINAL #############
#-----------------------------------------------------

- La función de producción es creciente en ambos argumentos 
    Esto quiere decir que:

        PM_k = A partial F/partial K > 0 y PM_L = A partial F/partial (e^(gamma*t)L_t) > 0

    Si aumentamos el factor de cualquier input, la producción aumenta.

- Luego la productividad marginal es decreciente, es decir
    
    partial PM_k/partial K = A partial^2 F/partial K^2 < 0 y partial PM_L/partial L = A partial^2 F/partial (e^(gamma*t)L_t)^2 < 0

La función de producción es concava. Cuando aumentamos el uso de un input, manteniendo constante el otro, la productividad aumenta, pero cada vez aumenta menos.

#-----------------------------------------------------
############## LAS CONDICIONES DE INADA ##############
#-----------------------------------------------------

- La productividad marginal tiende a 0 cuando la cantidad usada del factor tiende al infinito.

    lim_(K_t to infty) partial F / partial K_t = lim_(e^(gamma*t)L_t to infty) partial F / partial (e^(gamma*t)L_t) = 0.

- La productividad marginal tiende a infinito cuando la cantidad usada del factor tiende a cero:

    lim_(K_t to 0) partial F / partial K_t = lim_(e^(gamma*t)L_t to 0) partial F / partial (e^(gamma*t)L_t) = infty.

######################################################
## LA PRODUCTIVIVIDAD Y LA INTENSIDAD DE CAPITAL I ###
######################################################

Pro la propiedad de rendimientos constantes a escala, podemos escribir:

    Y_t = AF(K_t, e^(gamma*t)L_t) = e^(gamma*t) L_t AF(K_t/[e^(gamma*t)L_t], 1) = e^(gamma*t) L_t Af(k_t)
    
     Donde k_t = K_t/[e^(gamma*t)L_t] -> capital por unidad de trabajo efectivo y f es la función de (K_t/[e^(gamma*t)L_t], 1). Resolviendo:

    y_t = Af(k_t) donde y_t= Y_t/(e^(gamma*t)L_t). 

######################################################
LAS PRODUCTIVIDADES MARGINALES Y LA INTENSIDAD DE CAPITAL II
######################################################

    PM_k = Af'(k_t)

    PM_L = A[f(k_t)-k_tf'(k_t)] e^(gamma*t)

######################################################
LAS PRODUCTIVIDADES MARGINALES Y LA INTENSIDAD DE CAPITAL III
######################################################

Si derivamos,

    partial PM_K / partial K = A partial^2 F(K_t,e^(gamma*t)L_t)/partial K_t^2 Af''(k_t)*1/e^(gamma*t)L_t<0. => f''(k_t)<0.

Por lo tanto,

    lim_(k_t to infty) f'(k_t) = 0      y    lim_(k_t to 0) f'(k_t) = infty.


######################################################
###### LA FUNCIÓN DE PRODUCCIÓN COBB-DOUGLAS I #######
######################################################

La función de producción Cobb-Douglas es:

    Y_t = A K_t^alpha [e^(gamma*t)L_t]^beta

    Donde A>0 y 0<alpha, beta<1. Si beta = 1-\alpha, entonces la función es homogénea de grado 1

######################################################
#### COMPETENCIA PERFECTA Y MERCADO DE CAPITALES #####

- Suponemos competencia perfecta, el cual ninguna empresa influye en el mercado. Otra suposición es que las empresas alquilan sus empresas. Es decir todos los mercados de capital de alquilan. Eres diferente entre comprar y alquilar. 

######################################################
######## LA MAXIMIZACIÓN DE LOS BENEFICIOS ###########
######################################################

- La empresa en competencia perfecta, es decir toma los precios como dados, entonces maximiza los beneficios.
    max_(K_t,L_t) P_tAF(K_t,L_t) - W_tL_t - R_tK_t
"""
