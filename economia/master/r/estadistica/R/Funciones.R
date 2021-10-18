# Funciones

## función coeficiente de varianción
cv = function(x){ #Fucnión que calculo el coeficiente de variación de un vector de datos x
  sqrt(var(x)*(length(x)-1)/length(x))/mean(x)
}

## Moda
moda = function(x){ # Función que cálculo la moda de un vector de datos x
   sort(table(x), TRUE)[1]
}