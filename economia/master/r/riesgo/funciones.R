
############### FORMULAS DE LIMPIEZA DE DATOS ##################
limpiar <- function(df) {
  df[,1] <- as.Date(df[,1], format = "%d.%m.%Y")
  df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], 
                            function(x) gsub("\\.", "", x))
  df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], 
                            function(x) gsub(",", ".", x))
  df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], 
                            as.numeric)
  return(df)
}
################################################################


################# SIMULACIÓN HISTÓRICA #########################
simHistorica = function(x,prob,horizonte){
  res = diff(x, lag=horizonte)
  last = tail(x,n=1)
  Var = c()
  for(i in res){
    Var = c(Var,last+i)
  }
  rent = c()
  for(i in Var){
    rent = c(rent,log(i/last))
  }
  VaR = unname(quantile(rent,probs=prob,rm.na=FALSE))
  ES = sum(rent[rent<=VaR])/length(rent[rent<=VaR])
  result = cbind(VaR,ES)
  return(result)
}
################################################################