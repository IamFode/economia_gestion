#dev.off() # Cierra gráficos que podamos 
rm(list=ls()) # elimina todos los objetos que están en la memoria


# forzar conversión de elementos
as.character(7)
as.numeric("7")
as.numeric(TRUE)
as.numeric(FALSE)


# Nos devuelve el tipo de objeto
myname <- 'Iñigo'
class(myname)


# preguntar la clase
is.numeric(7)
is.character(7)


w <- c(0,2,7)
paste("Raíz de ",w,"es",round(sqrt(w),3))

# generar vector vacio
x<-vector("numeric")


avg <- function(x) {
    sum(x)/length(x)
}

avg(c(2,3,5,6,5,4,2,4,4,4.5,4.4,5))


acxraw <- get.hist.quote(instrument="ACX.MC",start=as.Date("2005-09-01"))


