############################ ANALISIS ESTADISTICO - Master BI y BD  ###############################
# Realizado por Araceli Macía Barrado


# Hacer uso del dataset "diamonds" que contendrá el precio (entre otras variables interesantes) de unos 54.000 diamantes.
#
# Objetivo : realizar distintos tipos de análisis estadístico de sus variables para intentar
# averiguar algún tipo de comportamiento oculto aparentemente en los datos. 
#
# Para ello os marco los siguientes pasos: tipos de variables, medidas de posición central, 
# medidas de dispersión, 
# distribución y relación entre ellas, más análisis de regresión
#
# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:
# price: Precio en dolares americanos
# carat: peso del diamante
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm 
# y: ancho en  mm 
# z: profundidad en mm 
# depth: porcentaje total de profundidad 
# table: anchura de la parte superior de diamante con relacion al punto mas ancho 

# Responde cada bloque cubriendo al menos lo indicado:

#install.packages("corrplot")


#Bibliografia utilizada:
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
#http://onlinestatbook.com/2/transformations/box-cox.html
#https://www.youtube.com/watch?v=HyJoMcTSomc#t=535.350851
#http://www2.uca.es/serv/fag/fct/downloads/tema1.pdf
#https://ocw.uca.es/pluginfile.php/281/mod_resource/content/1/ebrcmdr.c4.pdf
#http://halweb.uc3m.es/esp/Personal/personas/jmmarin/esp/ManualR/contrastes.html 


#Instalacion de las librerias necesarias para las funciones a utilizar.
library(ggplot2)
library(e1071)
library(prettyR)
library(corrplot)
library(car)
library(Rcmdr)
library(zoo)
library(lmtest)
library(MASS)
library(caTools)


dt<-as.data.frame(diamonds)  #cargo el dataframe
str(dt) #=> 53.940 observaciones en 10 variables.

# ------------ FUNCIONES USADAS ------------ #


#https://gist.github.com/mrdwab/6424112
stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}


AnalisisVariable <- function ( variable , nameVar){

    TitleH<- c("Histograma de ",  nameVar)
    par(mfrow=c(2,2))
    boxplot(variable)
    hist(variable, main =  TitleH)
    
    # muestra los graficos para valorar la normalidad 
    plot(density(variable))
    qqnorm(variable)
    qqline(variable, col=2)

    par(mfrow=c(1,1))
}

#Funcion para hacer el test de correlacion entre variables del dataset.
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


#########################################################################
#SIGNIFICADO DE LAS VARIABLES
#Antes de realizar el ejercicio, me he estado informando en internet de que significan
#las variables que tenemos, para entender mejor los datos.
#ejemplo, web que he revisado : http://www.lucejoyasplata.com/aprende_diamantes.jsp

#voy a "mirar" un poco los datos.
pc<-max(dt$price) #el precio mas caro 
dt[dt$price==pc,]
#El precio mas caro tiene corte premium, color I( que no es corte mejor), y claridad VS2 ( que tampoco es la mejor)
#sin embargo de carat tiene 2.29

#a ver cuanto pesa el diamante con mas kilates.
pc<-max(dt$carat)
dt[dt$carat==pc,]
#el diamantes con mas kilates, cuesta un poco menos que el anterior ( y eso que duplica el peso)
# y tiene color J, que es el peor, y claridad I1 que tambien el peor.

#De momento me resulta llamativo que el precio mayor tenga caractiristicas de cut, color y claridad, que no 
#son las mejores con un peso, o que dependa del peso con caracteristicas peores.
summary(dt$carat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2000  0.4000  0.7000  0.7979  1.0400  5.0100 
summary(dt$price)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 326     950    2401    3933    5324   18820 

#voy a ver cuanto cuesta un diamente que tenga las mejores caracteristicas de cut, color y claridad.
pr<-  dt[dt$cut=="Ideal",]
par(mfrow=c(2,2))
hist(pr$price) 
hist(pr$carat)
boxplot(pr$price~pr$color)
boxplot(pr$price~pr$clarity)
#en los graficos podemos ver que con el corte Ideal, tenemos variedad de precios, kilates y 
#tipo de color y claridad.
#No parece claro que solo con el corte podamos establecer ninguna relacioN.

#vamos a ver que pasa con las mejores carasteristicas del diamante.

pr<-  dt[ (dt$clarity=="IF" & dt$cut=="Ideal" & dt$color=="D"),]
par(mfrow=c(1,1))
plot(pr$price~pr$carat) 
#Es curioso, parece las mejores caracteristicas se da como en 3 grupos de intervalos de kilates, y el
#precio no termina de definirse.. para iguales caracteristicas y kilates, el precio es distinto, 
#incluso vemos que existe un precio mayor para un kilate que para uno con algo.. 

#voy a ver la diferencia entre los puntos de mayor precio y mayor kilates
pr[pr$price==max(pr$price),] #table es 56 ,depth 62 y menor kilates mayor precio
pr[pr$carat==max(pr$carat),] #table es 54, depth 60 y mayor kilates menor precio.

#Es decir, que parece que el precio del diamante depende del peso..pero no siempre se da..
#¿ Podriamos entender que los datos de depth y table nos indican que la forma o proporcion
#del diamante son mejores en el caso 1, y que por ello, aunque tenga mas kilates tiene precio
#inferior? 

par(mfrow=c(2,2))
plot(dt$price~dt$depth) #parece que  no existe correlacion, se ve que los mayores precios estan entre un rango de valores depth..
plot(dt$price~dt$table)#parece que  no existe correlacion, se ve que los mayores precios estan entre un rango de valores table..
plot(dt$price~dt$carat)#aqu se ve algo de correlacion entre ambas.
plot(dt$depth~dt$table) #veo que table va por rangos, no es continua, y que puntos de depth pertenecen a distintos rango de table.

qplot(carat,price,data=dt,facets=color~cut,color=clarity)

#En este grafico podemos ver que existe cierta relacion entre precio y carat, pero esta relacion
#depende de la combinacion de las caracteristicas de cut, color y clarity.
#y seguramente las variables de tamaño ( depth,x,z) tendran tambien mucho que aportar para la definicion del precio.

qplot(depth,price,data=dt,facets=color~cut,color=clarity)
#En este grafico podemos ver que deph se mueve en un intervalo de valores, que existe mas dispersion con el corte peor
#y a  medida de que el corte es mejor se va a ajustando a un intervalo de valores mas pequeño.

qplot(table,price,data=dt,facets=color~cut,color=clarity)
#En este grafico podemos ver que table se mueve en un intervalo de valores, que existe mas dispersion con el corte peor
#y a  medida de que el corte es mejor se va a ajustando a un intervalo de valores mas pequeño.
#pero los precios en definitiva se mueven dependiendo de las caracteristicas de cut, color y clarity.

#########################################################################
#Muestra representativa
# Selecciona una muestra representativa para "cut"
########################################################################

#variables de tipo factors son cut, color y clarity.

table(dt$cut)  # para ver el numero de filas de cada tipo de cut.
# Fair      Good Very Good   Premium     Ideal 
# 1610      4906     12082     13791     21551 
proporcion<-data.frame(prop.table(table(dt$cut)))
print (proporcion)

nMuestra<-5000  #Tamaño de la muestra que he decidido tratar.

porcentaje <- (nMuestra )/ nrow(dt)  #calculo el porcentaje para el tamaño de la muestra que quiero.

#voy a estratificar por corte, color y claridad, table ( table la selecciono porque va como por rangos)
test <- stratified(dt, c("cut", "color", "clarity","table"), size = porcentaje)

#test <- sample.split(dt$cut, SplitRatio = porcentaje) #Esta serie una funcion para estratificar solo por corte.
dtMCut <- test
table(dtMCut$cut)
proporcion2<-data.frame(prop.table(table(dtMCut$cut)))
print (proporcion2)

#La proporcion de la variable cut se mantiene , se ve al comparar las variables de proporcion y proporcion2.

# El resto de la practica lo hare con la muestra que he obtenido.

####################################################################################
#Analisis de las variables
# Analisis descriptivo de las variables: Tipo de variable, distribuci?n y representaci?n
# Deteccion de casos atipicos y su tratamiento
####################################################################################
str(dt)

#-----------   tipos de variable:
#carat : variable cuantitativa continua
#cut: variable cualitativa
#color : variable cualitativa
#clarity: variable cualitativa
# depth:cuantitativa continua
# table:cuantitativa continua
# price :cuantitativa continua
# x,y,z :cuantitativa continua



# ** => A partir de ahora, trabajare siempre con el dataframe de la muestra:dtMCut
#-----------   distribucion y representacion y Deteccion de casos atipicos y su tratamiento


# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable carat :# carat: peso del diamante
 
AnalisisVariable(dtMCut$carat,"carat")
summary(dtMCut$carat) # no hay valores invalidos.

# Casos atipicos los podemos ver en el boxplot.
# los buscamos y los quitamos. Con ello estamos quitando los diamantes con mayor kilates.
maxi<-quantile(dtMCut$carat,.75)+1.5*IQR(dtMCut$carat)  
dtMCut<-dtMCut[dtMCut$carat<maxi,]
AnalisisVariable(dtMCut$carat,"carat")
skew(dtMCut$carat) # es positiva, es sesgada a la derecha.
kurtosis(dtMCut$carat)
shapiro.test(dtMCut$carat)


#En boxplot se ve que la varianza esta equilibrada y que la mediana 
#esta en el centro, y luego vemos por los datos ( Summary) que la media y la mediana tienen valores muy cercanos
#entre ellas. 
# En el grafico de qqplot, se puede apreciar, que tenemos bastantes puntos en comun con una 
#distribucion normal, pero nuestra variable tiene picos,y aunque se asemeja no llega a ser del todo normal.
# El test de Kurtosis, devuelve un valor negativo, lo que significa que sigue una distribucion platicurtica.
# Finalmente, para comprobar la normalidad, he utilizado el test de shapiro que devuelve un valor 
#muy pequeño y < 0,05, por lo que podemos rechazar que siga una distribucion normal.

 

#  *-*-* *-*-* *-*-* *-*-*  variable cut

#cut: variable cualitativa =>  calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# distribucion y representacion y Deteccion de casos atipicos y su tratamiento

# la variable cut es un factor, la convierto a valores, para poder trabajar con ella.
# Pero para analizarlo utilizo otra variable.

table(dtMCut$cut) # aqui podemos ver el numero de corte
DatosCut <- as.numeric(dtMCut$cut)
table(DatosCut) #"1.Fair 2.Good 3.very good 4.Premium 5.Ideal"
AnalisisVariable(DatosCut,"Cut")
shapiro.test(DatosCut)


 # el histograma, para este tipo de variables, indica lo que ya vemos en
#con los numeros, que el tipo de corte que mas abunda es el Ideal, y luego los de good y very good
#tienen valores muy cercanos.
# He ralizado el test de shapiro, para confirmar que efectivamente no es una distribucion normal.
# El test devuelve un valor negativo ( inferior a 0.05)



# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable color :#  colour: color del diamante (desde D el mejor hasta J el peor)
table(dtMCut$color) # es un factor, con esta instruccion vemos la distribucion por color.
#lo pasamos a numeros
DatosColor<-as.numeric(dtMCut$color)
summary(DatosColor) 
table(DatosColor)
AnalisisVariable(DatosColor,"color") # los colores que mas abundan son del 2 al 4.
shapiro.test(DatosColor)
#Al ser variables cualitativas, en los graficos se puede ver que no tiene nada que ver 
#con la distribucion normal. Aun asi, he realizado el test de shapiro, que arroja menor que 0.05,  y confirma
#que no es una distribucion normal.



# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
table(dtMCut$clarity) # es un factor, con esta instruccion vemos la distribucion por color.
Datosclarity<- as.numeric(dtMCut$clarity)
table(Datosclarity)
AnalisisVariable(Datosclarity,"Clarity") #La claridad que mas abunda son la 3 y 4.
shapiro.test(Datosclarity)
#Al ser variables cualitativas, en los graficos se puede ver que no tiene nada que ver 
#con la distribucion normal. Aun asi, he realizado el test de shapiro, que arroja valor negativo y confirma
#que no es una distribucion normal.


# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable  price: Precio en dolares americanos
summary(dtMCut$price) # no hay valores invalidos.
AnalisisVariable(dtMCut$price,"price")
#tiene bastante cola a la derecha, ya que tiene bastantes valores atipicos..precios muy altos..
#los vamos a filtrar.
maxi<-quantile(dtMCut$price,.75)+1.5*IQR(dtMCut$price)  
dtMCut<-dtMCut[dtMCut$price<maxi,]
AnalisisVariable(dtMCut$price,"price") 

#Sigue teniendo muchos valores altos, vuelvo a filtrar.
#Soy consciente de que al filtrar estoy quitando los datos de los diamantes que tienen un precio muy alto.
#asumo que puedo estar sesgando el analisis.

maxi<-quantile(dtMCut$price,.75)+1.5*IQR(dtMCut$price)  
dtMCut<-dtMCut[dtMCut$price<maxi,]
AnalisisVariable(dtMCut$price,"price") 



skew(dtMCut$price) # es positiva, es sesgada a la derecha.
kurtosis(dtMCut$price)
shapiro.test(dtMCut$price)
summary(dtMCut$price) #vuelvo a ver los datos despues de quitar los outliers

# hay mayor cantidad de diamantes en torno a los 2000 dolares, pero como hay diamantes que
# que tienen un precio muy alto, hace que la media  en comparacion a la mediana. 
# y eso tambien lo podemos apreciar en el boxplot, que se ve que hay varianza, ya que la mediana
#no esta en el "medio" de la caja. 

#Se podria decir que sigue una distribucion con asimetria positiva,y apuntamiento leptocurtico ( el test de Kurtosis es positivo).
#En el grafico de qqplot, vemos que se acerca a la recta de lo que seria la normal, pero no llega 
#a ser del todo normal.
# Con el test de shapiro, vemos que dar un resultado negativo y menor a 0,05 podemos rechazar que la 
# distribucion sea normal.


# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable  x: longitud en mm 

AnalisisVariable(dtMCut$x,"x") 
summary(dtMCut$x) # no hay valores invalidos.
skew(dtMCut$x)  # es positivo, sesgado a la derecha.
kurtosis(dtMCut$x)
shapiro.test(dtMCut$x)
#No tiene casos atipicos que limpiar. 
#no sigue una distribucion clara.  En boxplot se ve que la varianza esta equilibrada y que la mediana 
#esta en el centro, y luego vemos por los datos que la media y la mediana tienen valores cercanos 
# En el histograma vemos con lo cual no se asemeja a una distribucion normal.
#en el grafico de qqplot lo podemos ver, que tiene bastantes puntos en comun en el centro, pero se aleja 
# en las colas.
# Con el test de shapiro, vemos que dar un resultado negativo y menor a 0,05 podemos rechazar que la 
# distrubucion sea normal.
#El test de Kurtosis, devuelve un valor negativo, lo que significa que sigue una distribucion platicurtica.


# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable  y: ancho en mm 
AnalisisVariable(dtMCut$y,"y") 
summary(dtMCut$y) # no hay valores invalidos.
skew(dtMCut$y) # es positivo, sesgado a la derecha.
kurtosis(dtMCut$y)
shapiro.test(dtMCut$y)
#No tiene casos atipicos que limpiar. 
#los datos son casi iguales que los de la variable x, con lo cual las apreciaciones son las mismas.

# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******   z: profundidad en mm 

AnalisisVariable(dtMCut$z,"z") 
summary(dtMCut$z) # no hay valores invalidos.
skew(dtMCut$z) # es positivo, sesgado a la derecha.
kurtosis(dtMCut$z)
shapiro.test(dtMCut$z)

#No tiene casos atipicos que limpiar. 
#los datos siguen el mismo patron que las variables x e y, con lo cual las apreciaciones son las mismas.

# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  depth: porcentaje total de profundidad 
AnalisisVariable(dtMCut$depth,"Depth") 
summary(dtMCut$depth) # no hay valores invalidos.

#En el boxplot se ve que hay muchos puntos atipicos, vamos a limpiarlos.
maxi<-quantile(dtMCut$depth,.75)+1.5*IQR(dtMCut$depth)  
dtMCut<-dtMCut[dtMCut$depth<maxi,]
mini<-quantile(dtMCut$depth,.25)-1.5*IQR(dtMCut$depth)  
dtMCut<-dtMCut[dtMCut$depth>mini,]

AnalisisVariable(dtMCut$depth,"Depth")  #hemos limpiado
summary(dtMCut$depth)
sd(dtMCut$depth)
skew(dtMCut$depth) # es negativa, sesgado a la izquierda.
kurtosis(dtMCut$depth)
shapiro.test(dtMCut$depth)


#Una vez limpiados los valores, se puede ver que esta variable tampoco  sigue una distribucion normal.
#aun una varianza muy cercana a 1, y con una media y mediana practicamente iguales.
#el test de shapiro devuevle un valor negativo, inferior a 0,05, con lo que no podemos aceptar que sigue una
#distribucion normal
#el test de kurtosis, nos da un valor muy pequeño muy cercano a 0, lo que indica que 
#sigue una mesocurtica


# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ****** table: anchura de la parte superior de diamante con relacion al punto mas ancho 
AnalisisVariable(dtMCut$table,"table") 
summary(dtMCut$table) # no hay valores invalidos.

#vamos a limpiar los puntos atipicos que se aprecian en el boxplot.
maxi<-quantile(dtMCut$table,.75)+1.5*IQR(dtMCut$table)  
dtMCut<-dtMCut[dtMCut$table<maxi,]
mini<-quantile(dtMCut$table,.25)-1.5*IQR(dtMCut$table)  
dtMCut<-dtMCut[dtMCut$table>mini,]
AnalisisVariable(dtMCut$table,"table")  #hemos limpiado los inferiores
summary(dtMCut$table)
sd(summary(dtMCut$table))
unique(dtMCut$table)
skew(dtMCut$table) # es positiva, sesgado a la derecha
kurtosis(dtMCut$table)
shapiro.test(dtMCut$table)

#No sigue para nada una distribucion normal. Aunque la media y la mediana sean casi iguales, 
#los valores suben y bajan, tal y como se ven en el diagrama de densidad y en el histograma.
#el test de shapiro, devuelve un valor negativo, lo que indica que podemos rechazar que siga una distribucion normal.
# parece una distribucion de Bernoulli, donde la variable parace tomar valores cercanos a 7 valores 
# concretos 54,55,56,57,58,59 y 60. Seria un compartamiento comparable a una variable cuantitativa discreta.
AnalisisVariable(round(dtMCut$table),"table")  #hemos limpiado los inferiores
#El test de Kurtosis, devuelve un valor negativo, lo que significa que sigue una distribucion platicurtica.

####################################################################################
#Inferencia
# Calcula un intervalo de confianza para la media de "carat" y "depth"
# Formula un test de hipotesis
####################################################################################
# Carat: peso del diamente, y depth es porcentaje de profundidad.

#-----  Calcular el intervalo de confianza  (95% ) para carat.

#Formula
#P(media-((Za/2)*desviacion)/5 < X < media+((Za/2)*desviacion)/5) = 0,95
#pero voy a calcularlo utilizando t.test, que es la funcion de R que me hace el calculo, por defecto con 0,95 de confianza.

t.test(dtMCut$carat)
intervalo <- t.test(dtMCut$carat)$conf.int[1:2]
intinferior <-intervalo[1]
intsuperior<-intervalo[2]
cat ("el intervalo de confianza para la media de carat esta entre [" , intinferior , "," , intsuperior , "], siendo la media  ",mean(dtMCut$carat))

# #Lo muestro en grafico para verlo mejor.
par(mfrow=c(1,1))

plot(density(dtMCut$carat))
abline(v=mean(dtMCut$carat),col="red",lty=2, lwd=1)
abline(v=intinferior,col="blue",lty=2, lwd=1)
abline(v=intsuperior,col="blue",lty=2, lwd=1)
#como se puede ver, el intervalo de confianza es muy pequeño con valores muy cercanos a la media de carat. 

#http://www.sthda.com/english/wiki/histogram-and-density-plots-r-base-graphs
#http://www.dm.uba.ar/materias/analisis_de_datos/2008/1/teoricas/Teor5.pdf
#-----  Calcular el intervalo de confianza  (95% ) para depth
#P(media-((Za/2)*desviacion)/5 < X < media+((Za/2)*desviacion)/5) = 0,95

#para calcular el intervarlo de confianza utilizo t.test, esta vez el grado de confianza lo pongo en 90%

t.test(dtMCut$depth, conf.level = 0.90)
intervalo <- t.test(dtMCut$depth)$conf.int[1:2]
intinferior <-intervalo[1]
intsuperior<-intervalo[2]


cat ("el intervalo de confianza para la media de depth esta entre [", intinferior , "," , intsuperior , "] siendo la media ",mean(dtMCut$depth) )

 plot(density(dtMCut$depth))
 abline(v=mean(dtMCut$depth),col="red",lty=2, lwd=1)
 abline(v=intinferior,col="blue",lty=2, lwd=1)
 abline(v=intsuperior,col="blue",lty=2, lwd=1)
 #como se puede ver, el intervalo de confianza es muy pequeño con valores muy cercanos a la media de depth 
 
 
#------- Formula un test de hipotesis

 #Test de hipotesis de que la media de depth es 63 al 90% de grado de confianza, alfa vale 10.
 #como estoy diciendo que la media es igual a un valor, tengo dos colas como alternativa.
  t.test(dtMCut$depth, alternative = "two.sided", mu=63, conf.level = 0.90) 
  
 #El pvalue es pequeño y es inferior a 0.10, con lo que podemos rechazar la hipotesis nula. 
 #la media de la depth no puede ser 63. Ademas al ejecutar la formula ya vemos que dicho valor 
 #no esta dentro del intervalo de confianza.
  
#Observando los datos que hemos visto anteriormente sobre la variable cut, vemos que el numero de diamantes
#Premium es muy cercano al numero de diamantes Good. ASi que vamos a analizar, si ambos tienen como media
#el mismo precio.
  
#Hipotesis es que la media de los precios (price) de los diamentes de corte Good, es igual a la
#media de los precios de los de tipo Premium (cut), con un nivel de confianza de 95%.
  
#Sigo la formula de contraste de media para dos muestras independientes.
#la hipotesis nula es que la media de las dos muestras son iguales.
#Vamos a suponer que la normalidad de cada una de las observaciones, asi como que tienen la misma varianza, con
#objeto de poder realizar el contraste con la t de student.  

  
pricePremium<-dtMCut[dtMCut$cut=="Premium", 7]
priceGood<-dtMCut[dtMCut$cut=="Good", 7]
boxplot(pricePremium,priceGood) # vemos en un diagrama los dos variables que vamos a analizar, 
#en el boxplot vemos unas medias muy parecidas, pero a ver que dice el analisis.


#Hacemos el test con R, utilizando la funcion t.test  como en el ejemplo anterior, un nivel de confianza del 95%
#Utilizamos el parametro Paired False, porquee las muestras sobre las que vamos a realizar la hipotesis
#no tienen la misma longitud.
hipo <- t.test(pricePremium, priceGood,  alternative='two.sided',conf.level=.95, paired=FALSE)
hipo
hipo$p.value>0.05


#Si el p-value es > 0.05, no podemos rechazar la hipotesis de que la media de los diamentes Premium y Good
#sean iguales.
# en caso contrario, podemos decir que la media de los diamantes Premium y Good no son iguales.
# En esta caso podemos decir que la media del precio de los diamantes Premium y Good es la misma.



#  Voy a utilizar un contraste de independencia
#  formulo la hipotesis de que la variable cut ( calidad del corte) esta relacionada con el color.
# h0 es que los dos factores son independientes.
# h1 es que no son independientes.

#hago mi tabla de datos, de corte y color.
dtCulColor <- table(dtMCut$cut , dtMCut$color)
dtCulColor

chi <- chisq.test(dtCulColor,simulate.p.value = TRUE)
print (chi)
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)
#El resultado de p-value es  menor que 0,05. Podemos rechazar la h0, asi que podemos decir que no 
#no son independientes,  el corte y el color estan relacionados.


####################################################################################
#Relaciones entre las variables
# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlacion)
####################################################################################

#Vamos a ver las relaciones de dependencia entre todas las variables excepto x,y y z.

chi <- chisq.test(dtMCut$carat,dtMCut$cut, simulate.p.value = TRUE) #0.0004997501 No son indendientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05) 

chi <- chisq.test(dtMCut$carat,dtMCut$color, simulate.p.value = TRUE) #0.0004997501 No son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)

chi <- chisq.test(dtMCut$carat,dtMCut$clarity, simulate.p.value = TRUE) #0.0004997501 No son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)

chi <- chisq.test(dtMCut$carat,dtMCut$depth, simulate.p.value = TRUE) #0.002998501 no son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)
 #
chi <- chisq.test(dtMCut$carat,dtMCut$table, simulate.p.value = TRUE) #0.005997001 no son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)

chi <- chisq.test(dtMCut$carat,dtMCut$price, simulate.p.value = TRUE) #0.0004997501 no son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$cut,dtMCut$color, simulate.p.value = TRUE) #0.6736632 son INDEPENDIENTES
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)

chi <- chisq.test(dtMCut$cut,dtMCut$clarity, simulate.p.value = TRUE) #0.0004997501 NO son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$cut,dtMCut$depth, simulate.p.value = TRUE) #0.0004997501 No son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$cut,dtMCut$table, simulate.p.value = TRUE) # 0.0004997501 No son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$cut,dtMCut$price, simulate.p.value = TRUE) #0.03798101 no son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$color,dtMCut$clarity, simulate.p.value = TRUE) # 0.0004997501 no son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$color,dtMCut$depth, simulate.p.value = TRUE) #0.6596702 SON INDEPENDIENTES
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)

chi <- chisq.test(dtMCut$color,dtMCut$table, simulate.p.value = TRUE) #1 SON INDEPENDIENTES
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$color,dtMCut$price, simulate.p.value = TRUE) #0.0004997501  no son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$clarity,dtMCut$depth, simulate.p.value = TRUE)  #0.003498251 no son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


chi <- chisq.test(dtMCut$clarity,dtMCut$price, simulate.p.value = TRUE) #0.0004997501 No son independientes
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)

chi <- chisq.test(dtMCut$price,dtMCut$table, simulate.p.value = TRUE) #0.4517741 SON INDEPENDIENTES
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)


#El resultado nos muestra que todas esta variables tienen relacion de dependencia, o no podemos descartar
#que no la tengan. A excepcion de cut y color, color y depth, color y table, que podemos asumir que son
#independientes

#Es decir, el precio parace que depende de la combinacion del resto de las variables.




#----- ANOVA 

#Segun las independencias vistas anteriormente, voy a comprobar que la media del porcentaje de 
#profundidad ( depth) es igual para cada uno de los tipos de color del diamante.

#Lo primero que hago es preparar los datos.  
#Primero selecciono los datos de depth segun el color
#D>1,E>2,F>3,G, H,I,J 

table(dtMCut$color)
pm1 <- dtMCut[dtMCut$color=="D",5] 
pm2 <- dtMCut[dtMCut$color=="E",5]
pm3 <- dtMCut[dtMCut$color=="F",5]
pm4 <- dtMCut[dtMCut$color=="G",5]
pm5 <- dtMCut[dtMCut$color=="H",5]
pm6 <- dtMCut[dtMCut$color=="I",5]
pm7 <- dtMCut[dtMCut$color=="J",5]


DpCla <- c(pm1,pm2,pm3,pm4,pm5,pm6,pm7)
FClar <- c(rep("D",length(pm1)),rep("E",length(pm2)),rep("F",length(pm3)),
           rep("G",length(pm4)),rep("H",length(pm5)),rep("I",length(pm6)),rep("J",length(pm7)))
FClar<-factor(FClar)
boxplot(DpCla~FClar) #En el grafico ya veo que existen outliers y que no parece que todos tengan la misma media de depth 


#Ya tengo mi muestra para hacer el test.
#El anova requiere que:
# 1) las poblaciones sean normales
# 2) que las muestras sean independientes, 
# 3) y que tengan igual varianza.


#1 -- Compruebar que sigue una distribucion normal. ya sabemos que no tenemos ninguna distribicion normal.
AnalisisVariable(DpCla,"dpcla")
shapiro.test(DpCla) #4.353e-12 Pvalue es < 0.05


# valor < 0.05. Se rechaza la hipotesis de que es normal. Tiene mucha coincidencia de puntos, pero se separa
#en las colas.

#en este punto se podria normalizar la muestra. 
#Pero vamos a continuar suponiendo que la muestra es normal.

#2-- contraste de independencia
# h0 es que los dos factores son independientes.
# h1 es que no son independientes.

chi <- chisq.test(DpCla,FClar,simulate.p.value=TRUE)
print (chi)
cat ("El resultado de pvalue :", chi$p.value , " es > que 0.05 : " , chi$p.value > 0.05)

#pvalue 0.6596702 es >0.05,  ya habiamos visto anteriormente que son independientes

#3--igualdad de varianzas.
#h0 las varianzas son iguales.
#h1 no son lo son. 
l <- leveneTest(DpCla, group=FClar)
l$`Pr(>F)`[1] > 0.05  #true, 0.3668146 las varianzas son iguales, no podemos descartar la hipotesis de que lo sean.


p.aov<-aov(DpCla ~FClar)  
summary(p.aov) # 0.0281 

#como el valor de pvalue es < que 0.05 , se entiende que  hay diferencias significativas.
#se rechaza la hipotesis de que las medias sean iguales, es decir hay diferencias significativas en depth dependiendo del color.
#aunque el valor esta muy cercano a 0.05

tukey<-TukeyHSD(p.aov) #los intervalos que no contienen el valor 0 son significativos, p-value<0,05
tukey
plot(tukey) #se puede ver en el grafico que  hay valores significativos.
#En el plot y boxplot podemos ver cuales son los intervalos significativos, los que no pasan por 0.
#en el plot se puede ver que todos los valores pasan por 0, 
#a mi parecer, el color que esta mas separado de resto es el H, y es que tiene mas diferencias en la media con el resto.


#----------CORRELACIONES

#voy a cambiar los valores categoricos por valores numericos.

dtMCut$clarity <- as.numeric(dtMCut$clarity) #1 es I1, 2 SI2.. etc ..I1  SI2  SI1  VS2  VS1 VVS2 VVS1   IF
dtMCut$cut <- as.numeric(dtMCut$cut) #Fair >1,Good>2, Very Good>3,Premium>4,Ideal>5
dtMCut$color<-as.numeric(dtMCut$color) #D>1,E>2,F>3,G, H,I,J 





#A continuacion muestro un grafico donde se puede ver la correlacion entre las variables de la muestra.
corrplot(cor(dtMCut), method="number")
#Segun este grafico, podemos ver que con valores azules, cuales son las variables que
#tienen mayor correlacion, como x,y,x con carat.  Lo que tiene sentido, porque todas son fisicas.
#La relacion con price es buena, pero es inferior a la otra.

corrplot(cor(dtMCut), type="upper")
#Con este grafico vemos lo mismo que el anterior, pero se ve mas claro las relaciones, 
#las variables  con mas relacion tienen los circulos mas grandesy son mas oscuras.

#A continuacion, utilizo otras funciones de R, para mostrar otro grafico , donde se puede ver las variables con mas correlacion
#, y se tachan aquellas que no tienen apenas, que por tanto no habria que considerar.

p.mat <- cor.mtest(dtMCut)
corrplot(cor(dtMCut), type="upper", order="hclust",  p.mat = p.mat, sig.level = 0.01)

#Segun estos graficos, podemos descartar correlaciones entre las variables de color con 
#cut y table, asi como depth con price, carat,x,y y color.

#Las correlaciones mas cercanos se producen entre carat,x,y,z, es decir entre los variables que indican las
#medidas del diameante: el peso, longitud, ancho y profundidad 



####################################################################################
#Analisis de regresion
# Formular un modelo de regresion y analiza los resultados
# Muestra los residuos y analiza los resultados
# Aplica una transformacion a la regresion y analiza los resultados
# Interpreta los coeficientes estandarizados de la regresion
####################################################################################

#https://www.youtube.com/watch?v=HyJoMcTSomc#t=535.350851

#Dado que he visto que la variable precio depende de las variables categoricas.
#He decidido que en vez de hacer el modelo con la muestra que tengo
#para sacar el modelo de todos los datos, lo voy a hacer  estratificando por tipo de diamante. 

#Para ello, quiero calcular el modelo de regresion del precio del diamante, pero
#de acuerdo a las siguientescaracteristicas:
 #cut : Ideal
 #Colour : F
 #clarity: SI1


Mn<-  dt[ (dt$clarity=="SI1" & dt$cut=="Ideal" & dt$color=="F"),]
par(mfrow=c(1,1))
plot(Mn$price~Mn$carat)  #se ve que no sigue una recta, sino que parace curva..

#Ahora quito las variables categoricas, puesto que ya se cual es su valor.

dtMCut2 <- data.frame(cbind(Mn$carat,Mn$depth,Mn$table, Mn$price,Mn$x,Mn$y,Mn$z))
colnames(dtMCut2)<-c("carat","depth","table","price","x","y","z")	

partial.cor(dtMCut2)  #con esta funcion vero la correlacion parcial de las variables fijandome en price que es la que me interesa.

# voy a ver que pasa si pongo todas las variables.

fit <- lm(dtMCut2$price~dtMCut2$carat+dtMCut2$depth+dtMCut2$table+dtMCut2$x+dtMCut2$y+dtMCut2$z)

summary(fit)
#En este resultado, ya podria ver si cada coeficiente es relevante o significativo, segun si su pvalue es > 0.05 o no. Si es > 0.05 
#no podemos rechazar la hipotesis de que ese valor puede ser 0, por lo que no seria relevante para el estudio.
#Entonces segun los datos, sacaria y y z del modelo, porque no aportan al modelo.

#Para salir de dudas, vamos a estudiar el IAC del modelo.

step(fit)
#Segun vemos, el IAC mejora si quitamos las variable que no aportan al modelo.
#nos devuelve cual seria las variables a emplear ( devuelven el IAC mas bajo)
#lm(formula = dtMCut2$price ~ dtMCut2$carat + dtMCut2$x + dtMCut2$depth + dtMCut2$table)

#volvemos a calcular con los datos que ha arrojado Step.
fit <- lm(dtMCut2$price ~ dtMCut2$carat + dtMCut2$x + dtMCut2$depth + dtMCut2$table)
sfit2<-summary(fit)
sfit2
#Vamos a calcular un intervalo de confianza de los valores de los coeficientes el modelo.
confint(fit) 
#Aqui tenemos el intervalo de confianza entre los que se mueven los coeficientes 
#que multiplican a cada una de  nuestras variables.

#Bondad del ajuste --------------

#Vamos a ver los graficos del modelo
par(mfrow=c(2,2))
plot(fit)

#la primera impresion despues de ver los graficos, es que el modelo no esta muy ajustado..

#----- 1. Calculo del error standar residual. 
#errores entre los valores predichos y los reales.
redStanError <- sfit2$sigma
cv <- 100 * ( redStanError/ mean(dtMCut$price)) #calculamos el coeficiente de variacion asociado.
cv > 10
# Si el coeficiente es mayor que 10 (umbral que ponemos), significa que hay mucha desviacion, el modelo no es bueno.
# ya se ve en el grafico que existe mucha variacion entre los residuos y los valores esperados.

#----- 2. Anova, que contesta a la pregunta de si las variables (coeficientes) pueden ser 0 o no 
#( que seria la hipotesis nula)
# Para ello nos fijamos en el pvalue de F-statistic,  en este caso, vemos que tiene un valor inferior a 
# 0.05, y es un valor muy pequeño, por lo que podemos rechazar la hipotesis nula, y ninguno de las variables
#puede valer 0. Lo que indicaria que el modelo puede ser bueno.
#F-statistic:  7272 on 4 and 603 DF,  p-value: < 2.2e-16
sfit2

#----- 3. Coeficiente de determinacion. El R-squared
#El  Multiple R-squared indica el porcentaje que explica el modelo, superior al 95%
#y no tiene muchas diferencias con el R cuadrado ajustado. Con lo que parece que el modelo es bueno.
#Multiple R-squared:  0.9797,	Adjusted R-squared:  0.9796 
sfit2$r.squared

#Diagnostico del modelo --------------

#-----1 Normalidad de los valores residuales.
par(mfrow=c(1,1))

e<- residuals(fit)
d <- e / sfit2$sigma
hist(d,probability = T,xlab="Residuos",xlim = c(-3,3))
d.seq <- seq(-3,3,length=50)
lines(d.seq,dnorm(d.seq,mean(d),sd(d)))
#Con el grafico podemos ver que casi se asemeja a la normal. 
#Pero para salir de dudas, haremos el test de shapiro.
shapiro.test(e)
#2.2e-16, Con un valor inferior a 0.05, podemos rechazar la hipotesis nula de que es normal.
#Asi que los valores residuales no siguen la distribucion normal.
#=>  No se cumple esta condicion de normalidad.

#-----2 Homocedasticidad  ,para cada valor de la variable independiente la varianza de los residuos
#es constante.
#Esto lo vamos a calcular con la funcion bptest, que tien como  hipotesis nula que hay homocedasticidad.
bptest(fit)

#si el valor es > 0.05 no podriamos rechazar la hipoteses de homocedasticidad, como ha sido un valor
#muy pequeño(con un valor de  2.2e-16,) podemos rechazar la hipotesis, con lo que no se cumple la homocedasticidad.

#-----3 Incorrelacion. 
#Para ver la incorrelacion del modelo, lo haremos con el test de Durbin-Watson, que tiene como hipotesis nula
#que la correlacion es 0, es decir no hay correlacion.

dwtest(fit, alternative = "two.sided")
#p-value = 7.301e-14, el p-vale es menor que 0.05,  se puede rechazar la hipotesis nula. Lo que quiere decir 
#que no existe incorrelacion.

#En definitiva, el modelo NO es bueno. 

#-------- Aplica una transformacion a la regresion y analiza los resultados
# Vamos a transformar la variable respuesta (price)
# Para ello, utilizo la funcion boxcox.
bc <- boxCox(fit,plotit = F)
lambda <- bc$x[which.max(bc$y)]
lambda
#Como lambda da un valor muy proximo a 0, hacemos la transformacion del logaritmo neperiano de la variable price.
Nprice <- log(dtMCut2$price)

#Volvemos a calcular el modelo con la variable transformada.

fitT <- lm(Nprice ~ dtMCut2$carat + dtMCut2$x + dtMCut2$depth + dtMCut2$table)
sfitT<-summary(fitT)
sfitT

#Vemos que con este modelo, el multiple R squared ha mejorado con respecto al anterior, es decir
#hay un mayor porcentaje  explicado. 


par(mfrow=c(2,2))
plot(fitT)

#la primera impresion despues de ver los graficos, es que el modelo esta mas ajustado que el anterior.

#------ 1. Calculo del error standar residual. 
redStanError <- sfitT$sigma
cv <- 100 * ( redStanError/ mean(Nprice)) #calculamos el coeficiente de variacion asociado.
cv > 10
# Si el coeficiente es menor que 10, significa que poca  desviacion, el modelo podria ser  bueno.

#------ 2. Anova, que contesta a la pregunta de si las variables pueden ser 0 o no ( que seria la hipotesis nula)
# Para ello nos fijamos en el pvalue de F-statistic,  en este caso, vemos que tiene un valor inferior a 
# 0.05, y es un valor muy pequeño, por lo que podemos rechazar la hipotesis nula, y ninguno de las variables
#puede valer 0. Lo que indicaria que el modelo puede ser bueno.
sfitT

#------ 3. Coeficiente de determinacion. El R-squared
#El  Multiple R-squared indica el porcentaje que explica el modelo, es mejor que el anterior.
sfitT$r.squared

#Diagnosticos del modelo --------------

#--------1 Normalidad de los valores residuales.
par(mfrow=c(1,1))

e<- residuals(fitT)
d <- e / sfitT$sigma
hist(d,probability = T,xlab="Residuos",xlim = c(-3,3))
lines(d.seq,dnorm(d.seq,mean(d),sd(d)))
d.seq <- seq(-3,3,length=50)
#Con el grafico podemos ver que se asemeja a la normal. 
#Pero para salir de dudas, haremos el test de shapiro.
shapiro.test(e)

#Con un valor superior a 0.05, no podemos rechazar la hipotesis nula de que es normal
#Se cumple la condicion

#-------2 Homocedasticidad  ,para cada valor de la variable independiente la varianza de los residuos
#es constante.
#Esto lo vamos a calcular con la funcion bptest, que tien como  hipotesis nula que hay homocedasticidad.
bptest(fitT)

#si el valor es < 0.05  podriamos rechazar la hipoteses de homocedasticidad, como ha sido un valor
#superior  0.7399, se cumple la Homocedasticidad

#-------3 Incorrelacion. 
#Para ver la incorrelacion del modelo, lo haremos con el test de Durbin-Watson, que tiene como hipotesis nula
#que la correlacion es 0, es decir no hay correlacion.

dwtest(fitT, alternative = "two.sided")
#el p-vale es menor que 0.05, no se puede rechazar la hipotesis nula. 
#Lo que quiere decir que existe incorrelacion.

#Conclusion, tenemos un modelo bueno para calcular el Precio, pero para los diamantes que tengan las caracteristicas
# de Cut Ideal, Color=F, Clarity=SI1
sfitT
# precio = -3.954885 + (-1.040193*dtMCut2$carat ) + (1.437889 * dtMCut2$x) + (0.061591*dtMCut2$depth )+ (0.008503*dtMCut2$table)


