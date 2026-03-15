
library(corrplot)
library(stats)
library(car)

# CARGAR DATOS

df <- read.csv("/Users/danielarenee/Downloads/df_regresion_estandarizado.csv")
head(df)


#RLM inicial

PIB<-df$PIB
x1<- df$inflacion_subyacente
x2<- df$inflacion_no_subyacente
x3<- df$tasa_desocupacion
x4<- df$tasa_participacion
x5<- df$importaciones_manufactura
x6<- df$exportaciones_manufactura
x7<- df$formacion_bruta_capital_fijo
x8<- df$turismo_interior
x9<- df$prod_laboral_constructoras
x10<- df$tasa_informalidad_laboral

modelo0 <- lm(PIB ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, data = df)
summary(modelo0)

correl0<-cor(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
par(mfrow=c(1,1))
corrplot(correl0, type="upper", tl.col="tomato", tl.srt=60)

vif(modelo0)

# Planteamos x11 = exportaciones_manufactura - importaciones_manufactura
# quitamos x8 y x10 por vif 

x11<-exportaciones_manufactura - importaciones_manufactura
modelo1 <- lm(PIB ~ x1 + x2 + x3 + x4 + x7 + x9 + x11, data = df)

correl1<-cor(cbind(x1,x2,x3,x4,x7,x9,x11))
par(mfrow=c(1,1))
corrplot(correl1, type="upper", tl.col="tomato", tl.srt=60)

vif(modelo1) # ya se ve mejor !

# notamos la correlación entre las variables laborales...
# intentamos hacer PCA pero PC1 solo explica el 58% de la varianza
# incialmente pensamos en proponer una variable laboral con x3,x4 y x10
# pero ya sacamos x10, y x3 y x4 son redundantes entre sí
# por lo que nos quedamos únicamente con x4, que tiene más correlación 
# con el PIB

modelo2 <- lm(PIB ~ x1 + x2 + x4 + x7 + x9 + x11, data = df)

correl2<-cor(cbind(x1,x2,x4,x7,x9,x11))
par(mfrow=c(1,1))
corrplot(correl2, type="upper", tl.col="tomato", tl.srt=60) # super bien!! 

vif(modelo2) # super bien!! 
summary(modelo2)


# calcular BIF , si nos salen bif altos y variables no sign, probar multicol
# ver valores influyentes y quitarlos
# calcular intervalos de confianza de los parametros 
# incluso con 4 varibales 
# metodo de seleccion de variables 



