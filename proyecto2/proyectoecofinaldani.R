
library(corrplot)
library(stats)
library(car)

#========================== 1. ELECCIÓN DE VARIABLES ===========================

############################## RONDA 1 #######################################

# RLM =====================================================
# Variable respuesta: Exportaciones manufactureras (Y)
# Regresoras candidatas:
#   x1a = Formación bruta de capital fijo
#   x2a = Importaciones de bienes y servicios
#   x3a = Índice de tipo de cambio real
#   x4a = Indicador global de la actividad económica
#   x5a = Índice nacional de precios al productor
#   x6a = Fondos federales de EUA
#   x7a = GDP de EUA 
#   x8a = Producción industrial de EUA 
# =====================================================

# datos

df <- read.csv("/Users/danielarenee/Desktop/MLR-Model-for-GDP/proyecto2/datos_trimestrales.csv")
head(df)
#Ruta Mario abajo
#ruta <- file.choose()
#df<- read.csv(ruta)
head(df)

Ya  <- df$exportaciones_manufactureras
x1a <- df$form_bruta_cap_fijo
x2a <- df$importaciones_bienes_servicios
x3a <- df$ind_tipo_cambio_real
x4a <- df$igae
x5a <- df$inpp
x6a <- df$fedfunds
x7a <- df$gdp_usa
x8a <- df$prod_industrial_usa

# empezaremos checando la correlación entre todas las variables...

correl_full <- cor(cbind(Ya, x1a, x2a, x3a, x4a, x5a, x6a, x7a, x8a))
round(correl_full, 4)

par(mfrow = c(1, 1))
corrplot(correl_full, method = "color", type = "upper",
         tl.col = "tomato", tl.srt = 60, addCoef.col = "black",
         number.cex = 0.8,
         title = "Correlaciones - Variables candidatas",
         mar = c(0, 0, 2, 0))

# 1. modelo saturado (8 regresoras)
modelo_saturado <- lm(Ya ~ x1a + x2a + x3a + x4a + x5a + x6a + x7a + x8a)
summary(modelo_saturado)
vif(modelo_saturado)

# los vif son catastróficos! hay 14 pares con correlación > 0.8
# Diagnosticamos Multicolinealidad

# primero, el grupo de "actividad económica" x1(FBCF), x2(importaciones)
# x4 (IGAE), x5(INPP), x7(GDP EUA) miden diferentes facetas de lo mismo...
# " la economía crece". todas tienen correlaciones de 0.85-0.99
# Hay que elegir UNA (criterio: correlacion con y e interpretabilidad teorica)

# segundo, el grupo de "demanda industrial de estados unidos": x7 (gdp eua),
# x8 (prod industrial) tienen correlación 0.82

# notamos que x3(ITCR, VIF = 1.5) y x x6(Fed Funds, VIF = 1.9) se ven bien :) 

# 2. modelo reducido (4 regresoras)

# ====================================================
# Variable respuesta: Exportaciones manufactureras (Y)
# Regresoras :
#   x1 = Formación bruta de capital fijo
#   x3 = Índice de tipo de cambio real
#   x6 = Fondos federales de EUA
#   x8 = Producción industrial de EUA 
# =====================================================

# Criterio de eliminación para multicolinealidad:

# grupo económico: elegimos FBCF (x1) porque tiene menor correlacion con x8 
# grupo EUA: elegimos prod_industrial (x8) porque es más especifico al sector manuf
# conservamos x3 y x6

modelo_red <- lm(Ya ~ x1a + x3a + x6a + x8a)
summary(modelo_red)
vif(modelo_red)

correl_red <- cor(cbind(x1a, x3a, x6a, x8a))
round(correl_red, 4)
corrplot(correl_red, method = "color", type = "upper",
         tl.col = "tomato", tl.srt = 60, addCoef.col = "black",
         number.cex = 0.8,
         title = "Correlaciones - Modelo reducido",
         mar = c(0, 0, 2, 0))

# notamos que todas las variables regresoras son significativas al 0.05,
# tienen un VIF por debajo de 4, y el modelo tiene un R^2 de 0.79

############################## RONDA 2 #######################################

# RLM =====================================================
# Variable respuesta: Exportaciones manufactureras (Y)
# Regresoras candidatas:
#   x1d = Formación bruta de capital fijo
#   x2d = Índice de tipo de cambio real
#   x3d = Fondos federales de EUA
#   x4d = Producción industrial de EUA 
#   x5d = Precio del petróleo
#   x6d = VIX (Índice de volatilidad)
#   x7d = Remesas
#   x8d = Inflación (variación trimestral)
# =====================================================

# datos
df2 <- read.csv("/Users/danielarenee/Desktop/MLR-Model-for-GDP/proyecto2/datos_trimestrales2.csv")
head(df2)
#Ruta Mario
#ruta1<- file.choose()
#df2<- read.csv(ruta1)
head(df2)


Yd  <- df2$exportaciones_manufactureras
x1d <- df2$form_bruta_cap_fijo
x2d <- df2$ind_tipo_cambio_real
x3d <- df2$fedfunds
x4d <- df2$prod_industrial_usa
x5d <- df2$precio_petroleo
x6d <- df2$vix
x7d <- df2$remesas
x8d <- df2$inflacion

# empezaremos checando la correlación entre todas las variables...

correl_full2 <- cor(cbind(Yd, x1d, x2d, x3d, x4d, x5d, x6d, x7d, x8d))
round(correl_full2, 4)

par(mfrow = c(1, 1))

corrplot(correl_full2, method = "color", type = "upper",
         tl.col = "tomato", tl.srt = 60, addCoef.col = "black",
         number.cex = 0.8,
         title = "Correlaciones - Variables candidatas",
         mar = c(0, 0, 2, 0))

# 1. modelo saturado 2 (8 regresoras)
modelo_saturado2 <- lm(Yd ~ x1d + x2d + x3d + x4d + x5d + x6d + x7d + x8d)
summary(modelo_saturado2)
vif(modelo_saturado2)

# notamos que x1d y x4d tienen VIF>5. Además, x4d no es significativa.
# eliminamos x4d... 

modelo_red2 <- lm(Yd ~ x1d + x2d + x3d + x5d + x6d + x7d + x8d)
summary(modelo_red2)
vif(modelo_red2)

# bien! 
# Mantendremos x2d aunque no salga significativa porque lo fue en la ronda 1

##################### DEFINICIÓN DEL DATASET FINAL #############################

# =====================================================
# Variable respuesta: Exportaciones manufactureras (Y)
# Regresoras:
#   x1 = Formación bruta de capital fijo
#   x2 = Índice de tipo de cambio real
#   x3 = Fondos federales de EUA
#   x4 = Precio del petróleo
#   x5 = VIX (Índice de volatilidad)
#   x6 = Remesas
#   x7 = Inflación (variación trimestral)
# =====================================================

Y  <- df2$exportaciones_manufactureras
x1 <- df2$form_bruta_cap_fijo
x2 <- df2$ind_tipo_cambio_real
x3 <- df2$fedfunds
x4 <- df2$precio_petroleo
x5 <- df2$vix
x6 <- df2$remesas
x7 <- df2$inflacion



#=========================== 2. SELECCIÓN DE MODELOS ===========================

# -- MODELO I: Lin-Lin (unidades originales) -----------------------------------
# Interpretación: un incremento de una unidad en xj, manteniendo
# las demás constantes, produce un cambio de Beta_j unidades en Y.

modelo_I <- lm(Y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7)
summary(modelo_I) # R^2 = 0.956
anova(modelo_I)

# Pruebas F parciales  ------------- 
# H0: Beta_j = 0  vs  H1: βeta_j =/ 0

sin_x1 <- lm(Y ~ x2 + x3 + x4 + x5 + x6 + x7)
sin_x2 <- lm(Y ~ x1 + x3 + x4 + x5 + x6 + x7)
sin_x3 <- lm(Y ~ x1 + x2 + x4 + x5 + x6 + x7)
sin_x4 <- lm(Y ~ x1 + x2 + x3 + x5 + x6 + x7)
sin_x5 <- lm(Y ~ x1 + x2 + x3 + x4 + x6 + x7)
sin_x6 <- lm(Y ~ x1 + x2 + x3 + x4 + x5 + x7)
sin_x7 <- lm(Y ~ x1 + x2 + x3 + x4 + x5 + x6)

cat("F parcial x1 (FBCF):\n")
print(anova(sin_x1, modelo_I))
cat("F parcial x2 (ITCR):\n")
print(anova(sin_x2, modelo_I))
cat("F parcial x3 (Fed Funds):\n")
print(anova(sin_x3, modelo_I))
cat("F parcial x4 (WTI):\n")
print(anova(sin_x4, modelo_I))
cat("F parcial x5 (VIX):\n")
print(anova(sin_x5, modelo_I))
cat("F parcial x6 (Remesas):\n")
print(anova(sin_x6, modelo_I))
cat("F parcial x7 (Inflación):\n")
print(anova(sin_x7, modelo_I))


# Criterios de evaluación  ------------- 
cat("R^2 adj  =", summary(modelo_I)$adj.r.squared, "\n")
cat("AIC     =", AIC(modelo_I), "\n")
cat("BIC     =", BIC(modelo_I), "\n")
cat("ERR STD  =", summary(modelo_I)$sigma, "\n")


# Stepwise (ambas direcciones) ------------- 

modelo_nulo <- lm(Y ~ 1) # sin regresoras
step_I <- step(modelo_nulo,
               scope = list(lower = modelo_nulo, upper = modelo_I),
               direction = "both", trace = 1)
summary(step_I)

# Obtenemos el modelo Ia, donde se excluye x2 y quedan 6 variables, todas
# significativas, con R^2 = 0.95 

modelo_Ia <- lm(Y ~ x1 + x3 + x4 + x5 + x6 + x7)
summary(modelo_Ia)
vif(modelo_Ia) # VIF < 5 para todas! 

# -- MODELO II: Log-Log --------------------------------------------------------
# Interpretación: Un incremento de 1% en xj produce
# un cambio de Beta_j% en Y, manteniendo igual lo demás

cat("min(x3) =", min(x3)) 
cat("min(x7) =", min(x7))  
# Notamos que hay valores negativos en la inflación (x7) y muy ceranos a 0 en 
# fedfunds (x3). Por lo que no las tomamos en cuenta

lnY  <- log(Y)
lnx1 <- log(x1)
lnx2 <- log(x2)
lnx4 <- log(x4)
lnx5 <- log(x5)
lnx6 <- log(x6)

modelo_II <- lm(lnY ~ lnx1 + lnx2 + lnx4 + lnx5 + lnx6)
summary(modelo_II)
anova(modelo_II)
vif(modelo_II)

# Pruebas F parciales ------------- 
sin_lnx1 <- lm(lnY ~ lnx2 + lnx4 + lnx5 + lnx6)
sin_lnx2 <- lm(lnY ~ lnx1 + lnx4 + lnx5 + lnx6)
sin_lnx4 <- lm(lnY ~ lnx1 + lnx2 + lnx5 + lnx6)
sin_lnx5 <- lm(lnY ~ lnx1 + lnx2 + lnx4 + lnx6)
sin_lnx6 <- lm(lnY ~ lnx1 + lnx2 + lnx4 + lnx5)

cat("F parcial lnx1 (FBCF):\n");    print(anova(sin_lnx1, modelo_II))
cat("F parcial lnx2 (ITCR):\n");    print(anova(sin_lnx2, modelo_II))
cat("F parcial lnx4 (WTI):\n");     print(anova(sin_lnx4, modelo_II))
cat("F parcial lnx5 (VIX):\n");     print(anova(sin_lnx5, modelo_II))
cat("F parcial lnx6 (Remesas):\n");  print(anova(sin_lnx6, modelo_II))

# Criterios de evaluación ------------- 
cat("R^2 adj  =", summary(modelo_II)$adj.r.squared, "\n")
cat("AIC     =", AIC(modelo_II), "\n")
cat("BIC     =", BIC(modelo_II), "\n")

# Stepwise ------------- 
modelo_nulo_II <- lm(lnY ~ 1)
step_II <- step(modelo_nulo_II,
                scope = list(lower = modelo_nulo_II, upper = modelo_II),
                direction = "both", trace = 1)
summary(step_II)

# Obtenemos el modelo IIa, donde se excluye x5 y quedan 4 variables, todas
# significativas, con R^2 = 0.945
# Nota: ln(x2) ahora sí es significativa, lo que respalda haberla conservado

modelo_IIa <- lm(lnY ~ lnx1 + lnx2 + lnx4 + lnx6)
summary(modelo_IIa)
vif(modelo_IIa) # VIF de lnx1 rozando a 5... hay que checar multicol. 

# -- MODELO III: Log-Lin --------------------------------------------------------
# Interpretación: un incremento de una unidad en xj produce un cambio
# de 100·Beta_j % en Y, manteniendo igual lo demás

modelo_III <- lm(lnY ~ x1 + x2 + x3 + x4 + x5 + x6 + x7)
summary(modelo_III)
anova(modelo_III)
vif(modelo_III)

# Pruebas F parciales ------------- 
sin_x1_III <- lm(lnY ~ x2 + x3 + x4 + x5 + x6 + x7)
sin_x2_III <- lm(lnY ~ x1 + x3 + x4 + x5 + x6 + x7)
sin_x3_III <- lm(lnY ~ x1 + x2 + x4 + x5 + x6 + x7)
sin_x4_III <- lm(lnY ~ x1 + x2 + x3 + x5 + x6 + x7)
sin_x5_III <- lm(lnY ~ x1 + x2 + x3 + x4 + x6 + x7)
sin_x6_III <- lm(lnY ~ x1 + x2 + x3 + x4 + x5 + x7)
sin_x7_III <- lm(lnY ~ x1 + x2 + x3 + x4 + x5 + x6)

cat("F parcial x1 (FBCF):\n");       print(anova(sin_x1_III, modelo_III))
cat("F parcial x2 (ITCR):\n");       print(anova(sin_x2_III, modelo_III))
cat("F parcial x3 (Fed Funds):\n");   print(anova(sin_x3_III, modelo_III))
cat("F parcial x4 (WTI):\n");        print(anova(sin_x4_III, modelo_III))
cat("F parcial x5 (VIX):\n");        print(anova(sin_x5_III, modelo_III))
cat("F parcial x6 (Remesas):\n");     print(anova(sin_x6_III, modelo_III))
cat("F parcial x7 (Inflación):\n");   print(anova(sin_x7_III, modelo_III))

# Criterios de evaluación ------------- 
cat("R^2 adj  =", summary(modelo_III)$adj.r.squared, "\n")
cat("AIC     =", AIC(modelo_III), "\n")
cat("BIC     =", BIC(modelo_III), "\n")

# Stepwise ------------- 
modelo_nulo_III <- lm(lnY ~ 1)
step_III <- step(modelo_nulo_III,
                 scope = list(lower = modelo_nulo_III, upper = modelo_III),
                 direction = "both", trace = 1)
summary(step_III)

# Obtenemos el modelo IIIa, donde se excluye x2 y quedan 6 variables,
# todas significativas, con R^2 = 0.968
# Comparando con Modelo II (misma respuesta lnY), Modelo III supera al II 
# en R^2 adj, AIC y BIC

modelo_IIIa <- lm(lnY ~ x1 + x3 + x4 + x5 + x6 + x7)
summary(modelo_IIIa)
vif(modelo_IIIa)

# Como x4 (WTI) es marginalmente significativa (p = 0.051), lo quitamos y 
# Obtenemos el modelo IIIb, donde quedan 5 variables,con R^2 = 0.967

modelo_IIIb <- lm(lnY ~ x1 + x3 + x5 + x6 + x7)
summary(modelo_IIIb)
vif(modelo_IIIb) # mejor :)

# pero ahora x5 es marginalmente significativa (p=0.068)
# Obtenemos el modelo IIIc, donde quedan 4 variables,con R^2 = 0.966

modelo_IIIc <- lm(lnY ~ x1 + x3 + x6 + x7)
summary(modelo_IIIc)
vif(modelo_IIIc) 

# este modelo mejora, porque justamente toma las variables que no pudimos tomar 
# en el modelo log-log (x3 y x7)

# la ventaja es que tenemos un modelo simple de pocas variables (principio de 
# parsimonia), que además explica bien. hay que verificar supuestos !

modelo_IIId <- lm(lnY ~ x1 + x3 + x6)
summary(modelo_IIId)
vif(modelo_IIId) 

# Criterios de evaluación ------------- 
cat("R^2 adj  =", summary(modelo_IIId)$adj.r.squared, "\n")
cat("AIC      =", AIC(modelo_IIId), "\n")
cat("BIC      =", BIC(modelo_IIId), "\n")

# Stepwise ------------- 
modelo_nulo_IIId <- lm(lnY ~ 1)
step_IIId <- step(modelo_nulo_IIId,
                  scope = list(lower = modelo_nulo_IIId, upper = modelo_IIId),
                  direction = "both", trace = 1)
summary(step_IIId)


# -- MODELO IV: Estandarizado --------------------------------
# Interpretación: Un incremento de 1 desviación estándar en xj produce
# un cambio de Beta_j desviaciones estándar en Y. (Regresión al origen)

# Ruta Mario
# ruta2 <- file.choose()
# df_std <- read.csv(ruta2)
df_std <- read.csv("/Users/danielarenee/Desktop/MLR-Model-for-GDP/proyecto2/datos_trimestrales2_std.csv")

head(df_std)

Ys  <- df_std$exportaciones_manufactureras
x1s <- df_std$form_bruta_cap_fijo
x2s <- df_std$ind_tipo_cambio_real
x3s <- df_std$fedfunds
x4s <- df_std$precio_petroleo
x5s <- df_std$vix
x6s <- df_std$remesas
x7s <- df_std$inflacion

# hacemos la regresión al origen...
modelo_IV <- lm(Ys ~ 0 + x1s + x2s + x3s + x4s + x5s + x6s + x7s)
summary(modelo_IV)
anova(modelo_IV)
vif(modelo_IV)

# 3. Pruebas F parciales ------------- 
sin_x1s <- lm(Ys ~ 0 + x2s + x3s + x4s + x5s + x6s + x7s)
sin_x2s <- lm(Ys ~ 0 + x1s + x3s + x4s + x5s + x6s + x7s)
sin_x3s <- lm(Ys ~ 0 + x1s + x2s + x4s + x5s + x6s + x7s)
sin_x4s <- lm(Ys ~ 0 + x1s + x2s + x3s + x5s + x6s + x7s)
sin_x5s <- lm(Ys ~ 0 + x1s + x2s + x3s + x4s + x6s + x7s)
sin_x6s <- lm(Ys ~ 0 + x1s + x2s + x3s + x4s + x5s + x7s)
sin_x7s <- lm(Ys ~ 0 + x1s + x2s + x3s + x4s + x5s + x6s)

cat("F parcial x1s (FBCF):\n");       print(anova(sin_x1s, modelo_IV))
cat("F parcial x2s (ITCR):\n");       print(anova(sin_x2s, modelo_IV))
cat("F parcial x3s (Fed Funds):\n");  print(anova(sin_x3s, modelo_IV))
cat("F parcial x4s (WTI):\n");        print(anova(sin_x4s, modelo_IV))
cat("F parcial x5s (VIX):\n");        print(anova(sin_x5s, modelo_IV))
cat("F parcial x6s (Remesas):\n");    print(anova(sin_x6s, modelo_IV))
cat("F parcial x7s (Inflación):\n");  print(anova(sin_x7s, modelo_IV))

# 4. Criterios de evaluación ------------- 
cat("R^2 adj  =", summary(modelo_IV)$adj.r.squared, "\n")
cat("AIC      =", AIC(modelo_IV), "\n")
cat("BIC      =", BIC(modelo_IV), "\n")

# 5. Stepwise ------------- 
# Nota: El modelo nulo sin intercepto se define explícitamente con el cero
modelo_nulo_IV <- lm(Ys ~ 0)
step_IV <- step(modelo_nulo_IV,
                scope = list(lower = modelo_nulo_IV, upper = modelo_IV),
                direction = "both", trace = 1)
summary(step_IV)

# 6. Obtenemos el modelo IVa, donde se excluye x2s y quedan 6 variables.
# Al estar estandarizado y al origen, los coeficientes miden el impacto relativo.

modelo_IVa <- lm(Ys ~ 0 + x1s + x3s + x4s + x5s + x6s + x7s)
summary(modelo_IVa)
vif(modelo_IVa)

#======================== 3. INTERPRETACIÓN DE MODELOS =========================

# -- Modelo Ia (Lin-Lin) -------------------------------------------------------

summary(modelo_Ia)
confint(modelo_Ia, level = 0.95)  # intervalos de confianza para los Beta_j
anova(modelo_Ia)

# Intervalos de respuesta media y predicción
x0 <- data.frame(
  x1 = mean(x1), x3 = mean(x3), x4 = mean(x4),
  x5 = mean(x5), x6 = mean(x6), x7 = mean(x7)
)

cat("Intervalo de respuesta media (Modelo Ia) ")
predict(modelo_Ia, newdata = x0, interval = "confidence", level = 0.95)

cat("Intervalo de predicción (Modelo Ia) ")
predict(modelo_Ia, newdata = x0, interval = "prediction", level = 0.95)

# -- Modelo IIa (Log-Log) ------------------------------------------------------

summary(modelo_IIa)
confint(modelo_IIa, level = 0.95)
anova(modelo_IIa)

x0_II <- data.frame(
  lnx1 = mean(lnx1), lnx2 = mean(lnx2),
  lnx4 = mean(lnx4), lnx6 = mean(lnx6)
)

cat("Intervalo de respuesta media (Modelo IIa)")
predict(modelo_IIa, newdata = x0_II, interval = "confidence", level = 0.95)

cat("Intervalo de predicción (Modelo IIa)")
predict(modelo_IIa, newdata = x0_II, interval = "prediction", level = 0.95)

# -- Modelo IIIc (Log-Lin) -----------------------------------------------------

summary(modelo_IIIc)
confint(modelo_IIIc, level = 0.95)
anova(modelo_IIIc)

x0_III <- data.frame(
  x1 = mean(x1), x3 = mean(x3),
  x6 = mean(x6), x7 = mean(x7)
)

cat("Intervalo de respuesta media (Modelo IIIc) ")
predict(modelo_IIIc, newdata = x0_III, interval = "confidence", level = 0.95)

cat("Intervalo de predicción (Modelo IIIc)")
predict(modelo_IIIc, newdata = x0_III, interval = "prediction", level = 0.95)


# -- Modelo IIId (Log-Lin reducido) --------------------------------------------

summary(modelo_IIId)
confint(modelo_IIId, level = 0.95)
anova(modelo_IIId)

x0_IIId <- data.frame(
  x1 = mean(x1), x3 = mean(x3), x6 = mean(x6)
)

cat("Intervalo de respuesta media (Modelo IIId) ")
predict(modelo_IIId, newdata = x0_IIId, interval = "confidence", level = 0.95)

cat("Intervalo de predicción (Modelo IIId) ")
predict(modelo_IIId, newdata = x0_IIId, interval = "prediction", level = 0.95)

# Nota: los intervalos de los modelos II y III están en escala ln(Y)
# para llevarlos a escala original: exp(valor)...

# mod II
cat("Intervalos en escala original (Modelo IIa)")
conf_II <- predict(modelo_IIa, newdata = x0_II, interval = "confidence", level = 0.95)
pred_II <- predict(modelo_IIa, newdata = x0_II, interval = "prediction", level = 0.95)
cat("Respuesta media:\n"); print(exp(conf_II))
cat("Predicción:\n"); print(exp(pred_II))

# mod IIIc
cat("Intervalos en escala original (Modelo IIIc)")
conf_III <- predict(modelo_IIIc, newdata = x0_III, interval = "confidence", level = 0.95)
pred_III <- predict(modelo_IIIc, newdata = x0_III, interval = "prediction", level = 0.95)
cat("Respuesta media:\n"); print(exp(conf_III))
cat("Predicción:\n"); print(exp(pred_III))

# mod IIId
cat("Intervalos en escala original (Modelo IIId)")
conf_IIId <- predict(modelo_IIId, newdata = x0_IIId, interval = "confidence", level = 0.95)
pred_IIId <- predict(modelo_IIId, newdata = x0_IIId, interval = "prediction", level = 0.95)
cat("Respuesta media:\n"); print(exp(conf_IIId))
cat("Predicción:\n"); print(exp(pred_IIId))


# Nota: Los intervalos del IIIc y IIId son más estrechos que los del IIa 
# lo cual confirma que es mejor modelo. Aparte de esto, ya sabíamos que tiene
# menor BIC
# también notamos que el IIId tiene casi los mismos resutlados que el IIIc,
# pero con una variable menos. esto respalda el principio de parsimonia

# no tiene caso hacerlo para el esatandarizado ya que todas las medias son 0


#======================== 4. VALIDACIÓN DE MODELOS =========================

library(lmtest)
library(car)

# =========================================================================
# FUNCIÓN DE VALIDACIÓN COMPLETA
# =========================================================================

validar_modelo <- function(modelo, nombre, k) {
  
  n <- nobs(modelo)
  res <- residuals(modelo)
  
  cat("\n=========================================================================")
  cat("\n  VALIDACIÓN DE SUPUESTOS:", nombre)
  cat("\n=========================================================================\n")
  
  # 1. Media cero de los residuales
  cat("\n--- 1. MEDIA CERO DE LOS RESIDUALES ---\n")
  cat("Media de los residuales:", round(mean(res), 10), "\n")
  print(t.test(res, mu = 0))
  
  # 2. Normalidad (Shapiro-Wilk)
  cat("\n--- 2. NORMALIDAD (Shapiro-Wilk) ---\n")
  print(shapiro.test(res))
  
  # 3. Homocedasticidad (Breusch-Pagan)
  cat("\n--- 3. HOMOCEDASTICIDAD (Breusch-Pagan) ---\n")
  print(bptest(modelo))
  
  # 4. Independencia (Ljung-Box, lag=4 por ser datos trimestrales)
  cat("\n--- 4. INDEPENDENCIA (Ljung-Box, lag=4) ---\n")
  print(Box.test(res, lag = 4, type = "Ljung-Box"))
  
  # 5. Correlograma
  par(mfrow = c(1, 1))
  acf(res, main = paste("ACF de los Residuales -", nombre))
  
  # =====================================================================
  # DIAGNÓSTICO DE OBSERVACIONES ATÍPICAS E INFLUYENTES
  # =====================================================================
  
  cat("\n=========================================================================")
  cat("\n  DIAGNÓSTICO DE INFLUENCIA:", nombre)
  cat("\n=========================================================================\n")
  
  res_estand <- rstandard(modelo)
  hats <- hatvalues(modelo)
  cooks <- cooks.distance(modelo)
  
  # Umbrales
  umbral_res <- 3
  umbral_hat <- 2 * (k + 1) / n
  
  # Identificación
  obs_atipicos <- which(abs(res_estand) > umbral_res)
  obs_apalancamiento <- which(hats > umbral_hat)
  obs_influyentes <- which(cooks > 1)
  
  cat("\n1. Valores atípicos (estandarizados > 3):",
      if(length(obs_atipicos) > 0) obs_atipicos else "Ninguno")
  cat("\n2. Puntos de apalancamiento (h_ii >", round(umbral_hat, 4), "):",
      if(length(obs_apalancamiento) > 0) obs_apalancamiento else "Ninguno")
  cat("\n3. Observaciones influyentes (D_i > 1):",
      if(length(obs_influyentes) > 0) obs_influyentes else "Ninguno")
  cat("\n=========================================================================\n")
  
  # Gráfico de diagnóstico por índice
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  influenceIndexPlot(modelo,
                     vars = c("Cook", "Studentized", "hat"),
                     id = list(n = 5, cex = 0.7, col = "red"),
                     main = paste("Diagnóstico por Índice:", nombre))
  par(mfrow = c(1, 1))
  
  # Las 5 observaciones con mayor distancia de Cook (para ejercicio diagnóstico)
  top5_cook <- order(cooks, decreasing = TRUE)[1:5]
  cat("\n5 observaciones con mayor distancia de Cook:", top5_cook, "\n")
  cat("Sus valores de D_i:", round(cooks[top5_cook], 4), "\n")
  
  return(list(
    obs_atipicos = obs_atipicos,
    obs_apalancamiento = obs_apalancamiento,
    obs_influyentes = obs_influyentes,
    top5_cook = top5_cook
  ))
}

# =========================================================================
# FUNCIÓN DE COMPARACIÓN: ORIGINAL VS LIMPIO
# =========================================================================

comparar_supuestos <- function(modelo, nombre, obs_a_eliminar) {
  
  modelo_limpio <- update(modelo, subset = -obs_a_eliminar)
  
  cat("\n=========================================================================")
  cat("\n  COMPARACIÓN ORIGINAL VS LIMPIO:", nombre)
  cat("\n=========================================================================")
  cat("\nDatos en modelo original:", nobs(modelo))
  cat("\nDatos en modelo limpio:", nobs(modelo_limpio),
      "(se eliminaron", length(obs_a_eliminar), "observaciones)\n")
  
  cat("\n--- NORMALIDAD ORIGINAL ---")
  print(shapiro.test(residuals(modelo)))
  cat("--- NORMALIDAD LIMPIO ---")
  print(shapiro.test(residuals(modelo_limpio)))
  
  cat("--- HOMOCEDASTICIDAD ORIGINAL ---")
  print(bptest(modelo))
  cat("--- HOMOCEDASTICIDAD LIMPIO ---")
  print(bptest(modelo_limpio))
  
  cat("--- INDEPENDENCIA ORIGINAL ---")
  print(Box.test(residuals(modelo), lag = 4, type = "Ljung-Box"))
  cat("--- INDEPENDENCIA LIMPIO ---")
  print(Box.test(residuals(modelo_limpio), lag = 4, type = "Ljung-Box"))
  
  return(modelo_limpio)
}

# =========================================================================
# EJECUCIÓN PARA CADA MODELO
# =========================================================================

# Modelo Ia (Lin-Lin)
diag_Ia <- validar_modelo(modelo_Ia, "Modelo Ia (Lin-Lin)", k = 6)
modelo_Ia_limpio <- comparar_supuestos(modelo_Ia, "Modelo Ia",
                                       diag_Ia$top5_cook)

# Modelo IIa (Log-Log)
diag_IIa <- validar_modelo(modelo_IIa, "Modelo IIa (Log-Log)", k = 4)
modelo_IIa_limpio <- comparar_supuestos(modelo_IIa, "Modelo IIa",
                                        diag_IIa$top5_cook)

# Modelo IIIc (Log-Lin, 4 variables)
diag_IIIc <- validar_modelo(modelo_IIIc, "Modelo IIIc (Log-Lin)", k = 4)
modelo_IIIc_limpio <- comparar_supuestos(modelo_IIIc, "Modelo IIIc",
                                         diag_IIIc$top5_cook)

# Modelo IIId (Log-Lin, 3 variables)
diag_IIId <- validar_modelo(modelo_IIId, "Modelo IIId (Log-Lin)", k = 3)
modelo_IIId_limpio <- comparar_supuestos(modelo_IIId, "Modelo IIId",
                                         diag_IIId$top5_cook)


# =========================================================================
# GRÁFICAS DE NORMALIDAD: ORIGINAL VS LIMPIO
# =========================================================================
library(ggplot2)
library(patchwork)

crear_grafica_individual <- function(residuos, titulo, color_fill, color_line) {
  df_res <- data.frame(Residuo = scale(residuos))
  
  ggplot(df_res, aes(x = Residuo)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20,
                   fill = color_fill, color = "white", alpha = 0.7) +
    geom_density(color = color_line, linewidth = 1.2) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                  color = "#2C3E50", linetype = "dashed", linewidth = 1) +
    labs(title = titulo, x = "Residuales Estandarizados", y = "Densidad") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
      panel.grid.minor = element_blank()
    ) +
    coord_cartesian(xlim = c(-4, 4), ylim = c(0, 0.65))
}

crear_panel_modelo <- function(mod_orig, mod_limp, nombre_modelo) {
  p_orig <- crear_grafica_individual(residuals(mod_orig),
                                     "1. Original",
                                     "#E74C3C", "#C0392B")
  p_limp <- crear_grafica_individual(residuals(mod_limp),
                                     "2. Sin valores atípicos",
                                     "#2980B9", "#1A5276")
  panel_completo <- (p_orig | p_limp) +
    plot_annotation(
      title = nombre_modelo,
      subtitle = "Línea punteada: distribución normal teórica",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
      )
    )
  return(panel_completo)
}


# =========================================================================
# EJERCICIO COMPLEMENTARIO: CRITERIO ALTERNATIVO DE COOK
# Umbral: D_i > 4/(n-k-1)
# =========================================================================

limpiar_por_criterio_alt <- function(modelo, k) {
  n <- nobs(modelo)
  cooks <- cooks.distance(modelo)
  umbral <- 4 / (n - k - 1)
  obs_eliminar <- which(cooks > umbral)
  cat("Umbral:", round(umbral, 4), "\n")
  cat("Observaciones eliminadas:", obs_eliminar, "\n")
  cat("Total eliminadas:", length(obs_eliminar), "\n\n")
  modelo_limpio <- update(modelo, subset = -obs_eliminar)
  return(modelo_limpio)
}

cat("=== Modelo Ia ===\n")
modelo_Ia_alt <- limpiar_por_criterio_alt(modelo_Ia, k = 6)

cat("=== Modelo IIa ===\n")
modelo_IIa_alt <- limpiar_por_criterio_alt(modelo_IIa, k = 4)

cat("=== Modelo IIIc ===\n")
modelo_IIIc_alt <- limpiar_por_criterio_alt(modelo_IIIc, k = 4)

cat("=== Modelo IIId ===\n")
modelo_IIId_alt <- limpiar_por_criterio_alt(modelo_IIId, k = 3)

# Supuestos de los modelos limpios
cat("\n=== SUPUESTOS LIMPIOS ===\n")
modelos_alt <- list(modelo_Ia_alt, modelo_IIa_alt, modelo_IIIc_alt, modelo_IIId_alt)
nombres <- c("Ia", "IIa", "IIIc", "IIId")

for (i in 1:4) {
  cat("\n---", nombres[i], "---\n")
  cat("Shapiro p =", round(shapiro.test(residuals(modelos_alt[[i]]))$p.value, 4), "\n")
  cat("BP p =", round(bptest(modelos_alt[[i]])$p.value, 4), "\n")
  cat("Ljung-Box p =", round(Box.test(residuals(modelos_alt[[i]]), lag=4, type="Ljung-Box")$p.value, 10), "\n")
}

# Gráficas de normalidad original vs limpio
panel_Ia_alt   <- crear_panel_modelo(modelo_Ia,   modelo_Ia_alt,   "Modelo Ia (Lin-Lin)")
print(panel_Ia_alt)

panel_IIa_alt  <- crear_panel_modelo(modelo_IIa,  modelo_IIa_alt,  "Modelo IIa (Log-Log)")
print(panel_IIa_alt)

panel_IIIc_alt <- crear_panel_modelo(modelo_IIIc, modelo_IIIc_alt, "Modelo IIIc (Log-Lin)")
print(panel_IIIc_alt)

panel_IIId_alt <- crear_panel_modelo(modelo_IIId, modelo_IIId_alt, "Modelo IIId (Log-Lin)")
print(panel_IIId_alt)



