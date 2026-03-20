
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

#=========================== 2. SELECCION DE MODELOS ===========================

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

# -- MODELO IV: Estandarizado --------------------------------------------------
# Interpretación: Beta_j = cambio en desviaciones estándar de Y por cada 
# desviación estándar de cambio en xj. 

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

modelo_IV <- lm(Ys ~ x1s + x2s + x3s + x4s + x5s + x6s + x7s)
summary(modelo_IV)
anova(modelo_IV)
vif(modelo_IV)

# Pruebas F parciales ------------- 
sin_x1s <- lm(Ys ~ x2s + x3s + x4s + x5s + x6s + x7s)
sin_x2s <- lm(Ys ~ x1s + x3s + x4s + x5s + x6s + x7s)
sin_x3s <- lm(Ys ~ x1s + x2s + x4s + x5s + x6s + x7s)
sin_x4s <- lm(Ys ~ x1s + x2s + x3s + x5s + x6s + x7s)
sin_x5s <- lm(Ys ~ x1s + x2s + x3s + x4s + x6s + x7s)
sin_x6s <- lm(Ys ~ x1s + x2s + x3s + x4s + x5s + x7s)
sin_x7s <- lm(Ys ~ x1s + x2s + x3s + x4s + x5s + x6s)

cat("F parcial x1s (FBCF):\n");       print(anova(sin_x1s, modelo_IV))
cat("F parcial x2s (ITCR):\n");       print(anova(sin_x2s, modelo_IV))
cat("F parcial x3s (Fed Funds):\n");   print(anova(sin_x3s, modelo_IV))
cat("F parcial x4s (WTI):\n");        print(anova(sin_x4s, modelo_IV))
cat("F parcial x5s (VIX):\n");        print(anova(sin_x5s, modelo_IV))
cat("F parcial x6s (Remesas):\n");     print(anova(sin_x6s, modelo_IV))
cat("F parcial x7s (Inflación):\n");   print(anova(sin_x7s, modelo_IV))

# Criterios de evaluación ------------- 
cat("R^2 adj  =", summary(modelo_IV)$adj.r.squared, "\n")
cat("AIC     =", AIC(modelo_IV), "\n")
cat("BIC     =", BIC(modelo_IV), "\n")

# Stepwise ------------- 
modelo_nulo_IV <- lm(Ys ~ 1)
step_IV <- step(modelo_nulo_IV,
                scope = list(lower = modelo_nulo_IV, upper = modelo_IV),
                direction = "both", trace = 1)
summary(step_IV)

# igual al lin-lin


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

# Nota: los intervalos de los modelos II y III están en escala ln(Y)
# para llevarlos a escala original: exp(valor)...

# mod II
cat("Intervalos en escala original (Modelo IIa)")
conf_II <- predict(modelo_IIa, newdata = x0_II, interval = "confidence", level = 0.95)
pred_II <- predict(modelo_IIa, newdata = x0_II, interval = "prediction", level = 0.95)
cat("Respuesta media:\n"); print(exp(conf_II))
cat("Predicción:\n"); print(exp(pred_II))

# mod III
cat("Intervalos en escala original (Modelo IIIc)")
conf_III <- predict(modelo_IIIc, newdata = x0_III, interval = "confidence", level = 0.95)
pred_III <- predict(modelo_IIIc, newdata = x0_III, interval = "prediction", level = 0.95)
cat("Respuesta media:\n"); print(exp(conf_III))
cat("Predicción:\n"); print(exp(pred_III))

# Nota: Los intervalos del IIIc son más estrechos que los del IIa 
# lo cual confirma que es mejor modelo. Aparte de esto, ya sabíamos que tiene
# menor BIC

#======================== 4. VALIDACIÓN DE MODELOS =========================









