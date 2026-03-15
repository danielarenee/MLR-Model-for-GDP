
# ── Cargar datos ─────────────────────────────────────────────────
df <- read.csv(file.choose())
# ── Regresión lineal múltiple ─────────────────────────────────────────────────
modelo <- lm(PIB ~ inflacion_subyacente + inflacion_no_subyacente + tasa_desocupacion + tasa_participacion +
               importaciones_manufactura + exportaciones_manufactura + formacion_bruta_capital_fijo +
               turismo_interior + prod_laboral_constructoras, data = df)

summary(modelo)

# Ronda 1: Eliminamos la variable prod_laboral_constructoral
modelo_ronda1 <- lm(PIB ~ inflacion_subyacente + inflacion_no_subyacente + 
                      tasa_desocupacion + tasa_participacion + 
                      importaciones_manufactura + exportaciones_manufactura + 
                      formacion_bruta_capital_fijo + turismo_interior, data = df)

summary(modelo_ronda1)

# Ronda 2: Eliminamos la siguiente variable inflacion_no_subyacente
modelo_ronda2 <- lm(PIB ~ inflacion_subyacente + tasa_desocupacion + 
                      tasa_participacion + importaciones_manufactura + 
                      exportaciones_manufactura + formacion_bruta_capital_fijo + 
                      turismo_interior, data = df)

summary(modelo_ronda2)


# Ronda 3: Eliminamos exportaciones_manufactura 
modelo_ronda3 <- lm(PIB ~ inflacion_subyacente + tasa_desocupacion + 
                      tasa_participacion + importaciones_manufactura + 
                      formacion_bruta_capital_fijo + turismo_interior, data = df)

summary(modelo_ronda3)

# Ronda 4: Eliminamos importaciones_manufactura (p-value: 0.845)
modelo_ronda4 <- lm(PIB ~ inflacion_subyacente + tasa_desocupacion + 
                      tasa_participacion + formacion_bruta_capital_fijo + 
                      turismo_interior, data = df)
summary(modelo_ronda4)
