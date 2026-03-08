
# ── Cargar datos ──────────────────────────────────────────────────────────────
df <- read.csv("/Users/danielarenee/Desktop/MLR-Model-for-GDP/df_final.csv")

# ── Regresión lineal múltiple ─────────────────────────────────────────────────
modelo <- lm(PIB ~ inflacion_subyacente + inflacion_no_subyacente + tasa_desocupacion + tasa_participacion +
               importaciones_manufactura + exportaciones_manufactura + formacion_bruta_capital_fijo +
               turismo_interior + prod_laboral_constructoras, data = df)

summary(modelo)