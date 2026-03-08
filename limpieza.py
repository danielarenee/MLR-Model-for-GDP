# %% Importar librerías
import pandas as pd

# %% Cargar y limpiar datos
ruta_var_respuesta = "/Users/danielarenee/Desktop/MLR-Model-for-GDP/datos_proyecto.csv"

df = pd.read_csv(ruta_var_respuesta, encoding="utf-16-le", skiprows=4, header=0)

# Renombrar columnas
df.columns = ["Fecha", "PIB"]
# Eliminar filas de pie de página (sin valor numérico en PIB)
df = df.dropna(subset=["PIB"]).reset_index(drop=True)

#%% Inflacion subyacente

df1 = pd.read_csv("/Users/danielarenee/Desktop/MLR-Model-for-GDP/inflacion subyacente.csv", encoding="utf-16-le", skiprows=4, header=0)
df1.columns = ["Fecha", "inflacion_subyacente"]
df1 = df1.dropna(subset=["inflacion_subyacente"]).reset_index(drop=True)

# Convertir mensual a trimestral (promedio de los meses del trimestre)
df1[["year", "month"]] = df1["Fecha"].str.split("/", expand=True).astype(int)
df1["trimestre"] = ((df1["month"] - 1) // 3) + 1
df1["Fecha_trim"] = df1["year"].astype(str) + "/" + df1["trimestre"].apply(lambda q: f"{q:02d}")

df1_trim = (
    df1.groupby("Fecha_trim", sort=True)["inflacion_subyacente"]
    .mean()
    .reset_index()
    .rename(columns={"Fecha_trim": "Fecha"})
)

# Merge con PIB desde la primera fecha de inflación
df_final = df.merge(df1_trim, on="Fecha", how="inner")

#%% Inflacion no subyacente

df2 = pd.read_csv("/Users/danielarenee/Desktop/MLR-Model-for-GDP/inflacion no subyacente.csv", encoding="utf-16-le", skiprows=4, header=0)
df2.columns = ["Fecha", "inflacion_no_subyacente"]
df2 = df2.dropna(subset=["inflacion_no_subyacente"]).reset_index(drop=True)

# Convertir mensual a trimestral (promedio de los meses del trimestre)
df2[["year", "month"]] = df2["Fecha"].str.split("/", expand=True).astype(int)
df2["trimestre"] = ((df2["month"] - 1) // 3) + 1
df2["Fecha_trim"] = df2["year"].astype(str) + "/" + df2["trimestre"].apply(lambda q: f"{q:02d}")

df2_trim = (
    df2.groupby("Fecha_trim", sort=True)["inflacion_no_subyacente"]
    .mean()
    .reset_index()
    .rename(columns={"Fecha_trim": "Fecha"})
)

# Agregar al df_final
df_final = df_final.merge(df2_trim, on="Fecha", how="inner")

#%%

