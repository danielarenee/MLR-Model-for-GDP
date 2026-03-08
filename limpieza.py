# %% Importar librerías
import pandas as pd

# %% Cargar y limpiar datos
ruta_var_respuesta = "/Users/danielarenee/Desktop/MLR-Model-for-GDP/datos_proyecto.csv"

df = pd.read_csv(ruta_var_respuesta, encoding="utf-16-le", skiprows=4, header=0)
df.columns = ["Fecha", "PIB"]
df = df.dropna(subset=["PIB"]).reset_index(drop=True)

#%% Función para convertir mensual a trimestral

def mensual_a_trimestral(ruta, col_nombre):
    d = None
    for enc in ["utf-16-le", "latin-1", "utf-8"]:
        try:
            tmp = pd.read_csv(ruta, encoding=enc, skiprows=4, header=0)
            if not tmp.empty:
                d = tmp
                break
        except Exception:
            continue
    if d is None:
        raise ValueError(f"No se pudo leer {ruta} con ningún encoding conocido")
    d.columns = ["Fecha", col_nombre]
    d["Fecha"] = d["Fecha"].str.extract(r"(\d{4}/\d{2})")
    d = d.dropna(subset=["Fecha", col_nombre]).reset_index(drop=True)
    d[["year", "month"]] = d["Fecha"].str.split("/", expand=True).astype(int)
    d["Fecha"] = d["year"].astype(str) + "/" + (((d["month"] - 1) // 3) + 1).apply(lambda q: f"{q:02d}")
    return d.groupby("Fecha", sort=True)[col_nombre].mean().reset_index()

#%% Construir df_final

base = "/Users/danielarenee/Desktop/MLR-Model-for-GDP/"

variables = [
    ("inflacion_subyacente.csv",           "inflacion_subyacente"),
    ("inflacion_no_subyacente.csv",        "inflacion_no_subyacente"),
    ("tasa_desocupacion.csv",              "tasa_desocupacion"),
    ("tasa_participacion.csv",             "tasa_participacion"),
    ("importaciones_manufactura.csv",      "importaciones_manufactura"),
    ("exportaciones_manufactura.csv",      "exportaciones_manufactura"),
    ("formacion_bruta_capital_fijo.csv",   "formacion_bruta_capital_fijo"),
]

df_final = df.copy()
for archivo, col in variables:
    df_trim = mensual_a_trimestral(base + archivo, col)
    df_final = df_final.merge(df_trim, on="Fecha", how="inner")

#%% Turismo interior (ya trimestral)

turismo = pd.read_csv(base + "turismo_interior.csv", encoding="utf-16-le", skiprows=4, header=0)
turismo.columns = ["Fecha", "turismo_interior"]
turismo = turismo[turismo["Fecha"].str.match(r"^\d{4}/\d{2}$", na=False)].reset_index(drop=True)

df_final = df_final.merge(turismo, on="Fecha", how="inner")

#%% Productividad laboral constructoras (ya trimestral)

prod_lab = pd.read_csv(base + "prod_laboral_constructoras.csv", encoding="utf-16-le", skiprows=4, header=0)
prod_lab.columns = ["Fecha", "prod_laboral_constructoras"]
prod_lab = prod_lab[prod_lab["Fecha"].str.match(r"^\d{4}/\d{2}$", na=False)].reset_index(drop=True)

df_final = df_final.merge(prod_lab, on="Fecha", how="inner")

#%% Guardar

df_final.to_csv("/Users/danielarenee/Desktop/MLR-Model-for-GDP/df_final.csv", index=False)

df_final