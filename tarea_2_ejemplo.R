# Ejemplo de la Parte 1 de la Tarea 2: IDW provincial de "atractor".
# Primero usamos todos los puntos del SII en el área urbana de la provincia
# de Santiago. Solo después de interpolar elegimos una comuna para el mapa
# y el indicador por manzana.

library(sf)
library(dplyr)
library(terra)
library(gstat)
library(exactextractr)
library(ggplot2)

crs_metrico <- 32719

# Área urbana completa de la provincia de Santiago.
zonas_urbanas <- readRDS("data/censo/zonas_urb_consolidadas.rds") |>
  filter(NOM_PROVIN == "SANTIAGO") |>
  st_transform(crs_metrico)
stopifnot(nrow(zonas_urbanas) > 0)
limite_provincial <- st_sf(
  geometry = st_union(st_geometry(zonas_urbanas))
)
limite_prov_vect <- terra::vect(limite_provincial)

# El .prj de urbes.shp declara WGS84 en grados, pero su CRS figura como
# "unknown". Asignamos EPSG:4326 antes de transformar las coordenadas.
sii_crudo <- st_read("data/sii/urbes.shp", quiet = TRUE)
stopifnot(st_bbox(sii_crudo)["xmin"] >= -180,
          st_bbox(sii_crudo)["xmax"] <= 180,
          st_bbox(sii_crudo)["ymin"] >= -90,
          st_bbox(sii_crudo)["ymax"] <= 90)
st_crs(sii_crudo) <- 4326

# La interpolación emplea TODOS los puntos dentro del área urbana provincial;
# aquí aún no filtramos por comuna.
sii_provincia <- sii_crudo |>
  st_transform(crs_metrico) |>
  st_filter(limite_provincial) |>
  filter(is.finite(atractor)) |>
  select(atractor)
stopifnot(nrow(sii_provincia) > 8)
cat("Puntos SII usados en la IDW provincial:", nrow(sii_provincia), "\n")

# Acotamos extremos usando la distribución provincial, no la comunal.
# Esta decisión cambia los valores interpolados y debe informarse.
limites <- quantile(sii_provincia$atractor, c(0.05, 0.95), na.rm = TRUE)
sii_provincia <- sii_provincia |>
  mutate(atractor_acotado = pmax(limites[1], pmin(atractor, limites[2])))
summary(sii_provincia[, c("atractor", "atractor_acotado")])

mapa_puntos_provincia <- ggplot() +
  geom_sf(data = limite_provincial, fill = "grey97", color = "grey55") +
  geom_sf(data = sii_provincia, aes(color = atractor_acotado),
          size = 0.2, alpha = 0.5) +
  scale_color_viridis_c(name = "Atractor\nacotado") +
  labs(title = "Valores de atractor en puntos SII",
       subtitle = "Área urbana de la provincia de Santiago",
       caption = "Valores acotados entre los percentiles provinciales 5 y 95") +
  theme_minimal()
print(mapa_puntos_provincia)

# Parámetros IDW: celda de 100 m, potencia 2 y hasta 8 vecinos por celda.
resolucion_m <- 100
potencia <- 2
vecinos_max <- 8

# La grilla cubre la provincia; se predice en sus centros de celda urbanos.
plantilla <- terra::rast(terra::ext(limite_prov_vect),
                        resolution = resolucion_m,
                        crs = terra::crs(limite_prov_vect))
xy_centros <- terra::xyFromCell(plantilla, seq_len(terra::ncell(plantilla)))
centros <- st_as_sf(as.data.frame(xy_centros), coords = c("x", "y"),
                    crs = crs_metrico) |>
  st_filter(limite_provincial)
stopifnot(nrow(centros) > 0)

modelo_idw <- gstat::gstat(
  formula = atractor_acotado ~ 1,
  data = sii_provincia,
  nmax = vecinos_max,
  set = list(idp = potencia)
)
predicciones <- predict(modelo_idw, newdata = centros, debug.level = 0)

r_idw_provincia <- terra::rasterize(
  terra::vect(predicciones), plantilla, field = "var1.pred"
)
names(r_idw_provincia) <- "atractor_idw"
r_idw_provincia <- terra::mask(r_idw_provincia, limite_prov_vect)

idw_df_provincia <- terra::as.data.frame(
  r_idw_provincia, xy = TRUE, na.rm = TRUE
)
mapa_idw_provincia <- ggplot(idw_df_provincia,
                             aes(x = x, y = y, fill = atractor_idw)) +
  geom_tile(width = resolucion_m, height = resolucion_m) +
  coord_equal() +
  scale_fill_viridis_c(name = "Atractor\ninterpolado") +
  labs(title = "IDW de atractor en la provincia de Santiago",
       subtitle = "Todos los puntos SII del área urbana provincial",
       x = "Este (m)", y = "Norte (m)") +
  theme_minimal()
print(mapa_idw_provincia)

# SOLO AHORA seleccionamos la comuna para analizar el resultado.
mi_comuna <- "LAS CONDES"
zonas_comuna <- zonas_urbanas |>
  filter(NOM_COMUNA == mi_comuna)
stopifnot(nrow(zonas_comuna) > 0)
limite_comuna <- st_sf(
  geometry = st_union(st_geometry(zonas_comuna))
)
limite_comuna_vect <- terra::vect(limite_comuna)

# Recortamos y enmascaramos la IDW YA CALCULADA a nivel provincial.
r_idw_comuna <- terra::crop(r_idw_provincia, limite_comuna_vect) |>
  terra::mask(limite_comuna_vect)
idw_df_comuna <- terra::as.data.frame(
  r_idw_comuna, xy = TRUE, na.rm = TRUE
)

mapa_idw_comuna <- ggplot(idw_df_comuna,
                          aes(x = x, y = y, fill = atractor_idw)) +
  geom_tile(width = resolucion_m, height = resolucion_m) +
  geom_sf(data = limite_comuna, fill = NA, color = "grey35",
          inherit.aes = FALSE, linewidth = 0.3) +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(name = "Atractor\ninterpolado") +
  labs(title = paste("IDW de atractor en", mi_comuna),
       subtitle = "Estimación provincial recortada a la comuna",
       x = "Este (m)", y = "Norte (m)") +
  theme_minimal()
print(mapa_idw_comuna)

# Vista interactiva del recorte comunal. Atractor es numérico continuo.
mapa_idw_interactivo <- mapview::mapview(
  r_idw_comuna,
  na.color = NA,
  col.regions = viridisLite::viridis(100),
  layer.name = paste("Atractor interpolado:", mi_comuna)
)
mapa_idw_interactivo

# Indicador territorial: promedio de la superficie provincial interpolada
# dentro de cada manzana de la comuna. No es una observación directa.
manzanas <- readRDS("data/censo/manzanas.rds") |>
  filter(NOM_COM == mi_comuna) |>
  st_transform(crs_metrico) |>
  st_filter(limite_comuna)
stopifnot(nrow(manzanas) > 0)

manzanas$atractor_promedio <- exactextractr::exact_extract(
  r_idw_comuna, manzanas, "mean", progress = FALSE
)
summary(manzanas$atractor_promedio)
sum(is.na(manzanas$atractor_promedio))

mapa_indicador <- ggplot(manzanas) +
  geom_sf(aes(fill = atractor_promedio), color = NA) +
  scale_fill_viridis_c(name = "Promedio\ninterpolado", na.value = "grey85") +
  labs(title = "Promedio de atractor interpolado por manzana",
       subtitle = paste("Área urbana de", mi_comuna),
       caption = "Gris: manzanas sin valor interpolado disponible") +
  theme_minimal()
print(mapa_indicador)

mapa_mz_interactivo <- mapview::mapview(
  manzanas, na.color = NA, zcol = "atractor_promedio",
  layer.name = paste("Atractor IDW", mi_comuna)
)
