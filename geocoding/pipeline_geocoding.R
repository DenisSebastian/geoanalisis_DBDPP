# Objetivo: Flujo de Trabajo para geocodificar
# Autor: denis.berroeta@uai.cl


# Definir Directorio de Trabajo -------------------------------------------

setwd("../geocoding/")



# Librerías ---------------------------------------------------------------
library(sf)
library(dplyr)
library(tidygeocoder)
library(tidyr)
library(stringdist)
library(openxlsx)
library(janitor)
library(mapview)
library(stringr)

# Agregar Carpetas por etapas ---------------------------------------------

# Función Si no existe directorio lo crea
make_dir <- function(path){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# crear un vector con rutas
path_etapas <- c(0:3) %>% 
  paste0("etapa_", .) %>% 
  paste0("../geocoding/", .)

# Crear directorio en la tuta correspondiente
for (etapa in path_etapas) {
  print(paste0("Creando carpeta en ", etapa))
  make_dir(etapa)
}  

# lo mismo anterior (programción funcional)
# purrr::map(.x = path_etapas, .f = make_dir) 

# sumaran aportes de geocificación por etapa mas adelante
aportes_etapas <- list()


# Lectura de Insumos ------------------------------------------------------
# Base Raw 
est_salud_raw <- read.xlsx("bases/original/Establecimientos_DEIS_MINSAL_22-07-2025.xlsx") 
View(est_salud_raw)

# comunas de Chile
comunas_polygonos <- st_read("../cgeoa-book/data/shape/Comunas_Chile.shp") %>% 
  select(COD_COMUNA_INE = COMUNA, NOM_COMUNA_INE = NOM_COMUNA, 
         COD_REGION_INE = REGION, NOM_REGION_INE = NOM_REGION) %>% 
  st_buffer(dist = 10) %>% 
  st_transform(4326) # transformar a coordenadas geográficas




# Etapa 0 -----------------------------------------------------------------


  # Agregar ID --------------------------------------------------------------


# Función para Crear ID
make_id <- function(base, iniciales, digitos = 6, separador = "_"){
  # base: data frame al que se le agregará id
  # iniciales: letras que comenzará ID (sugiere 2)
  # digitos: cantidad de digitos 0 antes del número
  # separador: Separador entre iniciales y los numeros consecutivos
  
  ceros <-  paste0("%0", digitos, "d")
  base_res <- base %>%
    dplyr::mutate(ID =paste0(iniciales, separador , sprintf(ceros, 1:nrow(base))))
  
  return(base_res)
}

# aplicar función crea una columna de ID correlativo
est_salud <- make_id(est_salud_raw, iniciales = "ES", digitos = 7)


  # Guardar Original con ID -------------------------------------------------

saveRDS(est_salud, "bases/original/original_id.rds")
write.xlsx(est_salud, "bases/original/original_id.xlsx")



  # Selección de Columnas ---------------------------------------------------
est_salud_cols <- est_salud %>% 
  janitor::clean_names() %>% # limpiar nombres de columnas
  select(id, codigo_vigente, tipo_establecimiento_unidad,
         codigo_region, nombre_region, codigo_comuna, nombre_comuna,
         via, numero, direccion)



# Filtro por Región -------------------------------------------------------
# esto se hace con fines didácticos y poder terminar 
est_salud_reg <- est_salud_cols %>% 
  filter(codigo_region == 1) 

total_0 = nrow(est_salud_reg)
# guardar los registros regionales ----------------------------------------

saveRDS(est_salud_reg, "etapa_0/base_r1_e0.rds")
write.xlsx(est_salud_reg, "etapa_0/base_r1_e0.xlsx")


# Etapa 1: Limpieza Básica ------------------------------------------------
  
  # Leer Etapa anterior -----------------------------------------------------
nombre_base = "base_r1"
etapa_actual = 1
etapa_anterior = etapa_actual - 1

base <- readRDS(paste0("etapa_", etapa_anterior, "/",
                       nombre_base, "_e", etapa_anterior, ".rds"))%>% 
  mutate(ETAPA = etapa_actual)




  # E1: Limpieza Inicial --------------------------------------------------------

# Función para dejar todo en mayúscula y sin tildes
limpieza <- function(x) {
  x <- stringr::str_to_upper(x) # mayúsculas
  x <- chartr("ÁÉÍÓÚ", "AEIOU", x)# eliminar tildes
  x <- gsub("(?!')[[:punct:]]", "", x, perl=TRUE) #Excepción apostrofe de o'higgins
  x <- gsub(pattern = 'º', "", x)
  x <- stringr::str_squish(x) # eliminar espacios al comienzo y espacios repetidos
  x <- ifelse(is.na(x), "", x)
  x  
}
# Separa números con letras unidos
separar_num_letra <- function(texto) {
  texto %>%
    str_replace_all("([0-9])([[:alpha:]])", "\\1 \\2") %>%
    str_replace_all("([[:alpha:]])([0-9])", "\\1 \\2")
}


# Detectar y eliminar "SN", "S/N", " S/N " u otras variantes
eliminar_sn <- function(texto) {
  str_replace_all(texto, "\\bS\\s*/?\\s*N\\b", "")
}


base_e1 <- base %>% 
  mutate(
    TIPO = limpieza(via),
    CALLE = limpieza(direccion),
    NUM = limpieza(numero),
    COMUNA = limpieza(nombre_comuna),
    REGION = limpieza(nombre_region)
  ) %>% 
  mutate(CALLE = separar_num_letra(CALLE)) %>% 
  mutate(CALLE = eliminar_sn(CALLE))

# E1: Generar columna de CONSUTA

base_e1 <- base_e1 %>% 
  mutate(CONSULTA = paste0(CALLE, " ", NUM, ", ", 
                           COMUNA, ", ", REGION, ", CHILE")) 


# E1: Geocoding -----------------------------------------------------------


# E1: Comsultar a Nominatim de OSM

library(tidygeocoder)
# geocode the addresses
geocoding_e1 <- base_e1 %>%
  geocode(CONSULTA, method = 'osm', lat = latitude , long = longitude)

geocoding_e1$latitude %>% is.na() %>% table()



# E1: Validación Espacial -------------------------------------------------

# convertir de Dataframe (con lat, lon) en objeto
df2sf <- function(df, lon ="lon", lat ="lat", crs_base = 4326) {
  sf_object <- df %>%
    dplyr::filter(!is.na(.data[[lon]])|!is.na(.data[[lat]])) %>%
    sf::st_as_sf(coords = c(x = lon, y = lat),
                 crs = crs_base, agr = "constant")
  return(sf_object)
}


geocoding_sf_e1 <- df2sf(geocoding_e1, lon = "longitude", lat = "latitude")
# mapview::mapview(geocoding_sf_e1)


geocoding_sf_e1_val <- st_join(
  x    = geocoding_sf_e1, 
  y    = comunas_polygonos, 
  join = st_within,     # predicado binario
  left = TRUE           # conserva todos los puntos
)




validados_e1 <-  geocoding_sf_e1_val %>%
  mutate(VALIDACION_COM = ifelse(COMUNA == NOM_COMUNA_INE, 1, 0)) %>% 
  filter(VALIDACION_COM == 1) %>% 
  select(!ends_with("INE"))



# Evaluación de Calidad ---------------------------------------------------

make_register <- function(registro, base, etapa) {
  registro[[paste0("etapa_", etapa)]] <- nrow(base)
  return(registro)
}


aportes_etapas <-  make_register(registro = aportes_etapas,
                                 base = validados_e1, 
                                 etapa = etapa_actual)



total_act <- aportes_etapas[[paste0("etapa_", etapa_actual)]]

# Suma acumulada de filas desde etapa_1 hasta etapa_actual
totales_acumulados <- sum(unlist(aportes_etapas))

cobertura_global <- round(totales_acumulados / total_0 * 100, 2)
aporte_actual    <- round(total_act / total_0 * 100, 2)


texto_reporte <- paste0(
  sprintf("📌 REPORTE DE CALIDAD — ETAPA %d\n", etapa_actual),
  sprintf("🔹 Total base original:          %d\n", total_0),
  sprintf("🔹 Total etapa actual:           %d\n", total_act),
  sprintf("🧮 Aporte actual (%% original):   %.2f%%\n", aporte_actual),
  sprintf("📊 Cobertura acumulada global:  %.2f%% (%d registros acumulados)\n",
          cobertura_global, totales_acumulados)
)

cat(texto_reporte)



# Guardar los Resultados ---------------------------------------------------
path_rds <- paste0("etapa_", etapa_actual, "/base_r1_e", etapa_actual, ".rds")
path_shape <- paste0("etapa_", etapa_actual, "/base_r1_e", etapa_actual, ".shp")

saveRDS(validados_e1,file = path_rds)
st_write(validados_e1, dsn = path_shape, delete_dsn = T)




# Etapa 2 -----------------------------------------------------------------
etapa_actual = 2


  # Rezagados etapa anterior ------------------------------------------------
  
id_rezagados <-  validados_e1$id
base_e2 <- base_e1 %>% 
  filter(!id  %in% id_rezagados)  %>% 
  mutate(ETAPA = etapa_actual)


  # Lectura de Tabla de Correcciones ----------------------------------------
patrones_correccion <- read.csv("bases/correcciones/correccion-abreviaturas.csv",
                                header = T, sep = ";")


# Función para reemplazar -------------------------------------------------


# Encuentra y reemplaza
find_replace <-  function(patron, reemplazo, data_vector){
  data_vector <- gsub(pattern = patron, replacement = reemplazo, data_vector)
  return(data_vector)
}



base_e2_replaced <- base_e2 %>% 
  mutate(
    CALLE = find_replace(patron = "ALDEA DE ", reemplazo = "", 
                         data_vector =  CALLE),
    CALLE = find_replace(patron = "CASERIO DE ", reemplazo = "", 
                         data_vector =  CALLE),
    CALLE = find_replace(patron = "GENERAL", reemplazo = "GRAL",
                         data_vector =  CALLE), #Error Forzado
    )

# *OJO:  find_replace(patron = "GENERAL", reemplazo = "GRAL"),
# se hace solo para analizar el reemplazo por tabla
  
  

# Función para reemplazar por tabla ---------------------------------------



find_replace_table <- function(data, col, ref_table) {
  col <- rlang::ensym(col)  # más limpio que enquo
  
  data %>%
    mutate(
      !!col := purrr::reduce2(
        .x = ref_table$error,
        .y = ref_table$correcto,
        .init = as.character(.[[rlang::as_string(col)]]),
        .f = function(acc, patron, reemplazo) {
          gsub(patron, reemplazo, acc, ignore.case = TRUE)
        }
      )
    )
}

base_e2_mod_ab <- find_replace_table(data = base_e2_replaced, 
                                     col = "CALLE", 
                                     ref_table = patrones_correccion)

# E2: Generar CONSULTA -----------------------------------------------------

base_e2_mod_ab <-  base_e2_mod_ab %>% 
  mutate(CONSULTA = paste0(CALLE, " ", NUM, ", ", 
                         COMUNA, ", ", REGION, ", CHILE"))



# E2: Geocoding -----------------------------------------------------------

# geocode the addresses
geocoding_e2 <- base_e2_mod_ab %>%
  geocode(CONSULTA, method = 'osm', lat = latitude , long = longitude)

geocoding_e2$latitude %>% is.na() %>% table()




# E2: Validación Espacial -------------------------------------------------


geocoding_sf_e2 <- df2sf(geocoding_e2, lon = "longitude", lat = "latitude")
mapview::mapview(geocoding_sf_e2)


geocoding_sf_e2_val <- st_join(
  x    = geocoding_sf_e2, 
  y    = comunas_polygonos, 
  join = st_within,     # predicado binario
  left = TRUE           # conserva todos los puntos
)




validados_e2 <-  geocoding_sf_e2_val %>%
  mutate(VALIDACION_COM = ifelse(COMUNA == NOM_COMUNA_INE, 1, 0)) %>% 
  filter(VALIDACION_COM == 1) %>% 
  select(!ends_with("INE"))



# E2: Evaluación de Calidad -----------------------------------------------



# Genera un registro por etapa
aportes_etapas <-  make_register(registro = aportes_etapas,
                                 base = validados_e2, 
                                 etapa = etapa_actual)



total_act <- aportes_etapas[[paste0("etapa_", etapa_actual)]]

# Suma acumulada de filas desde etapa_1 hasta etapa_actual
totales_acumulados <- sum(unlist(aportes_etapas))

cobertura_global <- round(totales_acumulados / total_0 * 100, 2)
aporte_actual    <- round(total_act / total_0 * 100, 2)


texto_reporte <- paste0(
  sprintf("📌 REPORTE DE CALIDAD — ETAPA %d\n", etapa_actual),
  sprintf("🔹 Total base original:          %d\n", total_0),
  sprintf("🔹 Total etapa actual:           %d\n", total_act),
  sprintf("🧮 Aporte actual (%% original):   %.2f%%\n", aporte_actual),
  sprintf("📊 Cobertura acumulada global:  %.2f%% (%d registros acumulados)\n",
          cobertura_global, totales_acumulados)
)

cat(texto_reporte)


# R2: Guardar los Resultados ---------------------------------------------------
path_rds <- paste0("etapa_", etapa_actual, "/base_r1_e", etapa_actual, ".rds")
path_shape <- paste0("etapa_", etapa_actual, "/base_r1_e", etapa_actual, ".shp")

saveRDS(validados_e2,file = path_rds)
st_write(validados_e2, dsn = path_shape, delete_dsn = T)


# Etapa 3: Análisis por proximididad --------------------------------------
etapa_actual = 3


# Rezagados etapa anterior ------------------------------------------------

id_rezagados <-  validados_e2$id
base_e3 <- base_e2 %>% 
  filter(!id  %in% id_rezagados) 


# Lectura maestro de calles OSM nacional ----------------------------------


master_calles_osm <- readRDS("bases/osm/master_calles_osm_procesado.rds") %>% 
  select(CALLE = name,COMUNA, REGION) %>% 
  mutate(CALLE = limpieza(CALLE)) %>% 
  mutate(COMUNA = limpieza(COMUNA)) %>% 
  mutate(REGION = limpieza(REGION)) %>% 
  st_drop_geometry()


# E3: Analisis de Proximidad Jaro Winkler --------------------------------


# Función matcher con Jaro-Winkler
match_calle_jw <- function(data, maestro, col_calle = "CALLE", 
                           col_comuna = "COMUNA", umbral = 0.1) {
  
  # Asegurar nombres estándar
  col_calle <- rlang::ensym(col_calle)
  col_comuna <- rlang::ensym(col_comuna)
  
  data %>%
    rowwise() %>%
    mutate(
      # Filtra maestro por comuna
      candidatos = list(
        maestro %>%
          filter(!!col_comuna == !!col_comuna) %>%
          pull(!!col_calle)
      ),
      # Calcula distancia con cada candidato
      distancias = list(
        stringdist(!!col_calle, candidatos, method = "jw")
      ),
      # Encuentra índice del mejor candidato
      idx_min = which.min(distancias),
      JW       = ifelse(length(idx_min) > 0, candidatos[[idx_min]], NA_character_),
      JW_DIST  = ifelse(length(idx_min) > 0, distancias[[idx_min]], NA_real_),
      MATCH_OK = !is.na(JW_DIST) & JW_DIST <= umbral,
      # Crea columna final reemplazada si supera umbral
      CALLE_MATCH = ifelse(!is.na(JW_DIST) & JW_DIST <= umbral, JW, !!col_calle)
    ) %>%
    ungroup() %>%
    select(-c(candidatos, distancias, idx_min))
}



base_e3_mod_jw <- match_calle_jw(base_e3, master_calles_osm, umbral = 0.15)





# E3: Generar CONSULTA -----------------------------------------------------

base_e3_mod_jw <-  base_e3_mod_jw %>% 
  mutate(CONSULTA = paste0(CALLE_MATCH, " ", NUM, ", ", 
                           COMUNA, ", ", REGION, ", CHILE"))



# E3: Geocoding -----------------------------------------------------------

# geocode the addresses
geocoding_e3 <- base_e3_mod_jw %>%
  geocode(CONSULTA, method = 'osm', lat = latitude , long = longitude)

geocoding_e3$latitude %>% is.na() %>% table()




# E3: Validación Espacial -------------------------------------------------


geocoding_sf_e3 <- df2sf(geocoding_e3, lon = "longitude", lat = "latitude")
mapview::mapview(geocoding_sf_e3)


geocoding_sf_e3_val <- st_join(
  x    = geocoding_sf_e3, 
  y    = comunas_polygonos, 
  join = st_within,     # predicado binario
  left = TRUE           # conserva todos los puntos
)




validados_e3 <-  geocoding_sf_e3_val %>%
  mutate(VALIDACION_COM = ifelse(COMUNA == NOM_COMUNA_INE, 1, 0)) %>% 
  filter(VALIDACION_COM == 1) %>% 
  select(!ends_with("INE"))



# E3: Evaluación de Calidad -----------------------------------------------



# Genera un registro por etapa
aportes_etapas <-  make_register(registro = aportes_etapas,
                                 base = validados_e3, 
                                 etapa = etapa_actual)



total_act <- aportes_etapas[[paste0("etapa_", etapa_actual)]]

# Suma acumulada de filas desde etapa_1 hasta etapa_actual
totales_acumulados <- sum(unlist(aportes_etapas))

cobertura_global <- round(totales_acumulados / total_0 * 100, 2)
aporte_actual    <- round(total_act / total_0 * 100, 2)


texto_reporte_e3 <- paste0(
  sprintf("📌 REPORTE DE CALIDAD — ETAPA %d\n", etapa_actual),
  sprintf("🔹 Total base original:          %d\n", total_0),
  sprintf("🔹 Total etapa actual:           %d\n", total_act),
  sprintf("🧮 Aporte actual (%% original):   %.2f%%\n", aporte_actual),
  sprintf("📊 Cobertura acumulada global:  %.2f%% (%d registros acumulados)\n",
          cobertura_global, totales_acumulados)
)

cat(texto_reporte_e3)


# R3: Guardar los Resultados ---------------------------------------------------
path_rds <- paste0("etapa_", etapa_actual, "/base_r1_e", etapa_actual, ".rds")
path_shape <- paste0("etapa_", etapa_actual, "/base_r1_e", etapa_actual, ".shp")

saveRDS(validados_e3,file = path_rds)
st_write(validados_e3, dsn = path_shape, delete_dsn = T)



# Consolidación General ---------------------------------------------------

library(purrr)


# Directorios
directorios <- list.dirs(path = ".", full.names = TRUE)
etapas_dirs <- directorios[grepl("etapa", directorios)]

# Solo RDS con número > 0 antes de .rds
# [1-9] → el dígito inicial debe ser 1–9 (evita el 0).
# [0-9]* → permite más dígitos después (10, 11, 123…).
archivos_rds <- map(etapas_dirs, 
                    ~ dir_ls(.x, regexp = "[1-9][0-9]*\\.rds$", recurse = TRUE))
archivos_rds <- unlist(archivos_rds, use.names = FALSE)


# Leer los rds de forma independiente
archivos_rds_list <- archivos_rds %>% map(readRDS)

# Unir y seleccionar columnas de los Geocodficados Correctamente

resultados_sf <-  archivos_rds_list %>% 
  map_df(~select(., id, ETAPA, CONSULTA))


id_geocod <-  resultados_sf$id
rezagados <- base_e3 %>% 
  filter(!id  %in% id_geocod) 


# Inspección visual -------------------------------------------------------

mapview(resultados_sf, zcol = "ETAPA")


# Guardar los Resultados ---------------------------------------------------


#geocodificados
saveRDS(resultados_sf,file = "resultados/geocododed_v1.rds")
st_write(resultados_sf, dsn = "resultados/geocododed_v1.shp")


#rezagados
saveRDS(rezagados, file = "resultados/rezagados.rds")
write.xlsx(rezagados, file = "resultados/rezagados.xlsx")



