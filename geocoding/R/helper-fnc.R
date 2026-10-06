# Objetivo: Script de Funciones Auxiliares
# Autor: denis.berroeta@uai.cl
# Proyecto: Geocodificación



# Crear Directorios -------------------------------------------------------

# Si no existe directorio lo crea
make_dir <- function(path){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}



# Generar ID --------------------------------------------------------------

# Crear ID
make_id <- function(base, iniciales, digitos = 6){
  # base: data frame al que se le agregará id
  # iniciales: letras que comenzará ID (sugiere 2)
  # digitos: cantidad de digitos 0 antes del número
  
  ceros <-  paste0("%0", digitos, "d")
  base_res <- base %>%
    dplyr::mutate(ID =paste0(iniciales, "_", sprintf(ceros, 1:nrow(base))))
  
  return(base_res)
  
}


# Guardar resultados ------------------------------------------------------

save_rds <- function(object, path_result, etapa, name){
  file_name <- paste0(path_result, "/", etapa,"/", name, ".rds")
  saveRDS(object, file_name)
  return(T)
}

save_csv <- function(object, path_result, etapa, name, overwrite = T){
  file_name <- paste0(path_result, "/", etapa,"/", name, ".csv")
  write.csv(x = object, file = file_name)
  return(T)
}

save_xlsx <- function(object, path_result, etapa, name, overwrite = T){
  file_name <- paste0(path_result, "/", etapa,"/", name, ".xlsx")
  openxlsx::write.xlsx(x = object, file = file_name, overwrite = overwrite)
  return(T)
}

save_tbls <- function(object, path_result, etapa, name, overwrite = T){
  rds_r <- save_rds(object, path_result, etapa, name)
  csv_r <- save_csv(object, path_result, etapa, name)
  xlsx_r <- save_xlsx(object, path_result, etapa, name, overwrite)
  return(list(rds = rds_r, 
              csv = csv_r, 
              xlsx = xlsx_r))
}