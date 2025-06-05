setwd("C:/Users/SIGEH/Desktop/Lalo/Gob/Proyectos")

datos = read.csv("../Extras/Tramites/ruts.tramites23abril2025.csv")
datos_informacion = datos
filtrar = read.csv("../Extras/Tramites/tramites_seleccion_variables_c_conteo_etapas_y_requisitos.csv")


datos_informacion = datos_informacion[datos_informacion$idtram %in% filtrar$idtram, ]
datos_informacion = datos_informacion |> 
  dplyr::select(idtram, nombre, tipo, secretaria.nombre, secretaria.siglas, dependencia.nombre, dependencia.siglas, 
                atencion.0..nombre:atencion.84..nombre, atencion.0..horario:atencion.84..horario,  atencion.0..coordenadas.coordinates.0.:atencion.84..coordenadas.coordinates.1.)  

nombre = datos_informacion |> 
  dplyr::select(idtram:atencion.84..nombre) |> 
  tidyr::pivot_longer(cols = atencion.0..nombre:atencion.84..nombre,
                      names_to = "Columnas_nombre",
                      values_to = "nombre_sitio") 

nombre = nombre |> 
  dplyr::mutate(Columnas_nombre = sub(x = Columnas_nombre, pattern = "\\.\\..*" , replacement = "")) |> 
  dplyr::filter(nombre_sitio != "") |> 
  dplyr::mutate(c_nom = sub(x = Columnas_nombre, pattern = "\\.\\..*", replacement = ""),
                merge = paste0(idtram,c_nom)) |> 
  dplyr::select(-c_nom,-Columnas_nombre)


                
horario = datos_informacion |> 
  dplyr::select(idtram:dependencia.siglas, atencion.0..horario:atencion.84..horario) |> 
  tidyr::pivot_longer(cols = atencion.0..horario:atencion.84..horario,
                      names_to = "Columnas_nombre",
                      values_to = "horario_sitio") 

horario = horario |> 
  dplyr::mutate(Columnas_nombre = sub(x = Columnas_nombre, pattern = "\\.\\..*" , replacement = "")) |> 
  dplyr::filter(horario_sitio != "") |> 
  dplyr::mutate(c_nom = sub(x = Columnas_nombre, pattern = "\\.\\..*", replacement = ""),
                merge = paste0(idtram,c_nom)) |> 
  dplyr::select(-c_nom,-Columnas_nombre)

  



coordenadas = datos_informacion |> 
  dplyr::select(idtram:dependencia.siglas,atencion.0..coordenadas.coordinates.0.:atencion.84..coordenadas.coordinates.1.) |> 
  tidyr::pivot_longer(cols = atencion.0..coordenadas.coordinates.0.:atencion.84..coordenadas.coordinates.1.,
                      names_to = "Columnas_nombre",
                      values_to = "coordenadas") |> 
  dplyr::mutate(c_nom = sub(x = Columnas_nombre, pattern = "\\.\\..*", replacement = ""),
                c_cor = sub(x = Columnas_nombre, pattern = ".*coordenadas.", replacement = "")) |> 
  dplyr::filter(coordenadas != "")


coordenadas_x = coordenadas |> 
  dplyr::filter(c_cor == "coordinates.0.") |> 
  dplyr::rename(coordenadas_x = coordenadas) |> 
  dplyr::mutate(merge = paste0(idtram,c_nom))

coordenadas_y = coordenadas |> 
  dplyr::filter(c_cor == "coordinates.1.") |> 
  dplyr::rename(coordenadas_y = coordenadas) |>
  dplyr::mutate(merge = paste0(idtram,c_nom))

coordenadas_correctas = merge(x = coordenadas_x, y = coordenadas_y |> dplyr::select(merge, coordenadas_y), by = "merge")
coordenadas_correctas = coordenadas_correctas |>  dplyr::select(-c_cor, -c_nom, -Columnas_nombre)  


# Juntar todo


todo = merge(x = nombre, y = horario |> dplyr::select(merge, horario_sitio), by = "merge")
todo = merge(x = todo, y = coordenadas_correctas |> dplyr::select(merge, coordenadas_x, coordenadas_y), by = "merge")
todo = todo |> dplyr::select(-merge)

write.csv(todo, "../Extras/Tramites/TramitesCOERG.csv", row.names = F, fileEncoding = "latin1")




# Verificar veracinad
anteriores = list.files("../Extras/Tramites/Tramites shp/shp_primero/", pattern = ".shp")
anteriores = unique(anteriores)
anteriores = sub(x = anteriores, pattern = ".shp", replacement = "")


ramas = datos_informacion$idtram 

all(as.numeric(anteriores) %in% ramas)
all(ramas %in% as.numeric(anteriores))

todo = read.csv("../Extras/Tramites/TramitesCOERG.csv")
ramas_todo = unique(todo$idtram)

all(ramas_todo %in% as.numeric(anteriores))
all(as.numeric(anteriores) %in% ramas_todo)








###
datos = read.csv("TramitesCOERG.csv", fileEncoding = "latin1")
mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")

nombres = datos |>  dplyr::select(idtram, nombre)
nombres = unique(nombres)

limpiar_texto = function(texto) {
  texto |>
    tolower() |>  # a minúsculas
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>  # elimina acentos
    gsub("\\s+", " ", x = _) |>  # reemplaza múltiples espacios por uno solo
    trimws() |>   # quita espacios al inicio y al final
    gsub(pattern = ":", replacement = "") |>
    gsub(pattern = "/", replacement = "") |> 
    gsub(pattern = " ", replacement = "_") |> 
    gsub(pattern = "\\.", replacement = "") |> 
    gsub(pattern = ",", replacement = "_")
}

for (i in 1:nrow(nombres)) {
  nom = nombres$nombre[i]
  cat("Vamos en", i, "con nombre de:", nom, "\n")
  interes = datos |>  dplyr::filter(idtram == nombres$idtram[i])
  interes = sf::st_as_sf(x = interes, coords = c("coordenadas_x", "coordenadas_y" ), crs = sf::st_crs(mun))
  sf::write_sf(interes, paste0("Tramites shp/", nombres$idtram[i],"_",limpiar_texto(nom),".shp"))
}

p = sf::read_sf("../Extras/Tramites/Tramites shp/1717_pago_de_creditos_fiscales_federales_coordinados.shp")


####### Condigo Antiguo
### General

for (i in 1:nrow(datos)) {
  i = 1
  coordenadas = matrix(unlist(c(datos[i, c(2:ncol(datos))])), ncol = 2, byrow = TRUE)
  eliminar = which(is.na(coordenadas[,1]))
  coordenadas = coordenadas[-eliminar,]
  
  if (length(coordenadas) == 2) {
    multipoint = sf::st_point(coordenadas)
  } else {
    multipoint = sf::st_multipoint(coordenadas)
  }
  multipoint = sf::st_sfc(multipoint, crs = sf::st_crs(p))
  sf::write_sf(multipoint, paste0("../Extras/Tramites/shp/", datos$idtram[i], ".shp"))
}



####################
archivos_shp = list.files(path = "../Extras/Tramites/shp/", pattern = "\\.shp$", full.names = TRUE)
nombres = basename(archivos_shp)
nombres = gsub(x = nombres, pattern = ".shp", replacement = "")

for (i in seq_along(archivos_shp)) {
  print(i)
  datos = sf::read_sf(archivos_shp[i])
  datos$idtram = nombres[i]
  datos = datos |> dplyr::select(idtram)
  sf::write_sf(datos, paste0("../Extras/Tramites/shp/", nombres[i], ".shp"))
}

