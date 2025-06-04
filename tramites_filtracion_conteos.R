setwd("C:/Users/SIGEH/Desktop/Lalo/Gob/Proyectos")

datos = read.csv("../Extras/Tramites/ruts.tramites23abril2025.csv")
p = sf::read_sf("Accidentes_Mapa/Datos/Filtrados/2025_C5/2025.shp")
filtrar = read.csv("../Extras/Tramites/tramites_seleccion_variables_c_conteo_etapas_y_requisitos.csv")


columnas_interes = which(grepl(pattern = "coordinates", x = colnames(datos)))
length(columnas_interes)
which(colnames(datos) == "idtram")
datos = datos[ , c(23, columnas_interes)]
datos[, c(2, ncol(datos))] = lapply(datos[, c(2, ncol(datos))], as.numeric)
datos = datos[datos$idtram %in% filtrar$idtram, ]


### General

for (i in 1:nrow(datos)) {
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



#########################
### Para hacer conteo ###
#########################

setwd("C:/Users/SIGEH/Desktop/Lalo/Gob/Proyectos")

datos = read.csv("../Extras/Tramites/ruts.tramites23abril2025.csv")
p = sf::read_sf("Accidentes_Mapa/Datos/Filtrados/2025_C5/2025.shp")
filtrar = read.csv("../Extras/Tramites/tramites_seleccion_variables_c_conteo_etapas_y_requisitos.csv")


columnas_interes = which(grepl(pattern = "coordinates", x = colnames(datos)))
length(columnas_interes)
which(colnames(datos) == "idtram")
datos = datos[ , c(23, columnas_interes)]
datos[, c(2, ncol(datos))] = lapply(datos[, c(2, ncol(datos))], as.numeric)
datos = datos[datos$idtram %in% filtrar$idtram, ]

idtram = rep(NA, 118)
conteo = rep(NA, 118)

for (i in 1:118) {
  coordenadas = matrix(unlist(c(datos[i, c(2:ncol(datos))])), ncol = 2, byrow = TRUE)
  eliminar = which(is.na(coordenadas[,1]))
  coordenadas = coordenadas[-eliminar,]
  id = datos$idtram[i]
  con = length(coordenadas)/2
  
  idtram[i] = id
  conteo[i] = con
}


c = as.data.frame(cbind(idtram, conteo))

write.csv(c, "../Extras/Tramites/conteo_centro_de_atencion.csv", fileEncoding = "UTF-8", row.names = F)


###############################################
### Prueba de todos juntos

archivos_shp = list.files(path = "../Extras/Tramites/shp/", pattern = "\\.shp$", full.names = TRUE)
nombres = basename(archivos_shp)
nombres = gsub(x = nombres, pattern = ".shp", replacement = "")
municipios = sf::read_sf("../Importantes_documentos_usar/Municipios/municipiosjair.shp")
municipios = municipios |> dplyr::select(CVEGEO,NOM_MUN)


plot(municipios$geometry)
for (i in seq_along(archivos_shp)) {
  datos = sf::read_sf(archivos_shp[i])
  inter = sf::st_within(datos, municipios)
  
  if (length(unlist(inter)) == 0) {
    cat("Vamos en el", i, " que es ", nombres[i], "\n")
    print("No hay intersección")
    print(datos$geometry)
    plot(datos$geometry, add = T, col = "red")
  }
  
  #plot(datos$geometry, add = T, col = "red")
}






