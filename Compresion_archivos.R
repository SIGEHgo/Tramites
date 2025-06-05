### Compresion
todos_archivos = list.files(path = "Tramites shp/", all.files = T, full.names = T)
archivos = list.files(path = "Tramites shp/", all.files = T, pattern = ".shp", full.names = T)
ramas = sub(x = basename(archivos), pattern = "_.*", replacement = "")

for (i in seq_along(ramas)) {
  archivos_interes= todos_archivos[sub("_.*", "", basename(todos_archivos)) == ramas[i]]
  nombre = basename(archivos_interes[1])
  nombre = sub(x = nombre, pattern = "\\.[^\\.]*$", replacement = "") # Quedarme con lo anterior del ultimo punto
  nombre = gsub(x = nombre, pattern = "\\.", replacement = "")
  nombre = substr(nombre, 1, 100)  # Cortar nombre porque es gigantesco
  
  cat("Vamos en", i, "corresponde a: ", nombre, "\n")
  zip::zip(zipfile = paste0("Comprimidos_Tramites_shp/", nombre,".zip"), 
           files = archivos_interes,
           mode = "cherry-pick",
           include_directories = FALSE
           )
}




