library(raster)

setwd("C:/Users/SIGEH/Desktop/Lalo/Gob/Extras/Tramites/")

uso_de_suelo=raster("Accesibilidad/uso_de_suelo_friccion.tif")
pendiente=raster("Accesibilidad/pendiente.tif")
carreteras=raster("Accesibilidad/carreteras.tif")
extent(carreteras)==extent(pendiente) &
  extent(uso_de_suelo)==extent(pendiente)

#Sí me voy a tomar la libertad de actualizar los valores del raster que estén cerca de 90 grados
pendiente[pendiente<95.9 & pendiente>=90]=95.9
pendiente[pendiente<=90 & pendiente>84.9]=84.9

####Accesibilidad a pie
slp_walk = 6 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.05))  # Calcula la velocidad de caminata ajustada por la pendiente.
terrain_walk_spd = uso_de_suelo * slp_walk       #Le quité el /5.0. Quiero pensar que es la velocidad de caminata según uso de suelo. El promedio es de 5.5 km/h         # Calcula la velocidad sobre el terreno ajustada por la pendiente y el uso de suelo.

##Accesibilidad por carreteras
slp_car = 50 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.12))  # Calcula la velocidad sobre carreteras ajustada por la pendiente.
sloped_road_spd = carreteras * slp_car / 50.0 # Calcula la velocidad ajustada por pendiente para carreteras y la convierte en un raster.
merged_spd = merge(sloped_road_spd, terrain_walk_spd)     # Combina los rasters de velocidad de carreteras y terreno.
friction = 1.0 / (merged_spd * 1000 / 60.0 ) 

library(gdistance)
Trans = transition(friction, function(x) 1 / mean(x), 8)  # Crea una matriz de transición basada en la fricción.
T.GC = geoCorrection(Trans, type="c") 

hidalgo=st_read("Accesibilidad/hidalgo/LIM_MUNICIPALES.shp")  #### HASTA AQUI CARGA



matrix(unlist(lugares_destino_ficticios$geometry),nrow = nrow(lugares_destino_ficticios),ncol = 2,byrow = T)

library(sf)
lugares_destino_ficticios = read_sf("Tramites shp/1717_pago_de_creditos_fiscales_federales_coordinados.shp") |> st_cast("POINT") |> st_transform(st_crs(hidalgo))
tiempo_zona = accCost(T.GC, sf::st_coordinates(lugares_destino_ficticios))
plot(tiempo_zona)
plot(lugares_destino_ficticios,add=T)
# tiempo_zona |> writeRaster("accesibilidad_id_572.tiff")

crs(tiempo_zona)=crs(hidalgo)
tiempo_zona[is.infinite(tiempo_zona)] = NA
tiempo_zona[tiempo_zona >= 300] = 300
hist(tiempo_zona)


#Paleta
tiempo_zona[is.infinite(tiempo_zona)] = NA
tiempo_zona[tiempo_zona >= 300] = 300
min_valor = raster::cellStats(tiempo_zona, stat = 'min')
max_valor = raster::cellStats(tiempo_zona, stat = 'max')


paleta = colorNumeric(
  palette = viridisLite::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5),
  domain = values(tiempo_zona),  
  na.color = "transparent"
)

mapa_web = leaflet() |> 
  addTiles() |> 
  addRasterImage(x = tiempo_zona, colors = viridis::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5)) |> 
  addLegend(values = values(tiempo_zona) , pal = paleta, title = paste0("Tiempo", "<br>","Aproximado", "<br>","(Minutos)"), position = "bottomright")

mapa_web

library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(leafem)

municipios = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")
mapa_web = leaflet() |> 
  addTiles() |> 
  addPolygons(data=municipios |> as("Spatial"),
              label = municipios$NOM_MUN,fillColor = "gray",fillOpacity = 0.1,color = "white",weight = 1,opacity = 0.4,group = "Municipios") |> 
  addRasterImage(x = tiempo_zona, colors = viridis::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5)) |> 
  addLegend(values = c(min_valor:max_valor) , pal = paleta, title = paste0("Tiempo", "<br>","Aproximado"), position = "bottomright",
            labFormat = labelFormat(
              between = " – ",
              suffix = " min",
              transform = function(x) {x}
            )) |> 
  addSearchFeatures(targetGroups = c("Municipios"),
                    options = searchFeaturesOptions(
                      zoom = 12,
                      openPopup = F,
                      firstTipSubmit =F,initial = F,
                      hideMarkerOnCollapse =T))|>setView(lng = -98.7591, lat = 20.0511, zoom = 9) |> 
  addLogo(img = "https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/laboratorio_planeacion.png", position = "bottomleft", src = "remote", width = "399px",height = "80px" )

mapa_web




################################
### Paleta de colores imagen ###
################################

my_palette = colorNumeric(
  palette = viridisLite::turbo(n = length(min_valor:max_valor), direction = -1),
  domain = values(tiempo_zona),  
  na.color = "transparent"
)

colors = turbo(n = length(min_valor:max_valor), direction = -1)

# Crear un data frame para graficar
df <- data.frame(
  x = 1:length(colors),
  y = 1,
  fill = colors
)

# Graficar la paleta como barra horizontal
p = ggplot(df, aes(x = x, y = y, fill = fill)) +
  geom_tile() +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0))

# Guardar como imagen
ggsave("../../../../paleta_colores.png", plot = p, width = 10, height = 1, dpi = 300)




### Intento


























paleta_fac = colorFactor(
  palette = viridisLite::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5),
  domain = as.factor(c("Inmediato", "50min", "1 hora 40 minutos", "5 horas o mas"), ),
  na.color = "transparent",
  levels = c("Inmediato", "50min", "1 hora 40 minutos", "5 horas o mas"))
)


tiempo_estimado = function(minutos) {
  if (minutos >= 300) {
    return("Más de 5 horas")
  }
  
  horas = floor(minutos / 60)
  mins = minutos %% 60
  
  horas_texto = if (horas > 0) paste(horas, ifelse(horas == 1, "hora", "horas")) else ""
  minutos_texto = if (mins > 0) paste(mins, ifelse(mins == 1, "minuto", "minutos")) else ""
  
  
  return(stringr::str_trim(ifelse(paste(horas_texto, minutos_texto) == "", yes = "Inmediato", no = paste(horas_texto, minutos_texto))))
}

unique(values(tiempo_zona)[!is.na(values(tiempo_zona))]) |> lapply(tiempo_estimado)


mapa_web = leaflet() |> 
  addTiles() |> 
  addRasterImage(x = tiempo_zona, colors = viridis::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5)) |> 
  addLegend(values = c("Inmediato", "50min", "1 hora 40 minutos", "5 horas o mas") , pal = paleta_fac, title = paste0("Tiempo", "<br>","Aproximado", "<br>","(Minutos)"), position = "bottomright",
            labFormat = labelFormat(
              transform = function(x) {
                # Redondear a horas y agregar "hora(s)"
                horas <- round(as.numeric(x) / 60)
                paste0(horas, ifelse(horas == 1, " hora", " horas"))
              }
            ))
mapa_web




