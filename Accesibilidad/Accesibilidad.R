####Cargamos los datos a utilizar:
#Pendiente
#Uso de Suelo
library(raster)
setwd("../../../../Github/Repos/Accesibilidad_Documentación")

uso_de_suelo=raster("Inputs/uso_de_suelo_friccion.tif")
pendiente=raster("Inputs/pendiente.tif")
carreteras=raster("Inputs/carreteras.tif")
extent(carreteras)==extent(pendiente) &
  extent(uso_de_suelo)==extent(pendiente)
#Sí me voy a tomar la libertad de actualizar los valores del raster que estén cerca de 90 grados
pendiente[pendiente<95.9 & pendiente>=90]=95.9
pendiente[pendiente<=90 & pendiente>84.9]=84.9
####Accesibilidad a pie
slp_walk = 6 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.05))  # Calcula la velocidad de caminata ajustada por la pendiente.
#plot(-90:90,6*exp(-0.4*abs(tan(-90:90*pi/180)))+0.05,'l',ylab='Velocidad km/h',main='Velocidad caminando en función de la pendiente',xlab='Grados')
terrain_walk_spd = uso_de_suelo * slp_walk       #Le quité el /5.0. Quiero pensar que es la velocidad de caminata según uso de suelo. El promedio es de 5.5 km/h         # Calcula la velocidad sobre el terreno ajustada por la pendiente y el uso de suelo.

#plot(NA,xlim=extent(terrain_walk_spd)[c(1,2)],
     #ylim=extent(terrain_walk_spd)[c(3,4)],main='Velocidad caminando\n en función de la pendiente y fricción del uso de suelo')
#plot(terrain_walk_spd,add=T)


##########Accesibilidad por carreteras
slp_car = 50 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.12))  # Calcula la velocidad sobre carreteras ajustada por la pendiente.

#plot(-90:90,50 * exp(-0.4 * abs(tan(-90:90 * pi / 180) + 0.12)),'l',ylab='Velocidad km/h',main='Velocidad en auto en función de la pendiente',xlab='Grados')
                       # Carga un raster de carreteras y multiplica por 10 para definir velocidad.
#plot(NA,xlim=extent(slp_car)[c(1,2)],
     #ylim=extent(slp_car)[c(3,4)],main='Velocidad en auto\n en función de la pendiente')
#plot(slp_car,add=T)



sloped_road_spd = carreteras * slp_car / 50.0 # Calcula la velocidad ajustada por pendiente para carreteras y la convierte en un raster.


merged_spd = merge(sloped_road_spd, terrain_walk_spd)     # Combina los rasters de velocidad de carreteras y terreno.

friction = 1.0 / (merged_spd * 1000 / 60.0 ) 
#plot(friction)
#install.packages("gdistance")
library(gdistance)
Trans = transition(friction, function(x) 1 / mean(x), 8)  # Crea una matriz de transición basada en la fricción.
T.GC = geoCorrection(Trans, type="c") 

hidalgo=st_read("Inputs/hidalgo/LIM_MUNICIPALES.shp")
n=15
lugares_destino_ficticios=st_sample(hidalgo$geometry,n)
tiempo_zona = accCost(T.GC, matrix(unlist(lugares_destino_ficticios),nrow = n,ncol = 2,byrow = T))  # Calcula el costo acumulado desde un punto de inicio (coordenadas especificadas) usando la matriz de transición corregida (T.GC).
plot(tiempo_zona)
plot(lugares_destino_ficticios,add=T)
