library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(leafem)
library(archive)
library(DT)
library(sf)
library(raster)

### Carga de otro archivo de R
source("app_web_auxiliars.R")

##### Accesibilidad Previa
municipios = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")

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

hidalgo= sf::st_read("Accesibilidad/hidalgo/LIM_MUNICIPALES.shp")



card <- function(title, ...) {
  htmltools::tags$div(
    class = "card",
    htmltools::tags$div(class = "card-header", title),
    htmltools::tags$div(class = "card-body", ...)
  )
}

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  includeCSS(css_btn_area),
  
  fluidRow(
    column(width = 4, style = "height: 100vh; background-color: #f5f5f5; padding: 20px;", # Added padding here
           h2("Cálculo de Accesibilidad"),
           HTML(
             "<p>
          La accesibilidad se calcula como el costo de traslado a un lugar de destino predefinido. Para obtenerlo, se considera:
          <ul>
            <li><strong>Vialidades carreteras</strong> en el estado, así como sus velocidades promedio.</li>
            <li>Tipo de <strong>uso de suelo</strong>.</li>
            <li>Modelo digital de <strong>elevación</strong>.</li>
          </ul>
          Un modelo de movilidad sobre grafos determina el costo mínimo de traslado (en minutos) desde cada punto del estado hacia el más cercano de los lugares destino.
        </p>"
           ),
           card(
             title = "Agrega las ubicaciones. Puedes seleccionar varios archivos o subir un archivo .rar",
             full_screen = FALSE,
             # Centering the content horizontally and vertically within the card's body
             div(style = " display: flex; flex-direction: column; justify-content: center; align-items: center;",
                 fileInputArea(
                   inputId = "filemap",
                   
                   label = "Arrastra o selecciona tus archivos .shp, .dbf, .shx, .prj, etc. aquí:",
                   buttonLabel = "Click para seleccionar archivos",
                   multiple = TRUE,
                   accept = c('.shp', ".dbf", ".shx", ".sbx", ".sbn", ".prg", ".rar", ".zip")
                 ),
                 shiny::tableOutput("files")
             )
           )
    ),
    column(width = 8, style = "height: 100vh;",
           leafletOutput("map", height = "100%")
    )
  )
)
# Función para manejar archivos temporales ----
rutina_crear_copias_temporales <- function(inputFiles) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  if (!grepl("\\.(rar|zip)$", inputFiles$datapath[1], ignore.case = TRUE)) {
    for (i in seq_along(inputFiles$name)) {
      file.copy(inputFiles$datapath[i], file.path(temp_dir, inputFiles$name[i]))
    }
  } else {
    file.copy(inputFiles$datapath[1], file.path(temp_dir, inputFiles$name[1]))
    archive_extract(file.path(temp_dir, inputFiles$name[1]), dir = temp_dir)
  }
  return(temp_dir)
}

# Servidor ----
server <- function(input, output, session) {
  # Cargar shapefile
  df <- reactive({
    req(input$filemap)
    temp_dir <- rutina_crear_copias_temporales(input$filemap)
    read_sf(list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE))
  })
  
  
  # Lista reactiva para almacenar filtros dinámicos
  filters <- reactiveVal(list())
  

  # Mostrar mapa
  output$map <- renderLeaflet({
    req(df())
    if(is.na(st_crs(df()))){
      df=st_set_crs(df(),value = )
    }
    
    puntos = df()
    puntos=puntos |> st_transform(st_crs(hidalgo))
    coordenadas = sf::st_coordinates(puntos)
    tiempo_zona = accCost(T.GC, coordenadas)
    raster::crs(tiempo_zona) = crs(hidalgo)

    ###Pendiente: Municipio de ubicación.
    
    tiempo_zona[is.infinite(tiempo_zona)] = NA
    tiempo_zona[tiempo_zona >= 300] = 300
    min_valor = raster::cellStats(tiempo_zona, stat = 'min')
    max_valor = raster::cellStats(tiempo_zona, stat = 'max')
    
    paleta = colorNumeric(
      palette = viridisLite::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5),
      domain = values(tiempo_zona),  
      na.color = "transparent"
    )
    

    leaflet() |>
      addTiles() |> 
      addMarkers(data=df() |> st_cast("POINT") |> as("Spatial"), 
                 popup = ~paste0(
                   "Nombre: <b>", nombre, "</b><br>",
                   "Secretaría: <b>", scrtr_n, "</b><br>",
                   "Dependencia: <b>", dpndnc_n, "</b><br>",
                   "Nombre establecimiento: <b>", nmbr_st, "</b><br>",
                   "Horario del establecimiento: <b>", horr_st, "</b>"
                 ),
                 label = ~nombre) |> 
      addRasterImage(x = tiempo_zona, colors = viridis::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5)) |> 
      addPolygons(data=municipios |> as("Spatial"),
                  label = municipios$NOM_MUN,fillColor = "gray",fillOpacity = 0.1,color = "white",weight = 1,opacity = 0.4,group = "Municipios") |> 
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
  })
}

shinyApp(ui, server)






  





