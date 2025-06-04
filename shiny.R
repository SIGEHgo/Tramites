library(shiny)
library(leaflet)
library(archive)
library(DT)
library(sf)

# UI ----
ui = fluidPage(
  titlePanel("Leaflet para Dummies"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "filemap",
        label = "Upload map. Choose shapefile",
        multiple = TRUE,
        accept = c('.shp', ".dbf", ".shx", ".sbx", ".sbn", ".prj", ".rar")
      ),
      actionButton("add_filter", "Agregar Filtro"),
      uiOutput("filters_ui") # Contenedor dinámico para los filtros
    ),
    mainPanel(
      fluidPage(
        titlePanel("Basic DataTable"),
        DT::dataTableOutput("contents")
      ),
      leafletOutput("map")
    )
  )
)

# Función para manejar archivos temporales ----
rutina_crear_copias_temporales = function(inputFiles) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  if (!grepl(".rar", inputFiles$datapath[1])) {
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
  
  # Agregar un nuevo filtro dinámico
  observeEvent(input$add_filter, {
    req(df())
    current_filters <- filters()
    filter_id <- paste0("filter_", length(current_filters) + 1)
    current_filters[[filter_id]] <- list(
      selectInput(
        inputId = paste0(filter_id, "_column"),
        label = "Selecciona una columna:",
        choices = colnames(df())
      ),
      uiOutput(outputId = paste0(filter_id, "_text"))
    )
    filters(current_filters)
  })
  
  # Renderizar los filtros dinámicos
  output$filters_ui <- renderUI({
    req(filters())
    filter_ui_list <- filters()
    do.call(tagList, lapply(names(filter_ui_list), function(filter_id) {
      tagList(
        filter_ui_list[[filter_id]][[1]], # SelectInput
        filter_ui_list[[filter_id]][[2]] # TextInput
      )
    }))
  })
  
  # Observar las selecciones y crear textInput dinámicos
  observe({
    lapply(names(filters()), function(filter_id) {
      column_input <- paste0(filter_id, "_column")
      text_input <- paste0(filter_id, "_text")
      
      observeEvent(input[[column_input]], {
        req(input[[column_input]])
        output[[text_input]] <- renderUI({
          textInput(
            inputId = paste0("filter_value_", filter_id),
            label = paste("Filtro para", input[[column_input]]),
            value = "",
            placeholder = "Por ejemplo: >5"
          )
        })
      })
    })
  })
  
  # Aplicar filtros dinámicos
  filtered_data <- reactive({
    req(df())
    data <- df()
    for (filter_id in names(filters())) {
      column <- input[[paste0(filter_id, "_column")]]
      value <- input[[paste0("filter_value_", filter_id)]]
      if (!is.null(column) && !is.null(value) && nchar(value) > 0) {
        tryCatch({
          filter_expr <- parse(text = paste(column, value))
          data <- data[eval(filter_expr, envir = data), ]
        }, error = function(e) {
          showNotification("Error en el filtro. Verifica la sintaxis.", type = "error")
        })
      }
    }
    data
  })
  
  # Mostrar tabla
  output$contents <- DT::renderDataTable({
    req(filtered_data())
    filtered_data()
  })
  
  # Mostrar mapa
  output$map <- renderLeaflet({
    req(filtered_data())
    if(is.na(st_crs(filtered_data()))){
      filtered_data()=st_set_crs(filtered_data())
    }
    leaflet() |>
      addTiles() |>
      addPolygons(data = filtered_data()|>st_zm()|>st_transform(st_crs("EPSG:4326")))
  })
}

shinyApp(ui, server)

