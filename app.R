# --- FIX PARA SERVIDORES SIN PANTALLA (como Codespaces) ---
options(rgl.useNULL = TRUE)
# --- FIN FIX ---

# 1. Cargar Librerías
library(shiny)
library(sf)
library(elevatr)
library(rayshader)
library(rgl) 
library(tibble)
library(tidygeocoder)
library(shinycssloaders) # Aunque no usemos su spinner, es bueno tenerlo.

# 2. Interfaz de Usuario (UI)
ui <- fluidPage(
    titlePanel("Generador de Mapas 3D Interactivo"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Introduce el nombre de cualquier lugar geográfico (ej: una montaña, ciudad o monumento) para generar un mapa 3D."),
            textInput("place_name", "Introduce el nombre del lugar:", "Monte Everest"),
            actionButton("generate_map", "Generar Mapa"),
            hr(),
            helpText("Nota: La generación del mapa puede tardar 1-2 minutos. Por favor, sé paciente.")
        ),
        
        mainPanel(
            # Ya no usamos withSpinner aquí, el progreso se mostrará como una notificación.
            rgl::rglwidgetOutput("map_3d", width = "100%", height = "80vh")
        )
    )
)

# 3. Lógica del Servidor
server <- function(input, output) {

    observeEvent(input$generate_map, {
        
        req(input$place_name)
        
        # --- LÓGICA DEL MAPA CON BARRA DE PROGRESO ---
        
        # Creamos el objeto de progreso. Tendrá 5 pasos.
        withProgress(message = 'Generando mapa...', value = 0, {
            
            # Paso 1: Iniciar
            incProgress(1/5, detail = "Iniciando proceso...")
            
            # a. Geocodificar el nombre del lugar
            location_coords <- tidygeocoder::geocode(
                tibble(address = input$place_name),
                address = address,
                method = 'osm'
            )
            
            if (is.na(location_coords$lat) || is.na(location_coords$long)) {
                showNotification("No se pudo encontrar la ubicación. Intenta con un nombre más específico.", type = "error")
                return()
            }
            
            # Paso 2: Actualizamos el progreso después de la geocodificación
            incProgress(1/5, detail = "Ubicación encontrada. Descargando datos de elevación...")

            # b. Crear un área de interés (bounding box)
            point_sf <- st_sfc(st_point(c(location_coords$long, location_coords$lat)), crs = 4326)
            bbox_sf <- st_as_sfc(st_bbox(st_buffer(point_sf, 0.2)))

            # c. Obtener datos de elevación (DEM)
            elevation_raster <- get_elev_raster(locations = bbox_sf, z = 10, clip = "bbox")
            
            # Paso 3: Actualizamos el progreso después de la descarga
            incProgress(1/5, detail = "Preparando datos para renderizado 3D...")

            # d. Convertir el raster a una matriz
            elevation_matrix <- raster_to_matrix(elevation_raster)

            # Paso 4: Actualizamos el progreso antes del renderizado final
            incProgress(1/5, detail = "Renderizando la escena 3D...")

            # e. Crear el mapa 3D con rayshader
            elevation_matrix %>%
                sphere_shade(texture = "imhof4") %>%
                plot_3d(elevation_matrix, zscale = 15, fov = 0, theta = 135, phi = 35, windowsize = c(1200, 800), zoom = 0.6)

            # f. Renderizar el widget 3D para la UI
            output$map_3d <- rgl::renderRglwidget({
                if (rgl::rgl.cur() > 0) {
                    rgl::rgl.close()
                }
                render_snapshot(clear = TRUE)
                rgl::rglwidget()
            })
            
            # Paso 5: Finalizar
            incProgress(1/5, detail = "¡Completado!")
            Sys.sleep(1) # Una pequeña pausa para que el usuario vea el mensaje "Completado"
        })
    })
}

# 4. Ejecutar la Aplicación
shinyApp(ui = ui, server = server)