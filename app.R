# --- FIX FOR HEADLESS SERVERS (like Codespaces) ---
# Tell rgl not to look for a display, and use its web-based capabilities
options(rgl.useNULL = TRUE)
# --- END FIX ---

# 1. Load Libraries
library(shiny)
library(sf)
library(elevatr)
library(rayshader)
library(rgl) 
library(tidygeocoder)
library(shinycssloaders)

# 2. User Interface (UI)
ui <- fluidPage(
    titlePanel("Interactive 3D Map Generator"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Enter the name of any geographical location (e.g., a mountain, city, or landmark) to generate a 3D map."),
            textInput("place_name", "Enter Place Name:", "Mount Everest"),
            actionButton("generate_map", "Generate Map"),
            hr(),
            helpText("Note: Map generation can take 1-2 minutes depending on the location and server load. Please be patient.")
        ),
        
        mainPanel(
            # Explicitly call rglwidgetOutput from the rgl package
            withSpinner(rgl::rglwidgetOutput("map_3d", width = "100%", height = "80vh"), type = 6) # <-- CHANGE HERE
        )
    )
)

# 3. Server Logic
server <- function(input, output) {

    map_generated <- reactiveVal(FALSE)

    observeEvent(input$generate_map, {
        
        req(input$place_name)
        
        location_coords <- tidygeocoder::geocode(
            tibble(address = input$place_name),
            address = address,
            method = 'osm'
        )
        
        if (is.na(location_coords$lat) || is.na(location_coords$long)) {
            showNotification("Could not find location. Please try a more specific name.", type = "error")
            return()
        }

        point_sf <- st_as_sfc(st_point(c(location_coords$long, location_coords$lat)), crs = 4326)
        bbox_sf <- st_as_sfc(st_bbox(st_buffer(point_sf, 0.2)))

        elevation_raster <- get_elev_raster(locations = bbox_sf, z = 10, clip = "bbox")
        elevation_matrix <- raster_to_matrix(elevation_raster)

        elevation_matrix %>%
            sphere_shade(texture = "imhof4") %>%
            plot_3d(elevation_matrix, zscale = 15, fov = 0, theta = 135, phi = 35, windowsize = c(1200, 800), zoom = 0.6)

        # Explicitly call renderRglwidget and rglwidget from the rgl package
        output$map_3d <- rgl::renderRglwidget({ # <-- CHANGE HERE
            if (rgl::rgl.cur() > 0) {
                rgl::rgl.close()
            }
            map_generated(TRUE)
            render_snapshot(clear = TRUE)
            rgl::rglwidget() # <-- CHANGE HERE
        })
    })
}

# 4. Run the Application
shinyApp(ui = ui, server = server)