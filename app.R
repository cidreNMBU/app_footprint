#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# 1. Import packages ------------------------------------------------------

library(bsicons)
library(bslib)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(sf)
library(suntools)
library(terra)
library(tidyterra)
library(tidyverse)

source("R/utils.R")

# 2. Load data ------------------------------------------------------------

## Load valeer data
valeer_tbl <- read_csv("data/valeer.csv")

## Load boundaries
boundaries_raw_sf <- read_rds("data/boundaries_vector.rds")

## Load UAV positions
uav_tbl <- read_rds("data/drone_position.rds")

## Digital Surface Model


# 3. UI -------------------------------------------------------------------

ui <- page_navbar(
    title = "Footprint Calculator",
    theme = bs_theme(preset = "litera"),
    ## 3.1. SIDEBAR --------------
    sidebar = sidebar(
        title = h3("Filters"),
        hr(),
        ## 3.1.1. Filters section
        virtualSelectInput(
            inputId  = "plot_id",
            label    = "Select plot",
            choices  = unique(valeer_tbl$Plot_ID),
            selected = "P1_1",
            search   = TRUE
        ),
        virtualSelectInput(
            inputId  = "hour",
            label    = "Select hour",
            choices  = paste(0:23, "00", sep = ":"),
            selected = "15:00"
        ),
        dateInput(
            inputId = "date",
            label   = "Select date",
            value   = "2023-06-01"
        ),
        hr(),
        actionBttn(
            inputId = "button",
            label   = "Apply",
            icon    = icon("play")
        ),
        hr(),
        ## 3.1.2. Download section
        h3("Download analysis"),
        hr(),
        p("Download the files from the current analysis"),
        downloadBttn(
            outputId = "download_data",
            label    = "Download",
            color    = "success"
        )
    ),
    ## 3.2. BODY ----------------
    nav_panel(
        title = "Analyzer",
        uiOutput("valueboxes"),
        
        layout_columns(
            card(
                card_header("Lidar Footprint Area"),
                plotOutput("footprint_plot"),
                full_screen = TRUE
            ),
            card(
                card_header("Sun Exposure Area"),
                plotOutput("shade_plot"),
                full_screen = TRUE
            ),
            card(
                card_header("Overlapping Area"),
                plotOutput("overlapping_plot"),
                full_screen = TRUE
            )
        )
    ),
    
    nav_panel(
        title = "Interactive",
        leafletOutput("leaflet_map")
    )
    
    
)

# 4. Server ---------------------------------------------------------------

server <- function(input, output) {
    
    ## 4.1. Get data ---------------------
    ### Boundaries
    boundaries_sf <- reactive({
        sel_plot <- boundaries_raw_sf[[input$plot_id]] 
        sel_plot |> 
            mutate(
                azimuth      = get_sun_position(
                    data  = sel_plot, 
                    date  = str_glue("{input$date} {input$hour}:00"), 
                    which = "azimuth"
                ),
                sun_altitude = get_sun_position(
                    data  = sel_plot, 
                    date  = str_glue("{input$date} {input$hour}:00"),
                    which = "altitude"
                ),
                .before = 1
            )
    }) |> bindEvent(input$button, ignoreNULL = FALSE)
    
    ### Raster
    lidar_footprint_sr <- reactive({
        rast(
            str_glue("data/lidar_footprints/{input$plot_id}.tif")
        )
    }) |> bindEvent(input$button, ignoreNULL = FALSE)
    
    ### DSM
    dsm_sr <- reactive({

        req(boundaries_sf())

        dsm <- rast("data/dsm.tif")
        crop(dsm, vect(boundaries_sf()))
    }) |> bindEvent(input$button, ignoreNULL = FALSE)
    
    ### UAV position
    uav_r <- reactive({
        
        uav_tbl |> 
            filter(plot_id == input$plot_id)
        
    }) |> bindEvent(input$button, ignoreNULL = FALSE)
    
    
    ## 4.2. Calc data --------------------
    ### Shade layer
    shade_sr <- reactive({

        req(dsm_sr())

        calculate_shadow(
            elevation_raster = dsm_sr(),
            azimuth          = boundaries_sf()$azimuth,
            altitude         = boundaries_sf()$sun_altitude
        )
    })
    ### Overlapping layer
    overlapping_sr <- reactive({

        req(lidar_footprint_sr())
        req(shade_sr())
        req(boundaries_sf())

        get_overlapping_area(
            x = lidar_footprint_sr(),
            y = shade_sr(),
            z = boundaries_sf()
        )
    })
    ### Calculate area
    plot_area <- reactive({
        req(boundaries_sf())
        
        boundaries_sf() |> 
            st_area() |>
            units::set_units(ha) |> 
            as.numeric() |> 
            round(2)
    })
    
    lidar_area <- reactive({
        req(lidar_footprint_sr())
        calculate_area(lidar_footprint_sr(), boundaries_sf())
    })
    
    shade_area <- reactive({
        req(shade_sr())
        calculate_area(shade_sr(), boundaries_sf())
    })
    
    overlapping_area <- reactive({
        req(overlapping_sr())
        calculate_area(overlapping_sr(), boundaries_sf())
    })

    
    ## 4.3. Visualizations ----------------
    
    output$footprint_plot <- renderPlot({
        plot_lidar_footprint(
            data       = lidar_footprint_sr(), 
            uav_data   = uav_r(),
            boundaries = boundaries_sf()
        )
    })
    
    output$shade_plot <- renderPlot({
        plot_sun_shadow(
            data       = shade_sr(),
            uav_data   = uav_r(),
            boundaries = boundaries_sf()
        )
    })
    
    output$overlapping_plot <- renderPlot({
        plot_overlap(
            data       = overlapping_sr(),
            uav_data   = uav_r(),
            boundaries = boundaries_sf()
        )
    })
    
    ## 4.4. Value boxes -----------------
    
    output$valueboxes <- renderUI({
        
        tagList(
            fluidRow(
                column(
                    3,
                    value_box(
                        title    = "Plot Size",
                        value    = paste0(plot_area(), "ha"),
                        showcase = bs_icon("graph-up"),
                        theme    = "primary"
                    )
                ),
                column(
                    3,
                    value_box(
                        title    = "LiDAR footprint",
                        value    = paste0(lidar_area(), "ha"),
                        showcase = bs_icon("graph-up"),
                        theme    = "teal"
                    )
                ),
                column(
                    3,
                    value_box(
                        title    = "Sun Radiation",
                        value    = paste0(shade_area(), "ha"),
                        showcase = bs_icon("graph-up"),
                        theme    = "secondary"
                    )
                ),
                column(
                    3,
                    value_box(
                        title    = "LiDAR footprint",
                        value    = paste0(overlapping_area(), "ha"),
                        showcase = bs_icon("graph-up"),
                        theme    = "primary"
                    ) 
                )
            )
        )
        
    })
    
    # 5. Leaflet map --------------
    
    ## React on click
    leaflet_map <- reactive({
        
        req(uav_r())
        req(overlapping_sr())
        req(lidar_footprint_sr())
        req(shade_sr())
        req(boundaries_sf())
        
        get_leaflet_map(
            uav_r              = uav_r(),
            overlapping_sr     = overlapping_sr(),
            shade_sr           = shade_sr(),
            lidar_footprint_sr = lidar_footprint_sr(),
            boundaries_sf      = boundaries_sf()
        )
    }) |> bindEvent(input$button, ignoreNULL = FALSE)
    
    output$leaflet_map <- renderLeaflet({
        
        leaflet_map()

    })
    
    # 6. Download data ------------
    output$download_data <- downloadHandler(
        
        filename = function() {
            fname <- paste("data", 
                  input$plot_id, 
                  input$date,
                  paste0(str_sub(input$time, 1, 2), "h"),
                  sep = "-")
            paste0(fname, ".zip")
        },
        
        content = function(file) {
            ## Download rasters
            download_rst <- c(
                lidar_footprint_sr(),
                shade_sr(),
                overlapping_sr()
            )
            names(download_rst) <- c("lidar_footprint", "shade", "overlapping")
            writeRaster(download_rst, "rasters.tif", overwrite = TRUE)
            ## Download boundaries
            write_sf(
                boundaries_sf(),
                "boundaries.geojson",
                append = FALSE
            )
            ## Zip files
            zip::zip(file, files = c("rasters.tif", "boundaries.geojson"))
        }
        
    )
    
    
    
}

shinyApp(ui, server)




