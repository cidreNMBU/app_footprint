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

## Digital Surface Model


# 3. UI -------------------------------------------------------------------

ui <- page_sidebar(
    title = "Footprint Calculator",
    theme = bs_theme(preset = "flatly"),
    ## 3.1. SIDEBAR --------------
    sidebar = sidebar(
        title = h3("Filters"),
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
        actionBttn(
            inputId = "button",
            label   = "Apply",
            icon    = icon("play")
        )
    ),
    ## 3.2. BODY ----------------
    
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

    ## 4.2. Calc data --------------------
    ### Shade
    shade_sr <- reactive({

        req(dsm_sr())

        calculate_shadow(
            elevation_raster = dsm_sr(),
            azimuth          = boundaries_sf()$azimuth,
            altitude         = boundaries_sf()$sun_altitude
        )
    })
    ### Overlapping
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
    ### Area
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
        plot_lidar_footprint(lidar_footprint_sr(), boundaries_sf())
    })
    
    output$shade_plot <- renderPlot({
        plot_sun_shadow(shade_sr(), boundaries_sf())
    })
    
    output$overlapping_plot <- renderPlot({
        plot_overlap(overlapping_sr(), boundaries_sf())
    })
    
    ## 4.4. Value boxes -----------------
    
    output$valueboxes <- renderUI({
        
        tagList(
            fluidRow(
                column(
                    3,
                    value_box(
                        title = "Plot Size",
                        value = paste0(plot_area(), "ha"),
                        showcase = bs_icon("graph-up"),
                        theme = "primary"
                    )
                ),
                column(
                    3,
                    value_box(
                        title = "LiDAR footprint",
                        value = paste0(lidar_area(), "ha"),
                        showcase = bs_icon("graph-up"),
                        theme = "teal"
                    )
                ),
                column(
                    3,
                    value_box(
                        title = "Sun Radiation",
                        value = paste0(shade_area(), "ha"),
                        showcase = bs_icon("graph-up"),
                        theme = "secondary"
                    )
                ),
                column(
                    3,
                    value_box(
                        title = "LiDAR footprint",
                        value = paste0(overlapping_area(), "ha"),
                        showcase = bs_icon("graph-up"),
                        theme = "primary"
                    ) 
                )
            )
        )
        
    })
    
    
    
    
}

shinyApp(ui, server)




