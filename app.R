#========================================================
# SETTINGS
#========================================================

source("global.R")
source("functions.R")

netcdf_root <- "NetCDF"

variables <- c("R99p", "R20mm", "FFDIgt50", "TX90p", "WSDI")

periods <- as.character(
  seq(
    2030,
    2090,
    by = 5
  )
)

#========================================================
# UI
#========================================================

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      #map {
        cursor: crosshair !important;
      }
    "))
  ),

  titlePanel(
    "NARCliM Climate Extraction Tool"
  ),

  sidebarLayout(

    sidebarPanel(

      radioButtons(
        "input_method",
        "Location Input",
        choices = c(
          "Point",
          "Rectangle Average",
          "CSV Upload"
        )
      ),

      conditionalPanel(
        condition =
          "input.input_method == 'CSV Upload'",

        fileInput(
          "csv_file",
          "Upload CSV"
        )
      ),

      selectInput(
        "variable",
        "Climate Variable",
        choices = variables,
      ),

      checkboxGroupInput(
        "scenario",
        "Scenario",
        choices = c(
          "SSP126",
          "SSP370"
        ),
      ),

      checkboxGroupInput(
        "period",
        "Period",
        choices = periods,
      ),

      actionButton(
        "extract",
        "Extract Data"
      ),

      br(),
      br(),

      downloadButton(
        "download_csv",
        "Download Results"
      )

    ),

    mainPanel(

      leafletOutput(
        "map",
        height = 500
      ),

      br(),

      tableOutput(
        "results_table"
      )

    )

  )

)

#========================================================
# SERVER
#========================================================

server <- function(
    input,
    output,
    session
){

  clicked <- reactiveVal(NULL)

  rectangle_bounds <- reactiveVal(NULL)

  #------------------------------------------------------
  # MAP
  #------------------------------------------------------

  output$map <- renderLeaflet({

    leaflet() |>

      addProviderTiles(
        providers$Esri.WorldTopoMap
      ) |>

      addDrawToolbar(
        targetGroup = "draw",

        rectangleOptions =
          drawRectangleOptions(),

        polygonOptions = FALSE,

        circleOptions = FALSE,

        markerOptions = FALSE,

        polylineOptions = FALSE,

        circleMarkerOptions = FALSE
      ) |>

      setView(
        lng = 134,
        lat = -25,
        zoom = 4
      )

  })

  observeEvent(
    input$map_click,
    {

      if(
        input$input_method !=
        "Point"
      ){
        return()
      }

      click <- input$map_click

      clicked(
        c(
          click$lng,
          click$lat
        )
      )

      leafletProxy("map") |>
        clearMarkers() |>
        addCircleMarkers(
          lng = click$lng,
          lat = click$lat,
          radius = 6,
          color = "red",
          weight = 2,
          fillOpacity = 1
        )

    }
  )

  observeEvent(
    input$map_draw_new_feature,
    {

      if(
        input$input_method !=
        "Rectangle Average"
      ){
        return()
      }

      feature <- input$map_draw_new_feature

      print(feature)

      coords <- feature$geometry$coordinates[[1]]

      coords_df <- do.call(
        rbind,
        lapply(
          coords,
          unlist
        )
      )

      rectangle_bounds(

        list(
          xmin = min(coords_df[,1]),
          xmax = max(coords_df[,1]),
          ymin = min(coords_df[,2]),
          ymax = max(coords_df[,2])
        )

      )

    }
  )

  #------------------------------------------------------
  # EXTRACTION
  #------------------------------------------------------

  results <- eventReactive(
    input$extract,
    {

      all_results <- list()

      if(
        input$input_method ==
          "Point"
      ){

        req(clicked())

        locations <- tibble(
          Location = "Map_Point",
          Longitude = clicked()[1],
          Latitude = clicked()[2]
        )

      } else if(

        input$input_method ==
          "Rectangle Average"

      ){

        req(
          rectangle_bounds()
        )

        locations <- tibble(
          Location = "Area_Average",
          Longitude = NA,
          Latitude = NA
        )

      } else {

        req(input$csv_file)

        locations <- read_csv(
          input$csv_file$datapath,
          show_col_types = FALSE
        )

        if(
          "Name" %in%
            names(locations)
        ){

          locations$Location <-
            locations$Name

        } else {

          locations$Location <- 
            paste0(
              "Site_",
              seq_len(
                nrow(locations)
              )
            )
          
      }

      leafletProxy("map") |>
        clearMarkers() |>
        addCircleMarkers(
          data = locations,
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 5,
          color = "blue",
          fillOpacity = 1,
          label = ~Location
        )

      nc_files <- list.files(
        netcdf_root,
        pattern = paste0("^", input$variable, ".*\\.nc$"),
        full.names = TRUE
      )

      for(file in nc_files){

        fname <- basename(file)

        scenario <- strsplit(
          fname,
          "_"
        )[[1]][2]

        if(
          !scenario %in%
            input$scenario
        ){
          next
        }

        model <- get_model(file)

        for(i in seq_len(
          nrow(locations)
        )){

          if(
            input$input_method ==
              "Rectangle Average"
          ){

            bounds <-
              rectangle_bounds()

            data <-
              extract_area_mean(

                file,

                bounds$xmin,
                bounds$xmax,

                bounds$ymin,
                bounds$ymax,

                input$period

              )

          } else {

            data <-
              extract_point(

                file,

                locations$Longitude[i],

                locations$Latitude[i],

                input$period

              )

          }

          data$Scenario <- scenario

          data$Model <- model

          data$Location <-
            locations$Location[i]

          data$Longitude <-
            locations$Longitude[i]

          data$Latitude <-
            locations$Latitude[i]

          all_results[[length(all_results) + 1]] <- data
        }

      }

      bind_rows(
        all_results
      )

    }
  )

  #------------------------------------------------------
  # RESULTS
  #------------------------------------------------------

  output$results_table <-
    renderTable({

      req(
        results()
      )

      results()

    })

  #------------------------------------------------------
  # DOWNLOAD
  #------------------------------------------------------

  output$download_csv <-
    downloadHandler(

      filename = function(){

        paste0(
          "NarClim_Results_",
          Sys.Date(),
          ".csv"
        )

      },

      content = function(file){

        write_csv(
          results(),
          file
        )

      }

    )

}

#========================================================
# RUN APP
#========================================================

shinyApp(
  ui,
  server
)
