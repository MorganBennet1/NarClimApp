#========================================================
# SETTINGS
#========================================================

source("global.R")
source("functions.R")

netcdf_root <- "NetCDF"

variables <- c("R99p")

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
          "Map Click",
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
        selected = "R99p"
      ),
      
      checkboxGroupInput(
        "scenario",
        "Scenario",
        choices = c(
          "SSP126",
          "SSP370"
        ),
        selected = c(
          "SSP126"
        )
      ),
      
      checkboxGroupInput(
        "period",
        "Period",
        choices = periods,
        selected = c(
          "2030",
          "2050",
          "2090"
        )
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
  
  #------------------------------------------------------
  # MAP
  #------------------------------------------------------
  
  output$map <- renderLeaflet({
    
    leaflet() |>
      addProviderTiles(
        providers$Esri.WorldTopoMap
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
      
      click <- input$map_click
      
      clicked(
        c(
          click$lng,
          click$lat
        )
      )
      
      leafletProxy("map") |>
        clearMarkers() |>
        clearMarkerClusters() |>
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
  
  #------------------------------------------------------
  # EXTRACTION
  #------------------------------------------------------
  
  results <- eventReactive(
    input$extract,
    {
      
      all_results <- list()
      
      if(
        input$input_method ==
        "Map Click"
      ){
        
        req(clicked())
        
        locations <- tibble(
          Location = "Map_Point",
          Longitude = clicked()[1],
          Latitude = clicked()[2]
        )
        
      } else {
        
        req(input$csv_file)
        
        locations <- read_csv(
          input$csv_file$datapath,
          show_col_types = FALSE
        )
        
        if(
          !"Location" %in%
          names(locations)
        ){
          
          locations$Location <- paste0(
            "Site_",
            seq_len(
              nrow(locations)
            )
          )
        }
        
      }
      
      nc_files <- list.files(
        netcdf_root,
        pattern = "\\.nc$",
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
          
          data <- extract_point(
            file,
            locations$Longitude[i],
            locations$Latitude[i],
            input$period
          )
          
          data$Scenario <- scenario
          data$Model <- model
          
          data$Location <-
            locations$Location[i]
          
          data$Longitude <-
            locations$Longitude[i]
          
          data$Latitude <-
            locations$Latitude[i]
          
          all_results[[length(all_results)+1]] <- data
          
        }
        
      }
      
      bind_rows(
        all_results
      )
      
    }
  )
  
  #------------------------------------------------------
  # RESULTS TABLE
  #------------------------------------------------------
  
  output$results_table <- renderTable({
    
    req(
      results()
    )
    
    results()
    
  })
  
  #------------------------------------------------------
  # DOWNLOAD
  #------------------------------------------------------
  
  output$download_csv <- downloadHandler(
    
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
