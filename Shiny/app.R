
library(shiny)
#install.packages('shinyWidgets')
library(shinyWidgets)
#install.packages('leaflet')
library(leaflet)
#install.packages('DT')
library(DT)
#install.packages(tigris)
library(tigris)
library(tidyverse)
library(spatialrisk)



crashes <- readRDS("Data/Crashes.rds") 
inventory <- readRDS("Data/Inventory.rds")
hazmat <- readRDS("Data/Hazmat.rds") %>%
  filter(!(is.na(Longitude)) || !(is.na(Latitude)))
nebraska <- tigris::counties(state = "NE", class = "sf") %>%
  sf::st_transform("+proj=longlat +datum=WGS84 +units=m")
listOfdfs <- list(crashes, inventory, hazmat)


# Define UI for application that sets up map and table
ui <- fluidPage(
  
  titlePanel("Nebraska Train Map"),
  includeCSS("www/styles.css"),
  sidebarLayout(
    
    #Our inputs that can be used on both tabs simaltenously
    sidebarPanel(
      
                                                pickerInput(inputId = "df",
                                                            "Choose a Dataset",
                                                            choices = c("Crashes" = 1, "Inventory" = 2, "Hazmat" = 3),
                                                            multiple = T,
                                                            selected = c(1:3)
                                                ),
                                                
                                                pickerInput("county",
                                                            "County Selection", 
                                                            choices=unique(crashes$County), 
                                                            options = list(`actions-box` = TRUE),multiple = T,
                                                            selected = unique(crashes$County)),

                                                sliderInput(inputId = "time",
                                                            label = strong("Time Range"),
                                                            min = 0,
                                                            max = 24,
                                                            step = 1,
                                                            value = c(0,24)),
                                                #Form a date filter
                                                dateRangeInput('dateRange',
                                                               label = "Date Range",
                                                               start = "2016-01-02",
                                                               end = "2020-12-28"),
                                                conditionalPanel(
                                                  condition = "input.df.includes('2')",
                                                  selectInput(
                                                    "radius", "Radius",
                                                    c(0.01, 0.1, 0.25, 0.5 , 0.75, 1, 2),
                                                    selected = .5
                                                  )
                                                ),
                                            
      )
  ,
  
  #Main showing of our app, which shows the map on one tab and a table on the another
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Map", leafletOutput("map", width = "100%", height = "500px")),
                tabPanel("Table", fluid = TRUE, titlePanel("Data Table"),
                                                           fluidRow(
                                                             conditionalPanel(
                                                               condition = "input.df.length > 1",
                                                               selectInput(
                                                                 "dataview", "Dataset View",
                                                                 c("Crashes", "Inventory", "Hazard")
                                                               )
                                                             )
                                                           ),
                                                            
                                                           fluidRow(
                                                               column(12,
                                                                         dataTableOutput('table'))
                                                           ),
                                                           fluidRow(
                                                             downloadButton('download', "Download")
                                                           ))
  )
  )
)
)

# Define server logic required to create a map based on inputs of train crashes
server <- function(input, output) {

  
  square_green <-
    makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/green-square-1.png",
             iconWidth = 6,
             iconHeight = 6)
  
  blue_triangle <-
    makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/light-blue-triangle-image-3.png",
             iconWidth = 6,
             iconHeight = 6)
  
  
  #Create a Map Error function. Returns a map when one of the datasets is empty no matter what. Also potentially return a message
  ErrorMap <- function(){
    Map <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(data = nebraska,
                  color = 'black',
                  layerId = ~COUNTYNS,
                  label = ~NAME,
                  highlightOptions = highlightOptions(color = 'white', weight = 3))
    return(Map)
  }
  
  
  CrashesInCircle <- function(radius, Crashmat, Crossings){
    Rad <- as.numeric(radius) * 1609.34
    
    if( nrow(Crashmat) == 0 || nrow(Crossings) == 0){
      #Output an error
      return(Crossings)
      
      # stop(safeError(paste("There is an empty dataframe input. Can not generate points in circle for Crashes")))
    }
    
    else{
    
    for(i in 1:nrow(Crossings)){
      pic <- spatialrisk::points_in_circle(Crashmat, Crossings$Longitude[i], Crossings$Latitude[i], Longitude, Latitude, radius = Rad)
      Crossings$TotalCrashes[i] <- nrow(pic)
      
    }
    return(Crossings)
    }
    
  }
  
  HazmatsInCircles <- function(radius, Crashmat, Crossings){
    Rad <- as.numeric(radius)
    
    if( nrow(Crashmat) == 0 | nrow(Crossings) == 0){
      return(Crossings)
      # stop(safeError(paste("There is an empty dataframe input. Can not generate points in circle for Hazmat")))
    }
    
    
    for(i in 1:nrow(Crossings)){
      pic <- spatialrisk::points_in_circle(Crashmat, Crossings$Longitude[i], Crossings$Latitude[i], Longitude, Latitude, radius = Rad)
      Crossings$TotalHazmats[i] <- nrow(pic)
      
    }
    return(Crossings)
    
  }
  
  
    #Three Map Functions
    HazmatMap <- function(hazmat){
      shiny::validate(need( length(hazmat) > 0, "No hazmat stuff"))
      
      leafletProxy("map", data = hazmat) %>%
        clearMarkers() %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = nebraska,
                    color = 'black',
                    layerId = ~COUNTYNS,
                    label = ~NAME,
                    highlightOptions = highlightOptions(color = 'white', weight = 3)) %>%
        addMarkers(data = hazmat,
                         lat = ~Latitude,
                         lng = ~Longitude,
                         label = ~ReportID,
                         icon = ~square_green,
                         popup = ~paste0(
                            "Overall Risk Score: ", "<br>",
                            "City: ", IncidentCity, "<br>",
                            "Date: ", IncidentDate, "<br>",
                            "Time: ", IncidentTime, "<br>",
                            "Quantity Released: ", QuantityReleased, "<br>",
                            "Commodity: ", `Commodity Long Name`, "<br>",
                            "Result: ", IncidentResult, "<br>"

                         )
        )
    }
    
    CrossingsMap <- function(crossings, radius){
      shiny::validate(need( length(crossings) > 0, "No hazmat stuff"))
      
      leafletProxy("map", data = crossings) %>%
        clearMarkers() %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = nebraska,
                    color = 'black',
                    layerId = ~COUNTYNS,
                    label = ~NAME,
                    highlightOptions = highlightOptions(color = 'white', weight = 3)) %>%
        addCircles(data = crossings,
                   lng = ~Longitude,
                   lat = ~Latitude,
                   label = ~CrossingID,
                   color = 'orange',
                   opacity = 1,
                   radius = as.numeric(radius) * 1609.344,
                   popup = ~paste0(
                     "Overall Risk Score: ", "<br>",
                     "Railroad Name: ", `Railroad Name`, "<br>",
                     "Crossing Type: ", CrossingType, "<br>",
                     "Total Crashes within radius of ", input$radius, " miles: ", TotalCrashes, "<br>",
                     "Total Hazmats within radius of ", input$radius, " miles: ", TotalHazmats, "<br>",
                     "Gate or Sign: ", GateOrSign, "<br>",
                     "Land Use: ", `Land Use`, "<br>",
                     "Surface Type: ", SurfaceType, "<br>",
                     "AADT: ", AADT, "<br>")
        )
      
      
    }
    
    CrashMap <- function(crash){
      shiny::validate(need( length(crash) > 0, "No crashes"))
      
  
      
      
      leafletProxy("map", data = crash) %>%
        clearMarkers() %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = nebraska,
                    color = 'black',
                    layerId = ~COUNTYNS,
                    label = ~NAME,
                    highlightOptions = highlightOptions(color = 'white', weight = 3)) %>%
        addMarkers(data = crash,
                         lat = ~Latitude,
                         lng = ~Longitude,
                         label = ~AccidentID,
                         icon = ~blue_triangle,
                         popup = ~paste0(
                           "Overall Risk Score: ", "<br>",
                           "Crash Day: ", Accident_Date, "<br>",
                           "Time: ", Accident_Time, "<br>",
                           "Weather at Crash: ", Weather, "<br>",
                           "Type of Crash: ", CrashType, "<br>",
                           "Accident Severity", AccSeverity, "<br>",
                           "Total Vehicles: ", TotVeh, "<br>",
                           "Total Injured: ", TotInj, "<br>",
                           "Total Fatalaties", TotFatal, "<br>"

                         )
        )
      
      
    }
  
  
    #Creates our dataset for multiple use in filtering
    datasetCrash <- reactive({
      
      

        if(length(input$df) == 3){
            Crashes <- listOfdfs[[1]]
            Inv <- listOfdfs[[2]]
            Hazmat <- listOfdfs[[3]]
            Crashes <- Crashes %>%
                filter(County %in% input$county) %>%
                # filter(Accident_Date >= input$dateRange[1] & Accident_Date <= input$dateRange[2]) %>%
                filter(Accident_Time >= (input$time[1] * 100) & Accident_Time <= (input$time[2] * 100))
            Inv <- Inv %>%
                filter(County %in% input$county)
            Inv <- CrashesInCircle(input$radius, Crashes, Inv)
            Inv <- HazmatsInCircles(input$radius, Hazmat, Inv)
            
            
            

            list(Crashes, Inv, Hazmat)
            
        }
      
        else if( length(input$df) == 2){
          
          Crashes <- listOfdfs[[1]]
          Inv <- listOfdfs[[2]]
          Hazmat <- listOfdfs[[3]]
          

          if( 1 %in% input$df){
            
            FirstDataset <- Crashes %>%
              filter(County %in% input$county) %>%
              filter(Accident_Time >= (input$time[1] * 100) & Accident_Time <= (input$time[2] * 100))

            if(2 %in% input$df){
            SecondDataset <- Inv %>%
              filter(County %in% input$county)
            
            SecondDataset <- CrashesInCircle(radius = input$radius, Crashmat = FirstDataset, Crossings = SecondDataset)
            SecondDataset <- HazmatsInCircles(input$radius, Hazmat, SecondDataset)
            
            list(FirstDataset, SecondDataset)
            
            }
            else{
              SecondDataset <- listOfdfs[[3]]
              list(FirstDataset, SecondDataset)
              
            }



          }

          else if(3 %in% input$df){
            SecondDataset <- Hazmat

            if(1 %in% input$df){
              FirstDataset <- listOfdfs[[1]] %>%
                filter(County %in% input$county) %>%
                # filter(Accident_Date >= input$dateRange[1] & Accident_Date <= input$dateRange[2]) %>%
                filter(Accident_Time >= (input$time[1] * 100) & Accident_Time <= (input$time[2] * 100))
              list(FirstDataset, SecondDataset)


            }
            else{
              FirstDataset <- listOfdfs[[2]] %>%
                filter(County %in% input$county)
              
              FirstDataset <- HazmatsInCircles(input$radius, SecondDataset, FirstDataset)
              FirstDataset <- CrashesInCircle(input$radius, SecondDataset, FirstDataset)
              
              list(FirstDataset, SecondDataset)
            }

          }
        }
      
      
        else if(length(input$df) == 1){
          
          Crashes <- listOfdfs[[1]]
          Inv <- listOfdfs[[2]]
          Hazmat <- listOfdfs[[3]]
          
          if(as.numeric(input$df)[1] == 1){
            OnlyDataset <- Crashes %>%
              filter(County %in% input$county) %>%
              # filter(Accident_Date >= input$dateRange[1] & Accident_Date <= input$dateRange[2]) %>%
              filter(Accident_Time >= (input$time[1] * 100) & Accident_Time <= (input$time[2] * 100))
          }
          else if((as.numeric(input$df)[1] == 2)){
            OnlyDataset <- Inv %>%
              filter(County %in% input$county)
            
            Inv <- CrashesInCircle(input$radius, Crashes, Inv)
            Inv <- HazmatsInCircles(input$radius, Hazmat, Inv)
              
          }
          else if((as.numeric(input$df)[1] == 3)){
            OnlyDataset <- listOfdfs[[3]]
          }

        }

    })

    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            addPolygons(data = nebraska,
                        color = 'black',
                        layerId = ~COUNTYNS,
                        label = ~NAME,
                        highlightOptions = highlightOptions(color = 'white', weight = 3))
    })
    
    
    #Need logic when both are selected
    #Use observe event
    
    #Observer event for creating a map based on all four inputs. Looks for dataset
    observeEvent( c(input$df , input$time ,  input$county, input$dateRange, input$radius), {

      
      if(length(input$df) == 3){
        
        if( nrow(datasetCrash()[[1]]) == 0 || nrow(datasetCrash()[[2]]) == 0 || nrow(datasetCrash()[[3]]) == 0){
          ErrorMap()
        }
        
        else{
        
        
        CrashMap(datasetCrash()[[1]]) %>%
            addCircles(data = datasetCrash()[[2]],
                       lng = ~Longitude,
                       lat = ~Latitude,
                       label = ~CrossingID,
                       color = 'orange',
                       opacity = 2,
                       radius = as.numeric(input$radius) * 1609.344,
                       popup = ~paste0(
                         "Overall Risk Score: ", "<br>",
                         "Railroad Name: ", `Railroad Name`, "<br>",
                         "Crossing Type: ", CrossingType, "<br>",
                         "Total Crashes within radius of ", input$radius, " miles: ", TotalCrashes, "<br>",
                         "Total Hazmats within radius of ", input$radius, " miles: ", TotalHazmats, "<br>",
                         "Gate or Sign: ", GateOrSign, "<br>",
                         "Land Use: ", `Land Use`, "<br>",
                         "Surface Type: ", SurfaceType, "<br>",
                         "AADT: ", AADT, "<br>")
            
          ) %>%
          addMarkers(data = datasetCrash()[[3]],
                     lat = ~Latitude,
                     lng = ~Longitude,
                     label = ~ReportID,
                     icon = ~square_green,
                     popup = ~paste0(
                       "Overall Risk Score: ", "<br>",
                       "City: ", IncidentCity, "<br>",
                       "Date: ", IncidentDate, "<br>",
                       "Time: ", IncidentTime, "<br>",
                       "Quantity Released: ", QuantityReleased,"<br>",
                       "Commodity: ", `Commodity Long Name`, "<br>",
                       "Result: ", IncidentResult, "<br>"
                       
                     )
          ) %>%
            addLegend(color = c('green', 'orange', 'blue'),
                      title = 'Map Legend',
                      labels = c('Hazards','Crossings','Crashes'),
                      position = 'bottomright',
                      opacity = 0.9)
        }
          

      }
      
      else if(length(input$df) == 2){
        
        if( nrow(datasetCrash()[[1]]) == 0 || nrow(datasetCrash()[[2]]) == 0){
          ErrorMap()
        }
        
        
        else if( 1 %in% input$df ){
          
          if( 2%in% input$df){
          
            CrashMap(datasetCrash()[[1]]) %>%
              addCircles(data = datasetCrash()[[2]],
                         lng = ~Longitude,
                         lat = ~Latitude,
                         label = ~CrossingID,
                         color = 'orange',
                         opacity = 2,
                         radius = as.numeric(input$radius) * 1609.344,
                         popup = ~paste0(
                           "Overall Risk Score: ", "<br>",
                           "Railroad Name: ", `Railroad Name`, "<br>",
                           "Crossing Type: ", CrossingType, "<br>",
                           "Total Crashes within radius of ", input$radius, " miles: ", TotalCrashes, "<br>",
                           "Total Hazmats within radius of ", input$radius, " miles: ", TotalHazmats, "<br>",
                           "Gate or Sign: ", GateOrSign, "<br>",
                           "Land Use: ", `Land Use`, "<br>",
                           "Surface Type: ", SurfaceType, "<br>",
                           "AADT: ", AADT, "<br>")
            ) %>%
              addLegend(color = c('orange', 'blue'),
                        title = 'Map Legend',
                        labels = c('Crossings','Crashes'),
                        position = 'bottomright',
                        opacity = 0.9)
          }
          else{
            HazmatMap(datasetCrash()[[2]]) %>%
              addMarkers(data = datasetCrash()[[1]],
                         lat = ~Latitude,
                         lng = ~Longitude,
                         label = ~AccidentID,
                         icon = ~blue_triangle,
                         popup = ~paste0(
                           "Overall Risk Score: ", "<br>",
                           "Crash Day: ", Accident_Date, "<br>",
                           "Time: ", Accident_Time, "<br>",
                           "Weather at Crash: ", Weather, "<br>",
                           "Type of Crash: ", CrashType, "<br>",
                           "Accident Severity", AccSeverity, "<br>",
                           "Total Vehicles: ", TotVeh, "<br>",
                           "Total Injured: ", TotInj, "<br>",
                           "Total Fatalaties", TotFatal, "<br>"
                           
                         )
                           
                         
              ) %>%
              addLegend(color = c('green',  'blue'),
                        title = 'Map Legend',
                        labels = c('Hazards','Crashes'),
                        position = 'bottomright',
                        opacity = 0.9)
          }
        }
        else if ( c(2, 3) %in% input$df ){
          HazmatMap(datasetCrash()[[2]]) %>%
            addCircles(data = datasetCrash()[[1]],
                       lng = ~Longitude,
                       lat = ~Latitude,
                       label = ~CrossingID,
                       color = 'orange',
                       opacity = 2,
                       radius = as.numeric(input$radius) * 1609.344,
                       popup = ~paste0(
                         "Overall Risk Score: ", "<br>",
                         "Railroad Name: ", `Railroad Name`, "<br>",
                         "Crossing Type: ", CrossingType, "<br>",
                         "Total Crashes within radius of ", input$radius, " miles: ", TotalCrashes, "<br>",
                         "Total Hazmats within radius of ", input$radius, " miles: ", TotalHazmats, "<br>",
                         "Gate or Sign: ", GateOrSign, "<br>",
                         "Land Use: ", `Land Use`, "<br>",
                         "Surface Type: ", SurfaceType, "<br>",
                         "AADT: ", AADT, "<br>")
            ) %>%
            addLegend(color = c('green', 'orange'),
                      title = 'Map Legend',
                      labels = c('Hazards','Crossings'),
                      position = 'bottomright',
                      opacity = 0.9)
        }
       
              
          }
          
        
          
    

      
      
      
      else if(length(input$df) == 1){
        
        if ( nrow(datasetCrash()) == 0){
          ErrorMap()
        }
        
        
        else if( as.numeric(input$df) == 1) {


          CrashMap(datasetCrash()  ) %>%
            addLegend(color = 'blue',
                      title = 'Map Legend',
                      labels = 'Crashes',
                      position = 'bottomright',
                      opacity = 0.9
            )
            }

        else if (as.numeric(input$df) == 2) {

          CrossingsMap(datasetCrash() , (input$radius)) %>%
            addLegend(color = 'orange',
                      title = 'Map Legend',
                      labels = 'Crossings',
                      position = 'bottomright',
                      opacity = 0.9
            )

        }
        else if (as.numeric(input$df) == 3){


          HazmatMap( datasetCrash()  ) %>%
            addLegend(color = 'green',
                      title = 'Map Legend',
                      labels = 'Hazards',
                      position = 'bottomright',
                      opacity = 0.9
            )
          }

        }
      
      else{
        leafletProxy("map") %>%
          clearMarkers() %>%
          clearControls() %>%
          clearShapes() %>%
          addPolygons(data = nebraska,
                      color = 'black',
                      layerId = ~COUNTYNS,
                      label = ~NAME,
                      highlightOptions = highlightOptions(color = 'white', weight = 3))
      }
    }, ignoreNULL = TRUE
      
    )
    
    
    DataTable <- eventReactive(input$dataview, {
      if( length(input$df) == 1){
        
        if( nrow(datasetCrash()) != 0){
          datasetCrash()
        }
        else{
          stop(safeError(paste(input$df[1], "has filters that are outputting nothing!")))
        }
      }
      else{
        if(input$dataview == 'Inventory'){
          if(2 %in% input$df){
            
            if( input$df[[1]] == 1){
              datasetCrash()[[2]]
              
              
            }
            else{
              datasetCrash()[[1]]
            }
            
          }
          
          else{
            stop(safeError(paste(input$dataview, "was not selected in dataframe input")))
          }
          
          
        }
        
        
        else if(input$dataview == "Hazard"){
          if(3 %in% input$df){
            if( length(input$df) > 2){
              datasetCrash()[[3]]
            }
            else{
              datasetCrash()[[2]]
            }
          }
          else{
            stop(safeError(paste(input$dataview, "was not selected in dataframe input")))
          }
          
        }
        
        
        else{
          if(1 %in% input$df){
            datasetCrash()[[1]]
          }
          else{
            stop(safeError(paste(input$dataview, "was not selected in dataframe input")))
          }
          
        }
      }
      
    })
    
    
    #Outputs table of our dataset
    output$table = DT::renderDataTable({
            DataTable()
    }
    )
          
        
  
    output$download <- downloadHandler(
      filename = function(){
        paste(input$dataview,".csv",sep="")},
      content = function(fname){
        
        write.csv(DataTable(),fname)
      }
    )
}



# Run the application 
shinyApp(ui = ui, server = server)
