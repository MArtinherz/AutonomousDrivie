
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



crashes <- readRDS("Data/Crashes.rds") 
inventory <- readRDS("Data/Inventory.rds")
hazmat <- readRDS("Data/Hazmat.rds")
nebraska <- tigris::counties(state = "NE", class = "sf")
listOfdfs <- list(crashes, inventory, hazmat)


# Define UI for application that sets up map and table
ui <- fluidPage(
  
  titlePanel("Nebraska Tries"),
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
                                                    c(0.0001,0.01, 0.1, 0.25, 0.5 , 0.75, 1, 5, 10),
                                                    selected = .5
                                                  )
                                                )
                                            
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
                                                             downloadButton('download', "Export as a .csv file")
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
  

    #Three Map Functions
    HazmatMap <- function(hazmat){
      shiny::validate(need( length(hazmat) > 0, "No hazmat stuff"))
      
      leafletProxy("map", data = hazmat) %>%
        clearMarkers() %>%
        clearControls() %>%
        addMarkers(data = hazmat,
                         lat = ~Latitude,
                         lng = ~Longitude,
                         label = ~ReportID,
                         icon = ~square_green,
                         popup = ~paste0(
                            "Overall Risk Score: ", "<br>",
                            "Date: ", IncidentDate, "<br>",
                            "Time: ", IncidentTime, "<br>"

                         )
        )
    }
    
    CrossingsMap <- function(crossings, radius){
      shiny::validate(need( length(crossings) > 0, "No hazmat stuff"))
      
      leafletProxy("map", data = crossings) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(data = crossings,
                   lat = ~Latitude,
                   lng = ~Longitude,
                   label = ~CrossingID,
                   radius = as.numeric(radius),
                   opacity = 1,
                   fillOpacity = 0.5,
                   fillColor = "orange",
                   weight = 1,
                   color = "black",
                   popup = ~paste0(
                     "Overall Risk Score: ", "<br>",
                     "Crossing Type: ", CrossingType, "<br>"
                     
                   )
        )
      
    }
    
    CrashMap <- function(crash){
      shiny::validate(need( length(crash) > 0, "No crashes"))
      
  
      
      
      leafletProxy("map", data = crash) %>%
        clearMarkers() %>%
        clearControls() %>%
        addMarkers(data = crash,
                         lat = ~Latitude,
                         lng = ~Longitude,
                         label = ~AccidentID,
                         icon = ~blue_triangle,
                         popup = ~paste0(
                           "Overall Risk Score: ", "<br>",
                           "Crash Day: ", Accident_Date, "<br>",
                           RoadClass
                           
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
            
            # validate(need(nrow(Inv) > 0, "This County missing its train crossings. Neither dataset will show on map or table. Change filter"))
            
            list(Crashes, Inv, Hazmat)
            
        }
      
        else if( length(input$df) == 2){

          if( 1 %in% input$df){
            
            FirstDataset <- listOfdfs[[1]] %>%
              filter(County %in% input$county) %>%
              filter(Accident_Time >= (input$time[1] * 100) & Accident_Time <= (input$time[2] * 100))

            if(2 %in% input$df){
            SecondDataset <- listOfdfs[[2]] %>%
              filter(County %in% input$county)
            list(FirstDataset, SecondDataset)
            
            }
            else{
              SecondDataset <- listOfdfs[[3]]
              list(FirstDataset, SecondDataset)
              
            }



          }

          else if(3 %in% input$df){
            SecondDataset <- listOfdfs[[3]]

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
              list(FirstDataset, SecondDataset)
            }

          }
        }
      
      
        else if(length(input$df) == 1){
          
          if(as.numeric(input$df)[1] == 1){
            OnlyDataset <- listOfdfs[[1]]
          }
          else if((as.numeric(input$df)[1] == 2)){
            OnlyDataset <- listOfdfs[[2]]
          }
          else if((as.numeric(input$df)[1] == 3)){
            OnlyDataset <- listOfdfs[[3]]
          }

        }

    })

    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            addPolygons(data = nebraska,
                        color = "red",
                        layerId = ~COUNTYNS,
                        label = ~NAME,
                        highlightOptions = highlightOptions(color = 'white', weight = 3))
    })
    
    
    #Need logic when both are selected
    #Use observe event
    
    #Observer event for creating a map based on all four inputs. Looks for dataset
    observeEvent( c(input$df , input$time ,  input$county, input$dateRange, input$radius, input$map_zoom), {

      
      if(length(input$df) == 3){
        
        if (length(datasetCrash()[2]) != 0){
        
        
        CrashMap(datasetCrash()[[1]]) %>%
          addCircleMarkers(data = datasetCrash()[[2]],
                           lat = ~Latitude,
                           lng = ~Longitude,
                           label = ~CrossingID,
                           opacity = 1,
                           fillOpacity = 0.5,
                           fillColor = "orange",
                           weight = case_when(input$map_zoom <=4 ~1, 
                                                       input$map_zoom ==5 ~2, 
                                                       input$map_zoom ==6 ~3, 
                                                       input$map_zoom ==7 ~5, 
                                                       input$map_zoom ==8 ~7, 
                                                       input$map_zoom ==9 ~9, 
                                                       input$map_zoom >9 ~11),
                           radius = as.numeric(input$radius),
                           color = "orange",
                           popup = ~paste0(
                             "Overall Risk Score: ", "<br>",
                             "Crossing Type: ", CrossingType, "<br>"

                           )
          ) %>%
          addMarkers(data = datasetCrash()[[3]],
                     lat = ~Latitude,
                     lng = ~Longitude,
                     label = ~ReportID,
                     icon = ~square_green,
                     popup = ~paste0(
                       "Overall Risk Score: ", "<br>",
                       "Date: ", IncidentDate, "<br>",
                       "Time: ", IncidentTime, "<br>"
                       
                     )
          )
        }
          
        else{
          CrashMap(datasetCrash()[[1]]) %>%
            addMarkers(data = datasetCrash()[[3]],
                       lat = ~Latitude,
                       lng = ~Longitude,
                       label = ~ReportID,
                       icon = ~square_green,
                       popup = ~paste0(
                         "Overall Risk Score: ", "<br>",
                         "Date: ", IncidentDate, "<br>",
                         "Time: ", IncidentTime, "<br>"

                       )
            )
        }
      }
      
      else if(length(input$df) == 2){
        if( 1 %in% input$df ){
          
          if( 2%in% input$df){
          
            CrashMap(datasetCrash()[[1]]) %>%
              addCircleMarkers(data = datasetCrash()[[2]],
                             lat = ~Latitude,
                             lng = ~Longitude,
                             label = ~CrossingID,
                             opacity = 1,
                             fillOpacity = 0.5,
                             fillColor = "orange",
                             weight = 1,
                             radius = as.numeric(input$radius),
                             color = "black",
                             popup = ~paste0(
                               "Overall Risk Score: ", "<br>",
                               "Crossing Type: ", CrossingType, "<br>"
                               
                             )
            )
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
                           RoadClass
                           
                         )
              )
          }
        }
        else if ( c(2, 3) %in% input$df ){
          HazmatMap(datasetCrash()[[2]]) %>%
            addCircleMarkers(data = datasetCrash()[[1]],
                             lat = ~Latitude,
                             lng = ~Longitude,
                             label = ~CrossingID,
                             opacity = 1,
                             fillOpacity = 0.5,
                             fillColor = "orange",
                             weight = 1,
                             radius = as.numeric(input$radius),
                             color = "black",
                             popup = ~paste0(
                               "Overall Risk Score: ", "<br>",
                               "Crossing Type: ", CrossingType, "<br>"
                               
                             )
            )
        }
       
              
          }
          
        
          
    

      
      
      
      else if(length(input$df) == 1){
        
        
        if( as.numeric(input$df) == 1) {


          CrashMap(datasetCrash()  ) %>%
            addLegend(color = 'blue',
                      title = 'Map Legend',
                      labels = 'Crashes',
                      position = 'bottomright',
                      opacity = 0.9
            )
            }

        else if (as.numeric(input$df) == 2) {
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearControls()
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
          clearControls()
      }
    }, ignoreNULL = TRUE
      
    )
    
    
    #Outputs table of our dataset
    output$table = DT::renderDataTable({
          if( length(input$df) == 1){
            
            if( length(datasetCrash()) != 0){
            
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
    }
    )
          
        
  
    output$download <- downloadHandler(
      filename = function(){"download.csv"},
      content = function(fname){
        write.csv(download(),fname)
      }
    )
}



# Run the application 
shinyApp(ui = ui, server = server)
