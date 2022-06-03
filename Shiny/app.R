
library(shiny)
library(shinyWidgets)
library(leaflet)
library(DT)
library(tigris)
library(tidyverse)

crashes <- readRDS("Data/Crashes.rds")
inventory <- readRDS("Data/Inventory.rds")

nebraska <- tigris::counties(state = "NE", class = "sf")
listOfdfs <- list(crashes, inventory)


# Define UI for application that sets up map and table
ui <- fluidPage(

    # Application title
    navbarPage("Nebraska and Trains", 
               tabPanel("Nebraska Accidents Map", fluid = TRUE, 
                        tags$style("li a {
                            font-size: 15px;
                            font-weight: bold;
                        }
                                   "),
                        titlePanel("Nebraska Train Crashes and Crossing Map"),
                    sidebarLayout(
                        sidebarPanel(
                            pickerInput(inputId = "df",
                                        "Choose a Dataset",
                                        choices = c("Crashes" = 1, "Inventory" = 2),
                                        multiple = T
                            ),
                            selectInput(inputId = "county",
                                        "County Selection",
                                        choices = unique(crashes$County),
                                        
                                        selected = NULL),
                            
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
                            actionButton("update", "Update")
                        ),
                        
                        #Create an update button
                
                        # Show a plot of the generated distribution
                        mainPanel(
                           leafletOutput("map")
                           #Create a table output on a separate or same tab
                        )
                    )
               ),
               tabPanel("Data Table", fluid = TRUE, titlePanel("Crashes Table"),
                        fluidRow(
                            column(6,
                                   selectizeInput(inputId = "County",
                                               "County Selection",
                                               choices = unique(crashes$County),
                                               multiple = T,
                                               )
                        ),
                            column(6,
                                   sliderInput(inputId = "Time",
                                               label = strong("Time Range"),
                                               min = 0,
                                               max = 24,
                                               step = 1,
                                               value = c(0,24))),
                            column(6,
                                   dateRangeInput('DateRange',
                                                  label = "Date Range",
                                                  start = "2016-01-02",
                                                  end = "2020-12-28"))
                        ),
                       
                        fluidRow(
                            column(12,
                                   dataTableOutput('table'))
                        )
    )
))

# Define server logic required to create a map based on inputs of train crashes
server <- function(input, output) {

    datasetCrash <- reactive({
        
        # output$dateRange <- renderText({
        #     validate(
        #         need(try(input$dateRange[2] > input$dateRange[1]), "End date must be after than start date")
        #     )
        # })
        # 
        # output$df <- renderText({
        #     validate(
        #         need(!(is.null(input$df)),"Please select a dataset!")
        #     )
        # })
        if(length(input$df) > 1){
            Crashes <- listOfdfs[[1]]
            Inv <- listOfdfs[[2]]
            Crashes <- Crashes %>%
                filter(County == input$county) %>%
                filter(Accident_Date >= input$dateRange[1] & Accident_Date <= input$dateRange[2]) %>%
                filter(Accident_Time >= (input$time[1] * 100) & Accident_Time <= (input$time[2] * 100))
            Inv <- Inv %>%
                filter(`County Name` == input$county)
            list(Crashes, Inv)
        }
        

        else if(as.numeric(input$df) == 1){
            dataset <- listOfdfs[[1]]
            dataset %>%
                filter(County == input$county) %>%
                filter(Accident_Date >= input$dateRange[1] & Accident_Date <= input$dateRange[2]) %>%
                filter(Accident_Time >= (input$time[1] * 100) & Accident_Time <= (input$time[2] * 100))
        }
        
        else{
            dataset <- listOfdfs[[2]]
            dataset %>%
                filter(`County Name` == input$county)

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
    observeEvent(input$update,{

        req(input$update)
        if(length(input$df) > 1){
            leafletProxy("map", data = datasetCrash()) %>%
                clearMarkers() %>%
                clearControls() %>%
                
                addCircleMarkers(data = datasetCrash()[[1]],
                                 lat = ~Latitude,
                                 lng = ~Longitude,
                                 label = ~Accident_Key,
                                 opacity = 0.9,
                                 color = "blue") %>%
                addCircleMarkers(data = datasetCrash()[[2]],
                                 lat = ~Latitude,
                                 lng = ~Longitude,
                                 label = ~CrossingID,
                                 opacity = 0.9,
                                 color = "black") %>%
                addLegend(colors = c('black','blue'),
                          title = "Crashes and Crossings",
                          labels = c("Crossings", "Accident Sites"),
                          position = 'bottomright',
                          opacity = 0.9
                )
        }
        else if(length(input$df) == 1){
            leafletProxy("map", data = datasetCrash()) %>%
                clearMarkers() %>%
                clearControls() %>%
                    addCircleMarkers(data = datasetCrash(),
                                 lat = ~Latitude,
                                 lng = ~Longitude,
                                 color = "blue",
                                 ) %>%
                    addLegend(color = 'blue',
                            title = 'Map Legend',
                            labels = 'Locations',
                            position = 'bottomright',
                            opacity = 0.9
                    )}
        else{
            leafletProxy("map") %>%
                clearMarkers() %>%
                clearControls()
        }
        
    }, ignoreNULL = TRUE)
    
    
    
    #Data Table for Crashes
    
    
    Crash <- reactive({
        crashes %>%
            filter(County %in% input$County) %>%
            filter(Accident_Date >= input$DateRange[1] & Accident_Date <= input$DateRange[2]) %>%
            filter(Accident_Time >= (input$Time[1] * 100) & Accident_Time <= (input$Time[2] * 100))
    })
    
    output$table = DT::renderDataTable({
        Crash()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
