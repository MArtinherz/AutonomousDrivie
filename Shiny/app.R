

#Make the map first, then the table

library(shiny)
library(shinyWidgets)
library(leaflet)

crashes <- readRDS("Data/Crashes.rds")
inventory <- readRDS("Data/Inventory.rds")

listOfdfs <- list(crashes, inventory)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Train Crashes Map"),

    # Sidebar with a slider input for number of bins 
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
           leafletOutput("map") #Zoom in for Nebraska
           #Create a table output on a separate or same tab
        )
    )
)

# Define server logic required to create a map based on inputs of train crashes
server <- function(input, output) {
    datasetCrash <- reactive({
        if(length(input$df) > 1){
            Crashes <- listOfdfs[[1]]
            Inv <- listOfdfs[[2]]
            Crashes <- Crashes %>%
                filter(County == input$county) %>%
                filter(Accident_Date >= input$dateRange[1] & Accident_Date <= input$dateRange[2])
            Inv <- Inv %>%
                filter(`County Name` == input$county)
            list(Crashes, Inv)
        }
        

        else if(as.numeric(input$df) == 1){
            dataset <- listOfdfs[[1]]
            dataset %>%
                filter(County == input$county) %>%
                filter(Accident_Date >= input$dateRange[1] & Accident_Date <= input$dateRange[2])
        }
        
        else{
            dataset <- listOfdfs[[2]]
            dataset %>%
                filter(`County Name` == input$county)

        }

    })
    #Reactive for our dataset but have a defined update button
    
    # datasetInventory <- reactive({
    #     
    #     
    # })
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -100, lat = 41.5, zoom = 5)
    })
    
    #Need logic when both are selected
    #Use observe event
    observeEvent(input$update,{

        req(input$update)
        if(length(input$df) > 1){
            leafletProxy("map", data = datasetCrash()) %>%
                clearMarkers() %>%
                addCircleMarkers(data = datasetCrash()[[1]],
                                 lat = ~Latitude,
                                 lng = ~Longitude,
                                 label = ~Accident_Key,
                                 color = "blue") %>%
                addCircleMarkers(data = datasetCrash()[[2]],
                                 lat = ~Latitude,
                                 lng = ~Longitude,
                                 label = ~CrossingID,
                                 color = "red")
        }
        else if(as.numeric(input$df) == 1){
            leafletProxy("map", data = datasetCrash()) %>%
                clearMarkers() %>%
                addCircleMarkers(data = datasetCrash(),
                                 lat = ~Latitude,
                                 lng = ~Longitude,
                                 label = ~Accident_Key,
                                 color = "blue")
        }
        else{
            leafletProxy("map", data = datasetCrash()) %>%
                clearMarkers() %>%
                addCircleMarkers(data = datasetCrash(),
                                 lat = ~Latitude,
                                 lng = ~Longitude,
                                 label = ~CrossingID,
                                 color = "red")
        }
        
    })
    #Create a function

}

# Run the application 
shinyApp(ui = ui, server = server)
