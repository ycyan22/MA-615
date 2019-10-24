library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(dplyr)
library(tidyverse)
library(corpcor)

Universities <- read.csv(file="Colleges_and_Universities.csv", header=TRUE, sep=",")
Hubway <- read.csv(file="Hubway_Stations.csv", header=TRUE, sep=",")
Charging <- read.csv(file="Charging_Stations.csv", header=TRUE, sep=",")
library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("More Widgets"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Include clarifying text ----
            helpText("Green circles are loactions of universities, you can choose to see locations of Hubway Stations and Charging Stations by checking the box."),
            
            checkboxInput("Hubway", "Hubway Stations", value = FALSE),
            
            checkboxInput("Charging", "Charging Stations", value = FALSE),
            
            
            # Input: Select a dataset ----
            selectInput("dataset", "Choose a dataset:",
                        choices = c("Universities", "Hubway Stations", "Charging Stations")),
            
            # Input: Specify the number of observations to view ----
            numericInput("obs", "Number of observations to view:", 10),
            
            # Include clarifying text ----
            helpText("Note: while the data view will show only the specified",
                     "number of observations, the summary will still be based",
                     "on the full dataset."),
            
            # Input: actionButton() to defer the rendering of output ----
            # until the user explicitly clicks the button (rather than
            # doing it immediately when inputs change). This is useful if
            # the computations required to render output are inordinately
            # time-consuming.
            actionButton("update", "Update View")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            leafletOutput("Umap"),
            
            # Output: Header + summary of distribution ----
            h4("Summary"),
            verbatimTextOutput("summary"),
            
            # Output: Header + table of distribution ----
            h4("Observations"),
            tableOutput("view")
        )
        
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
    
    # Return the requested dataset ----
    # Note that we use eventReactive() here, which depends on
    # input$update (the action button), so that the output is only
    # updated when the user clicks the button
    datasetInput <- eventReactive(input$update, {
        switch(input$dataset,
               "Universities" = Universities,
               "Hubway Stations" = Hubway,
               "Charging Stations" = Charging)
    }, ignoreNULL = FALSE)
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    # Show the first "n" observations ----
    # The use of isolate() is necessary because we don't want the table
    # to update whenever input$obs changes (only when the user clicks
    # the action button)
    output$view <- renderTable({
        head(datasetInput(), n = isolate(input$obs))
    })
    
    
    points1 <- cbind(Universities$Longitude, Universities$Latitude)
    points2 <- cbind(Hubway$X, Hubway$Y)
    points3 <- cbind(Charging$X, Charging$Y)
    
    
    output$Umap <- renderLeaflet({
        leaflet() %>%
            setView(lng = -71, lat = 42.3, zoom = 10.2)  %>%
            addTiles() %>%
            addCircleMarkers(data = points1, color = "green")
    })
    
    observe({
        proxy <- leafletProxy("Umap", data = points1)
        # Remove any existing legend, and only if the legend is # enabled, create a new one.
        proxy %>% clearShapes()
        if (input$Hubway==TRUE) {
            proxy %>% 
                addCircles(data = points2, color = "blue")
        } 
        
        if (input$Charging==TRUE) {
            proxy %>% 
                addCircles(data = points3, color = "red")
        } })
    

}

# Create Shiny app ----
shinyApp(ui, server)
