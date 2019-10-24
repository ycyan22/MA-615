library(shiny)
library(ggmap)
library(maptools)
library(maps)
library(ggplot2)
library(sp)
library(shinydashboard)

mapWorld <- map_data("world")

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("World Map"),
    
    selectInput("projection", "Choose a projection",
                c("cylindrical" = "cylindrical1", 
                  "mercator" = "mercator1", 
                  "sinusoidal" = "sinusoidal1", 
                  "gnomonic" = "gnomonic1", 
                  "rectangular" = "rectangular1", 
                  "cylequalarea" = "cylequalarea1")),
            plotOutput("view")
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    output$view <- renderPlot({
        if(input$projection == "cylindrical1"){
            ggplot(mapWorld, aes(x=long, y=lat, group=group))+
                geom_polygon(fill="white", color="black") +
                coord_map(xlim=c(-180,180), ylim=c(-60, 90))+ 
                coord_map("cylindrical",xlim=c(-180,180), ylim=c(-60, 90))
            
        }else if(input$projection == "mercator1"){
            ggplot(mapWorld, aes(x=long, y=lat, group=group))+
                geom_polygon(fill="white", color="black") +
                coord_map(xlim=c(-180,180), ylim=c(-60, 90))+ 
                coord_map("mercator",xlim=c(-180,180), ylim=c(-60, 90))
            
        }else if(input$projection == "sinusoidal1"){ 
            ggplot(mapWorld, aes(x=long, y=lat, group=group))+
                geom_polygon(fill="white", color="black") +
                coord_map(xlim=c(-180,180), ylim=c(-60, 90))+ 
                coord_map("sinusoidal", xlim=c(-180,180), ylim=c(-60, 90))
            
        }else if(input$projection == "gnomonic1"){
            ggplot(mapWorld, aes(x=long, y=lat, group=group))+
                geom_polygon(fill="white", color="black") +
                coord_map(xlim=c(-180,180), ylim=c(-60, 90))+ 
                coord_map("gnomonic", xlim=c(-180,180), ylim=c(-60, 90))
            
        }else if(input$projection == "rectangular1"){
            ggplot(mapWorld, aes(x=long, y=lat, group=group))+
                geom_polygon(fill="white", color="black") +
                coord_map(xlim=c(-180,180), ylim=c(-60, 90))+ 
                coord_map("rectangular", parameters = 0, xlim=c(-180,180), ylim=c(-60, 90))
            
        }else if(input$projection == "cylequalarea1"){
            ggplot(mapWorld, aes(x=long, y=lat, group=group))+
                geom_polygon(fill="white", color="black") +
                coord_map(xlim=c(-180,180), ylim=c(-60, 90))+ 
                coord_map("cylequalarea", parameters = 0, xlim=c(-180,180), ylim=c(-60, 90))
        }})
    
}

# Create Shiny app ----
shinyApp(ui, server)