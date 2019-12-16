#Data Import
library(readr)
library(dplyr)
library(tidyverse)
library(shiny)
library(ggplot2) 
library(stringi)
library(psych) 
library(GPArotation)
library(corpcor)
library(kableExtra)
library(randomForest)

mydata <- read_csv('~/Desktop/boston/Boston2017.csv')
BA <- drop_na(mydata) 
mydata$reviews<-as.numeric(mydata$reviews)
mydata$overall_satisfaction<-as.numeric(mydata$overall_satisfaction)
mydata$latitude<-as.numeric(mydata$latitude)
mydata$longitude<-as.numeric(mydata$longitude)
sapply(mydata, is.numeric)
cordata <- mydata[, sapply(mydata, is.numeric)]
cor.ma <- cor(cordata, method = "pearson")



library(shiny)


# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Boston Airbnb"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Show parallel analysis ----
            checkboxInput("PAplot", "Parallel Analysis Scree Plot", value = TRUE),
            
            
            # Input: Show factor analysis ----
            checkboxInput("FA", "Factor Analysis", value = TRUE),
            
            # Include clarifying text ----
            helpText("Now that we’ve arrived at probable number number of factors, let’s start off with 4 as the number of factors."
            ),
            
            
            
            # Input: Select a dataset ----
            selectInput("dataset", "Boston Airbnb dataset:",
                        choices = c("BA")),
            
            # Input: Specify the number of variables to view ----
            sliderInput("var", "Number of variables to view", value = 6, min = 0, max = 15),
            
            
            # Input: actionButton() to defer the rendering of output ----
            # until the user explicitly clicks the button (rather than
            # doing it immediately when inputs change). This is useful if
            # the computations required to render output are inordinately
            # time-consuming.
            actionButton("update", "Update View")
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            h4("Parallel Analysis"),
            verbatimTextOutput("PAplot1"),
            
            h4("Factor Analysis"),
            plotOutput("FA1"),
            
            # Output: Header + table of distribution ----
            h4("Observations"),
            tableOutput("view")
        )
        
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    
    # Return the requested dataset ----
    # Note that we use eventReactive() here, which depends on
    # input$update (the action button), so that the output is only
    # updated when the user clicks the button
    datasetInput <- eventReactive(input$update, {
        switch(input$dataset,
               "BA" = BA)
    }, ignoreNULL = FALSE)
    
    # Show the "n" variables ----
    # The use of isolate() is necessary because we don't want the table
    # to update whenever input$obs changes (only when the user clicks
    # the action button)
    output$view <- renderTable({
        select(datasetInput(), 1:isolate(input$var))
    })
    
    output$PAplot1 <- renderPrint({if(input$PAplot == TRUE){
        parallel <- fa.parallel(cordata, fm = 'minres', fa = 'fa')  
    }})
    
    output$FA1 <- renderPlot({if(input$FA == TRUE){
        fourfactor <- fa(cordata,nfactors = 4,rotate = "oblimin",fm="minres") # 4 factor analysis
        fa.diagram(fourfactor)
    }})
    
}

# Create Shiny app ----
shinyApp(ui, server)

