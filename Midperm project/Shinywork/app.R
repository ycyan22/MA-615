#Data Import
library(readxl)
library(dplyr)
library(tidyverse)
library(shiny)
library(ggplot2) 
library(psych) 
library(GPArotation)
library(corpcor)

survey <- read_excel('~/Desktop/world value survey/WV6.xlsx')
mydata <- select(survey, 11,12,24,25,57,58,62,102,104,201,306,307,309,311,319) 
names(mydata) <- c("q1", "q2","q3", "q4","q5", "q6","q7", "q8","q9", "q10","q11", "q12","q13", "q14","q15")
mydata[mydata < 0] <- NA 
WVS <- drop_na(mydata) 

library(shiny)


# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("World Value Survey"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Show parallel analysis ----
            checkboxInput("PAplot", "Parallel Analysis Scree Plot", value = TRUE),
            
            # Include clarifying text ----
            helpText("Parallel analysis suggests that the number of factors =  6  and the number of components = NA."
            ),
            
            # Input: Show factor analysis ----
            checkboxInput("FA", "Factor Analysis", value = TRUE),
            
            # Include clarifying text ----
            helpText("Now that we’ve arrived at probable number number of factors, let’s start off with 6 as the number of factors."
            ),
            
            # Input: Show PCA ----
            checkboxInput("PCA", "Factor extraction (PCA)", value = TRUE),
            
            # Include clarifying text ----
            helpText("According to output of PCA, we understand relationships among variables and could summarize factors."
            ),
            
            
            # Input: Select a dataset ----
            selectInput("dataset", "Choose a dataset:",
                        choices = c("WVS")),
            
            # Input: Specify the number of variables to view ----
            sliderInput("var", "Number of variables to view", value = 6, min = 0, max = 15),
            
            # Include clarifying text ----
            helpText("q1 - V10: Feeling of happiness"),  
            helpText("q2 - V11: State of health (subjective)"),  
            helpText("q3 - V23: Satisfaction with your life"),   
            helpText("q4 - V24: Most people can be trusted"),   
            helpText("q5 - V55: How much freedom of choice and control over own life"),   
            helpText("q6 - V56: Do you think most people would try to take advantage of you if they got a chance, or would they try to be fair? people can be trusted"),   
            helpText("q7 - V59: Satisfaction with financial situation of household"),   
            helpText("q8 - V98: Government responsibility"),   
            helpText("q9 - V100: Hard work brings success"),   
            helpText("q10 - V170: Secure in neighborhood"),   
            helpText("q11 - V237: Family savings during past year"),   
            helpText("q12 - V238: Social class (subjective)"),   
            helpText("q13 - V240: Sex"),   
            helpText("q14 - V242: Age"),   
            helpText("q15 - V248: Highest educational level attained"),
           
           
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
            plotOutput("PAplot1"),
            
            h4("Factor Analysis"),
            plotOutput("FA1"),
            
            h4("Factor extraction (PCA)"),
            verbatimTextOutput("PCA1"),
            
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
               "WVS" = WVS)
    }, ignoreNULL = FALSE)
    
    # Show the "n" variables ----
    # The use of isolate() is necessary because we don't want the table
    # to update whenever input$obs changes (only when the user clicks
    # the action button)
    output$view <- renderTable({
        select(datasetInput(), 1:isolate(input$var))
    })
    
    output$PAplot1 <- renderPlot({if(input$PAplot == TRUE){
        parallel <- fa.parallel(mydata, fm = 'minres', fa = 'fa') 
    }})
    
    output$FA1 <- renderPlot({if(input$FA == TRUE){
        sixfactor <- fa(mydata,nfactors = 6,rotate = "oblimin",fm="minres")
        fa.diagram(sixfactor)
    }})
    
    output$PCA1 <- renderPrint({if(input$PCA == TRUE){
        pc3 <- principal(mydata, nfactors=6, rotate="varimax")
        print.psych(pc3, cut = 0.3, sort = TRUE)
    }})
    
}

# Create Shiny app ----
shinyApp(ui, server)

