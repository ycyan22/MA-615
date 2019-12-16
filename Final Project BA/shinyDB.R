library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2) 
library(stringi)
library(psych) 
library(GPArotation)
library(corpcor)
library(kableExtra)
library(randomForest)
library(leaflet)
library(leaflet.extras)

# Read data
mydata <- read_csv('Boston2017.csv')
mydata <- mydata%>%select("room_id","host_id","room_type","neighborhood","reviews","overall_satisfaction","accommodates","bedrooms","price","latitude","longitude")
mydata <- drop_na(mydata) 
mydata$reviews<-as.numeric(mydata$reviews)
mydata$overall_satisfaction<-as.numeric(mydata$overall_satisfaction)
mydata$latitude<-as.numeric(mydata$latitude)
mydata$longitude<-as.numeric(mydata$longitude)

# Data for obs and map
mydata1 <- mydata%>%select("room_type","neighborhood","reviews","overall_satisfaction","accommodates","bedrooms","price","latitude","longitude")
pricelevel1 <- filter(mydata1, price<=150)
pricelevel2 <- filter(mydata1, price>150 & price<=300)
pricelevel3 <- filter(mydata1, price>300)

# Data for correlation
cordata <- mydata[, sapply(mydata, is.numeric)]
cor.ma <- cor(cordata, method = "pearson")

# Data for model
modeldata <- mydata
modeldata$reviews[modeldata$reviews=="0"] <- NA
modeldata <- drop_na(modeldata)
modeldata <- modeldata[!(modeldata$price==max(modeldata$price)),]
modeldata <- modeldata[!(modeldata$price==max(modeldata$price)),]
modeldata <- modeldata[!(modeldata$price==max(modeldata$price)),]



# Shiny
ui <- dashboardPage(
    dashboardHeader(title = "Boston Airbnb"),
    
    dashboardSidebar(
        sidebarMenu(menuItem("Dataset", tabName = "dataset", icon = icon("dashboard")),
            menuItem("Mapping", tabName = "mapping", icon = icon("th")),
            menuItem("EDA", tabName = "eda", icon = icon("th")),
            menuItem("EFA", tabName = "efa", icon = icon("th")),
            menuItem("Method", tabName = "method", icon = icon("th")),
            menuItem("Model", tabName = "model", icon = icon("th")),
            menuItem("Citation", tabName = "citation", icon = icon("th"))
            )
    ),
    
    dashboardBody(
        tabItems(
            # 1st tab content
            tabItem(tabName = "dataset",
                    fluidRow(
                        box(h4("Room search"),
                            selectInput("dataset", "Choose a price range:",
                                        choices = c("Below 150", "Between 150 and 300", "Above 300")),
                            sliderInput("obs", "Number of observations to view:", value = 6, min = 0, max = 100),
                            actionButton("update", "Update View")
                            ),
                        tableOutput("view")
                        )
            ),
            
            
            # 2nd tab content
            tabItem(tabName = "mapping",
                    h2("Location mapping"),
                    box(checkboxInput("pl1", "Below 150", value = FALSE),
                        checkboxInput("pl2", "Between 150 and 300", value = FALSE),
                        checkboxInput("pl3", "Above 300", value = FALSE)
                        ),
                    leafletOutput("Umap")
            ),
            

            
            # 3rd tab content
            tabItem(tabName = "eda",
                    h2("Exploratory data analysis"),
                    box(checkboxInput("NPplot", "Neighborhood and price", value = TRUE),
                        plotOutput("NPplot1")
                        ),
                    box(checkboxInput("table", "Location and price by average rating", value = TRUE),
                        htmlOutput("table1")
                    )
                            
            ),
            
            
            # 4th tab content
            tabItem(tabName = "efa",
                    h2("EFA"),
                    box(checkboxInput("PAplot", "Parallel Analysis Scree Plot", value = TRUE),
                        helpText("Parallel analysis suggests that the number of factors =  4  and the number of components =  NA "),
                        plotOutput("PAplot1")
                    ),
                    
                    box(
                        checkboxInput("FA", "Factor Analysis", value = TRUE),
                        helpText("Now that we’ve arrived at probable number number of factors, let’s start off with 4 as the number of factors."),
                        plotOutput("FA1")
                    )
            ),
            
            
            
            # 5th tab content
            tabItem(tabName = "method",
                    h2("Correlation"),
                    box(checkboxInput("Corplot", "Correlation among variables", value = TRUE),
                        plotOutput("Corplot1")
                        ),
                    
                    h2("Random forest"),
                    box(helpText("Random Forest combines the output of multiple decision trees 
                                 and then finally come up with its own output. 
                                 We use random forest to choose variables to fit model.
                                 Since it would make R work really slow, we did not include here.
                                 You can find plot in final report.")
                        ),
                    
                    h2("Concerns"),
                    box(helpText("Zero values in “the number of reviews” and “the average rating” 
                    may lead to potential problems. Usually, living spots with unattractive 
                    appearance or location probably have few or no reviews. But new posted houses 
                    also have zero review since no one has stayed before. If I keep these zero 
                    values in the fitted model, the model will predict relatively low prices for 
                    those new lodgings. In addition, the plot shows 3 outliers with pretty high price 
                    above $3000, which might make regression less reliable. In this way, I remove 
                    these observations.")
                        )
            ),
            
            
            # 6th tab content
            tabItem(tabName = "model",
                    h2("Model"),
                    box(helpText("price~log(accommodates)+bedrooms+reviews*overall_satisfaction
                                 +as.factor(neighborhood)+as.factor(room_type)"),
                        checkboxInput("MResult", "Display estimated coefficient result", value = FALSE),
                        verbatimTextOutput("MResult1")
                    ),
                    
                    h2("Check model"),
                    box(checkboxInput("Qplot", "Display QQ plot", value = TRUE),
                        plotOutput("Qplot1")
                    )
            ),
            
            
            # 7th tab content
            tabItem(tabName = "citation",
                    h2("Data and review resource"),
                    fluidRow(
                    p(em("http://tomslee.net/airbnb-data-collection-get-the-data")),
                    p(em("http://insideairbnb.com/get-the-data.html"))
            ))
            
        )
    ))

server <- function(input, output) {
    
    ### 1st tab output
    datasetInput <- eventReactive(input$update, {
        switch(input$dataset,
               "Below 150" = pricelevel1,
               "Between 150 and 300" = pricelevel2,
               "Above 300" = pricelevel3)
    }, ignoreNULL = FALSE)
    
    output$view <- renderTable({
        head(datasetInput(), n = isolate(input$obs))
    })
    
    
    ### 2nd tab output
    points1 <- cbind(pricelevel1$longitude, pricelevel1$latitude)
    points2 <- cbind(pricelevel2$longitude, pricelevel2$latitude)
    points3 <- cbind(pricelevel3$longitude, pricelevel3$latitude)
    
    
    output$Umap <- renderLeaflet({
        leaflet() %>%
            setView(lng = -71, lat = 42.3, zoom = 10.2)  %>%
            addTiles() 
    })
    
    observe({
        proxy <- leafletProxy("Umap", data = points1)
        # Remove any existing legend, and only if the legend is # enabled, create a new one.
        proxy %>% clearShapes()
        if (input$pl1==TRUE) {
            proxy %>% 
                addCircles(data = points1, color = "blue")
        } 
        
        if (input$pl2==TRUE) {
            proxy %>% 
                addCircles(data = points2, color = "red")
        } 
        
        if (input$pl3==TRUE) {
            proxy %>% 
                addCircles(data = points3, color = "green")
        } 
        
        })
    
    
    ### 3nd tab output
    output$NPplot1 <- renderPlot({if(input$NPplot == TRUE){
        ggplot(data = modeldata, aes(x = neighborhood, y = price, color = room_type)) +
            geom_point(size = 3) +
            geom_line() 
    }})
    
    output$table1 <- renderText({if(input$table == TRUE){
        df1 <- modeldata[,c("neighborhood","accommodates","price")]
        df2 <- aggregate(df1[,2:3],by=list(df1$neighborhood),mean)
        kable(df2, digits = 2,       
              col.names = c("Location", "Average Rating", "Price"),align = 'c') %>%
            kable_styling(latex_options = 'hold_position',font_size = 12,full_width = F,position = "center")%>%
            column_spec(1,bold = T)
    }})
    
    
    
    
    ### 4th tab output
    output$PAplot1 <- renderPlot({if(input$PAplot == TRUE){
        parallel <- fa.parallel(cordata, fm = 'minres', fa = 'fa')  
    }})
    
    output$FA1 <- renderPlot({if(input$FA == TRUE){
        fourfactor <- fa(cordata,nfactors = 4,rotate = "oblimin",fm="minres") # 4 factor analysis
        fa.diagram(fourfactor)
    }})
    
    
    
    ### 5th tab output
    output$Corplot1 <- renderPlot({if(input$Corplot == TRUE){
        corrplot::corrplot(cor.ma, method = "circle", type = "upper", diag = F)   
    }})
    
    
    
    ### 6th tab output
    output$MResult1 <- renderPrint({if(input$MResult == TRUE){
        fit1 <- lm(price~log(accommodates)+bedrooms+reviews*overall_satisfaction+as.factor(neighborhood)+as.factor(room_type), data=modeldata)
        arm::display(fit1)  
    }})  
    
    output$Qplot1 <- renderPlot({if(input$Qplot == TRUE){
        car::qqPlot(fit1$residuals)   
    }})
    
    
        
}

shinyApp(ui, server)
