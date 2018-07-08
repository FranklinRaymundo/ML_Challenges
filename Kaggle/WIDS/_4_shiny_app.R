
library(xgboost)
library(lime)
library(shiny)

# Loading explanation:
explanation_xgb <- readRDS('explanation_xgb.Rda')

# Creating Shinny app
ui <- fluidPage(
    sidebarLayout(
        
        sidebarPanel(
            
            h1
            
        )
        
    )
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

