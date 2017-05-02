########server for shiny app##########

####load required packages
library(shiny)
library(dplyr)
library(plotly)

####declaire source package
source("predictor.R")

####define shiny server function
shinyServer(function(input, output) {
    
    output$sentenceOutput <- renderPlotly({
      
        words <- predictor.predict_word(input$sentenceInput)
        
        if(!is.null(words) && nrow(words) > 0) {
          
            words <- head(words, 15)
            
            words <- arrange(words, probAdj)
            
            words$word4 <- factor(words$word4, levels = as.character(words$word4))
            
            words$probAdj <- ((words$probAdj / sum(words$probAdj)) * 100)
            
            ####define return 
            return(
                plot_ly(words, 
                        y = ~word4, 
                        x = ~probAdj, 
                        type = "bar", 
                        hoverinfo = TRUE,
                        name = "Next Word Probability") %>%
                  
                    layout(yaxis = list(title = ""), 
                           xaxis = list(title = "Probability Percentage", range = c(0, 110)), 
                           margin = list(l = max(sapply(as.character(words$word4), nchar)) * 10))   
            )
        } else {
          
            return(NULL)
          
        }
    })
  
})
