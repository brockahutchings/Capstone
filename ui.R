######ui for shiny app#############

####load required packages
library(shiny)
library(plotly)

####define ui elements
shinyUI(fluidPage(
  
    ####create panel for interaction
    tabsetPanel(
      
        ####create input elements
        tabPanel("Prediction", 
            h3("Predict the following word"), 
            sidebarLayout(
                sidebarPanel(
                    textInput("sentenceInput", "Enter sentence you wish to complete: "),
                    helpText(br(), "How to:", br(), br(), "simply input text above and hit return.  A chart will ge gereated on the right showing the top to results.") 
                             
                ),
                
                ####create main pannel
                mainPanel(
                    h3("probabilities for following words:"),
                    plotlyOutput("sentenceOutput")
                )
            )
        ),
        
        ####create tab that conains links to further documantation
        tabPanel("Breif Explenation and Links", 
            h3("Author: Brock Hutchings    Date Created 2017/05/02"),
            h3("How to use the shiny app"),
            p("Enter the code fragment or partial sentence for which you want the next word to be predicted from and submit for processing."),
            h3("further documentation"),
            p("A site hosting the milestone project which demonstrates my origional data analysis approach can be found at: ", a("milestone project", href = "http://rpubs.com/BrockH/272857", target="_blank")),
            p("A brief presentation introducing the final project can be found at: ", a("project pitch", href = "https://brock.shinyapps.io/final_prediction/", target="_blank")),
            p("The full source code for the final project can be found at: ", a("code Repository", href = "https://github.com/brockahutchings/Capstone", target="_blank"))
        )
    ),
    tags$script('
        $(document).on("ready", function (e) {
            $("#sentenceInput").focus()
        });
    ') 
))
