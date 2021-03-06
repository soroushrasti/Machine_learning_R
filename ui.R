#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Predict Horsepower from MPG"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("skiderMPG", "What is the MPG of the car",
                        min = 10,
                        max = 35,
                        value = 20),
        checkboxInput('showmodel1','show/hide model1',value=TRUE),
        checkboxInput('showmodel2','show/hide model2',value=TRUE),
        submitButton('submit')
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot1"),
            h3('predicted horsepower from model1'),
            textOutput('pred1'),
            h3('predicted horsepower from model2'),
            textOutput('pred2')
        )
    )
))
