#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


        mtcars$mpgsp    <- ifelse(mtcars$mpg-20>0, mtcars$mpg-20,0)
        model1<- lm(hp~mpg, data=mtcars)
        model2<- lm(hp~mpg+mpgsp, data=mtcars)
        
        model1pred<-reactive({
            mpginput<-input$skiderMPG
            predict(model1,newdata=data.frame(mpg=mpginput))
        })
        model2pred<-reactive({
            mpginput<-input$skiderMPG
            predict(model2,newdata=data.frame(mpg=mpginput, mpgsp=ifelse(mpginput-20>0,mpginput-20,0)))
        })
        output$plot1<-renderPlot({
            mpginput<-input$skiderMPG
            plot(mtcars$mpg,mtcars$hp, xlab = 'MPG', ylab='HP',bty='n', pch=16,xlim=c(10,35), ylim = c(50,350))
            if(input$showmodel1){
                abline(model1,col='red',lwd=2)
            }
            if(input$showmodel2){
                model2lines <- predict(model2,newdata=data.frame(mpg=10:35,mpgsp=ifelse(10:35-20>0,10:35-20,0)))
                lines(10:35,model2lines, col='green', lwd=2)
            }
                legend(25,250, c('model 1 prediction','model 2 prediction'),col=c('red', 'green'),bty='n', pch=16,cex=1.2)
                points(mpginput,model1pred(), col='red')
                points(mpginput,model2pred(), col='blue')
        })
        output$pred1=renderText({model1pred()})
        output$pred2=renderText({model2pred()})
        
    })

