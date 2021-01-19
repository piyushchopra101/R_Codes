

library(shiny)
myfood<-myfunc()
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    library(ggplot2)
    
    #install.packages("hexbin")
    library("hexbin")
    
    library(lattice)
  
    #x11()
    mysample <- myfood[sample(1:nrow(food), 80010,replace=FALSE),]
    #x11()
    #plot(density(mysample$Num_Violations))
    
    #x11()
    #histogram(mysample$Num_Violations)
    #dotplot(mysample$Num_Violations~mysample$Zip)
    #x11()
    dotplot(mysample$Facility.Type~mysample$Num_Violations,pch=5,cex.lab=3,width=3000,height=4000)
    
   
    })
  output$distPlot2 <- renderPlot({
    library(ggplot2)
    
    #install.packages("hexbin")
    library("hexbin")
    
    library(lattice)
    
    #x11()
    mysample <- myfood[sample(1:nrow(food), 80010,replace=FALSE),]
    #x11()
    #plot(density(mysample$Num_Violations))
    
    #x11()
    #histogram(mysample$Num_Violations)
    #dotplot(mysample$Num_Violations~mysample$Zip)
    x11()
    dotplot(mysample$Zip~mysample$Num_Violations,pch=5,cex.lab=3,width=3000,height=4000)
  })
  
})
