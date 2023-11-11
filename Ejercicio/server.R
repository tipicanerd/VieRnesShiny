#install.packages("pacman")
library(pacman)

p_load(dplyr, ggplot2, shiny, stringr)

source("funcionesPlots.R")



function(input, output, session) {

  output$plotFinal <- renderPlot({
    
    #Revisamos que pestania fue seleccionada
    tab <- input$tab
    #Revisamos las univariables
     if (tab=="univariable"){
       
       #Creamos la base
       base_uni1 <- ggplot(iris, aes_string(y = "Species", x=input$col, fill="Species"))
       base_uni2 <- ggplot(iris, aes_string(x=input$col, fill="Species"))
                          
        
       #Revisamos el tipo de grafica
       graf_uni <- input$graf_uni
       
       
       switch (graf_uni,
         boxplot = { plot<-boxplot(base_uni1, input$col)},
         densidad = {plot<-densidad(base_uni2, input$col)},
         histograma = {plot<-histograma(base_uni2, input$col)},
         densidad = {plot<-densidad(base_uni2, input$col)},
         violin = {plot<-violin(base_uni1, input$col)}
       )
       
     }
    else{
      base_bi <- ggplot(iris, aes_string(x=input$colx, y=input$coly, color="Species"))
      
      #Revisamos el tipo de grafica
      graf_bi <- input$graf_bi
      
      
      switch (graf_bi,
              dispersion = {plot<-dispersion(base_bi, input$colx, input$coly)},
              densidad2d = {plot<-densidad2d(base_bi, input$colx, input$coly)},
      )
      
    }
    
    
    plot+theme_light()
  })

}
