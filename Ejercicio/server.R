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
       base_uni1 <- ggplot(data, aes_string(y = input$color, x=input$col, fill=input$color))
       base_uni2 <- ggplot(data, aes_string(x=input$col, fill=input$color))
                          
        
       #Revisamos el tipo de grafica
       graf_uni <- input$graf_uni
       
       
       switch (graf_uni,
         boxplot = { plot<-boxplot(base_uni1, input$col, input$color)},
         densidad = {plot<-densidad(base_uni2, input$co, input$colorl)},
         histograma = {plot<-histograma(base_uni2, input$col, input$color)},
         violin = {plot<-violin(base_uni1, input$col, input$color)}
       )
       
     }
    else{
      base_bi <- ggplot(data, aes_string(x=input$colx, y=input$coly, color=input$color))
      
      #Revisamos el tipo de grafica
      graf_bi <- input$graf_bi
      
      
      switch (graf_bi,
              dispersion = {plot<-dispersion(base_bi, input$colx, input$coly , input$color)},
              densidad2d = {plot<-densidad2d(base_bi, input$colx, input$coly, input$color)},
      )
      
    }
    
    
    plot+theme_light()
  })

}
