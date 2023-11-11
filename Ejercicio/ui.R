#install.packages("pacman")

library(pacman)

p_load(shiny, shinythemes)

#### VARIABLES AUXILIARES ####
columnas <- c(
  "Longitud del sépalo"="Sepal.Length",
  "Ancho del sépalo"="Sepal.Width",
  "Longitud del pétalo"="Petal.Length",
  "Ancho del pétalo"="Petal.Width"
)

graficas_univariables <- c(
  "Boxplot" = "boxplot",
  "Densidad" = "densidad",
  "Histograma" = "histograma",
  "Violin" = "violin"
)


graficas_bivariables <- c(
  "Densidad 2D" = "densidad2d",
  "Dispersión" = "dispersion"
)

#### CONTROLES ####

### Univariable
col_control <- selectInput("col", "Columna",
                            choices = columnas,
                            selected = "Sepal.Length"
)


graf_uni_control <- selectInput("graf_uni", "Tipo de Gráfica",
                            choices = graficas_univariables,
                            selected =  "boxplot"
                            

)

tab_univariable <- tabPanel("Univariable", value="univariable",
                            col_control,
                            graf_uni_control
)


### Bivariable
colx_control <- selectInput("colx", "Eje X",
                           choices = columnas,
                           selected = "Sepal.Length"
)

coly_control <- selectInput("coly", "Eje Y",
                            choices = columnas,
                            selected = "Sepal.Length"
)

graf_bi_control <- selectInput("graf_bi", "Tipo de Gráfica",
                                choices = graficas_bivariables,
                                selected =  "dispersion"
                                
                                
)


tab_bivariable <- tabPanel("Bivariable", value="bivariable",
                           colx_control, coly_control,
                           graf_bi_control
)




#### UI #####


ui <- fluidPage(
    #Tema
   theme = shinytheme("cerulean"),
  
    #Titulo
    titlePanel("Exploración Gráfica Básica de Iris"),

    #Pestanias
    sidebarLayout(
        sidebarPanel(
          tabsetPanel( type="tabs", id="tab",
                       tab_univariable,
                       tab_bivariable,
          )
            
        ),

        # Mostrar el plot
        mainPanel(
            plotOutput("plotFinal")
        )
    ),
   tags$footer("Hecho con amor 🖤, 2023")
)


