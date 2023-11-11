#install.packages("pacman")

library(pacman)

p_load(shiny, shinythemes)

#### VARIABLES AUXILIARES ####

coloreado <- c(
    "Sexo" = "Sex",
    "Estado civil" = "Marital",
    "Raza" = "Race",
    "Síndrome Metabólico" = "MetabolicSyndrome"
)


columnas <- c(
  "Edad" = "Age",
  "Ingreso" = "Income",
  "Circunferencia de cintura" = "WaistCirc",
  "IMC" = "BMI",
  "Albuminuria" = "Albuminuria",
  "Proporción albúmina-creatinina" = "UrAlbCr",
  "Nivel de Ácido úrico" = "UricAcid",
  "Nivel de Glucosa" = "BloodGlucose",
  'Nivel de colesterol "bueno"' = "HDL",
  "Triglicéridos"="Triglycerides"
)

graficas_univariables <- c(
  "Boxplot" = "boxplot",
  "Densidad" = "densidad",
  "Histograma" = "histograma",
  "Violín" = "violin"
)


graficas_bivariables <- c(
  "Densidad 2D" = "densidad2d",
  "Dispersión" = "dispersion"
)

#### CONTROLES ####

color_control <- selectInput("color", "Color",
                             choices = coloreado,
                             selected = "MetabolicSyndrome"
                             )

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
                            color_control,
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
                           color_control,
                           colx_control, coly_control,
                           graf_bi_control
)




#### UI #####


ui <- fluidPage(
    #Tema
   theme = shinytheme("cerulean"),
  
    #Titulo
    titlePanel("Exploración Gráfica Básica de Personas con y sin Síndrome Metabólico"),

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


