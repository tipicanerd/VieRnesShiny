#install.packages("pacman")

library(pacman)

p_load(ggdist, ggplot2)


data <- read.csv("./data/MetabolicSyndrome.csv")

data$MetabolicSyndrome <- as.factor(data$MetabolicSyndrome)

cols <- c(
    "Age" = "Edad",
    "Income" = "Ingreso",
    "WaistCirc" = "Circunferencia de cintura",
    "BMI" = "IMC",
    "Albuminuria" = "Albuminuria",
    "UrAlbCr" = "Proporción albúmina-creatinina",
    "UricAcid" = "Nivel de Ácido úrico",
    "BloodGlucose" = "Nivel de Glucosa",
    "HDL" = 'Nivel de colesterol "bueno"',
    "Triglycerides"="Triglicéridos"
)

colores <- c(
    "Sex"="Sexo",
    "Marital" = "Estado civil",
    "Race" = "Raza",
    "MetabolicSyndrome" = "Síndrome Metabólico"
)

#### UNIVARIABLES ####

boxplot <- function(base,col_name, fill_name){
  plot <- base+
    geom_boxplot()+
      labs(title="Distribución de los datos de personas con Síndrome Metabólico", 
           x=cols[col_name],
           y=colores[fill_name],
           fill=colores[fill_name],
           caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
    
}

densidad <- function(base, col_name, fill_name){
  plot <- base+
    geom_density()+
      labs(title="Distribución de los datos de personas con Síndrome Metabólico", 
           x=cols[col_name],
           y=colores[fill_name],
           fill=colores[fill_name],
           caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
  
}

histograma <- function(base, col_name, fill_name){
  plot <- base+
    geom_histogram()+
      labs(title="Distribución de los datos de personas con Síndrome Metabólico", 
           x=cols[col_name],
           y=colores[fill_name],
           fill=colores[fill_name],
           caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
}


violin <- function(base, col_name, fill_name){
  plot <- base+
    geom_violin(trim=FALSE) +
      labs(title="Distribución de los datos de personas con Síndrome Metabólico", 
           x=cols[col_name],
           y=colores[fill_name],
           color=colores[fill_name],
           caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
  
}


                  
#### BIVARIABLES ####

dispersion <- function(base, col_namex, col_namey, fill_name){
  plot <- base + 
    geom_point()+
      labs(title="Distribución de los datos de personas con Síndrome Metabólico", 
           x=cols[col_namex],
           y=cols[col_namey],
           color=colores[fill_name],
           caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
}

densidad2d <- function(base, col_namex, col_namey, fill_name){
  plot <- base+
    geom_density2d()+
      labs(title="Distribución de los datos de personas con Síndrome Metabólico", 
           x=cols[col_namex],
           y=cols[col_namey],
           color=colores[fill_name],
           caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
}

