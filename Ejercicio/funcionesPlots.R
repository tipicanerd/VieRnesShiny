#install.packages("pacman")

library(pacman)

p_load(ggdist, ggplot2)

cols <- c(
  "Sepal.Length" = "Longitud del sépalo",
  "Sepal.Width" = "Ancho del sépalo",
  "Petal.Length" = "Longitud del pétalo",
  "Petal.Width" = "Ancho del pétalo"
)


#### UNIVARIABLES ####

boxplot <- function(base,col_name){
  plot <- base+
    geom_boxplot()+
    labs(title="Exploración Gráfica de Iris", 
         x=cols[col_name],
         y="Especies",
         caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
    
}

densidad <- function(base, col_name){
  plot <- base+
    geom_density()+
    labs(title="Exploración Gráfica de Iris", 
         x=cols[col_name],
         caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
  
}

histograma <- function(base, col_name){
  plot <- base+
    geom_histogram()+
    labs(title="Exploración Gráfica de Iris", 
         x=cols[col_name],
         caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
}


violin <- function(base, col_name){
  plot <- base+
    geom_violin(trim=FALSE) +
    labs(title="Exploración Gráfica de Iris", 
         x=cols[col_name],
         y="Especies",
         caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
  
}

#### BIVARIABLES ####

dispersion <- function(base, col_namex, col_namey){
  plot <- base + 
    geom_point()+
  labs(title="Exploración Gráfica de Iris", 
                     x=cols[col_namex],
                     y=cols[col_namey],
                     caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
}

densidad2d <- function(base, col_namex, col_namey){
  plot <- base+
    geom_density2d()+
    labs(title="Exploración Gráfica de Iris", 
         x=cols[col_namex],
         y=cols[col_namey],
         caption = "Nota: Elaboración propia (2023)")
  
  return(plot)
}

