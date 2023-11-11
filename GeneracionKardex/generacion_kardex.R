library(pacman)

p_load(dplyr, officer, openxlsx, stringi, zip)

template_path <- "./templates/"

materias <- read.csv("./data/Maestria_LT_Materias.csv", row.names=1, encoding = "UTF-8")

calificaciones <- c(
    "1"="UNO", "2"="DOS", "3"="TRES", "4"="CUATRO", "5"="CINCO",
    "6"="SEIS", "7"="SIETE", "8"="OCHO", "9"="NUEVE", "10"="DIEZ"
)

get_semestre_cols <- function(programa, semestre){
    switch(
        programa,
        Doctorado={
            switch(
                semestre,
                "1" = return(8:11),
                "2" = return(12:15),
                "3" = return(16:19),
                "4" = return(20:23),
                "5" = return(24:26)
            )
        },
        Maestría={
            switch (semestre,
                    "1" = return(9:13),
                    "2" = return(14:18),
                    "3" = return(19:23),
                    "4" = return(24:26)
            )
        }
    )
}


get_data <- function(data_path, programa, generacion,semestre){
    
    if (programa=="Doctorado"){ inicio <- 1:7}
    else{inicio <- 1:8}
    
    alumnos <- openxlsx::read.xlsx(data_path,sheet = programa, fillMergedCells = TRUE)[, c(inicio, semestre)]
    
    cols <- alumnos[1,]
    
    colnames(alumnos) <- cols
    
    alumnos <- alumnos %>% filter(Gen==generacion)
    
    
    
    return(alumnos)
    
}

get_template_name <- function(programa, semestre){
    if(programa=="Doctorado"){base_name <- "KARDEX_DC"}
    else{base_name <- "KARDEX_MC"}
    
    return(paste(base_name,semestre, ".docx", sep=""))
}



generate_files <- function(template_name, alumnos, programa, generacion, n_sem, semestre, fecha_inicio, finicio, ffin){
    plantilla <- officer::read_docx(paste(template_path,template_name, sep=""))
    
    docs <- c()
    
    if(programa=="Doctorado"){inicio_col<-7}
    else{inicio_col<-8}
    
    if(programa=="Doctorado" || n_sem<3){
        for (i in 1:dim(alumnos)[1]){
            nombre_alumno <- paste(alumnos[i,"Apellido paterno"], alumnos[i,"Apellido materno"],alumnos[i,"Nombre"])
            
            
            body_replace_all_text(plantilla,"<<Nombre>>", nombre_alumno)
            body_replace_all_text(plantilla,"<<CURP>>", alumnos[i,"CURP"])
            body_replace_all_text(plantilla,"<<Matricula>>", alumnos[i,"Matrícula"])
            body_replace_all_text(plantilla,"<<Inicio>>", fecha_inicio)
            body_replace_all_text(plantilla,paste("<<Finicio", n_sem,">>", sep = ""), finicio)
            body_replace_all_text(plantilla,paste("<<Ftermino", n_sem,">>", sep= ""), ffin)
            
            for (j in 1:(length(semestre)-1)){
                materia <- paste("<<Materia",j,".",n_sem,">>", sep="")
                
                body_replace_all_text(plantilla,materia, colnames(alumnos)[inicio_col+j])
                
                cal <- paste("<<P",j,".",n_sem,">>", sep="")
                calificacion <- as.character(round(as.numeric(alumnos[i, inicio_col+j])))
                body_replace_all_text(plantilla,cal, calificacion)
                lcal <- paste("<<P",j,".",n_sem,"L>>", sep="")
                body_replace_all_text(plantilla,lcal, calificaciones[calificacion])
            }
            
            filename <- paste(nombre_alumno," ", generacion,"S", n_sem, ".docx", sep="")
            
            docs <- append(docs,filename)
            
            print(plantilla, filename)
            
            plantilla <- officer::read_docx(paste(template_path, template_name, sep=""))
            
        }
    }
    
    else{
        for (i in 1:dim(alumnos)[1]){
            nombre_alumno <- paste(alumnos[i,"Apellido paterno"], alumnos[i,"Apellido materno"],alumnos[i,"Nombre"])
            body_replace_all_text(plantilla,"<<Nombre>>", nombre_alumno)
            body_replace_all_text(plantilla,"<<CURP>>", alumnos[i,"CURP"])
            body_replace_all_text(plantilla,"<<Matricula>>", alumnos[i,"Matrícula"])
            body_replace_all_text(plantilla,"<<Inicio>>", fecha_inicio)
            body_replace_all_text(plantilla,paste("<<Finicio", n_sem,">>", sep = ""), finicio)
            body_replace_all_text(plantilla,paste("<<Ftermino", n_sem,">>", sep=""), ffin)
            
            
            
            
            for (j in 1:(length(semestre)-1)){
                materia <- paste("<<Materia",j,".",n_sem,">>", sep="")
                
                nombre_materia <- colnames(alumnos)[inicio_col+j]
                
                if (stri_detect(nombre_materia, regex="LT")){
                    nombre_materia <- materias[alumnos[i,"Programa"], stri_replace(nombre_materia, ".", regex=" ")]
                }
                
                body_replace_all_text(plantilla,materia, nombre_materia)
                
                cal <- paste("<<P",j,".",n_sem,">>", sep="")
                calificacion <- as.character(round(as.numeric(alumnos[i, inicio_col+j])))
                body_replace_all_text(plantilla,cal, calificacion)
                lcal <- paste("<<P",j,".",n_sem,"L>>", sep="")
                body_replace_all_text(plantilla,lcal, calificaciones[calificacion])
            }
            
            filename <- paste(nombre_alumno," ", generacion,"S", n_sem, ".docx", sep="")
            
            docs <- append(docs,filename)
            
            
            print(plantilla, filename)
            
            plantilla <- officer::read_docx(paste(template_path, template_name, sep=""))
            
        }
        
    }
    
    
    return(docs)
}


