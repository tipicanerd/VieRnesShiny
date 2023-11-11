library(pacman)

p_load(dplyr, openxlsx, stringi)

materias <- read.csv("./data/Maestria_LT_Materias.csv", row.names=1, encoding = "UTF-8")


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




get_data_cal <- function(data_path, programa, generacion,semestre){
    
    if (programa=="Doctorado"){
        inicio <- 1:7
    
        alumnos <- openxlsx::read.xlsx(data_path,sheet = programa, fillMergedCells = TRUE)[,c(inicio,semestre)]
        
        
        cols <- as.character(alumnos[1,])
        
        colnames(alumnos) <- make.names(cols)
        
        alumnos <- alumnos %>% filter(Gen==generacion)
        
        alumnos <- 
            alumnos %>%
            select(-c(Promedio,Gen, Estado)) %>%
            dplyr::mutate(
                Nombre = paste(Apellido.paterno, Apellido.materno,Nombre),
                .after = CURP,
                .keep = "unused"
            )
    }
    else{
        inicio <- 1:8
        
        als <- openxlsx::read.xlsx(data_path,sheet = programa, fillMergedCells = TRUE)[,c(inicio,semestre)]
        
        cols <- as.character(als[1,])
        colnames(als) <- make.names(cols)
        als <- als %>% filter(Gen==generacion)
        
        
        alumnos <- 
            als %>%
            select(-c(Promedio,Gen, Estado)) %>%
            dplyr::mutate(
                Nombre = paste(Apellido.paterno, Apellido.materno,Nombre),
                .after = CURP,
                .keep = "unused"
            )
        
        
        linea_terminal <- sum(stri_count(cols, regex="LT")) > 0
        
        if(linea_terminal==TRUE){
             nombres <- colnames(alumnos)
             nombres <- nombres[nombres!="Programa"]
             alumnos <- c()

             for (programa in unique(als$Programa)){
                 cols2 <- cols
                 for (i in 1:length(cols)){
                     if (stri_detect(cols[i], regex="LT")){
                         cols2[i] <- materias[programa, stri_replace(cols[i], ".", regex=" ")]
                     }
                 }
                 als2 <- als %>% 
                     filter(Programa==programa) %>%
                     select(-c(Promedio,Gen,Programa, Estado)) %>%
                     dplyr::mutate(
                         Nombre = paste(Apellido.paterno, Apellido.materno,Nombre),
                         .after = CURP,
                         .keep = "unused"
                     )
                 
                 
                 names(als2) <- nombres
                 
                 
                 cols2 <- cols2[(length(cols2)-dim(als2)[2]):(length(cols2)-1)]
                 cols2[1:3] <- rep(NA,3)
                 names(cols2) <- nombres
                 
                 alumnos <- bind_rows(alumnos,cols2,als2)
                 
                 
             }
             alumnos <- as.data.frame(alumnos)
             colnames(alumnos) <- nombres
        }
        else{
            alumnos <- alumnos %>% select(-c(Programa))
        }
        
    }
        
    colnames(alumnos) <- stri_replace_all(colnames(alumnos), " ", regex="\\.")
    
    alumnos$Obs <- NA
    alumnos$m <- NA
    
    
    alumnos <- alumnos %>% 
        relocate(
            Nombre,
            .after = CURP
        ) %>%
        relocate(
            Matrícula,
            .after = Nombre
        ) %>%
        relocate(
            Obs,
            .before = Matrícula
        ) %>%
        relocate(
            m,
            .after = Matrícula
        )
    
}

get_data_ins <- function(data_path, programa, generacion,semestre){
    
    if (programa=="Doctorado"){
        inicio <- 1:7
        
        alumnos <- openxlsx::read.xlsx(data_path,sheet = programa, fillMergedCells = TRUE)[,c(inicio,semestre)]
        
        cols <- alumnos[1,]
        
        colnames(alumnos) <- make.names(cols)
        
        alumnos <- alumnos %>% filter(Gen==generacion)
        
        alumnos$Promedio <- NA
        
        alumnos <- 
            alumnos %>%
            select(-c(Promedio,Gen, Estado)) %>%
            dplyr::mutate(
                Nombre = paste(Apellido.paterno, Apellido.materno,Nombre),
                .after = CURP,
                .keep = "unused"
            )
        alumnos[,4:dim(alumnos)[2]] <- NA
    }
    else{
        inicio <- 1:8
        
        als <- openxlsx::read.xlsx(data_path,sheet = programa, fillMergedCells = TRUE)[,c(inicio,semestre)]
        
        cols <- als[1,]
        colnames(als) <- make.names(cols)
        als <- als %>% filter(Gen==generacion)
        
        als$Promedio <- NA
        
        alumnos <- als %>%
            select(-c(Promedio,Gen, Estado)) %>%
            dplyr::mutate(
                Nombre = paste(Apellido.paterno, Apellido.materno,Nombre),
                .after = CURP,
                .keep = "unused"
            )
        alumnos[,4:dim(alumnos)[2]] <- NA
        linea_terminal <- sum(stri_count(cols, regex="LT")) > 0
        
        if(linea_terminal==TRUE){
            nombres <- colnames(alumnos)
            alumnos <- c()
            
            for (programa in unique(als$Programa)){
                cols2 <- cols
                for (i in 1:length(cols)){
                    if (stri_detect(cols[i], regex="LT")){
                        cols2[i] <- materias[programa, stri_replace(cols[i], ".", regex=" ")]
                    }
                }
                
                als2 <- als %>%
                    select(-c(Promedio,Gen, Estado)) %>%
                    dplyr::mutate(
                        Nombre = paste(Apellido.paterno, Apellido.materno,Nombre),
                        .keep = "unused"
                    )
                als2[,4:dim(als2)[2]] <- NA
                names(als2) <- nombres
                
                cols2 <- cols2[(length(cols2)-dim(als2)[2]):(length(cols2)-1)]
                cols2[1:3] <- rep(NA,3)
                names(cols2) <- nombres
                
                alumnos <- bind_rows(alumnos,cols2,als2)
                
                
            }
            alumnos <- as.data.frame(alumnos)
            colnames(alumnos) <- nombres
        }
        alumnos <- alumnos %>% select(-c(Programa))
    }
    colnames(alumnos) <- stri_replace_all(colnames(alumnos), " ", regex="\\.")
    
    alumnos$Obs <- NA
    
    alumnos$m <- NA

    
    alumnos <- alumnos %>% 
        relocate(
            Nombre,
            .after = CURP
        ) %>%
        relocate(
            Matrícula,
            .after = Nombre
        ) %>%
        relocate(
            Obs,
            .before = Matrícula
        ) %>%
        relocate(
            m,
            .after = Matrícula
        )
    
    
    return(alumnos)
    
}


