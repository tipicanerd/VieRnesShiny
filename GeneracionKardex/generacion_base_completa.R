library(pacman)

p_load(dplyr, openxlsx, plyr, stringi)


nombres <- c(
    "CT", "CURP", "AP1" ,"AP2", "NOMBRE", "IND",
    "GRADO", "GRUPO", "MATRICLA", "OBS", "TIPO", "MES", "CICLO"
)

fecha <- as.numeric(unlist(strsplit(as.character(Sys.Date()), "-")))
mes <- fecha[2]
if (mes<8){
    ciclo <- paste((fecha[1]-1)%%100, fecha[1]%%100, sep="/")
}else{
    ciclo <- paste(fecha[1]%%100, (fecha[1]+1)%%100, sep="/")
}


indicadores <- read.csv("./data/Indicadores_SEP.csv", row.names=1, encoding = "UTF-8")

centroTrabajo <- "-"

get_semestre_doctorado <- function(x){
    semestre_actual <- c(
        "8" = 1,
        "9" = 2,
        "10" = 3,
        "11" = 4,
        "12" = 5
    )
    
    for (i in 1:length(x)){
        if (is.na(x[i])){
            return(as.numeric(semestre_actual[as.character(i)]))
        }
    }
}

get_semestre_maestria <- function(x){
    semestre_actual <- c(
        "9" = 1,
        "10" = 2,
        "11" = 3,
        "12" = 4
    )
    
    for (i in 1:length(x)){
        if (is.na(x[i])){
            return(as.numeric(semestre_actual[as.character(i)]))
        }
    }
}



get_data_base <- function(data_path){
    doctorado <- openxlsx::read.xlsx(data_path,sheet = "Doctorado", fillMergedCells = TRUE)
    maestria <- openxlsx::read.xlsx(data_path,sheet = "Maestría", fillMergedCells = TRUE)   
    
    doctorado <- doctorado %>% 
        select(which(!duplicated(names(.)))) %>%
        filter(Estado=="ACTIVO") %>%
        mutate(
            GRUPO = "A",
            INDICADOR = indicadores["Doctorado en Ciencias Administrativas","Indicador"]
        ) 
    
    doctorado$GRADO <- apply(doctorado, 1, get_semestre_doctorado)
    
    doctorado <- doctorado[, -c(1,3, 8:14)]
    
    doctorado <- doctorado[,c(5,2,3,4,7,8,6,1)]

    
    maestria <- maestria %>%
        select(which(!duplicated(names(.)))) %>%
        filter(Estado=="ACTIVO") %>%
        mutate(
            GRUPO = "A",
            INDICADOR = indicadores["Maestría en Ciencias","Indicador"]
        )
    
    maestria$GRADO <- apply(maestria, 1, get_semestre_maestria)
    maestria <- maestria[, -c(1,3,9:14)]
    maestria[maestria$GRADO>2,"INDICADOR"] <- laply(maestria[maestria$GRADO>2,"Programa"], function(x){indicadores[x,"Indicador"]})
    
    for (i in unique(maestria[maestria$GRADO>2,"GRADO"])) {
            indicadores <- unique(maestria[maestria$GRADO==i,"INDICADOR"])
            for (j in length(indicadores)){
                maestria[maestria$GRADO==i&maestria$INDICADOR==indicadores[j], "GRUPO"] <- LETTERS[j]
            }
    }
    
    maestria <- maestria[,c(5,2,3,4,8,9,7,1)]
    
    alumnos <- cbind(centroTrabajo, bind_rows(doctorado,maestria)) %>%
        mutate(
            OBS = - NA,
            MES = mes,
            TIPO = "S",
            CICLO = ciclo
        )
    
    colnames(alumnos) <-  nombres
    return(alumnos)
       
}

get_data_base <- function(data_path){
    doctorado <- openxlsx::read.xlsx(data_path,sheet = "Doctorado", fillMergedCells = TRUE)
    maestria <- openxlsx::read.xlsx(data_path,sheet = "Maestría", fillMergedCells = TRUE)   
    
    doctorado <- doctorado %>% 
        select(which(!duplicated(names(.)))) %>%
        filter(Estado=="ACTIVO") %>%
        mutate(
            GRUPO = "A",
            INDICADOR = indicadores["Doctorado en Ciencias Administrativas","Indicador"]
        ) 
    
    doctorado$GRADO <- apply(doctorado, 1, get_semestre_doctorado)
    
    doctorado <- doctorado[, -c(1,3, 8:14)]
    
    doctorado <- doctorado[,c(5,2,3,4,7,8,6,1)]

    
    maestria <- maestria %>%
        select(which(!duplicated(names(.)))) %>%
        filter(Estado=="ACTIVO") %>%
        mutate(
            GRUPO = "A",
            INDICADOR = indicadores["Maestría en Ciencias","Indicador"]
        )
    
    maestria$GRADO <- apply(maestria, 1, get_semestre_maestria)
    maestria <- maestria[, -c(1,3,9:14)]
    maestria[maestria$GRADO>2,"INDICADOR"] <- laply(maestria[maestria$GRADO>2,"Programa"], function(x){indicadores[x,"Indicador"]})
    
    for (i in unique(maestria[maestria$GRADO>2,"GRADO"])) {
            indicadores <- unique(maestria[maestria$GRADO==i,"INDICADOR"])
            for (j in length(indicadores)){
                maestria[maestria$GRADO==i&maestria$INDICADOR==indicadores[j], "GRUPO"] <- LETTERS[j]
            }
    }
    
    maestria <- maestria[,c(5,2,3,4,8,9,7,1)]
    
    alumnos <- cbind(centroTrabajo, bind_rows(doctorado,maestria)) %>%
        mutate(
            OBS = - NA,
            MES = mes,
            TIPO = "S",
            CICLO = ciclo
        )
    
    colnames(alumnos) <-  nombres
    return(alumnos)
       
}


get_edad <- function(CURP){
    curp <- substr(CURP,5,10)
    currentYearCentury <- as.integer (substr (as.character (Sys.Date ()), 1, 2)) * 100
    birthYear <- as.integer (substr (curp, 1, 2))
    birthCentury <- ifelse (birthYear >= as.integer (substr (as.character (Sys.Date ()), 3, 4)), currentYearCentury - 100, currentYearCentury)
    birthMonth <- as.integer (substr (curp, 3, 4)) - 1
    birthDay <- as.integer (substr (curp, 5, 6))
    birthDate <- as.Date (paste0 (birthCentury + birthYear, '-', birthMonth + 1, '-', birthDay), '%Y-%m-%d')
    age <- floor(as.numeric(difftime (Sys.Date (), birthDate, units = 'weeks')) / 52.25)
    return(age)
}




get_data_seguro <- function(data_path){
    doctorado <- openxlsx::read.xlsx(data_path,sheet = "Doctorado", fillMergedCells = TRUE)
    maestria <- openxlsx::read.xlsx(data_path,sheet = "Maestría", fillMergedCells = TRUE)   
    
    doctorado$Nombre <- paste(doctorado$Apellido.paterno, doctorado$Apellido.materno,doctorado$Nombre)
    
    
    doctorado <- doctorado %>% 
        select(which(!duplicated(names(.)))) %>%
        filter(Estado=="ACTIVO") %>%
        mutate(
            Edad = lapply(CURP, get_edad),
            programa = "DC"
        ) %>%
        select(c(Nombre,Edad, programa))
    
    maestria$Nombre <- paste(maestria$Apellido.paterno, maestria$Apellido.materno,maestria$Nombre)
    maestria <- maestria %>% 
        select(which(!duplicated(names(.)))) %>%
        filter(Estado=="ACTIVO") %>%
        mutate(
            Edad = lapply(CURP, get_edad),
            programa = "MC"
        ) %>%
        select(c(Nombre,Edad, programa))
       
    alumnos <- bind_rows(maestria,doctorado)
    
    return(alumnos)
    
}
