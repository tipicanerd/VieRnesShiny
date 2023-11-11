library(pacman)

p_load(dplyr, officer, openxlsx, stringi, zip)


template_path <- "./templates/"

materias <- read.csv("./data/Maestria_LT_Materias.csv", row.names=1, encoding = "UTF-8")

## #+TITLE: 
## #+AUTHOR: Emilio Torres Manzanera
## #+DATE: Time-stamp: <2020-07-18 11:24 emilio on emilio-XPS-15-9570>
## #+TAGS: 
## #+PROPERTY: header-args :results output :exports both :session 
## Not 1 nor 21 in this list.
tablanumeros <- read.delim("./data/listanumeros.txt", sep = "|")[, c(2, 3)]

escribirnumeroenteroliteral <- function(x, uno="un", masculino = TRUE){
    if (abs(x) >= 10^18) { ## Trillions? No, thank you
        return(x)
    } else if (x != round(x)) { ## Decimal number? No, thank you
        return(x)
    } else if (x == 0) {
        return("cero")
    } else if (x < 0) { ## Is it a negative number?
        return(paste0("menos ", escribirnumeroenteroliteral(abs(x), uno, masculino)))
    }
    if (x == 1) { # A very special case: un, uno, una
        if (masculino) {
            if (uno == "un") {
                return("un")
            } else {
                return("uno")
            }
        } else {
            return("una")
        }
    } else if (x <= 100) { # Between 2 and 100
        literal <- tablanumeros[tablanumeros$numero == x, "literal"]
        if (identical(literal, character(0))) {
            ## Not in the table
            if (x == 21) { ## Special case 21
                if (masculino) {
                    if (uno == "un") {
                        return("veintiún")
                    } else {
                        return("veintiuno")
                    }
                } else {
                    return("veintiuna")
                }
            } else { # Between 31 and 99
                filtro <- x / tablanumeros$numero > 1
                numerotablapordebajo <- max(tablanumeros$numero[filtro])
                literal <- tablanumeros[ tablanumeros$numero==numerotablapordebajo, "literal"]
                return(paste0(literal, " y ",
                              escribirnumeroenteroliteral(x - numerotablapordebajo, uno, masculino)))
            }
        } else { # It is in the table 2,..,29,30,40,50,...,90
            return(literal)
        }
    } else if (x < 1000) { # 101,...,999
        literal <- tablanumeros[tablanumeros$numero == x, "literal"]
        if (identical(literal, character(0))) {
            ## No existe en la tabla
            filtro <- x / tablanumeros$numero > 1
            numerotablapordebajo <- max(tablanumeros$numero[filtro])
            literal <- tablanumeros[tablanumeros$numero == numerotablapordebajo, "literal"]
            if (literal == "cien") {
                literal <- "ciento"
            } else {
                literal <- ifelse(masculino, paste0(literal, "tos"), paste0(literal, "tas"))
            }
            return(paste0(literal, " ",
                          escribirnumeroenteroliteral(x - numerotablapordebajo, uno, masculino)))
        } else { ## It is 100, 200, 300,..., 900
            if (literal != "cien") {
                literal <- ifelse(masculino, paste0(literal, "tos"), paste0(literal, "tas"))
            }
            return(literal)
        }
    } else if (x < 10^6) { # 1000, ..., 999 999
        numeropordebajo <- floor(x / 1000)
        literal <- if (numeropordebajo == 1) {
            "mil"
        } else {
            paste0(escribirnumeroenteroliteral(numeropordebajo, uno, masculino), " mil")
        }
        resto <- x - 1000 * numeropordebajo
        if (resto > 0) {
            literal <- paste0(literal, " ", escribirnumeroenteroliteral(resto, uno, masculino))
        }
        return(literal)
    } else if (x < 10^12) {
        numeropordebajo <- floor(x / 10^6)
        literal <- if (numeropordebajo == 1) {
            "un millón"
        } else {
            paste0(escribirnumeroenteroliteral(numeropordebajo, uno, masculino),
                   " millones")
        }
        resto <- x - 10^6 * numeropordebajo
        if (resto > 0) {
            literal <- paste0(literal, " ",
                              escribirnumeroenteroliteral(resto, uno, masculino))
        }
        return(literal)
    } else if (x < 10^18) {
        numeropordebajo <- floor(x / 10^12)
        literal <- if (numeropordebajo == 1) {
            "un billón"
        } else {
            paste0(escribirnumeroenteroliteral(numeropordebajo, uno, masculino),
                   " billones")
        }
        resto <- x - 10^12 * numeropordebajo
        if (resto > 0) {
            literal <- paste0(literal, " ",
                              escribirnumeroenteroliteral(resto, uno, masculino))
        }
        return(literal)
    }
    x
}


get_semestre_cols_cert <- function(programa, semestre){
    switch(
        programa,
        Doctorado={
            switch(
                semestre,
                "1" = return(8:10),
                "2" = return(11:13),
                "3" = return(14:16),
                "4" = return(17:19),
                "5" = return(20:21)
            )
        },
        Maestría={
            switch (semestre,
                    "1" = return(9:12),
                    "2" = return(13:16),
                    "3" = return(17:20),
                    "4" = return(21:22)
            )
        }
    )
}


obtener_matriz_calificaciones <- function(data_path, programa, generacion){
    
    if (programa=="Doctorado"){ inicio <- 1:7}
    else{inicio <- 1:8}
    
    alumnos <- openxlsx::read.xlsx(data_path,sheet = programa, fillMergedCells = TRUE)
    
    
    cols <- alumnos[1,]
    
    colnames(alumnos) <- cols
    
    alumnos <- alumnos %>% 
        select(-Promedio) %>% 
        filter(Gen==generacion)
    
    
    
    return(alumnos)
    
}

get_nombre_certicado <- function(programa, r=FALSE){
    if(programa=="Doctorado"){base_name <- "CERTIFICADO_DC"}
    else{base_name <- "CERTIFICADO_MC"}
    
    if(r){
        base_name <- paste(base_name,"R",sep="")
    }
    
    return(paste(base_name, ".docx", sep=""))
}



generate_certificados <- function(template_name, alumnos, programa, generacion, fecha_inicio){
    num_letra <- c( "CERO",
        "UNO","DOS","TRES","CUATRO","CINCO",
        "SEIS","SIETE","OCHO","NUEVE","DIEZ"
        )
    if (fecha_inicio[2] < 8){
        cs1 <- paste(fecha_inicio[1]-1,fecha_inicio[1],sep="-")
        cs2 <- paste(fecha_inicio[1],fecha_inicio[1]+1,sep="-")
        cs3 <- paste(fecha_inicio[1],fecha_inicio[1]+1,sep="-")
        cs4 <- paste(fecha_inicio[1]+1,fecha_inicio[1]+2,sep="-")
        cs5 <- paste(fecha_inicio[1]+1,fecha_inicio[1]+2,sep="-")
    }else{
        cs1 <- paste(fecha_inicio[1],fecha_inicio[1]+1,sep="-")
        cs2 <- paste(fecha_inicio[1],fecha_inicio[1]+1,sep="-")
        cs3 <- paste(fecha_inicio[1]+1,fecha_inicio[1]+2,sep="-")
        cs4 <- paste(fecha_inicio[1]+1,fecha_inicio[1]+2,sep="-")
        cs5 <- paste(fecha_inicio[1]+2,fecha_inicio[1]+3,sep="-") 
    }
    
    
    ciclos <- c(cs1,cs2,cs3,cs4,cs5)
    plantilla <- officer::read_docx(paste(template_path,template_name, sep=""))
    
    certificados <- c()
    
    meses <- c(
        "ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
        "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"
    )
    
    Lfecha_elaboracion <- as.numeric(unlist(strsplit(as.character(Sys.Date()), "-")))
    
    dia <- toupper(escribirnumeroenteroliteral(Lfecha_elaboracion[3]))
    anio <- toupper(escribirnumeroenteroliteral(Lfecha_elaboracion[1]))
    
    mes <- paste(meses[Lfecha_elaboracion[2]], "DEL AÑO", anio)
    
    if(programa=="Doctorado"){
        inicio_col<-7
        
        for (i in 1:dim(alumnos)[1]){
            
            body_replace_all_text(plantilla,"<<DIA>>", as.character(dia))
            body_replace_all_text(plantilla,"<<MES>>", as.character(mes))
            
            nombre_alumno <- paste(alumnos[i,"Apellido paterno"], alumnos[i,"Apellido materno"],alumnos[i,"Nombre"])

            promedio <- round(as.numeric(alumnos[i,"Promedio Global"]),1)
            body_replace_all_text(plantilla,"<<FC>>", as.character(promedio))
            
            decimal <- round((promedio - floor(promedio))*10)
            promedio_letra <- paste(num_letra[floor(promedio)+1],"PUNTO", num_letra[decimal+1])
            body_replace_all_text(plantilla,"<<FCL>>", promedio_letra)
            
            n_materia <- 1
            
            for (s in 1:5){
                semestre <- get_semestre_cols_cert(programa,s)
                body_replace_all_text(plantilla,paste("<<CS",s,">>",sep=""), ciclos[s])
                for (j in semestre){
                    materia <- paste("<<M",n_materia,">>", sep="")
                    
                    body_replace_all_text(plantilla,materia, colnames(alumnos)[j])
                    
                    cal <- paste("<<C",n_materia,">>", sep="")
                    calificacion <- round(as.numeric(alumnos[i,j]))
                    body_replace_all_text(plantilla,cal,as.character(calificacion))
                    
                    cal_letra <- paste("<<C",n_materia,"L>>", sep="")
                    body_replace_all_text(plantilla,cal_letra, num_letra[calificacion+1])
                    
                    n_materia <- n_materia + 1
                    
                    
                }
            }
            
            filename <- paste(nombre_alumno," ", programa, generacion, ".docx", sep="")
            
            certificados <- append(certificados,filename)
            
            print(plantilla, filename)
            
            plantilla <- officer::read_docx(paste(template_path, template_name, sep=""))
            
        }
        
        
    }
    
    else{
        inicio_col <- 8
        for (i in 1:dim(alumnos)[1]){
            nombre_alumno <- paste(alumnos[i,"Apellido paterno"], alumnos[i,"Apellido materno"],alumnos[i,"Nombre"])
            
            body_replace_all_text(plantilla,"<<DIA>>", as.character(dia))
            body_replace_all_text(plantilla,"<<MES>>", as.character(mes))
            
            promedio <- round(as.numeric(alumnos[i,"Promedio Global"]),1)
            
            body_replace_all_text(plantilla,"<<FC>>", as.character(promedio))
            decimal <- round((promedio - floor(promedio))*10)
            promedio_letra <- paste(num_letra[floor(promedio)+1],"PUNTO", num_letra[decimal+1])
            body_replace_all_text(plantilla,"<<FCL>>", promedio_letra)
            
            n_materia <- 1
            
            for (s in 1:4){
                semestre <- get_semestre_cols_cert(programa,s)
                body_replace_all_text(plantilla,paste("<<CS",s,">>",sep=""), ciclos[s])
                
                for (j in semestre){
                    
                    materia <- paste("<<M",n_materia,">>", sep="")
                    nombre_materia <- colnames(alumnos)[j]
                    
                    if (stri_detect(nombre_materia, regex="LT")){
                        nombre_materia <- materias[alumnos[i,"Programa"], stri_replace(nombre_materia, ".", regex=" ")]
                    }
                    
                    body_replace_all_text(plantilla,materia, nombre_materia)
                    
                    cal <- paste("<<C",n_materia,">>", sep="")
                    calificacion <- round(as.numeric(alumnos[i,j]))
                    body_replace_all_text(plantilla,cal,as.character(calificacion))
                    
                    cal_letra <- paste("<<C",n_materia,"L>>", sep="")
                    body_replace_all_text(plantilla,cal_letra, num_letra[calificacion+1])
                    
                    n_materia <- n_materia + 1
                    
                    
                }
            }
            
            filename <- paste(nombre_alumno," ", programa, generacion, ".docx", sep="")
        
            
            certificados <- append(certificados,filename)
            
            print(plantilla, filename)
            
            plantilla <- officer::read_docx(paste(template_path, template_name, sep=""))
            
        }
        
    }
    
    
    return(certificados)
}

generate_certificados_reverso <- function(template_name, alumnos, programa, generacion, jefe){
    plantilla <- officer::read_docx(paste(template_path,template_name, sep=""))
    certificados <- c()
    for (i in 1:dim(alumnos)[1]){
        nombre_alumno <- paste(alumnos[i,"Apellido paterno"], alumnos[i,"Apellido materno"],alumnos[i,"Nombre"])
        body_replace_all_text(plantilla,"<<NOMBRE>>", nombre_alumno)
        body_replace_all_text(plantilla,"<<JEFE_SEP>>", jefe)
        
        filename <- paste(nombre_alumno," ", programa, generacion, "R", ".docx", sep="")
        
        certificados <- append(certificados,filename)
        
        print(plantilla, filename)
        
        plantilla <- officer::read_docx(paste(template_path, template_name, sep=""))
            
    }
    
    
    return(certificados)
}

