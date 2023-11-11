library(pacman)

p_load(openxlsx, shiny)

source("generacion_base_completa.R")
source("generacion_cefiyas.R")
source("generacion_certificados.R")
source("generacion_kardex.R")



function(input, output, session) {
    
    #### KARDEX ####
    data_file <- reactive({
        if (is.null(input$file)) {
            return("")
        }
        
        programa <- input$programa
        generacion <- paste("G",input$generacion, sep="")
        semestre <- get_semestre_cols(programa,input$n_sem)
        
        return(get_data(input$file$datapath,programa,generacion,semestre))
    })
    
    
    
    output$table <- DT::renderDataTable({
        req(data_file())
        data_file()
    })

    
    
    output$download <- downloadHandler(
        filename = function(){paste(input$programa,input$generacion,"S",input$n_sem,"KARDEX",".zip", sep="")},
        content = function(fname) {
            meses <- c(
                "ENE", "FEB", "MAR", "ABR", "MAY", "JUN",
                "JUL", "AGO", "SEP", "OCT", "NOV", "DIC"
            )
            
            
            programa <- input$programa
            n_sem <- input$n_sem
            semestre <- get_semestre_cols(programa, n_sem)
            alumnos <- data_file()
            template_name <- get_template_name(programa, n_sem)
            
            generacion <- paste("G",input$generacion, sep="")
            
            Lfecha_inicio <- as.numeric(unlist(strsplit(as.character(input$fecha_inicio), "-")))
            if(Lfecha_inicio[3]<10){diafecha_inicio<- paste("0",Lfecha_inicio[3], sep="")} else{diafecha_inicio <- Lfecha_inicio[3]}
            fecha_inicio <- paste(diafecha_inicio,meses[Lfecha_inicio[2]],Lfecha_inicio[1]%%100, sep="-")
            
            Lfinicio <- as.numeric(unlist(strsplit(as.character(input$fechas_semestre[1]), "-")))
            if(Lfinicio[3]<10){diafinicio<- paste("0",Lfinicio[3], sep="")} else{diafinicio <- Lfinicio[3]}
            finicio <- paste(diafinicio,meses[Lfinicio[2]],Lfinicio[1]%%100, sep="-")
            
            Lffin <- as.numeric(unlist(strsplit(as.character(input$fechas_semestre[2]), "-")))
            if(Lffin[3]<10){diaffin<- paste("0",Lffin[3], sep="")} else{diaffin <- Lffin[3]}
            ffin <- paste(diaffin,meses[Lffin[2]],Lffin[1]%%100, sep="-")
            
            docus <- generate_files(template_name, alumnos, programa, generacion, n_sem, semestre, fecha_inicio, finicio, ffin)
            
            #docus <- convert2pdf(docus)
            
            zipfile <- paste(programa,generacion,"S",n_sem,".zip", sep="")
            
            zip(zipfile=fname, files=docus)
        },
        contentType = "application/zip"
    )
    
    #### CEFIYAS ####
    cefiya_cal <- reactive({
        if (is.null(input$file)) {
            return("")
        }
        
        programa <- input$programa
        generacion <- paste("G",input$generacion, sep="")
        semestre <- get_semestre_cols(programa,input$n_sem)
        
        return(get_data_cal(input$file$datapath,programa,generacion,semestre))
    })
    
    cefiya_ins <- reactive({
        if (is.null(input$file)) {
            return("")
        }
        
        programa <- input$programa
        generacion <- paste("G",input$generacion, sep="")
        semestre <- get_semestre_cols(programa,input$n_sem+1)
        
        return(get_data_ins(input$file$datapath,programa,generacion,semestre))
    })
    
    output$table_cc <- DT::renderDataTable({
        req(cefiya_cal())
        cefiya_cal()
    })
    
    output$table_ci <- DT::renderDataTable({
        req(cefiya_ins())
        cefiya_ins()
    })
    
    output$download_cc <- downloadHandler(
        filename = function(){paste(input$programa,input$generacion,"S",input$n_sem,"CEFIYA",".xlsx", sep="")},
        content = function(fname) {
            libro <- list('Materias' = cefiya_cal(), 'Inscripcion' = cefiya_ins())
            write.xlsx(libro, fname)
            
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet (.xlsx)"
    )
    
    #### BASE COMPLETA ####
    base <- reactive({
        if (is.null(input$file)) {
            return("")
        }
        
        return(get_data_base(input$file$datapath))
    })
    
    output$table_bc <- DT::renderDataTable({
        req(base())
        base()
    })

    
    output$download_bc <- downloadHandler(
        filename = function(){paste("baseSEPlista",as.character(Sys.Date()),".xlsx", sep="")},
        content = function(fname) {
            write.xlsx(base(), fname)
            
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet (.xlsx)"
    )
    
    #### BASE COMPLETA ####
    base <- reactive({
        if (is.null(input$file)) {
            return("")
        }
        
        return(get_data_base(input$file$datapath))
    })
    
    output$table_bc <- DT::renderDataTable({
        req(base())
        base()
    })

    
    output$download_bc <- downloadHandler(
        filename = function(){paste("baseSEPlista",as.character(Sys.Date()),".xlsx", sep="")},
        content = function(fname) {
            write.xlsx(base(), fname)
            
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet (.xlsx)"
    )
    
    
    
    #### CERTIFICADOS ####
    data_certificados <- reactive({
        if (is.null(input$file)) {
            return("")
        }
        
        programa <- input$programa
        generacion <- paste("G",input$generacion, sep="")
        
        return(obtener_matriz_calificaciones(input$file$datapath,programa,generacion))
    })
    
    
    
    output$table_certificados <- DT::renderDataTable({
        req(data_certificados())
        data_certificados()
    })
    
    
    
    output$download_certificados <- downloadHandler(
        filename = function(){paste(input$programa,input$generacion,"CERTIFICADOS",".zip", sep="")},
        content = function(fname) {
            
            
            programa <- input$programa
            alumnos <- data_certificados()
            anverso <- get_nombre_certicado(programa)
            generacion <- paste("G",input$generacion, sep="")
            jefe <- input$jefe
            
            fecha_inicio <- as.numeric(unlist(strsplit(as.character(input$fecha_inicio), "-")))
            
            certificados_anverso <- generate_certificados(anverso, alumnos, programa, generacion, fecha_inicio)
            
            reverso <- get_nombre_certicado(programa,r=TRUE)
            certificados_reverso <- generate_certificados_reverso(reverso, alumnos, programa, generacion, jefe)
            
            certificados <- c(certificados_anverso, certificados_reverso)
            
            zipfile <- paste(programa,generacion,"CERTIFICADOS",".zip", sep="")
            
            zip(zipfile=fname, files=certificados)
        },
        contentType = "application/zip"
    )
    
    #### DATOS SEGURO ####
    seguro <- reactive({
        if (is.null(input$file)) {
            return("")
        }
        
        return(get_data_seguro(input$file$datapath))
    })
    
    output$table_ds <- DT::renderDataTable({
        req(seguro())
        seguro() %>% select(-c(programa))
    })
    
    
    output$download_ds <- downloadHandler(
        filename = function(){paste("baseSegurolista",as.character(Sys.Date()),".xlsx", sep="")},
        content = function(fname) {
            alumnos <- seguro()
            maestria <- alumnos %>%
                filter(programa =="MC") %>%
                select(-c(programa))
            doctorado <- alumnos %>%
                filter(programa =="DC") %>%
                select(-c(programa))
            
            libro <- list('Maestría' = maestria, 'Doctorado' = doctorado)
            write.xlsx(libro, fname)
            
            
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet (.xlsx)"
    )
    
}
