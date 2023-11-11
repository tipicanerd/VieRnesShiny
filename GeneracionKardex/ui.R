library(pacman)
p_load(shiny, shinythemes)

#### VARIABLES AUXILIARES ####
programas <- c(
    "Doctorado" = "Doctorado",
    "Maestría" = "Maestría"
)


#### PARTES DE LA UI ####
control_archivo <- fileInput("file", "Seleccione un archivo", buttonLabel = "Explorar",
          accept = ".xlsx", placeholder = "No se ha seleccionado archivo" )

control_programa <- selectInput(
    "programa", "Programa",
    choices = programas,
    selected = "Maestría"
)

control_generacion <- numericInput(
    "generacion", "Generación",
    1, min=1, max =100, step = 1
)

control_semestre <- numericInput(
    "n_sem", "Semestre",
    1, min=1, max =5, step = 1
)

control_prom_gen <- fixedRow(
    column(
        width = 6,
        control_programa
    ),
    column(
        width = 6,
        control_generacion
    )
)


control_fecha_inicio <- dateInput(
    "fecha_inicio", "Fecha de inicio de generación",
    format = "dd-MM-yy",
    language = "es",
    value = Sys.Date()
)

control_fecha_semestre <- dateRangeInput(
    "fechas_semestre", "Período del semestre",
    format = "dd-M-yy",
    language = "es",
    min = "2018-02-15",
    separator = "a"
)


#### UI ####
fluidPage(
    theme = shinytheme("sandstone"),
    # Application title
    titlePanel("Generación de Documentos"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
    sidebarPanel(
        control_archivo,
        control_prom_gen,
        control_semestre,
        control_fecha_inicio,
        control_fecha_semestre,
        markdown("
          
          
            ")
    ),
    mainPanel(
        tabsetPanel(
            type="tabs", id="tab",
            tabPanel("KARDEX", value="kardex",
                markdown("
                ### Instrucciones
                1. Subir el archivo de Excel correspondiente a la Matriz de Calificaciones.
                2. Seleccionar el programa y la generación de la cual se desea generar los Kardex.
                3. Seleccionar el semestre del que se desea generar el Kardex.
                4. Seleccionar la fecha correcta del ingreso de la generación (Solo necesario para generar los Kardex de primer semestre).
                5. Seleccionar el periodo del semestre del cual se desea generar el Kardex.
                6. Verificar que los alumnos que se muestran en la tabla sean los correctos.
                7. Hacer clic en el botón _Descargar Carpeta Comprimida_.
                "),
                downloadButton('download',"Descargar Carpeta Comprimida"),
                DT::dataTableOutput("table"),
            ),
            tabPanel("CEFIYA", value="cefiya",
                     markdown("
                ### Instrucciones
                1. Subir el archivo de Excel correspondiente a la Matriz de Calificaciones.
                2. Seleccionar el programa y la generación de la cual se desea generar la cefiya
                3. Seleccionar el semestre del que se desea generar la cefiya.
                4. Seleccionar la fecha correcta del ingreso de la generación.
                5. Seleccionar el periodo del semestre del cual se desea generar el Cefiya.
                6. Verificar que los alumnos que se muestran en la tabla sean los correctos.
                7. Hacer clic en el botón _Descargar Cefiya_.
                "),
                     downloadButton('download_cc',"Descargar Cefiya"),
                     tabsetPanel(type="tabs", id="cefiyas",
                        tabPanel("Calificaciones", value="calificaciones",
                            DT::dataTableOutput("table_cc")
                        ),
                        tabPanel("Inscripción", value="inscripcion",
                                 DT::dataTableOutput("table_ci")
                        ) 
                     ),
            ),
            tabPanel("Base Completa", value="base_completa",
                     markdown("
                ### Instrucciones
                1. Subir el archivo de Excel correspondiente a la Matriz de Calificaciones.
                2. Verificar que los alumnos que se muestran en la tabla sean los correctos.
                3. Hacer clic en el botón _Descargar Base Completa_.
                "),
                     downloadButton('download_bc',"Descargar Base Completa"),
                     DT::dataTableOutput("table_bc")
            ),
            tabPanel("Certificados", value="certificados",
                     markdown("
                ### Instrucciones
                1. Subir el archivo de Excel correspondiente a la Matriz de Calificaciones.
                2. Verificar que los alumnos que se muestran en la tabla sean los correctos.
                3. Agregar el nombre del jefe de la SEP.
                4. Hacer clic en el botón _Descargar Certificados_.
                "),
                     textInput("jefe","Encargado SEP"),
                     downloadButton('download_certificados',"Descargar Certificados"),
                     DT::dataTableOutput("table_certificados"),
            ),
            tabPanel("Datos Seguro", value="datos_seguro",
                     markdown("
                ### Instrucciones
                1. Subir el archivo de Excel correspondiente a la Matriz de Calificaciones.
                2. Verificar que los alumnos que se muestran en la tabla sean los correctos.
                3. Hacer clic en el botón _Descargar Datos Seguro_.
                "),
                     downloadButton('download_ds',"Descargar Datos Seguro"),
                     DT::dataTableOutput("table_ds")
            ),
        ),
    )
    ),
    tags$footer("Hecho con amor 🖤, 2023")
)

