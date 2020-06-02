#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(neurobase)
library(ANTsR)
library(extrantsr)
library(RNifti)
library(caret)
library(randomForest)
library(shinythemes)


read_image_as_array<-function(path){
  nift=antsImageRead(path)
  if(length(dim(nift))==3) return(nift[,,])
  return(nift[,,,])
}

ui <- fluidPage(theme=shinytheme("cerulean"),
                titlePanel("Detección de lesiones"),
                navbarPage("MS LESION DETECTION",
                    tabPanel("Inicio",
                             sidebarLayout(titlePanel("Introducción"),mainPanel(tags$hr(),width=24,
                               "La Esclerosis Múltiple (MS) es una enfermedad autoinmune 
                               que degrada las vainas de mielina de las neuronas del Sistema Nervioso Central 
                               afectado a las conexiones nerviosas. Esto hace que la enfermedad se manifieste 
                               en cada persona de manera diferente y con síntomas impredecibles tales como dolores, 
                               pérdida de memoria, hormigueo o incluso parálisis.  A pesar de los grandes avances 
                               en medicina y tecnología, aún se desconoce el origen de la enfermedad, sin embargo, 
                               podemos estudiarla y diagnosticarla a través de herramientas como las Imágenes de Resonancia Magnética. 
                               Es por esto que, en ocasiones los médicos expertos en diagnóstico por imágenes, tengan serias dificultades para interpretar las lesiones ocasionadas por la enfermedad en el cerebro."
                              )),
                             sidebarLayout(titlePanel("Descripción"),mainPanel(tags$hr(),width=24,
                               "MS LESION DETECTION es una aplicación web enfocada a la 
                               detección de lesiones cerebrales relacionadas con la Esclerosis Múltiple 
                               vía Imágenes de resonancia Magnética basandonos en modelos de machine Learning"
                               )),
                             sidebarLayout(titlePanel("Flujo de funcionamiento"),mainPanel(
                               tags$div(img(src="Flujo de aplicacion,jpg"))
                             ))),
                    tabPanel("Subir Imagenes",
                             titlePanel("Sube aquí sus imágenes"),
                               "Recuerde que sus imágenes deben tener extensión .nii o .nii.gz",
                             tags$hr(),
                             fluidRow(column(4,fileInput("ImagenFlair","FLAIR", multiple = FALSE, accept = c(".nii",".nii.gz"),placeholder = "Suba su imagen FLAIR")),
                                      column(4,fileInput("ImagenT1","T1", multiple = FALSE, accept = c(".nii",".nii.gz"),placeholder = "Suba su imagen T1")),
                                      column(4,actionButton("botonSubir","Subir imagenes"),
                                             tags$h6(textOutput(outputId="clics")))
                             ),
                             fluidRow(column(6,withSpinner(plotOutput("plotFLAIR",width = 400,height = 400,"FLAIR"))),
                                      column(6,withSpinner(plotOutput("plotT1",width = 400,height = 400,"T1"))),
                             )),
                    tabPanel("Preprocesado",
                             sidebarLayout(titlePanel("Descripción"),mainPanel(tags$hr(),width=24,
                               tags$article("Aquí prepararemos sus imágenes. En este proceso, aplicaremos a sus imágenes el siguiente flujo de subrutinas. Esto puede tardar varios minutos.
                                aquí pondría alguna señal de por que parte va el procesado de las imágenes/loading correcion n3 90%",
                               ),
                               actionButton("preprocesado","Comenzar Prepreocesado"),
                               tags$h6(textOutput("preprocesando"))),
                               #tags$h4("Flujo de Preprocesado"),
                             ),
                            withSpinner(imageOutput("flujoPreprocesado")),
                    ),
                    tabPanel("Obtención de características",
                             titlePanel("Descripción"),
                             mainPanel("A continuación, la aplicación sacará las características de las MRI.",
                               actionButton("executeFeatures","Obtención dataset"),
                               tags$h6(textOutput("features"))
                             )),
                    tabPanel("Predicción",
                             sidebarLayout(titlePanel("¿Qué modelos de Machine Learning desea aplicar?"),mainPanel(
                               checkboxGroupInput(inputId = "ml",label="Clasificadores",choiceNames  = list("Random Forest","K-Nearest-Neighbor","Naïve Bayes"),choiceValues = list("rf","knn","nb"),selected = list("rf","knn","nb")),
                               fluidRow(column(6,actionButton("cargaModelos",tags$h5("Carga seleccionados")),
                               tags$h6(textOutput("textCargado"))),
                               column(6,actionButton("predice",tags$h5("Predecir Lesiones")))
                               )
                             ))),
                    tabPanel(
                      "Resultados",
                        fluidRow(
                        column(3,tags$h5("RANDOM FOREST")),
                        column(3,tags$h5("NAIVE BAYES")),
                        column(3,tags$h5("KNN")),
                        column(3,tags$h5("Comité Expertos"),textOutput("nombrez"))
                        )
                    )))
                
server <- function(input, output,session) {
  options(shiny.maxRequestSize = 500*1024^2)
  
  #Subir imagenes
  app_imagenes<-eventReactive(input$botonSubir,{
    withProgress(
      if(!is.null(input$ImagenFlair) & !is.null(input$ImagenT1)){
        datapath=input$ImagenFlair$datapath
        datapath2=input$ImagenT1$datapath
        if(tools::file_ext(datapath)=="gz" & tools::file_ext(datapath2)=="gz"){
          datapath=sub("gz$","nii.gz",datapath)
          datapath2=sub("gz$","nii.gz",datapath2)
          file.rename(input$ImagenFlair$datapath,datapath)
          file.rename(input$ImagenT1$datapath,datapath2)
        }
        FLAIR=antsImageRead(datapath)
        T1=antsImageRead(datapath2)
        lista=list(FLAIR,T1)
        return(lista)
      }else{
        #FLAIR<-read_image_as_array(input$ImagenFlair)
        "Suba ambas imágenes porfavor"
      }
    ,message = "Cargando imágenes al sistema")
  })
  output$clics<-eventReactive(input$botonSubir,{
    if(!is.null(input$ImagenFlair) & !is.null(input$ImagenT1)){
      "Imágenes subidas con éxito. Continue en la ventana de Preprocesasdo"
    }else{
      "Suba ambas imágenes porfavor"
    }
  })
  #Muestra la imagen FLAIR subida
  output$plotFLAIR<-renderPlot({
    ortho2(app_imagenes()[[1]])
  })
  #Muestra la Imagen T1 Subida
  output$plotT1<-renderPlot({
    ortho2(app_imagenes()[[2]])
  })
  observeEvent(input$botonSubir,{
    print("he clickeado")
  })
  
  #Preprocesado
  imagenes<-eventReactive(input$preprocesado,{
    withProgress(
      if(TRUE){
        listaImagenes=list()
        #listaImagenes=preprocesadoPaciente(app_imagenes)
        return(listaImagenes)
      }
    , message = "Preprocesando las Imágenes")
  })
  output$imagenPreprocesado<-renderPlot({
    list(src="/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/TFG/MS_LESION_DETECTION/Flujo preprocesado.png",contentType = "image/png",width=400,height=400)
  })
  output$preprocesando<-eventReactive(input$preprocesado,{
    "Iniciando preprocesado."
  })
  
  
  #Obtención Características
  datosPaciente<-eventReactive(input$executeFeatures,{
    datos=prueba
    #datos=preprocesadoPaciente(imagenes)
    return(datos)
  })
  output$features<-eventReactive(input$executeFeatures,{
    paste0("Comenzando Extracción de Características. ",
    "Este proceso puede tardar varios minutos.")
  })
  
  
  #Cargar modelos
  modelos<-eventReactive(input$cargaModelos,{
    #modelos = input$ml
    withProgress(
      if(TRUE){
        model=list()
        if(!is.null(input$ml)){
          for(i in 1:length(input$ml)){
            if(input$ml[[i]]==("rf")){
              modelRf=readRDS("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/TFG/RandomForest_dataset2500.rds")
            }
            return(modelRf)
          }
        }
      },
      message = "Cargando y ejecutando los modelos",
      detail = "Este proceso puede tardar unos minutos")
  })
  modeloKnn<-eventReactive(input$cargaModelos,{
    #modelos = input$ml
    withProgress(
      if(TRUE){
        if(!is.null(input$ml)){
          for(i in 1:length(input$ml)){
            if(input$ml[[i]]==("knn")){
              knn=readRDS("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/TFG/Knn_dataset2500.rds")
            }
            retrun(knn)
          }
        }
      },
      message = "Cargando y ejecutando los modelos",
      detail = "Este proceso puede tardar unos minutos")
  })
  modeloNb<-eventReactive(input$cargaModelos,{
    withProgress(
      if(TRUE){
        if(!is.null(input$ml)){
          for(i in 1:length(input$ml)){
            if(input$ml[[i]]==("nv")){
              nb=readRDS("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/TFG/Bayesian_dataset2500.rds")
            }
            return(nb)
          }
        }
      },
      message = "Cargando y ejecutando los modelos",
      detail = "Este proceso puede tardar unos minutos")
  })
  
  output$textCargado<-eventReactive(input$cargaModelos,{
    if(is.null(input$ml)){
      "Selecciona algún modelo."
    }
  })
  observeEvent(input$cargaModelos,{
    print("he clickeado cargamodelos")
    print(input$ml[[1]]=="rf")
    print(str(modelos()))
  })
  #Cuando el usuario hace click en Comenzar preprocesado, se realiza el prepro y se generan las demás imágenes
  print("desppues")
}

# Run the application 
shinyApp(ui = ui, server = server)