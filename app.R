#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#librerias
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(RNifti)
library(neurobase)
library(ANTsR)
library(extrantsr)
library(caret)
library(randomForest)
library(tree)
library(dplyr)
library(modeest)
library(papayar)
library(papayaWidget)
library(OpenImageR)
library(oro.nifti)
library(malf.templates)
source("eligeVoxelPaciente.R")
source("preprocesadoPaciente.R")
source("obtenCoord.R")
source("predice.R")
source("aplicaFuncion.R")
source("resultado.R")
source("recorreImagenes.R")
source("valoresImagen.R")

ui <- fluidPage(theme=shinytheme("cerulean"),
                titlePanel("Aplicación web para detección de lesiones en Esclerosis Múltiple"),
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
                               Es por esto, que en ocasiones los médicos expertos en diagnóstico por imágenes, tienen serias dificultades para interpretar las lesiones ocasionadas por la enfermedad en el cerebro."
                              )),
                             sidebarLayout(titlePanel("Descripción"),mainPanel(tags$hr(),width=24,
                               "MS LESION DETECTION es una aplicación web enfocada a la 
                               detección de lesiones cerebrales relacionadas con la Esclerosis Múltiple 
                               vía Imágenes de resonancia Magnética basándose en modelos de",em("Machine Learning")
                               )),
                             sidebarLayout(titlePanel("Flujo de funcionamiento"),mainPanel(tags$hr(),
                              withSpinner(plotOutput("imagenINICIO",width=800,height = 700))
                             ))),
                    tabPanel("Subir Imagenes",
                             titlePanel("Sube aquí sus imágenes"),
                               "Recuerde que sus imágenes deben tener extensión .nii o .nii.gz",
                             tags$hr(),
                             fluidRow(column(4,fileInput("ImagenFlair","FLAIR", multiple = FALSE, accept = c(".nii",".nii.gz"),placeholder = "Suba su imagen FLAIR")),
                                      column(4,fileInput("ImagenT1","T1", multiple = FALSE, accept = c(".nii",".nii.gz"),placeholder = "Suba su imagen T1")),
                                      column(4,actionButton("botonSubir",tags$base("Subir imagenes")),
                                             tags$h6(textOutput(outputId="clics")))
                             ),
                             fluidRow(column(6,withSpinner(plotOutput("plotFLAIR",width = 400,height = 400))),
                                      column(6,withSpinner(plotOutput("plotT1",width = 400,height = 400))),
                             )),
                    tabPanel("Preprocesado",
                             sidebarLayout(titlePanel("¿Cómo vamos a tratar sus imágenes?"),mainPanel(tags$hr(),width=24,
                               tags$article("En esta parte del proceso, aplicaremos a sus imágenes el siguiente flujo de subrutinas.")),
                             ),
                            tags$h5("Diagrama de flujo del pre-procesado"),
                            withSpinner(plotOutput("flujoPreprocesado",width = 1000,height = 600)),
                            actionButton("preprocesado",tags$base("Comenzar preprocesado")),
                            tags$h6(textOutput("preTerminado"))
                    ),
                    tabPanel("Obtención de características",
                             titlePanel("¿Cómo vamos a sacar las características de las imágenes?"),
                             mainPanel(tags$hr(),width = 24,tags$article("A continuación, la aplicación sacará las características de las MRI siguiendo los pasos de la figura inferior."),
                               tags$h5("Diagrama de flujo de obtención de características"),
                               withSpinner(plotOutput("flujoDataset",width=800,height = 700)),
                               actionButton("executeFeatures",tags$base("Obtención dataset")),
                               tags$h6(textOutput("features"))
                             )),
                    tabPanel("Predicción",
                             sidebarLayout(titlePanel("¿Qué modelos de Machine Learning desea aplicar?"),mainPanel(
                               tags$hr(),checkboxGroupInput(inputId = "ml",label="Clasificadores",choiceNames  = list("Random Forest","K-Nearest-Neighbor","Naïve Bayes"),choiceValues = list("rf","knn","nb"),selected = list("rf","knn","nb")),
                               fluidRow(column(6,actionButton("cargaModelos",tags$base("Cargar seleccionados")),
                               tags$h6(textOutput("textCargado")))
                               ),withSpinner(plotOutput("imagePrediccion",width=400,height =400))
                             ))),
                    tabPanel("Resultados",
                      #RANDOMFOREST
                        fluidRow(
                        column(6,offset = 2, tags$h5("RANDOM FOREST")),actionButton("prediRF","Resultados RF")),tags$hr(),
                        withSpinner(papayaOutput("resRF",width = 600, height = 500)),
                      #K-NEAREST-NEIGHBOR
                        fluidRow(
                        column(6,offset = 2, tags$h5("KNN")),actionButton("prediKNN","Resultados K-N-N")),tags$hr(),
                        withSpinner(papayaOutput("resKNN",width = 600, height = 500)),
                      #NAIVE BAYES
                        fluidRow(
                        column(6,offset = 2,tags$h5("NAIVE BAYES")),actionButton("prediNB","Resultados NB")),tags$hr(),
                        withSpinner(papayaOutput("resNB",width = 600, height = 500)),
                      #EXPERTOS
                        fluidRow(
                        column(6,offset = 2, tags$h5("Comité Expertos")),actionButton("prediEXP","Resultados EXPERTOS")),tags$hr(),
                        withSpinner(papayaOutput("resComite",width = 600, height = 500))
                      )))
                
server <- function(input, output,session) {
  options(shiny.maxRequestSize = 500*1024^2)
  output$imagenINICIO<-renderPlot({
    flux = OpenImageR::readImage("Flujo de aplicacion.jpg")
    imageShow(flux)
  })
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
        "Suba ambas imágenes porfavor"
      }
    ,message = "Cargando imágenes al sistema")
  })

  #Muestra la imagen FLAIR subida
  output$plotFLAIR<-renderPlot({
    if(!is.null(input$ImagenFlair) & !is.null(input$ImagenT1)){
      ortho2(app_imagenes()[[1]])
    }
  })

  #Muestra la Imagen T1 Subida
  output$plotT1<-renderPlot({
    if(!is.null(input$ImagenFlair) & !is.null(input$ImagenT1)){
      ortho2(app_imagenes()[[2]])
    }  })
  observeEvent(input$botonSubir,{
    output$clics<-renderText({
      if(!is.null(input$ImagenFlair) & !is.null(input$ImagenT1)){
        "Imágenes subidas con éxito. Continue en la ventana de Preprocesasdo"
      }else{
        "Suba ambas imágenes porfavor"
      }
    })
    print("he clickeado")
  })
  
  #Preprocesado
  imagenes<-eventReactive(input$preprocesado,{
    withProgress(
      if(TRUE){
        #listaImag=listaImagenes
        listaImag=preprocesadoPaciente(app_imagenes())
        return(listaImag)
      }
    , message = "Preprocesando las Imágenes")
  })
  output$flujoPreprocesado<-renderPlot({
    flux = OpenImageR::readImage("Flujo preprocesado.png")
    imageShow(flux)
  })
  observeEvent(input$preprocesado,{
    output$preTerminado<-renderText({
      if(length(imagenes())==6){
        print("Imágenes preprocesadas con éxito. Pase a la siguiente ventana.")
      }else{
        print("Carge sus imágenes en la ventana 'SUBIR IMÁGENES'")
      }
    })
    print(length(imagenes()))
  })
  
  
  #Obtención Características
  output$flujoDataset<-renderPlot({
    flux = OpenImageR::readImage("flujo obtenciondataset.png")
    imageShow(flux)
  })
  coordenadas<-eventReactive(input$executeFeatures,{
    eligeVoxelPaciente(imagenes()[[1]])
    })

  datosPaciente<-eventReactive(input$executeFeatures,{
    withProgress(
      if(TRUE){
        #elegimos los vóxeles basandonos en los picos de la flair
        #coordenadas=eligeVoxelPaciente(imagenes()[[1]])
        if(!is.null(coordenadas())){
          datos=recorreImagenes(imagenes(),coordenadas())
          features=aplicaFuncion(datos,c(mean,min,max,sd,median))
          lesiones=c(rep(0,nrow(features)))
          features = cbind2(features,lesiones)
        }
        return(features)
      },
      message = "Sacando características",
      detail = "Esto puede tardar unos minutos")
  })
  observeEvent(input$executeFeatures,{
    output$features<-renderText({
      "Obtención terminada. Pase a la ventana de Predicción."
    })
  })

  observeEvent(input$executeFeatures,{
    print("obtencaract")
    print(ncol(datosPaciente()))
    print((head(datosPaciente())))
    sol = matrix(datosPaciente(),nrow = nrow(datosPaciente()),ncol=ncol(datosPaciente()))
    print(head(sol))
  })
  
  #Cargar modelos
  modeloRf<-eventReactive(input$cargaModelos,{
    #modelos = input$ml
    withProgress(
      if(TRUE){
        val=which(is.element(input$ml,"rf")==TRUE)
        if(length(val>0)){
              modelRf=readRDS("RandomForest5000_r2_v3.rds")
              return(modelRf)
         }
      },
      message = "Cargando RANDOM FOREST. ",
      detail = "Este proceso puede tardar unos minutos")
  })
  modeloKnn<-eventReactive(input$cargaModelos,{
    #modelos = input$ml
    withProgress(
      if(TRUE){
        val=which(is.element(input$ml,"knn")==TRUE)
        if(length(val)>0){
            knn=readRDS("Knn5000_r2_v3.rds")
            return(knn)
          }
    },
      message = "Cargando KNN. ",
      detail = "Este proceso puede tardar unos minutos")
  })
  modeloNb<-eventReactive(input$cargaModelos,{
    withProgress(
      if(TRUE){
        val=which(is.element(input$ml,"nb")==TRUE)
        if(length(val)>0){
            nb=readRDS("Bayesian5000_r2_v3.rds")
            return(nb)
          }
      },
      message = "Cargando NAIVE BAYES.",
      detail = "Este proceso puede tardar unos minutos")
  })
  
  output$textCargado<-eventReactive(input$cargaModelos,{
    if(is.null(input$ml)){
      "Selecciona algún modelo."
    }else{
      "Modelos cargados. Continue en la ventana de Resultados."
    }
  })
  output$imagePrediccion<-renderPlot({
    flux = OpenImageR::readImage("prediccion.jpg")
    imageShow(flux)
  })
  observeEvent(input$cargaModelos,{
    print("he clickeado cargamodelos")
    #print(input$ml[[2]]=="knn")
    print(paste0("Cargado Naive Bayes = ",!is.null((modeloNb()))))
    print(paste0("Cargado Random Forest = ",!is.null(modeloRf())))
    print(paste0("Cargado Knn = ",!is.null(modeloKnn())))
  })
  #PREDICCION
  resultadoRF<-eventReactive(input$prediRF,{
    print("compruebo RF")
    if(!is.null(modeloRf())){
      withProgress(
        if(TRUE){
            print("RandomForest")
            predi_rf=predice(modeloRf(),datosPaciente())
            print("FIN PREDICCION RF")
            return(predi_rf)
        },message = "Prediciendo con Random Forest",
        detail = "Esto puede tardar algunos minutos."
      )
    }
  })
  resultadoNB<-eventReactive(input$prediNB,{
    print("compruebo NB")
    if(!is.null(modeloNb())){
      withProgress(
        if(TRUE){
          print("NB")
          #eliminamos los warings
          options(warn = -1)
          predi_nb=predice(modeloNb(),datosPaciente())
          print("FIN PREDICCION NB")
          #print(predi_nb)
          return(predi_nb)
        },message = "Prediciendo con Naive Bayes.",
        detail = "Esto puede tardar algunos minutos."
      )
    }
  })
  resultadoKnn<-eventReactive(input$prediKNN,{
    print("compruebao knn")
    if(!is.null(modeloKnn())){
      withProgress(
        if(TRUE){
          print("KNN")
          predi_knn=predice(modeloKnn(),datosPaciente())
          print("FIN PREDICCION KNN")
          return(predi_knn)
        },message = "Prediciendo con KNN.",
        detail = "Esto puede tardar algunos minutos."
      )
    }
  })
  observeEvent(input$prediEXP,{
    print("he pinxao comité")
    print(is.null(resultadoRF()))
    print(is.null(resultadoKnn()))
    #print(is.null(resultadoNB()))
  })
  resultadoEXP<-eventReactive(input$prediEXP,{
    #solo tiene sentido hacer la moda cuando hay más de 2 clasificadores.
    if(!is.null(resultadoRF())&!is.null(resultadoKnn())&!is.null(resultadoNB())){
      print("hay randomforest, KNN y NB")
      listaPred = list(resultadoRF(),resultadoKnn(),resultadoNB())
      comite = c(1:length(coordenadas()))
      for(i in 1:length(comite))
      {
        comite[i]=mfv(c(listaPred[[1]][[i]],listaPred[[2]][[i]],listaPred[[3]][[i]]))
      }
    }
      return(comite)
  })
  
  #REPRESENTACIÓN
  output$resRF<-renderPapaya({
    if(!is.null(resultadoRF())){
      withProgress(
        if(TRUE){
          print("MOSTRANDO RF")
          mask = resultado(imagenes()[[1]],coordenadas(),resultadoRF())
          papaya(list(imagenes()[[1]],mask))
        },message = "Representando Imagen RF. ",
        detail = "Esto puede tardar unos segundos."
      )
    }
  })
  output$resNB<-renderPapaya({
    if(!is.null(resultadoNB())){
      withProgress(
        if(TRUE){
          print("MOSTRANDO NB")
          mask = resultado(imagenes()[[1]],coordenadas(),resultadoNB())
          papaya(list(imagenes()[[1]],mask))
        },message = "Representando Imagen NB. ",
        detail = "Esto puede tardar unos minutos."
        )
    }
  })
  output$resKNN<-renderPapaya({
    if(!is.null(resultadoKnn())){
      withProgress(
        if(TRUE){
          print("MOSTRANDO KNN")
          mask = resultado(imagenes()[[1]],coordenadas(),resultadoKnn())
          papaya(list(imagenes()[[1]],mask))
          #resultado(FLAIR,COORDENADAS,predi_knn)
        },message=("Representando Imagen KNN. "),
        detail = "Esto puede tardar unos segundos."
      )
    }
  })
  observeEvent(input$prediEXP,{
    print("pruebbb")
    print(is.null(resultadoEXP()))
    print(resultadoEXP())
  })
  output$resComite<-renderPapaya({
    print("representado comité")
    if(!is.null(resultadoEXP())){
      withProgress(
        if(TRUE){
          mask = resultado(imagenes()[[1]],coordenadas(),resultadoEXP())
          papaya(list(imagenes()[[1]],mask))
        },message = "Representando resultados del Comité de Expertos. ",
        detail = "Esto puede tardar unos segundos."
      )
    }
})
  
  #Cuando el usuario hace click en Comenzar preprocesado, se realiza el prepro y se generan las demás imágenes
  print("desppues")
}
# Run the application 
shinyApp(ui = ui, server = server)