#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(neurobase)
library(ANTsR)
library(extrantsr)
library(RNifti)
library(shinythemes)
RFmodel=readRDS("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/TFG/RandomForest_dataset2500.rds")


read_image_as_array<-function(path){
  nift=antsImageRead(path)
  if(length(dim(nift))==3) return(nift[,,])
  return(nift[,,,])
}

ui <- fluidPage(theme=shinytheme("cerulean"),
                titlePanel("Detección de lesiones"),
                navbarPage("MS LESION DETECTION",
                    tabPanel("Inicio",titlePanel("Introducción"),
                             "La Esclerosis Múltiple (MS) es una enfermedad autoinmune que degrada las vainas de mielina de las neuronas del Sistema Nervioso Central, afectado aí a las conexiones nerviosas. 
                             Esto hace que la enfermedad se manifieste en cada persona de manera diferente y con síntomas impredecibles tales como dolores, pérdida de memoria, hormigueo o incluso parálisis. 
                             A pesar de los grandes avances en medicina y tecnología, aún se desconoce el origen de la enfermedad, no obstante, podemos estudiar y diagnosticar si una persona la padece o podrá padecerla en un futuro.
                             ",titlePanel("Aplicación"),
                             "Esta web es una aplicación web basada en modelos de machine learning que utilizan clasificadores con gran reputación como RandomForest, K-nearest-neighbour, otros... para detectar lesiones cerebrales en pacientes con Esclerosis Múltiple.",
                             img(src="rstudio.png",height=140,width=400)),
                    tabPanel("Subir Imagenes",
                             mainPanel(
                                 tags$h1("Sube aquí sus imágenes"),
                                 "Recuerde que sus imágenes deben tener extensión .nii o .nii.gz",
                                 tags$hr(),
                                 fileInput("ImagenFlair","FLAIR", multiple = FALSE, accept = c(".nii",".nii.gz"),placeholder = "Suba su imagen FLAIR"),
                                 fileInput("ImagenT1","T1", multiple = FALSE, accept = c(".nii",".nii.gz"),placeholder = "Suba su imagen T1"),
                                 actionButton("botonSubir","Subir imagenes"),
                                 actionLink("gonext","Siguiente paso",icon = icon("arrow-alt-circle-right")),                                 
                                 tags$h6(textOutput(outputId="clics")),
                                 actionButton("botonComprobar","Compruebo"),
                                 tags$h6(textOutput(outputId = "compr"))
                                 )),
                    tabPanel("Preprocesado",
                             titlePanel("Descripción"),mainPanel("Aquuí prepararemos sus imágenes. Esto puede tardar varios minutos.",
                                                                  "aquí pondría alguna señal de por que parte va el procesado de las imágenes/loading correcion n3 90%",
                                                                  actionButton("Execute_Prepro","Comenzar Prepreocesado"))
          
                    ),
                    tabPanel("Obtención de características",
                             actionButton("executeFeatures","Obtención dataset")
                    ),
                    tabPanel("Predicción",
                             titlePanel("¿Qué modelos de Machine Learning desea aplicar?"),
                             checkboxGroupInput(inputId = "ml",label="Clasificadores",choiceNames  = list("Random Forest","k-nearest-neighbor","Naïve Bayes"),choiceValues = list("rf","knn","nb"),selected = list("rf","knn","nb")),
                             actionButton("executeClass","Aplica Predicción")
                    ),
                    tabPanel(
                      "Resultados",
                        fluidRow(
                        column(3,tags$h6("RANDOM FOREST")),
                        column(3,tags$h6("NAIVE BAYES")),
                        column(3,tags$h6("KNN")),
                        column(3,tags$h6("Comité Expertos"),textOutput("nombrez"))
                        )
                    )))
                
server <- function(input, output,session) {
  options(shiny.maxRequestSize = 500*1024^2)
  print("antes")
  #cuando el usuario hace click en Subir Imagenes, estas se cargan 
  FLAIR=NULL
  T1=NULL
  app_imagenes<-eventReactive(input$botonSubir,{
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
  })
  output$clics<-eventReactive(input$botonSubir,{
    if(!is.null(input$ImagenFlair) & !is.null(input$ImagenT1)){
      "Imágenes subidas con éxito. Continue en la ventana de Preprocesasdo"
    }else{
      "Suba ambas imágenes porfavor"
    }
  })
  observeEvent(input$botonSubir,{
    print("he clickeado")
    print(str(app_imagenes()[[1]]))
  })
  #Cuando el usuario hace click en Comenzar preprocesado, se realiza el prepro y se generan las demás imágenes
  print("desppues")
}

# Run the application 
shinyApp(ui = ui, server = server)