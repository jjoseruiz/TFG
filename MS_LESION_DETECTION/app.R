#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ANTsR)
library(shinythemes)
# Define UI for application that draws a histogram
read_image_as_array<-function(path){
  nift=antsImageRead(path)
  if(length(dim(nift))==3)return (nift[,,])
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
                             ),
                    tabPanel("Subir Imagenes",
                             mainPanel(
                                 tags$h1("Sube aquí sus imágenes"),
                                 "Recuerde que sus imágenes deben tener extensión .nii o .nii.gz",
                                 tags$hr(),
                                 fileInput("ImagenFlair","FLAIR", multiple = FALSE, accept = c(".nii",".nii.gz"),placeholder = "Imagen FLAIR subida con éxito"),
                                 fileInput("ImagenT1","T1", multiple = FALSE, accept = c(".nii",".nii.gz"),placeholder = "Imagen T1 subida con éxito"),
                                 actionLink("gonext","Siguiente paso",icon = icon("arrow-alt-circle-right"))                                 
                             )),
                    tabPanel("Preprocesado",
                             titlePanel("Descripción"),
                             "Estamos preparando sus imágenes. Esto puede tardar varios minutos.",
                             "aquí pondría alguna señal de por que parte va el procesado de las imágenes/loading correcion n3 90%"),
                    tabPanel("Predicción",
                             titlePanel("¿Qué modelos de Machine Learning desea aplicar?"),
                             checkboxGroupInput(inputId = "ml",label="Clasificadores",choiceNames  = list("Random Forest","k-nearest-neighbor","Naïve Bayes"),choiceValues = list("rf","knn","nb"),selected = list("rf","knn","nb"))
                    ),
                    tabPanel("Resultados",
                             mainPanel(
                               sidebarLayout (h6("Random Forest"),mainPanel = mainPanel(
                                 
                               )),
                               sidebarLayout(h6("knn"),mainPanel = mainPanel(
                                 
                               )),
                               sidebarLayout(h6("Bayes"),mainPanel = mainPanel(
                                 
                               ))
                             ),
                             ))
)
server <- function(input, output) {
  options(shiny.maxRequestSize = 500*1024^2)
  
    app_flair<-reactive({
      if(is.null(input$ImagenFlair)){
        out<-read_img_as_array(input$ImagenFlair)
      }else{
        datapath=input$ImagenFlair$datapath
        if(tools::file_ext(datapath)=="gz"){
          file.rename(input$ImagenFlair$datapath,datapath)
        }
        out<-read_image_as_array(datapath)
      }
      return (out)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
