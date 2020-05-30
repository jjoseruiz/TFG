library(neurobase)
library(ANTsR)
library(extrantsr)
library(malf.templates)
source("correccion.R")
source("eligeVoxelPaciente.R")
source("obtencionDatasetPaciente.R")
source("preprocesadoPaciente.R")
source("recorreImagenes.R")
source("aplicaFuncion.R")
source("obtenCoord.R")
num_images_sujeto=6
paciente<-function(FLAIR,T1){
  listaFunciones = c(mean,min,max,sd,median)
  imagenes=preprocesadoPaciente(list(FLAIR,T1))
  #sabiendo que el primer elemento de la lista de imagenes es la flair.
  coordenadas = eligeVoxelPaciente(imagenes[[1]])
  vecinos = recorreImagenes(imagenes,coordenadas)
  datasetPaciente = matrix(nrow = length(coordenadas),ncol = length(listaFunciones)*num_images_sujeto)
  
  datasetPaciente = aplicaFuncion(vecinos,listaFunciones)
  nombrecolumnas = c("MEAN_FLAIR","MIN_FLAIR","MAX_FLAIR","SD_FLAIR","MEDIAN_FLAIR",
                     "MEAN_T1","MIN_T1","MAX_T1","SD_T1","MEDIAN_T1",
                     "MEAN_FLAIR_SYM","MIN_FLAIR_SYM","MAX_FLAIR_SYM","SD_FLAIR_SYM","MEDIAN_FLAIR_SYM",
                     "MEAN_FLAIR_ASYM","MIN_FLAIR_ASYM","MAX_FLAIR_ASYM","SD_FLAIR_ASYM","MEDIAN_FLAIR_ASYM",
                     "MEAN_T1_SYM","MIN_T1_SYM","MAX_T1_SYM","SD_T1_SYM","MEDIAN_T1_SYM",
                     "MEAN_T1_ASYM","MIN_T1_ASYM","MAX_T1_ASYM","SD_T1_ASYM","MEDIAN_T1_ASYM","LESION")
  colnames(datasetPaciente)<-nombrecolumnas
  datasetPaciente
  return(datasetPaciente)
}