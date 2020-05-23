library(neurobase)
library(ANTsRCore)
library(ANTsR)
library(e1071)
source("estaEnLista.R")
source("elegirVoxeles.R")
source("obtenCoord.R")
source("valoresImagen.R")
source("recorreImagenes.R")
source("devuelveImagenes.R")
source("aplicaFuncion.R")
source("sacoValorVecinos.R")
listaFunciones = c(mean,min,max,sd,median)
numero_voxeles = 2500
nSujetos = 30
num_images_sujeto = 6
numImagenes = nSujetos*num_images_sujeto
almacen = matrix(nrow = numero_voxeles*nSujetos,ncol = length(listaFunciones)*num_images_sujeto+1)
nombrecolumnas = c("MEAN_FLAIR","MIN_FLAIR","MAX_FLAIR","SD_FLAIR","MEDIAN_FLAIR",
                   "MEAN_T1","MIN_T1","MAX_T1","SD_T1","MEDIAN_T1",
                   "MEAN_FLAIR_SYM","MIN_FLAIR_SYM","MAX_FLAIR_SYM","SD_FLAIR_SYM","MEDIAN_FLAIR_SYM",
                   "MEAN_FLAIR_ASYM","MIN_FLAIR_ASYM","MAX_FLAIR_ASYM","SD_FLAIR_ASYM","MEDIAN_FLAIR_ASYM",
                   "MEAN_T1_SYM","MIN_T1_SYM","MAX_T1_SYM","SD_T1_SYM","MEDIAN_T1_SYM",
                   "MEAN_T1_ASYM","MIN_T1_ASYM","MAX_T1_ASYM","SD_T1_ASYM","MEDIAN_T1_ASYM",
                   "LESION")
colnames(almacen)<-nombrecolumnas
j=0
for(l in 1:nSujetos){
  rootflairl = paste0("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/","S",l,"_FLAIR_BRAIN.nii.gz")
  roott1l = paste0("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/","S",l,"_T1_BRAIN.nii.gz")
  rootflairsyml=paste0("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/","S",l,"_FLAIR_SIMETRICA.nii.gz")
  rootflairasyml=paste0("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/","S",l,"_FLAIR_ASIMETRICA.nii.gz")
  roott1syml=paste0("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/","S",l,"_T1_SIMETRICA.nii.gz")
  roott1asyml=paste0("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/","S",l,"_T1_ASIMETRICA.nii.gz")
  rootconsensol = paste0("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/CONSENSO/","S",l,"_CONSENSO.nii.gz")
  rots = list(rootflairl,roott1l,rootflairsyml,rootflairasyml,roott1syml,roott1asyml)
  print("Leyendo imagenes")
  mismoSujeto = lapply(rots,antsImageRead)
  consenso =antsImageRead(paste0("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/CONSENSO/","S",l,"_CONSENSO.nii.gz"))
  
  coordenadas = elegirVoxeles(numero_voxeles,consenso)
  vecinos = recorreImagenes(mismoSujeto,coordenadas)
  
  #i será el número de imágenes y j el número de vóxeles por imagen
  dataset = aplicaFuncion(vecinos,listaFunciones)
  #sabemos que los nvoxel/2 eran sanos por lo que, la feature gt será
  
  almacen[(j+1):(j+nrow(dataset)),1:ncol(dataset)]=dataset
  almacen[(j+1):(j+nrow(dataset)/2),ncol(almacen)]=1
  almacen[((j+nrow(dataset)/2)+1):(j+nrow(dataset)),ncol(almacen)]=0
  j=j+nrow(dataset)
  print(paste0("dataset sujeto ",l))
}
write.csv(almacen,"almacenMediano")

