IMG_consensus = leeImagen(ROOT = "/Users/juanjoseruizpenela/Documents/IMG1/raw_images/","consensus",4)
Si_FLAIR_BRAIN = leeImagen(ROOT = "/Users/juanjoseruizpenela/Documents/IMG1/BRAIN_IMAGES/","FLAIR_BRAIN",4)
Si_T1_BRAIN = leeImagen(ROOT = "/Users/juanjoseruizpenela/Documents/IMG1/BRAIN_IMAGES/","T1_BRAIN",4)

BRAINS_T1=lapply(lB_T1,readnii)
BRAINS_FLAIR=lapply(lB_FLAIR,readnii)
##Normalizado

##
t1_normalizada_zs = zscore_img(S1_T1_BRAIN,IMG_MASK)
##
flair_normalizada_zs = zscore_img(copia_FLAIR,IMG_MASK)
##Simetria
ants_S1_FLAIR_BRAIN=antsImageRead("S1_FLAIR_BRAIN.nii.gz")
asym = reflectImage(ants_S1_FLAIR_BRAIN,1,"AffineFast")$warpedmovout
asimetria = ants_S1_FLAIR_BRAIN-asym
#Acceso a voxel hypercubico
centro = dim(ants_S1_FLAIR_BRAIN)/2
radio = rep(1,3)
cubo = getNeighborhoodAtVoxel(ants_S1_FLAIR_BRAIN,center = centro,radio)

double_ortho(robust_window(readnii("S1_T1_BRAIN")))
#imagenesCorrecion<-correccion(T1)
#double_ortho(T1,imagenesCorrecion)
#WOBABCT1<-fslbet(imagenesCorrecion)
#ortho2(imagenesCorrecion,WOBABCT1, col = "blue")
#reg = extrantsr::registration(filename = FLAIR,template.file = T1,typeofTransform = "Affine",interpolator = "Linear",verbose = FALSE)
#registro full de todas la imgs
T1_imgs = lapply(lT1,readnii)
#T1_imgs_correc=lapply(T1_imgs,correccion)
#no podemos ver las dos imágenes del sujeto 1 simultaneamente debido a que no tienen la misma dimesion

FLAIR_imgs = lapply(lFLAIR,readnii)
FLAIR_S1_Corregida=correccion(FLAIR_imgs[[1]])
#FLAIR_imgs_correc=lapply(FLAIR_imgs,correccion)
#la función registro, realiza un registro de la imágen que se le pasa como argumento respecto a la imagen FLAIR del sujeto 1.
#T1_S1_registrada_F1 = registro(T1_imgs[[1]])
#T1_reg_prueba = lapply(T1_imgs_correc,registro)
#T1_S1_registrada_F1_Corregida=registro(T1_imgs[[1]])
T1_S1_CORREGIDA = correccion(T1_imgs[[1]])
#Leemos las mascaras
Brain_masks=lapply(lBMASK,readnii)
S1_T1_BRAIN = mask_img(img=T1_S1_CORREGIDA,Brain_masks[[1]])
#la mascara y la T1 no tienen las mismas dimensiones
S1_FLAIR_BRAIN = mask_img(FLAIR_S1_Corregida,Brain_masks[[1]])
ortho2(FLAIR_S1_Corregida,S1_FLAIR_BRAIN)
#la máscara con fslbet es peor que utilizando la que viene dada por la fuente
#mask_flair=fslbet(FLAIR_S1_Corregida)
#ortho2(FLAIR_S1_Corregida,mask_flair)
S1_T1_BRAIN=mask_img(T1_S1_registrada_F1_Corregida$outfile,Brain_masks[[1]])
ortho2(T1_S1_registrada_F1_Corregida$outfile,S1_T1_BRAIN)
#En el directorio que queramos, guardamos las imágenes del cerebro extraido.
write_nifti(nim=S1_FLAIR_BRAIN,filename = "S1_FLAIR_BRAIN")
write_nifti(nim=S1_T1_BRAIN,filename = "S1_T1_BRAIN")
##TISSUE SEGMENTATION

##NORMALIZATION


#Prueba preprocesadoPaciente.
library(neurobase)
library(ANTsR)
library(ANTsRCore)
library(extrantsr)
library(oro.nifti)
library(malf.templates)

#PRUEBAS EN LA APP
s1_f = antsImageRead("s1_FLAIR_CORRECTED.nii.gz")

FLAIR = antsImageRead("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/S2_FLAIR_BRAIN.nii.gz")
FLAIR = readnii("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/S2_FLAIR_BRAIN.nii.gz")
FLAIR = antsImageRead(paste0("/Users/juanjoseruizpenela/Documents/IMG1/raw_images/","patient",30,"_FLAIR.nii.gz"))
ortho2(FLAIR_CORREGIDA3)
T1 = antsImageRead("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/S2_T1_BRAIN.nii.gz")
T1 = readnii("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/S2_T1_BRAIN.nii.gz")
FLAIR_CORREGIDA3 = correccion(FLAIR_RAW)
FLAIR_RAW = readnii("/Users/juanjoseruizpenela/Documents/IMG1/raw_images/patient30_FLAIR.nii.gz")
FLAIR_RAW = antsImageRead("/Users/juanjoseruizpenela/Documents/IMG1/raw_images/patient1_FLAIR.nii.gz")
T1_RAW = readnii("/Users/juanjoseruizpenela/Documents/IMG1/raw_images/patient30_T1W.nii.gz")
brain_mask = readnii("/Users/juanjoseruizpenela/Documents/IMG1/raw_images/patient30_brainmask.nii.gz")
brain_mask = antsImageRead("/Users/juanjoseruizpenela/Documents/IMG1/raw_images/patient1_brainmask.nii.gz")
FLAIR_SYM=antsImageRead("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/S2_FLAIR_SIMETRICA.nii.gz")
FLAIR_SYM=readnii("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/S2_FLAIR_SIMETRICA.nii.gz")
FLAIR_ASYM = antsImageRead("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/S2_FLAIR_ASIMETRICA.nii.gz")
FLAIR_ASYM = readnii("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/S2_FLAIR_ASIMETRICA.nii.gz")
T1_SYM = antsImageRead("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/S2_T1_SIMETRICA.nii.gz")
T1_SYM = readnii("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/S2_T1_SIMETRICA.nii.gz")
T1_ASYM = antsImageRead("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/S2_T1_ASIMETRICA.nii.gz")
T1_ASYM = readnii("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/SIMETRIA/S2_T1_ASIMETRICA.nii.gz")
consenso= antsImageRead("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/CONSENSO/S2_CONSENSO.nii.gz")
papayar::papaya(list(FLAIR,consenso))
listaImagenes = list(FLAIR,T1,FLAIR_SYM,FLAIR_ASYM,T1_SYM,T1_ASYM)
multi_overlay(listaImagenes)
rm(FLAIR)
rm(T1)
rm(FLAIR_SYM)
rm(FLAIR_ASYM)
rm(T1_SYM)
rm(T1_ASYM)
source("obtenCoord.R")
source("predice.R")
source("preprocesadoPaciente.R")
source("recorreImagenes.R")
source("eligeVoxelPaciente.R")
source("valoresImagen.R")
source("sacoValorVecinos.R")
source("aplicaFuncion.R")
source("resultado.R")
#proceso para predecir
segme = antsImageRead("segmentacion_s2.nii.gz")
red0.5 = alpha()
ortho2(FLAIR,segme==2,col.y = red0.5)
cordis = eligeVoxelPaciente(listaImagenes[[1]])
vecinos = recorreImagenes(listaImagenes,cordis)
listaFunciones = c(mean,min,max,sd,median)
features = aplicaFuncion(vecinos,listaFunciones)
lesiones=c(rep(1,nrow(features)))
features = cbind2(features,lesiones)
#modelo = readRDS("Knn_dataset2500.rds")
modelo = readRDS("RandomForest5000_r2_v3.rds")
modelo = readRDS("Bayesian5000_r2_v3.rds")
modelo = readRDS("Knn5000_r2_v3.rds")
prediccion_knn = predice(modelo,features)
mask = resultado(listaImagenes[[1]],cordis,prediccion)
#Representacion
papayar::papaya(list(FLAIR,mask))
#eliminamos todos los warnings que nos sale del bayesiano -1. Para activarlos 0.
#options(warn = -1)



#prueba comité
#solo tiene sentido hacer la moda cuando hay más de 2 clasificadores.
if(!is.null(predi_RF)&!is.null(prediKnn)&!is.null(prediBayes)){
  print("hay randomforest, KNN y NB")
  listaPred = list(predi_RF,prediKnn,prediBayes)
  comite = c(1:length(cordis))
  for(i in 1:length(comite)){
    comite[i]=mfv(c(listaPred[[1]][[i]],listaPred[[2]][[i]],listaPred[[3]][[i]]))
  }
}



#K-MEANS
idx_foregorund<-FLAIR>0
v<-FLAIR[idx_foregorund]
cl<-kmeans(x=v,centers = 3)


#
centers<-cl$centers
o<-order(centers)
cluster<-cl$cluster
cluster_new<-cluster
for(i in 1:3){
  cluster_new[cluster==o[i]]<-i
}



imagen_nueva<-FLAIR

imagen_nueva[idx_foregorund]<-cluster_new
imagen_nueva[imagen_nueva<0]<-0


ortho2(imagen_nueva==2)

ortho2(flair)

ortho2(FLAIR)
flair=correccion(FLAIR)
library(fslr)
ruta = "/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/BRAIN_IMAGES/S2_FLAIR_BRAIN.nii.gz"
seg_nobias = fslr::fast_nobias(file = ruta)
seg_fast=fast(file = FLAIR,type = "T2")
corr_n4 = correccion(FLAIR)
seg_nobias = fast_nobias()

segme = antsImageRead("segmentacion_s2.nii.gz")

red0.5=scales::alpha("red",0.5)

##
segmentacion = otropos(FLAIR,MASK)
segme = antsImageRead("segmentacion_otropos.nii.gz")
#otropos == 2 --> WhiteMatter
writenii(segmentacion$segmentation,"segmentacion_otropos")
###########