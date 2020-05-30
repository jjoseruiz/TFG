preprocesadoPaciente<-function(listaImagenes){
  library(neurobase)
  library(ANTsR)
  library(extrantsr)
  library(malf.templates)
  source("correccion.R")
  #Correccion
  flair=correccion(listaImagenes[[1]])
  t1=correccion(listaImagenes[[2]])
  #Registro
  wtx = antsRegistration(s1_FLAIR_CORRECTED,flair,typeofTransform = "AffineFast")
  wtt1 = antsRegistration(s1_FLAIR_CORRECTED,t1,typeofTransform = "AffineFast")
  flairReg=antsApplyTransforms(s1_FLAIR_CORRECTED,flair,transformlist = wtx$fwdtransforms)
  t1Reg=antsApplyTransforms(s1_FLAIR_CORRECTED,t1,transformlist = wtt1$fwdtransforms)
  #Extraccion
  #Generando máscaras
  imgs=mass_images(n_templates=5)
  mFlair=extrantsr::malf(
    infile = flairReg,
    template.images = imgs$images,
    template.structs = imgs$mask,
    keep_images = FALSE
  )  
  mT1=extrantsr::malf(
    infile = t1Reg,
    template.images = imgs$images,
    template.structs = imgs$mask,
    keep_images = FALSE
  )
  flair_extr=maskImage(flairReg,mFlair)
  t1_extr=maskImage(t1Reg,mT1)
  #Normalizado
  FLAIRFINAL=zscore_img(flair_extr)
  T1FINAL=zscore_img(t1_extr)
  #Simetrias
  mf=min(FLAIRFINAL)
  mt=min(T1FINAL)
  FLAIR_POS=FLAIRFINAL-mf+1
  T1_POS=T1FINAL-mt+1
  #FLAIR
  reflejoFlair = reflectImage(FLAIR_POS,0,"AffineFast",verbose=TRUE)
  reflejoFlair=reflejoFlair$warpedmovout+mf-1
  asymFlair=FLAIRFINAL-reflejoFlair
  #T1
  reflejoT1=reflectImage(T1_POS,0,"AffineFast",verbose=TRUE)
  reflejoT1=reflejoT1$warpedmovout+mt-1
  asymT1=T1FINAL-reflejoT1
  lista=list(FLAIRFINAL,T1FINAL,reflejoFlair,asymFlair,reflejoT1,asymT1)
  return (lista)
}