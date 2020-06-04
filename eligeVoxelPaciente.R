eligeVoxelPaciente<-function(FLAIR){
  valores = getPixels(FLAIR,1:dim(FLAIR)[1],1:dim(FLAIR)[2],1:dim(FLAIR)[3])
  umbral=quantile(valores,0.9999)
  cordis=which(valores>umbral)
  return(cordis)
}