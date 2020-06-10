eligeVoxelPaciente<-function(FLAIR){
  #Eliminamos los valores fondo
  valores = FLAIR[FLAIR>0]
  #calculamos el umbral a partir del cual es probable que los vóxeles sean lesion
  umbral = quantile(valores,0.15)+3*IQR(valores)
  #umbral = quantile(valores,0.999)
  #calculamos las coordenadas de esos vóxeles
  cordis = which(FLAIR>=umbral)
  return(cordis)
}