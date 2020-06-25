eligeVoxelPaciente<-function(FLAIR){
  #Eliminamos los valores fondo
  seg = fslr::fast(FLAIR)
  #seg = segme
  mascara_cereb = FLAIR>min(FLAIR)
  valores = FLAIR[mascara_cereb]
  #calculamos el umbral a partir del cual es probable que los vóxeles sean lesion
  umbral = quantile(valores,0.75)+0.75*IQR(valores)
  #calculamos las coordenadas de esos vóxeles
  #cordis= which(FLAIR>=umbral)
  cordis = which((seg == 2 | seg == 3) & (FLAIR>=umbral))

  #wm=which(im==TRUE)
  #cordis = which(is.element(wm,mayor_)==TRUE)
  #cordis = mayor_[cordis]
  return(cordis)
}