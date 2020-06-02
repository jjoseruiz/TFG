predice<-function(MODELO,DATOS){
  DATOS$LESION=factor(DATOS$LESION)
  set.seed(1924562)
  prediModelo=c(predict(MODELO,DATOS))
  #devuelve un vector de tipo factor indicando que vóxeles son sano y que voxeles son lesion
  return(prediModelo)
}