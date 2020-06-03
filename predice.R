predice<-function(MODELO,DATOS){
  nombrecolumnas = c("MEAN_FLAIR","MIN_FLAIR","MAX_FLAIR","SD_FLAIR","MEDIAN_FLAIR",
                     "MEAN_T1","MIN_T1","MAX_T1","SD_T1","MEDIAN_T1",
                     "MEAN_FLAIR_SYM","MIN_FLAIR_SYM","MAX_FLAIR_SYM","SD_FLAIR_SYM","MEDIAN_FLAIR_SYM",
                     "MEAN_FLAIR_ASYM","MIN_FLAIR_ASYM","MAX_FLAIR_ASYM","SD_FLAIR_ASYM","MEDIAN_FLAIR_ASYM",
                     "MEAN_T1_SYM","MIN_T1_SYM","MAX_T1_SYM","SD_T1_SYM","MEDIAN_T1_SYM",
                     "MEAN_T1_ASYM","MIN_T1_ASYM","MAX_T1_ASYM","SD_T1_ASYM","MEDIAN_T1_ASYM",
                     "LESION")
  colnames(DATOS)<-nombrecolumnas
  #DATOS$LESION=factor(DATOS$LESION)
  set.seed(1924562)
  prediModelo=c(predict(MODELO,DATOS))-1
  #devuelve un vector de tipo factor indicando que vóxeles son sano y que voxeles son lesion
  return(prediModelo)
}