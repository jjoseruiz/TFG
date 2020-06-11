source("obtenCoord.R")
resultado<-function(IMAGEN,COORDENADAS,PREDICCION){
  cordenadas=c(COORDENADAS)
  prediccion=c(PREDICCION)
  valoresLesion=which(cordenadas&prediccion==TRUE)
  lesiones=array(data=rep(0,dim(IMAGEN)[1]*dim(IMAGEN)[2]*dim(IMAGEN)[3]),dim=dim(IMAGEN))
  for(i in 1:length(valoresLesion)){
    cor=obtenCoord(cordenadas[valoresLesion[[i]]],IMAGEN)
    lesiones[cor[1],cor[2],cor[3]]=1
  }
  copi=maskImage(IMAGEN,lesiones)
  copi[copi>0]=1
  #ortho2(FLAIR,copi,col.y = "red",useRaster = TRUE)
  return(copi)
}