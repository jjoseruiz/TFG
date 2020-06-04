source("obtenCoord.R")
resultado<-function(IMAGEN,COORDENADAS,PREDICCION){
  cordenadas=c(COORDENADAS)
  prediccion=c(PREDICCION)
  valoresLesion=which(cordenadas&prediccion==TRUE)
  lesiones=array(data=rep(0,192*512*512),dim=c(192,512,512))
  for(i in 1:length(valoresLesion)){
    cor=obtenCoord(cordenadas[valoresLesion[[i]]],IMAGEN)
    lesiones[cor[1],cor[2],cor[3]]=1
  }
  copi=maskImage(FLAIR,lesiones)
  ortho2(FLAIR,copi,col.y = "red",useRaster = TRUE)
}