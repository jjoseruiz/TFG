source("valoresImagen.R")
source("sacoValorVecinos.R")
recorreImagenes<-function(IMAGENES,COORDENADAS)
{
  #las IMAGENES VENDRAN EN UNA LISTA 
  #Las coordenadas serán una matriz de coordenadas
  bol=is.matrix(COORDENADAS)
  if(bol){
    mat = matrix(nrow = nrow(COORDENADAS),ncol = length(IMAGENES)*27)
  }else{
    mat = matrix(nrow = length(COORDENADAS),ncol = length(IMAGENES)*27)
  }
  l=0
  for(i in 1:length(IMAGENES)){
    print(paste0("imagen ",i))
    v = valoresImagen(IMAGENES[[i]],COORDENADAS)
    if(bol){
      valores = sacoValorVecinos(IMAGENES[[i]],v)
    }else{
      valores=v
    }
    mat[1:nrow(valores),(1+l):(l+ncol(valores))]=valores
    l=l+ncol(valores)
  }
  return(mat)
}