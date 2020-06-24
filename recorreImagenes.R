recorreImagenes<-function(IMAGENES,COORDENADAS)
{
  vecinos_xvoxel = 125
  #las IMAGENES VENDRAN EN UNA LISTA 
  #Las coordenadas serán una matriz de coordenadas
  bol=is.matrix(COORDENADAS)
  if(bol){
    mat = matrix(nrow = nrow(COORDENADAS),ncol = length(IMAGENES)*vecinos_xvoxel)
  }else{
    mat = matrix(nrow = length(COORDENADAS),ncol = length(IMAGENES)*vecinos_xvoxel)
  }
  l=0
  for(i in 1:length(IMAGENES)){
    print(paste0("imagen ",i))
    v = valoresImagen(IMAGENES[[i]],COORDENADAS)
    if(bol){
      #valores = sacoValorVecinos(IMAGENES[[i]],v)
      valores = v
    }else{
      valores=v
    }
    mat[1:nrow(valores),(1+l):(l+ncol(valores))] = valores
    l=l+ncol(valores)
  }
  return(mat)
}